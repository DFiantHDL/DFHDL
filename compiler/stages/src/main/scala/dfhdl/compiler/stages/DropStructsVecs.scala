package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFTypeAny, widthIntParam, IntParam, get}
import dfhdl.compiler.ir.DFVal.Alias.ApplyIdx
import scala.collection.mutable

/** This stage drops all structs and (non-Bit) vectors that are not standard block-ram accesses. It
  * drops them by flattening them into Bits.
  */
case object DropStructsVecs extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    object StructOrVecVal:
      def unapply(dfVal: DFVal)(using MemberGetSet): Boolean = dfVal.dfType match
        // all structs are dropped
        case _: DFStruct => true
        // all vectors are dropped, except for var with block-ram and an initial value cast to vector
        case _: DFVector =>
          dfVal match
            case BlockRamVar()                              => false
            case initialVal @ InitialValueOf(BlockRamVar()) =>
              initialVal match
                case initVal: DFVal.Alias.AsIs if initVal.isAnonymous => false
                case _                                                => true
            case _ => true
        case _ => false
    end StructOrVecVal
    val replacementMap = mutable.Map.empty[DFVal, DFVal]
    val handledPartials = mutable.Set.empty[DFVal]
    object PartialSel:
      import DFVal.Alias.*
      def unapply(partial: ApplyIdx | ApplyRange | SelectField)(using MemberGetSet): Boolean =
        replacementMap.contains(partial.relValRef.get) && !handledPartials.contains(partial)
    ///////////////////////////////////////////////////////////////////////////////
    // Stage 1: Replace structs and vectors with Bits
    ///////////////////////////////////////////////////////////////////////////////
    // replacementMap maps the updated DFVal to the original DFVal
    val patchList: List[(DFMember, Patch)] = designDB.members.collect {
      case dfVal @ StructOrVecVal() =>
        val dsn = new MetaDesign(
          dfVal,
          Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
        ):
          val width = dfVal.dfType.asFE[DFTypeAny].widthIntParam
          val updatedDFType = DFBits(width.ref)
          val updatedDFVal = dfVal match
            case const: DFVal.Const =>
              val updatedData =
                const.dfType.dataToBitsData(const.data.asInstanceOf[const.dfType.Data])
              const.copy(dfType = updatedDFType, data = updatedData)
            case _ => dfVal.updateDFType(updatedDFType)
          replacementMap += (updatedDFVal -> dfVal)
          plantMember(updatedDFVal)
        dsn.patch
    }
    val stage1 = designDB.patch(patchList)
    ///////////////////////////////////////////////////////////////////////////////
    // Stage 2: Replace partial references with Bits
    ///////////////////////////////////////////////////////////////////////////////
    locally {
      import stage1.getSet
      given RefGen = RefGen.fromGetSet
      // we need to reverse the list because we want to handle the innermost partial first
      val patchList2: List[(DFMember, Patch)] = stage1.members.view.reverse.collect {
        case partial @ PartialSel() =>
          val dsn = new MetaDesign(
            partial,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
          ):
            // looping through the partial references to find the outermost related value and its index
            var currentPartial = partial
            var relVal = currentPartial.relValRef.get
            var idxLow: IntParam[Int] = 0
            var explore: Boolean = true
            while (explore)
              currentPartial match
                case elemSel: DFVal.Alias.ApplyIdx =>
                  val elemIdxVal = elemSel.relIdx.get
                  val elemIdx = elemIdxVal.getConstData match
                    case Some(Some(idx: BigInt)) if elemIdxVal.isAnonymous =>
                      idx.toInt.asInstanceOf[IntParam[Int]]
                    case _ => elemIdxVal.asValAny.asInstanceOf[IntParam[Int]]
                  val elemWidth = elemSel.asValAny.widthIntParam
                  val relValWidth = relVal.asValAny.widthIntParam
                  idxLow = (relValWidth - elemWidth * (elemIdx + 1)) + idxLow
                    .asInstanceOf[IntParam[Int]]
                case rangeSel: DFVal.Alias.ApplyRange =>
                  val elemWidth =
                    replacementMap(relVal).dfType.asInstanceOf[DFVector]
                      .cellType.asFE[DFTypeAny].widthIntParam
                  val relValWidth = relVal.asValAny.widthIntParam
                  idxLow = (relValWidth - elemWidth * (rangeSel.idxHighRef.get + 1)) + idxLow
                    .asInstanceOf[IntParam[Int]]
                case fieldSel: DFVal.Alias.SelectField =>
                  var relBitLow: IntParam[Int] = idxLow
                  val dfType = replacementMap(relVal).dfType.asInstanceOf[DFStruct]
                  dfType.fieldMap.toList.reverse.exists((fieldName, fieldType) =>
                    val relWidth = fieldType.asFE[DFTypeAny].widthIntParam
                    val relBitHigh = ((relWidth + relBitLow) - 1).asInstanceOf[IntParam[Int]]
                    if (fieldName == fieldSel.fieldName)
                      idxLow = relBitLow
                      true
                    else
                      relBitLow = relBitLow + relWidth
                      false
                  )
              end match
              relVal match
                case nextPartial @ PartialSel() if nextPartial.isAnonymous =>
                  handledPartials += nextPartial
                  currentPartial = nextPartial
                  relVal = currentPartial.relValRef.get
                  explore = true
                case _ =>
                  explore = false
            end while
            val requireCast = partial.dfType match
              case _: DFBits   => false
              case _: DFVector => false
              case _: DFStruct => false
              case _           => true
            val bitsMeta = if (requireCast) partial.meta.anonymize else partial.meta
            val idxHigh: IntParam[Int] =
              (partial.asValAny.widthIntParam + idxLow - 1).asInstanceOf[IntParam[Int]]
            val bitsVal =
              dfhdl.core.DFVal.Alias.ApplyRange(
                relVal.asValOf[Bits[Int]],
                idxHigh.cloneAnonValueAndDepsHere,
                idxLow.cloneAnonValueAndDepsHere
              )(using
                dfc.setMeta(bitsMeta)
              )
            if (requireCast)
              dfhdl.core.DFVal.Alias.AsIs.forced(partial.dfType, bitsVal.asIR)(using
                dfc.setMeta(partial.meta)
              )
          dsn.patch
      }
        // TODO: we need to reverse the list to avoid the issue of the partial references being replaced before the related value
        // maybe patch can be fixed to handle this
        .toList.reverse
      stage1.patch(patchList2)
    }
  end transform
end DropStructsVecs

extension [T: HasDB](t: T)
  def dropStructsVecs(using CompilerOptions): DB =
    StageRunner.run(DropStructsVecs)(t.db)
