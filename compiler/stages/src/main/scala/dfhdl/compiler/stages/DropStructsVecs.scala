package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFTypeAny, widthIntParam, IntParam, get, DFValAny}
import dfhdl.compiler.ir.DFType as irDFType
import dfhdl.compiler.ir.DFVector as irDFVector
import scala.collection.mutable
import scala.collection.immutable.ListMap
import dfhdl.core.DFVal.Func.Op as FuncOp

/** This stage drops all structs and (non-Bit) vectors that are not standard block-ram accesses. It
  * drops them by flattening them into Bits.
  */
case object DropStructsVecs extends GlobalStage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
  // NOTE: `DropWholeVecAssign` must run BEFORE this stage (it unrolls whole-vector constant drives
  // while the vector type is still there to unroll, and the flattening below would otherwise turn
  // its cell selections into variable-bound part-selects). That ordering is expressed by their
  // relative positions in `BackendPrepStage`, and NOT as a dependency here: `DropWholeVecAssign`
  // depends on `ToED`, which would drag the whole pre-backend pipeline into every direct
  // `.dropStructsVecs` invocation (its spec included).
  override def dependencies: List[Stage] = List(ExplicitRomVar)
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  def transformGlobal(designDB: DB)(using co: CompilerOptions, refGen: RefGen): DB =
    object StructOrVecVal:
      def unapply(dfVal: DFVal)(using MemberGetSet): Boolean = dfVal.dfType match
        // all structs are dropped
        case _: DFStruct => true
        // all vectors are dropped, except for var with block-ram.
        // also anonymous initial vector values are specially handled within the declaration
        // patch, so we skip over them here
        case _: DFVector =>
          dfVal match
            // case BlockRamVar()                          => false
            case InitialValueOf(_) if dfVal.isAnonymous => false
            case _                                      => true
        case _ => false
    end StructOrVecVal
    // replacementMap maps the updated (Bits) DFVal back to the original struct/vec
    // DFVal. Shared across every sub-DB so the stage-2 partial rewrite resolves a
    // partial's (possibly cross-design global) related value uniformly.
    val replacementMap = mutable.Map.empty[DFVal, DFVal]
    val handledPartials = mutable.Set.empty[DFVal]
    // A global struct/vec value lives (by identity) in every referencing sub-DB's
    // closure. Its stage-1 replacement patch is built ONCE and reused for every
    // such sub-DB, so all of them get the SAME replacement member object and
    // `newToOld` dedups it to a single global (building a fresh replacement per
    // sub-DB would emit duplicate, divergent globals).
    val globalStage1Patch = mutable.Map.empty[DFVal, (DFMember, Patch)]
    object PartialSel:
      import DFVal.Alias.*
      def unapply(partial: ApplyIdx | ApplyRange | SelectField)(using MemberGetSet): Boolean =
        partial.relValRef.get match
          case relVal if replacementMap.contains(relVal) => true
          // a selection into a Bits-typed link of the chain (a bits field select or a
          // vector bits-cell select, which are never themselves replaced) is part of the
          // chain as well: left out, it would select into the folded range selection,
          // an illegal chained select in Verilog (a select must consume a declared
          // dimension of a name). a NAMED link legally breaks the chain, since it is
          // emitted as its own net declaration
          case chainLink: (ApplyIdx | ApplyRange | SelectField) if chainLink.isAnonymous =>
            unapply(chainLink)
          case _ => false

    ///////////////////////////////////////////////////////////////////////////////
    // Stage 1: Replace structs and vectors with Bits
    ///////////////////////////////////////////////////////////////////////////////
    def stage1Patch(dfVal: DFVal)(using MemberGetSet): (DFMember, Patch) =
      val dsn = new MetaDesign(
        dfVal,
        Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
      ):
        def updateArg(arg: DFVal): DFValAny = arg.dfType match
          // Structs and Vectors will be replaced with Bits in a different patch
          case _: (DFStruct | DFVector | DFBitsWL) => arg.asValAny
          case _ if !arg.isAnonymous               => arg.asValAny.bits
          case _                                   => arg.asValAny.bits
        def typeToBits(dfType: irDFType): DFTypeAny =
          val width = dfType.asFE[DFTypeAny].widthIntParam
          DFBits(width.ref).asFE[DFTypeAny]
        def recurToBits(dfVal: DFVal): DFVal =
          val updatedDFType = typeToBits(dfVal.dfType)
          dfVal match
            // update constant data to bits
            case const: DFVal.Const =>
              val updatedData =
                const.dfType.dataToBitsData(const.data.asInstanceOf[const.dfType.Data])
              dfhdl.core.DFVal.Const.forced(updatedDFType, updatedData)(using
                dfc.setMeta(const.meta)
              ).asIR
            // update vector concatenation arguments to bits as well
            case concat @ DFVal.Func(op = FuncOp.++, args = args) =>
              val updatedArgs = args.map(a => updateArg(a.get))
              dfhdl.core.DFVal.Func(updatedDFType, FuncOp.++, updatedArgs)(using
                dfc.setMeta(concat.meta)
              ).asIR
            // update vector repeated argument to bits as well
            case repeat @ DFVal.Func(
                  op = FuncOp.repeat,
                  args = repeatedArgRef :: repeatCntArgRef :: Nil
                ) =>
              val repeatedArg = repeatedArgRef.get
              val updatedArgs = List(updateArg(repeatedArg), repeatCntArgRef.get.asValAny)
              dfhdl.core.DFVal.Func(updatedDFType, FuncOp.repeat, updatedArgs)(using
                dfc.setMeta(repeat.meta)
              ).asIR
            // for other values, just update the DFType
            case _ => plantMember(dfVal.updateDFType(updatedDFType.asIR))
          end match
        end recurToBits
        // memoize the block ram variable check
        val isBlockRamVar = dfVal match
          case BlockRamVar() => true
          case _             => false
        val updatedDFVal = dfVal match
          // declarations are special cased to handle the initial value
          case dcl: DFVal.Dcl if dcl.initRefList.nonEmpty || isBlockRamVar =>
            val updatedDclType =
              // for block ram variables, we keep the type as-is, unless the cell type is a struct or vector,
              // in which case we convert it to bits.
              if (isBlockRamVar) dcl.dfType match
                case dfType @ irDFVector(cellType = cellType: (DFVector | DFStruct)) =>
                  dfType.copy(cellType = typeToBits(cellType).asIR)
                case dfType => dfType
              else typeToBits(dcl.dfType).asIR
            // block ram variables are not replaced with bits, because they are handled
            // by the verilog backend, but the initial value is converted to bits. so we
            // need to cast it back to the block ram vector type for the IR to be legal,
            // and later ignore the casting in the verilog backend.
            def toVector(initVal: DFVal): DFVal =
              if (isBlockRamVar) dfhdl.core.DFVal.Alias.AsIs.forced(updatedDclType, initVal)
              else initVal
            val updatedInits = dcl.initRefList.view.map(_.get).map {
              // for block ram variables, this initial value appears to be already casting from
              // bits to vector, so it is already in the correct type.
              case asIs: DFVal.Alias.AsIs if asIs.isAnonymous && isBlockRamVar => asIs
              // anonymous initial values are converted to bits, and maybe cast back to vector,
              // if the declaration is a block ram variable (see `toVector`).
              case initVal if initVal.isAnonymous => toVector(recurToBits(initVal))
              // named initial values only may need to be cast to vector (see `toVector`).
              case initVal => toVector(initVal)
            }.map(_.asConstAny).toList
            val modifier = new dfhdl.core.Modifier(dcl.modifier)
            dfhdl.core.DFVal.Dcl(updatedDclType.asFE[DFTypeAny], modifier, updatedInits)(using
              dfc.setMeta(dcl.meta)
            ).asIR
          case _ => recurToBits(dfVal)
        // block ram variables are not replaced with bits, because they are handled
        // by the verilog backend
        if (!isBlockRamVar)
          replacementMap += (updatedDFVal -> dfVal)
      dsn.patch
    end stage1Patch

    // A `length` query over a vector is folded into the vector's element-count parameter
    // value: the drop retargets the query's argument to the flattened Bits replacement,
    // whose own width/length is the vector's TOTAL width, so the element count must be
    // materialized while the vector type still exists. (A `width` query needs no fold:
    // flattening preserves the total width, and the pre-SV dialects inline it at print.)
    def lengthFoldPatch(func: DFVal.Func, vecType: DFVector)(using
        MemberGetSet
    ): (DFMember, Patch) =
      val dsn = new MetaDesign(
        func,
        Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
      ):
        // a fresh clone for an anonymous element-count cone: the original stays referenced
        // by the (block-ram) vector type, and an anonymous value may be read exactly once
        val lengthParam = vecType.cellDimParamRefs.head.get.cloneAnonValueAndDepsHere
        lengthParam.toScalaIntOpt match
          case Some(int) =>
            dfhdl.core.DFConstInt32(int, named = true)(using dfc.setMeta(func.meta))
          case None =>
            // parametric element count: rebind the referenced value under the query's meta
            dfhdl.core.DFVal.Alias.AsIs.ident(lengthParam.toDFConst(using dfc.anonymize))(using
              dfc.setMeta(func.meta)
            )
      dsn.patch
    end lengthFoldPatch

    val stage1Subs: ListMap[StaticRef, DB] = ListMap.from(
      designDB.subDBs.iterator.map { (key, sub) =>
        val patchList = sub.atGetSet {
          sub.members.collect {
            case dfVal @ StructOrVecVal() =>
              // a global value is replaced once and the same patch reused across
              // every sub-DB that holds it (see `globalStage1Patch`)
              if (dfVal.isGlobal) globalStage1Patch.getOrElseUpdate(dfVal, stage1Patch(dfVal))
              else stage1Patch(dfVal)
            case func @ DFVal.Func(op = FuncOp.length, args = List(DFRef(DFVector.Val(vecType)))) =>
              lengthFoldPatch(func, vecType)
          }
        }
        key -> sub.patch(patchList)
      }
    )
    val stage1Root = designDB.update(subDBs = stage1Subs)

    ///////////////////////////////////////////////////////////////////////////////
    // Stage 2: Replace partial references with Bits
    ///////////////////////////////////////////////////////////////////////////////
    def stage2Patch(
        partial: DFVal.Alias.ApplyIdx | DFVal.Alias.ApplyRange | DFVal.Alias.SelectField
    )(using MemberGetSet): (DFMember, Patch) =
      val dsn = new MetaDesign(
        partial,
        Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
      ):
        // the low index of an unreplaced Bits-typed chain link (a bits field select or a
        // vector bits-cell select), against which the selection's absolute indices are
        // translated; a nonzero low arises only from an explicit BitsHL construction
        def bitsLinkLow(relVal: DFVal): IntParam[Int] = relVal.dfType match
          case bt: DFBitsWL => bt.lowIdxRef.get
          case _            => 0
        // looping through the partial references to find the outermost related value and its index
        var currentPartial = partial
        var relVal = currentPartial.relValRef.get
        var idxLow: IntParam[Int] = 0
        var explore: Boolean = true
        while (explore)
          currentPartial match
            case elemSel: DFVal.Alias.ApplyIdx =>
              val elemIdxVal = elemSel.relIdx.get
              val elemIdx = elemIdxVal.getConstData[Option[BigInt]].toOption match
                case Some(Some(idx: BigInt)) if elemIdxVal.isAnonymous =>
                  idx.toInt.asInstanceOf[IntParam[Int]]
                case _ => elemIdxVal.asValAny.asInstanceOf[IntParam[Int]]
              if (replacementMap.contains(relVal))
                // vector cell selection: cells are packed MSB-first
                val elemWidth = elemSel.asValAny.widthIntParam
                val relValWidth = relVal.asValAny.widthIntParam
                idxLow = (relValWidth - elemWidth * (elemIdx + 1)) +
                  idxLow
                    .asInstanceOf[IntParam[Int]]
              else
                // bit selection into a Bits-typed chain link: the index is absolute
                // in the link's own [low, high] range, so translate by the link's low
                idxLow = (elemIdx - bitsLinkLow(relVal)) +
                  idxLow
                    .asInstanceOf[IntParam[Int]]
            case rangeSel: DFVal.Alias.ApplyRange =>
              if (replacementMap.contains(relVal))
                // vector cell range selection: indices are in cell units, MSB-first
                val elemWidth =
                  replacementMap(relVal).dfType.asInstanceOf[DFVector]
                    .cellType.asFE[DFTypeAny].widthIntParam
                val relValWidth = relVal.asValAny.widthIntParam
                idxLow = (relValWidth - elemWidth * (rangeSel.idxHighRef.get + 1)) +
                  idxLow
                    .asInstanceOf[IntParam[Int]]
              else
                // range selection into a Bits-typed chain link: absolute indices,
                // translated by the link's low
                idxLow = (rangeSel.idxLowRef.get - bitsLinkLow(relVal)) +
                  idxLow
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
        // a single-bit selection folds into a bit selection rather than a degenerate
        // one-bit range selection: a bit selection stays legal in v95/v2001 even with
        // a runtime index, where a part-select requires constant bounds
        val bitSelFold = partial match
          case _: DFVal.Alias.ApplyIdx =>
            partial.dfType match
              case DFBit | DFBool => true
              case _              => false
          case _ => false
        val requireCast = partial.dfType match
          case _: DFBitsWL         => false
          case _: DFVector         => false
          case _: DFStruct         => false
          case DFBit if bitSelFold => false
          case _                   => true
        val bitsMeta = if (requireCast) partial.meta.anonymize else partial.meta
        val bitsValIR: DFVal =
          if (bitSelFold)
            dfhdl.core.DFVal.Alias.ApplyIdx(
              dfhdl.core.DFBit,
              relVal.asValAny,
              idxLow.cloneAnonValueAndDepsHere.toDFConst(using dfc.anonymize)
            )(using dfc.setMeta(bitsMeta)).asIR
          else
            val idxHigh: IntParam[
              Int
            ] = (partial.asValAny.widthIntParam + idxLow - 1).asInstanceOf[IntParam[Int]]
            dfhdl.core.DFVal.Alias.ApplyRange(
              relVal.asValOf[Bits[Int]],
              idxHigh.cloneAnonValueAndDepsHere,
              idxLow.cloneAnonValueAndDepsHere
            )(using dfc.setMeta(bitsMeta)).asIR
        if (requireCast)
          dfhdl.core.DFVal.Alias.AsIs.forced(partial.dfType, bitsValIR)(using
            dfc.setMeta(partial.meta)
          )
      dsn.patch
    end stage2Patch

    val stage2Subs: ListMap[StaticRef, DB] = ListMap.from(
      stage1Root.subDBs.iterator.map { (key, sub) =>
        // we need to reverse the list because we want to handle the innermost partial first
        val patchList2 = sub.atGetSet {
          sub.members.view.reverse.collect {
            case partial @ PartialSel() if !handledPartials.contains(partial) =>
              stage2Patch(partial)
          }
            // TODO: we need to reverse the list to avoid the issue of the partial references being replaced before the related value
            // maybe patch can be fixed to handle this
            .toList.reverse
        }
        key -> sub.patch(patchList2)
      }
    )
    stage1Root.update(subDBs = stage2Subs)
  end transformGlobal
end DropStructsVecs

extension [T: HasDB](t: T)
  def dropStructsVecs(using CompilerOptions): DB =
    StageRunner.run(DropStructsVecs)(t.db)
