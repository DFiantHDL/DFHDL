package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.ApplyIdx
import dfhdl.compiler.stages.verilog.VerilogDialect
import scala.collection.mutable

/** This stage creates a ROM variable for a constant vector access pattern that matches a block-ram
  * access. In verilog v95/v2001 this is mandatory.
  */
case object ExplicitRomVar extends Stage:
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 | VerilogDialect.v2001 => true
          case _                                         => false
      case _ => false
  override def dependencies: List[Stage] = List()
  override def nullifies: Set[Stage] = Set(DFHDLUniqueNames)
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    object ROMIndexAccessOf:
      private val handled = mutable.Set.empty[(DFDesignBlock, DFVal)]
      def unapply(applyIdx: ApplyIdx)(using MemberGetSet): Option[DFVal] =
        val relVal = applyIdx.relValRef.get
        relVal.dfType match
          case _: DFVector if !applyIdx.relIdx.get.isConst =>
            relVal match
              case DclConst() =>
                val design = applyIdx.getOwnerDesign
                val key = (design, relVal)
                if (handled.contains(key)) None
                else
                  handled.add(key)
                  Some(relVal)
              case _ => None
          case _ => None
    end ROMIndexAccessOf

    val patchList: List[(DFMember, Patch)] = designDB.members.flatMap {
      case applyIdx @ ROMIndexAccessOf(initVal) =>
        val (positionMember, config) =
          if (initVal.isGlobal) (applyIdx.getOwnerDesign, Patch.Add.Config.InsideFirst)
          else (initVal, Patch.Add.Config.After)
        val dsn = new MetaDesign(positionMember, config):
          val romVar =
            initVal.asValAny.genNewVar.asUninitialized.initForced(List(initVal.asConstAny))(using
              dfc.setName(s"${initVal.getName}_rom")
            ).asIR
        val filter = new Patch.Replace.RefFilter:
          val currentDesign = applyIdx.getOwnerDesign
          def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
            refs.filter {
              case r: DFRef.TwoWayAny => r.originMember match
                  case applyIdx: ApplyIdx => applyIdx.getOwnerDesign == currentDesign
                  case _                  => false
              case _ => false
            }
        val replaceIndexRefPatch =
          initVal -> Patch.Replace(dsn.romVar, Patch.Replace.Config.ChangeRefOnly, filter)
        dsn.patch :: replaceIndexRefPatch :: Nil
      case _ => Nil
    }
    designDB.patch(patchList)
  end transform
end ExplicitRomVar

extension [T: HasDB](t: T)
  def explicitRomVar(using CompilerOptions): DB =
    StageRunner.run(ExplicitRomVar)(t.db)
