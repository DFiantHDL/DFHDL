package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import scala.collection.immutable.ListSet
import dfhdl.compiler.ir.DFConditional.DFMatchHeader
import dfhdl.compiler.ir.DFConditional.DFCaseBlock
import dfhdl.compiler.ir.DFConditional.DFIfElseBlock
import scala.collection.mutable

/** This stage drops process(all) by transforming it to a process with explicit sensitivity list
  */
case object DropProcessAll extends HierarchyStage:
  // rebind off: `departialDcl` / `collectRelMembers` walk refs across designs.
  override def rebindGetSet: Boolean = false
  override def dependencies: List[Stage] = List(ToED, DropLocalDcls)
  override def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean =
    // only the very old dialects do not support a "wildcard" all sensitivity list
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 => true
          case _                  => false
  def transformSubDB(subDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patchList: List[(DFMember, Patch)] =
      subDB.members
        // patching all process(all) blocks
        .collect { case pb @ ProcessBlock(sensitivity = Sensitivity.All) =>
          // recursively through value dependents
          def getDFValDependents(dfVal: DFVal): collection.View[DFVal] =
            dfVal.getRefs.view.filterNot(_.isInstanceOf[DFRef.TypeRef]).map(_.get).collect {
              case dfVal: DFVal => dfVal
            }.flatMap(getDFValDependents).++(Some(dfVal))
          // recursively through internal conditional block members
          def getBlockDependents(block: DFBlock): collection.View[DFVal] =
            val members = subDB.blockMemberTable(block)
            members.view.flatMap {
              case DFNet.Assignment(_, fromVal) => Some(fromVal)
              case mh: DFMatchHeader            => Some(mh.selectorRef.get)
              case cb: DFConditional.Block      => getBlockDependents(cb) ++ cb.getGuardOption
              case _                            => None
            }.flatMap(getDFValDependents)
          // memoization of added port-by-name
          val addedCPs = mutable.Set.empty[ConnectPoint]
          // get all dependent declarations (except local variables)
          val dcls =
            ListSet.from(getBlockDependents(pb).flatMap(_.departialPBNS.map(_._1)))
              // filter out local variables, but keep port-by-name which may be inside the process,
              // but refer to vias outside of it. we also need to account that different PBNS are
              // considered to be different values, so we use `addedCPs` to only add one port-by-name per connect point.
              .view.filter {
                case pbns: DFVal.PortByNameSelect =>
                  val cp = ConnectPoint.Via(pbns)
                  if (addedCPs.contains(cp)) false
                  else
                    addedCPs += cp
                    true
                case v => !v.isInsideOwner(pb)
              }.toList
          val dsn = new MetaDesign(
            pb,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
          ):
            dfhdl.core.Process.Block.list(dcls.map(_.asValAny))(using dfc.setMeta(pb.meta))

          dsn.patch
        }
    subDB.patch(patchList)
  end transformSubDB
end DropProcessAll

extension [T: HasDB](t: T)
  def dropProcessAll(using co: CompilerOptions): DB =
    StageRunner.run(DropProcessAll)(t.db)
