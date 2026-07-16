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
  override def dependencies: List[Stage] = List(ToED, DropLocalDcls)
  override def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean =
    // the very old dialects do not support a "wildcard" all sensitivity list at all;
    // newer VHDL dialects still need this stage for processes calling phantom-carrying
    // ED methods (see below)
    co.backend match
      case be: dfhdl.backends.vhdl    => true
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 => true
          case _                  => false
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val co = summon[CompilerOptions]
    // under VHDL-2008/2019, `process(all)` is sensitive only to signals read within the
    // process TEXT — signals read inside a called impure function's body (ED method
    // phantom captures) are missed. Such processes get an explicit list instead.
    // (SystemVerilog is unaffected: `always_comb` is sensitive to function contents.)
    val dropAllProcesses = co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => true // verilog v95 (the only verilog dialect passing runCondition)
    def hasPhantomCall(pb: ProcessBlock): Boolean =
      pb.members(MemberView.Flattened).exists {
        case net: DFNet => net.hasTagOf[PhantomTag]
        case _          => false
      }
    val patchList: List[(DFMember, Patch)] =
      subDB.members
        // patching all process(all) blocks
        .collect {
          case pb @ ProcessBlock(sensitivity = Sensitivity.All)
              if dropAllProcesses || hasPhantomCall(pb) =>
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
                // ED method (function) call arguments — explicit and phantom-captured
                // alike — are read through the call's input connections
                case DFNet.Connection(
                      toVal = PortOfDesignDef(DFVal.Modifier.IN, _),
                      fromVal = fromVal
                    ) =>
                  Some(fromVal)
                case mh: DFMatchHeader       => Some(mh.selectorRef.get)
                case cb: DFConditional.Block => getBlockDependents(cb) ++ cb.getGuardOption
                case _                       => None
              }.flatMap(getDFValDependents)
            end getBlockDependents
            // memoization of added port-by-name
            val addedCPs = mutable.Set.empty[ConnectPoint]
            // get all dependent declarations (except local variables)
            val dcls =
              ListSet.from(getBlockDependents(pb).flatMap(_.departialPBNS.map(_._1)))
                // filter out local variables, but keep port-by-name which may be inside the process,
                // but refer to vias outside of it. we also need to account that different PBNS are
                // considered to be different values, so we use `addedCPs` to only add one port-by-name per connect point.
                .view.filter {
                  // HDL subprogram call ports are not signals — the call's actual reads are
                  // collected through the call's input connections instead
                  case pbns: DFVal.PortByNameSelect
                      if pbns.getDesignInst.getDesignBlock.isHDLSubprogram =>
                    false
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
              val updatedDcls = dcls.map(_.cloneAnonValueAndDepsHere.asValAny)
              dfhdl.core.Process.Block.list(updatedDcls)(using dfc.setMeta(pb.meta))

            dsn.patch
        }
    subDB.patch(patchList)
  end transformSubDB
end DropProcessAll

extension [T: HasDB](t: T)
  def dropProcessAll(using co: CompilerOptions): DB =
    StageRunner.run(DropProcessAll)(t.db)
