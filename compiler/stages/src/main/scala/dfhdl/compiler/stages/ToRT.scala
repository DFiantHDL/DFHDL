package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.History.Op as HistoryOp
import dfhdl.compiler.ir.DFDesignBlock.InstMode
case object ToRT extends HierarchyStage:
  def dependencies: List[Stage] = List(DropDesignDefs, ExplicitState)
  def nullifies: Set[Stage] = Set()
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patchList = subDB.members.collect {
      case h @ DFVal.Alias.History(op = HistoryOp.Pipe) =>
        h -> Patch.Replace(
          h.copy(op = HistoryOp.State),
          Patch.Replace.Config.FullReplacement
        )
      case d @ DFDesignBlock(domainType = DomainType.DF) =>
        d -> Patch.Replace(
          d.copy(domainType = DomainType.RT),
          Patch.Replace.Config.FullReplacement
        )
      case i @ DFInterfaceOwner(domainType = DomainType.DF) =>
        i -> Patch.Replace(
          i.copy(domainType = DomainType.RT),
          Patch.Replace.Config.FullReplacement
        )
    }
    subDB.patch(patchList)
  end transformSubDB
end ToRT

//converts the dataflow domains to RT domains
//TODO: this is a very basic implementation. Needs to handle valid, ready and stall.
extension [T: HasDB](t: T) def toRT(using CompilerOptions): DB = StageRunner.run(ToRT)(t.db)
