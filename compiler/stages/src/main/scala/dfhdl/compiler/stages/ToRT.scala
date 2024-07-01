package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import DFVal.Alias.History.Op as HistoryOp
import dfhdl.compiler.ir.DFDesignBlock.InstMode
case object ToRT extends Stage:
  def dependencies: List[Stage] = List(DropDesignDefs, ExplicitState)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList = designDB.members.collect {
      case h @ DFVal.Alias.History(_, _, _, HistoryOp.Pipe, _, _, _, _) =>
        h -> Patch.Replace(
          h.copy(op = HistoryOp.State),
          Patch.Replace.Config.FullReplacement
        )
      case d @ DFDesignBlock(DomainType.DF, _, _, _, _, _) =>
        d -> Patch.Replace(
          d.copy(domainType = new DomainType.RT(RTDomainCfg.DerivedCfg)),
          Patch.Replace.Config.FullReplacement
        )
      case i @ DFInterfaceOwner(DomainType.DF, _, _, _) =>
        i -> Patch.Replace(
          i.copy(domainType = new DomainType.RT(RTDomainCfg.DerivedCfg)),
          Patch.Replace.Config.FullReplacement
        )
    }
    designDB.patch(patchList)
  end transform
end ToRT

//converts the dataflow domains to RT domains
//TODO: this is a very basic implementation. Needs to handle valid, ready and stall.
extension [T: HasDB](t: T) def toRT(using CompilerOptions): DB = StageRunner.run(ToRT)(t.db)
