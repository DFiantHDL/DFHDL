package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFVal.Alias.History.Op as HistoryOp
case object ToRT extends Stage2:
  def dependencies: List[Stage2] = List()
  def nullifies: Set[Stage2] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList = designDB.members.collect {
      case h @ DFVal.Alias.History(_, _, _, HistoryOp.Prev | HistoryOp.Pipe, _, _, _, _) =>
        h -> Patch.Replace(h.copy(op = HistoryOp.Reg), Patch.Replace.Config.FullReplacement)
      case d @ DFDesignBlock(DomainType.DF, _, _, _, _, _, _) =>
        d -> Patch.Replace(
          d.copy(domainType = new DomainType.RT(DerivedCfg, DerivedCfg)),
          Patch.Replace.Config.FullReplacement
        )
      case i @ DFInterfaceOwner(DomainType.DF, _, _, _) =>
        i -> Patch.Replace(
          i.copy(domainType = new DomainType.RT(DerivedCfg, DerivedCfg)),
          Patch.Replace.Config.FullReplacement
        )
    }
    designDB.patch(patchList)
  end transform
end ToRT

//converts the dataflow domains to RT domains
//TODO: this is a very basic implementation. Needs to handle valid, ready and stall.
extension [T: HasDB](t: T) def toRT: DB = StageRunner.run(ToRT)(t.db)
