package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFVal.Alias.History.Op as HistoryOp
private class ToRT(db: DB) extends Stage(db):
  override def transform: DB =
    val patchList = designDB.members.collect {
      case h @ DFVal.Alias.History(_, _, _, HistoryOp.Prev | HistoryOp.Pipe, _, _, _) =>
        h -> Patch.Replace(h.copy(op = HistoryOp.Reg), Patch.Replace.Config.FullReplacement)
      case d @ DFDesignBlock(DomainType.DF, _, _, _, _, _, _) =>
        d -> Patch.Replace(
          d.copy(domainType = new DomainType.RT.HL()),
          Patch.Replace.Config.FullReplacement
        )
      case i @ DFInterfaceOwner(DomainType.DF, _, _, _) =>
        i -> Patch.Replace(
          i.copy(domainType = new DomainType.RT.HL()),
          Patch.Replace.Config.FullReplacement
        )
    }
    designDB.patch(patchList)
  end transform
end ToRT

//converts the dataflow domains to high-level RT domains
//TODO: this is a very basic implementation. Needs to handle valid, ready and stall.
extension [T: HasDB](t: T) def toRT: DB = new ToRT(t.db).transform
