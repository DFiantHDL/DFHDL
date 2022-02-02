package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

private class NoLocalVars(db: DB) extends Stage(db):
  override def transform: DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        // only var declarations
        .collect { case m @ DclVar() => m }
        .map(m => (m, m.getOwnerBlock))
        .flatMap {
          // only var declarations inside conditional blocks
          case (dcl, cb: DFConditional.Block) =>
            val topHeader = cb.getTopConditionalHeader
            Some(topHeader -> Patch.Move(dcl, Patch.Move.Config.Before))
          case _ => None
        }
        .toList
    designDB.patch(patchList)
  end transform
end NoLocalVars

//This stage moves the local vars (at the conditional block level) to the design level
extension [T: HasDB](t: T) def noLocalVars: DB = new NoLocalVars(t.db).transform
