package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

private class NoLocalDcls(db: DB) extends Stage(db):
  override def transform: DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view
        // only var declarations
        .collect {
          case m @ DclVar()                     => m
          case c: DFVal.Const if !c.isAnonymous => c
        }
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
end NoLocalDcls

//This stage moves the local vars or named constants (at the conditional block level) to the design level
extension [T: HasDB](t: T) def noLocalDcls: DB = new NoLocalDcls(t.db).transform
