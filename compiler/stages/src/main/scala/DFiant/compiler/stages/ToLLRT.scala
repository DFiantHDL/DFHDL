package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

private class ToLLRT(db: DB) extends Stage(db):
  override protected def preTransform: DB = db.toRT
  override def transform: DB =
    val patchList = designDB.members.collect {
      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
        m -> Patch.Remove
    }
    designDB.patch(patchList)

extension [T: HasDB](t: T) def toLLRT: DB = new ToLLRT(t.db).transform
