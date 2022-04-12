package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

private class ToED(db: DB) extends Stage(db):
  override protected def preTransform: DB = db.toRT
  override def transform: DB = db
//    val patchList = designDB.members.collect {
//      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
//        m -> Patch.Remove
//    }
//    designDB.patch(patchList)

extension [T: HasDB](t: T) def toED: DB = new ToED(t.db).transform
