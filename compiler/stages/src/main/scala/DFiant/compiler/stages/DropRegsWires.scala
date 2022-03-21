package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*
import DFiant.internals.*

import scala.collection.mutable

private class DropRegsWires(db: DB) extends Stage(db):
  override def transform: DB =
//    val patchList = designDB.members.collect {
//      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
//        m -> Patch.Remove
//    }
    designDB // .patch(patchList)

extension [T: HasDB](t: T) def dropRegsWires: DB = new DropRegsWires(t.db).transform
