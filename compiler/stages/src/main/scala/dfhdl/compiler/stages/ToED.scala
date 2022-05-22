package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*

case object ToED extends Stage:
  def dependencies: List[Stage] = List(ToRT)
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
//    val patchList = designDB.members.collect {
//      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
//        m -> Patch.Remove
//    }
//    designDB.patch(patchList)

extension [T: HasDB](t: T) def toED: DB = StageRunner.run(ToED)(t.db)
