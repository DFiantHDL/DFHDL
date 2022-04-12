package DFiant.compiler.stages

import DFiant.compiler.analysis.*
import DFiant.compiler.ir.*
import DFiant.compiler.patching.*

case object ToED extends Stage2:
  def dependencies: List[Stage2] = List(ToRT)
  def nullifies: Set[Stage2] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB = designDB
//    val patchList = designDB.members.collect {
//      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
//        m -> Patch.Remove
//    }
//    designDB.patch(patchList)

extension [T: HasDB](t: T) def toED: DB = StageRunner.run(ToED)(t.db)
