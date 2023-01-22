package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*

case object DropUnreferenced extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList = designDB.members.collect {
      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
        m -> Patch.Remove
    }
    designDB.patch(patchList)

extension [T: HasDB](t: T) def dropUnreferenced: DB = StageRunner.run(DropUnreferenced)(t.db)