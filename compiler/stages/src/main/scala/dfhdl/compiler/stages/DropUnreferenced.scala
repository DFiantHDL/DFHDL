package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*

case object DropUnreferencedVars extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList = designDB.members.collect {
      case m @ DclVar() if !designDB.memberTable.contains(m) && m.externalInit.isEmpty =>
        m -> Patch.Remove
    }
    designDB.patch(patchList)

case object DropUnreferencedAnons extends Stage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using MemberGetSet): DB =
    val patchList = designDB.members.flatMap {
      // skipping over conditional headers that can be considered values as well.
      case _: DFConditional.Header => None
      case m: DFVal if m.isAnonymous && designDB.memberTable.getOrElse(m, Set()).isEmpty =>
        Some(m -> Patch.Remove)
      case _ => None
    }
    if (patchList.isEmpty) designDB
    else
      // recursively running until no more unreferenced values to remove.
      // recalling the stage is required because unreferenced removed value that refers to values that
      // are only referenced by the removed value creates more unreferenced values that need to be
      // removed.
      val newDesignDB = designDB.patch(patchList)
      transform(newDesignDB)(using newDesignDB.getSet)
  end transform
end DropUnreferencedAnons

extension [T: HasDB](t: T)
  def dropUnreferencedVars: DB = StageRunner.run(DropUnreferencedVars)(t.db)
  def dropUnreferencedAnons: DB = StageRunner.run(DropUnreferencedAnons)(t.db)
