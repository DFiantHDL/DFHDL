package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.annotation.tailrec

case object DropUnreferencedVars extends HierarchyStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      refGen: RefGen
  ): DB =
    val patchList = subDB.members.collect {
      case m @ DclVar() if !subDB.memberTable.contains(m) && m.initRefList.isEmpty =>
        m -> Patch.Remove()
    }
    if (patchList.isEmpty) subDB else subDB.patch(patchList)

case object DropUnreferencedAnons extends HierarchyStage, NoCheckStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  @tailrec private def loop(rootDB: DB)(using MemberGetSet, RefGen): DB =
    // the kind-level criteria (which members may be dropped and which are always kept) are the
    // shared `isDroppableIfUnread` predicate, so this stage and elaboration's end-of-design
    // sweep (`DesignContext.sweepUnreadAnons`) can never drift; only the "is it read" question
    // differs (here: origin tracking on the immutable DB)
    val patchList = subDB.members.flatMap {
      case m if m.isDroppableIfUnread && m.originMembers.isEmpty => Some(m -> Patch.Remove())
      case _                                                     => None
    }
    if (patchList.isEmpty) subDB
    else
      // recursively running until no more unreferenced values to remove.
      // recalling is required because unreferenced removed value that refers to values
      // that are only referenced by the removed value creates more unreferenced values
      // that need to be removed.
      val patched = subDB.patch(patchList)
      loop(patched)(using patched.getSet, summon[RefGen])
  end loop
  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      refGen: RefGen
  ): DB = loop(rootDB)
end DropUnreferencedAnons

extension [T: HasDB](t: T)
  def dropUnreferencedVars(using CompilerOptions): DB =
    StageRunner.run(DropUnreferencedVars)(t.db)
  def dropUnreferencedAnons(using CompilerOptions): DB =
    StageRunner.run(DropUnreferencedAnons)(t.db)
