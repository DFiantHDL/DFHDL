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
    val patchList = subDB.members.flatMap {
      // skipping over conditional headers that can be considered values as well.
      case _: DFConditional.Header => None
      // idents are always kept
      case Ident(_)                                             => None
      case m: DFVal if m.isAnonymous && m.originMembers.isEmpty =>
        Some(m -> Patch.Remove())
      case m: DFRange if m.originMembers.isEmpty => Some(m -> Patch.Remove())
      case _                                     => None
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
