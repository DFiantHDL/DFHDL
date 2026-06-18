package dfhdl.compiler.stages
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import dfhdl.core.{Design}

trait Stage extends Product, Serializable, HasTypeName derives CanEqual:
  final lazy val depSet: Set[Stage] = dependencies.toSet
  def dependencies: List[Stage]
  def nullifies: Set[Stage]
  def runCondition(using CompilerOptions): Boolean = true
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB

/** Phase-2 bridge for stages that need cross-design information or whose work cannot be decomposed
  * cleanly per-sub-DB. The stage implements `transformGlobal(designDB)` and operates on the
  * hierarchical DB (root + per-design sub-DBs), which is the native representation threaded through
  * the whole pipeline by `StageRunner` (it does the single `oldToNew`/`newToOld` at the pipeline
  * boundary, so individual stages neither convert at entry nor flatten at exit).
  *
  * Use `GlobalStage` when the body needs:
  *   - cross-design tracking state (e.g. shared opaque type maps)
  *   - global analyses that span the entire hierarchy
  *   - patches that affect multiple sub-DBs (e.g. dedup, renaming)
  *
  * Otherwise prefer `HierarchyStage` for per-sub-DB decomposition.
  */
trait GlobalStage extends Stage:
  def transformGlobal(designDB: DB)(using
      co: CompilerOptions,
      refGen: RefGen
  ): DB

  override def transform(designDB: DB)(using
      outerGetSet: MemberGetSet,
      co: CompilerOptions
  ): DB =
    // `designDB` is the hierarchical root. Seed RefGen from it (root-aware:
    // `RefGen.fromGetSet` aggregates across sub-DBs) — the root's own getSet is
    // non-functional, so the body must dispatch any ref resolution through
    // sub-DB getSets explicitly.
    val refGen = RefGen.fromGetSet(using outerGetSet)
    transformGlobal(designDB)(using co, refGen)
end GlobalStage

/** Phase-2 bridge for stages whose work decomposes cleanly per-sub-DB.
  *
  * The stage implements `transformSubDB(subDB)` which returns the TRANSFORMED sub-DB (typically via
  * `subDB.patch(patches)`). `designDB` is the hierarchical root threaded through the pipeline by
  * `StageRunner` (which does the single `oldToNew`/`newToOld` at the boundary). The trait handles:
  *   - per-DB dispatch of `transformSubDB` on every sub-DB in the hierarchy. The root DB is a pure
  *     hierarchy container (empty members, empty refTable) and is NOT passed to `transformSubDB`;
  *     all design content lives in `subDBs`.
  *   - reassembly via `.update(subDBs = ...)` of the patched sub-DBs back into the root.
  *
  * If every `transformSubDB` returned its input by reference (no change), the original `designDB`
  * is returned by reference too. This lets iterative stages (e.g. `BreakOps`,
  * `DropUnreferencedAnons`) terminate via `result eq designDB`.
  *
  * `transformSubDB` always runs with the implicit `MemberGetSet` rebound to the current sub-DB's
  * own getSet (so the `subDB` helper resolves that design's members); the root DB is passed as the
  * parameter for cross-design needs (e.g. `rootDB.resolvedClkRstMap`).
  */
trait HierarchyStage extends Stage:
  final protected def subDB(using MemberGetSet): DB = getSet.designDB
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB

  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    import scala.collection.immutable.ListMap
    given refGen: RefGen = RefGen.fromGetSet
    var changed = false
    def run(subDB: DB): DB =
      // `transformSubDB` always sees the root DB and the current sub-DB's getSet.
      val result = transformSubDB(designDB)(using subDB.getSet, co, refGen)
      if (!(result eq subDB)) changed = true
      result
    val transformedSubs: ListMap[StaticRef, DB] =
      designDB.subDBs.map { case (k, subDB) => k -> run(subDB) }
    if (!changed) designDB
    else designDB.update(subDBs = transformedSubs)
  end transform
end HierarchyStage

trait HasDB[T]:
  def apply(t: T): DB
object HasDB:
  given HasDB[DB] with
    def apply(t: DB): DB = t
  given [D <: Design]: HasDB[D] with
    def apply(t: D): DB = t.getDB
  given HasDB[StagedDesign] with
    def apply(t: StagedDesign): DB = t.stagedDB
  given HasDB[CompiledDesign] with
    def apply(t: CompiledDesign): DB = t.staged.stagedDB

extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
