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
  * cleanly per-sub-DB. The stage implements `transformGlobal(newDB)` and operates on the NEW-STYLE
  * hierarchical DB.
  *
  * The trait handles:
  *   - `oldToNew` at entry — converts a legacy flat DB into the hierarchical representation (root +
  *     per-design sub-DBs) so the body can walk the hierarchy via `subDBs` and patch each sub-DB
  *     independently.
  *   - `newToOld` at exit — flattens the result back into an old-style DB for the rest of the
  *     pipeline.
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
    // Seed RefGen from the flat old-style DB, whose refTable still has every
    // ref. Under B-pure, the new-style root has empty refTable so
    // `RefGen.fromGetSet(newDB.getSet)` would crash. The body must dispatch
    // any ref resolution through sub-DB getSets explicitly — root's getSet
    // is non-functional.
    val refGen = RefGen.fromGetSet(using outerGetSet)
    val newDB = designDB.oldToNew
    val transformed = transformGlobal(newDB)(using co, refGen)
    transformed.newToOld
end GlobalStage

/** Phase-2 bridge for stages whose work decomposes cleanly per-sub-DB.
  *
  * The stage implements `transformSubDB(subDB)` which returns the TRANSFORMED sub-DB (typically via
  * `subDB.patch(patches)`). The trait handles:
  *   - flat old-style → B-pure new-style conversion at entry (`oldToNew`)
  *   - per-DB dispatch of `transformSubDB` on every sub-DB in the hierarchy. The root DB is a pure
  *     hierarchy container (empty members, empty refTable) and is NOT passed to `transformSubDB`;
  *     all design content lives in `subDBs`.
  *   - reassembly via `.copy(subDBs = ...).newToOld` to flatten back into an old-style DB for the
  *     rest of the pipeline
  *
  * If every `transformSubDB` returned its input by reference (no change), the original `designDB`
  * is returned by reference too. This lets iterative stages (e.g. `BreakOps`,
  * `DropUnreferencedAnons`) terminate via `result eq designDB`.
  *
  * Configuration knob:
  *   - `rebindGetSet` (default `true`): rebind the implicit `MemberGetSet` to each DB's own getSet
  *     while `transformSubDB` runs. Set to `false` when the body needs full-hierarchy resolution
  *     (reverse lookups like `memberTable` / `getReadDeps`, or cross-design tables like
  *     `connectionTable` / `resolvedClkRstMap`) — those need the outer flat-DB getSet.
  */
trait HierarchyStage extends Stage:
  def rebindGetSet: Boolean = true
  final protected def subDB(using MemberGetSet): DB = getSet.designDB
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB

  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    import scala.collection.immutable.ListMap
    given refGen: RefGen = RefGen.fromGetSet
    val newDB = designDB.oldToNew
    var changed = false
    def run(subDB: DB): DB =
      val rebindDB = if (rebindGetSet) newDB else subDB
      val rebindGS = if (rebindGetSet) subDB.getSet else designDB.getSet
      val result = transformSubDB(rebindDB)(using rebindGS, co, refGen)
      if (!(result eq subDB)) changed = true
      result
    val transformedSubs: ListMap[DFOwner.Ref, DB] =
      newDB.subDBs.map { case (k, subDB) => k -> run(subDB) }
    if (!changed) designDB
    else newDB.update(subDBs = transformedSubs).newToOld
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
