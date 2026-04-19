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

/** Phase-2 bridge for stages whose work decomposes cleanly per-sub-DB.
  *
  * The stage implements `transformSubDB(subDB)` which returns the TRANSFORMED
  * sub-DB (typically via `subDB.patch(patches)`). The trait handles:
  *   - flat old-style → option-(a) new-style conversion at entry (`oldToNew`)
  *   - per-DB dispatch of `transformSubDB` on every DB in the hierarchy
  *     (root AND each sub-DB). Each DB is self-contained under option (a),
  *     so its patches apply to its own `members` + `refTable` independently
  *   - reassembly via `.copy(internalDBs = ...).newToOld` to flatten back
  *     into an old-style DB for the rest of the pipeline
  *
  * If every `transformSubDB` returned its input by reference (no change),
  * the original `designDB` is returned by reference too. This lets iterative
  * stages (e.g. `BreakOps`, `DropUnreferencedAnons`) terminate via
  * `result eq designDB`.
  *
  * Configuration knob:
  *   - `rebindGetSet` (default `true`): rebind the implicit `MemberGetSet`
  *     to each DB's own getSet while `transformSubDB` runs. Set to `false`
  *     when the body needs full-hierarchy resolution (reverse lookups like
  *     `memberTable` / `getReadDeps`, or cross-design tables like
  *     `dupPortsByName` / `connectionTable` / `explicitRTDomainCfgMap`) —
  *     those need the outer flat-DB getSet.
  */
trait HierarchyStage extends Stage:
  def rebindGetSet: Boolean = true
  def transformSubDB(subDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      refGen: RefGen
  ): DB

  def transform(designDB: DB)(using
      outerGetSet: MemberGetSet,
      co: CompilerOptions
  ): DB =
    import scala.collection.immutable.ListMap
    given RefGen = RefGen.fromGetSet
    val newDB = designDB.oldToNew
    var changed = false
    def run(subDB: DB): DB =
      val ctxGetSet: MemberGetSet = if (rebindGetSet) subDB.getSet else outerGetSet
      val result = transformSubDB(subDB)(using ctxGetSet, co, summon[RefGen])
      if (!(result eq subDB)) changed = true
      result
    val transformedSubs: ListMap[DFOwner.Ref, DB] =
      newDB.internalDBs.map { case (k, subDB) => k -> run(subDB) }
    val transformedRoot = run(newDB)
    if (!changed) designDB
    else transformedRoot.copy(internalDBs = transformedSubs).newToOld
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
