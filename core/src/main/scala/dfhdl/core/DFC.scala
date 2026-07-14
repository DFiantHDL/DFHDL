package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir
import dfhdl.options.ElaborationOptions
import dfhdl.hw.annotation.getActiveHWAnnotations
import scala.reflect.ClassTag
import collection.mutable
import scala.annotation.Annotation
import scala.annotation.implicitNotFound
import ir.annotation.HWAnnotation

@implicitNotFound(
  "Missing local design context.\nEither this operation is not supported in global context or `using DFC` is missing."
)
final case class DFC(
    nameOpt: Option[String],
    position: Position,
    docOpt: Option[String],
    annotations: List[HWAnnotation] = Nil, // TODO: removing default causes stale symbol crash
    mutableDB: MutableDB = new MutableDB(),
    refGen: ir.RefGen = ir.RefGen.initial,
    tags: ir.DFTags = ir.DFTags.empty,
    elaborationOptionsContr: () => ElaborationOptions = () =>
      summon[ElaborationOptions.Defaults[Design]]
) extends MetaContext:
  lazy val elaborationOptions: ElaborationOptions = elaborationOptionsContr()
  def setMeta(
      nameOpt: Option[String] = nameOpt,
      position: Position = position,
      docOpt: Option[String] = docOpt,
      annotations: List[Annotation] = Nil
  ) =
    if (refGen.getGrpId == (0, 0))
      refGen.setGrpId(DFC.getGrpId(position))
    copy(
      nameOpt = nameOpt,
      position = position,
      docOpt = docOpt,
      annotations = annotations.getActiveHWAnnotations
    ).asInstanceOf[this.type]
  def setMeta(
      meta: ir.Meta
  ) =
    if (refGen.getGrpId == (0, 0))
      refGen.setGrpId(DFC.getGrpId(position))
    copy(
      nameOpt = meta.nameOpt,
      position = meta.position,
      docOpt = meta.docOpt,
      annotations = meta.annotations
    ).asInstanceOf[this.type]
  def setTags(tags: ir.DFTags) = copy(tags = tags)
  def tag[CT <: ir.DFTag: ClassTag](customTag: CT) = setTags(tags.tag(customTag))
  def emptyTags = setTags(ir.DFTags.empty)
  given getSet: ir.MemberGetSet = mutableDB.getSet
  def getMeta: ir.Meta = ir.Meta(nameOpt, position, docOpt, annotations)
  def enterOwner(owner: DFOwnerAny): Unit =
    mutableDB.OwnershipContext.enter(owner.asIR)
  def exitOwner(): Unit = mutableDB.OwnershipContext.exit()
  def owner: DFOwnerAny = mutableDB.OwnershipContext.owner.asFE
  def enterLate(): Unit =
    mutableDB.OwnershipContext.enterLate()
  def exitLate(): Unit =
    mutableDB.OwnershipContext.exitLate()
  def lateConstruction: Boolean = mutableDB.OwnershipContext.lateConstruction
  def ownerOption: Option[DFOwnerAny] =
    mutableDB.OwnershipContext.ownerOption.map(_.asFE)
  // Returns the IR ref for the current owner, or a ref to DFMember.Empty when there is no
  // owner in the context. Prefer this over `dfc.owner.ref` when constructing raw IR members
  // (e.g. ir.Goto) inside a MetaDesign body: the `ref` extension method requires
  // `import dfhdl.core.*` in scope, which can conflict with other `dfhdl.core` imports.
  def ownerOrEmptyRef: ir.DFOwner.Ref =
    ownerOption.map(_.asIR.ref(using this)).getOrElse(ir.DFMember.Empty.ref(using this))
  def setName(name: String): this.type =
    copy(nameOpt = Some(name)).asInstanceOf[this.type]
  def setAnnotations(annotations: List[HWAnnotation]): this.type =
    copy(annotations = annotations).asInstanceOf[this.type]
  def anonymize: this.type = copy(nameOpt = None).asInstanceOf[this.type]
  def logEvent(event: LogEvent): Unit = mutableDB.logger.logEvent(event)
  def injectEvents(newEvents: List[LogEvent]): Unit = mutableDB.logger.injectEvents(newEvents)
  def getErrors: List[DFError] = mutableDB.logger.getErrors
  def getWarnings: List[DFWarning] = mutableDB.logger.getWarnings
  def getEvents: List[LogEvent] = mutableDB.logger.getEvents
  def inMetaProgramming: Boolean = mutableDB.inMetaProgramming
  def clearEvents(): Unit = mutableDB.logger.clearEvents()
end DFC
object DFC:
  import java.util.concurrent.atomic.AtomicInteger

  /** Thread-safe cache for generating unique group IDs based on position hash codes.
    *
    * Thread Safety Guarantees:
    *   - Uses `TrieMap` for thread-safe concurrent access to the cache
    *   - Each hash code gets its own `AtomicInteger` counter for unique ID generation
    *   - `getOrElseUpdate` atomically checks and creates new counters if needed
    *   - `AtomicInteger.getAndIncrement()` provides atomic increment operations
    *
    * This design ensures that:
    *   1. Multiple threads can safely access the cache concurrently
    *   2. Each position hash code gets a unique incremental ID
    *   3. No race conditions occur during counter creation or increment
    *   4. Memory usage is bounded by the number of unique position hash codes
    */
  private val positionCache = collection.concurrent.TrieMap.empty[Int, AtomicInteger]

  /** Generates a unique group ID tuple for a given position.
    *
    * The tuple consists of:
    *   - First element: The position's hash code (for grouping similar positions)
    *   - Second element: A unique incremental ID for positions with the same hash code
    *
    * Thread Safety:
    *   - This method is thread-safe and can be called concurrently by multiple threads
    *   - Uses atomic operations to ensure unique ID generation without race conditions
    *   - Each position hash code gets its own counter, preventing ID conflicts
    *
    * @param position
    *   The position to generate a group ID for
    * @return
    *   A tuple (hashCode, uniqueId) where uniqueId is guaranteed to be unique for this position
    */
  private def getGrpId(position: Position): (Int, Int) =
    val hashCode = position.hashCode()
    val counter = positionCache.getOrElseUpdate(hashCode, new AtomicInteger(0))
    (hashCode, counter.getAndIncrement())

  def empty(eo: ElaborationOptions): DFC =
    DFC(None, Position.unknown, None, elaborationOptionsContr = () => eo)
  def emptyNoEO: DFC = DFC(None, Position.unknown, None)
  /** The scope capability lattice (see devdocs/scope-lattice-plan.md).
    *
    * Two kinds of trait, and the distinction is a HARD RULE:
    *   - CAPABILITIES are mixins. They grant constructs. They NEVER have a given.
    *   - PLACES are where the user's code actually is. They mix capabilities together, and they are
    *     the only traits with a given.
    *
    * The base `Scope` itself carries the constant capabilities (`<> CONST` declarations, and
    * arithmetic/logic/conversion on constants), so those are legal everywhere, `Global` included.
    *
    * ~~~ THE RULE THAT MAKES THIS SOUND ~~~
    *
    * A guard must SUMMON THE INNERMOST BARE `Scope` AND SUBTYPE-TEST IT. It must NEVER summon a
    * capability (`AssertGiven[Scope.Local]`) nor put a scope under a bare `NotGiven`
    * (`NotGiven[Scope.Process]`). Both of those search for ANY given in scope that satisfies them,
    * which finds an ENCLOSING scope's given rather than the innermost one: inside a process,
    * summoning `Concurrent` would find the enclosing design's given and legalize `<>` connections
    * and port declarations there. And because `Function`'s given is AMBIENT (see `ScopeLP` below)
    * and `Function` is a `Local`, a summon of `Local` would succeed even at global scope, undoing
    * the "declarations cannot be global" guard entirely.
    *
    * The correct form pins the innermost scope first, then tests it. Both of these are safe, since
    * `s` is already the innermost:
    * {{{
    *   inline def foo(using s: DFC.Scope)(using AssertGiven[s.type <:< DFC.Scope.Concurrent, "..."])
    *   inline def bar(using s: DFC.Scope)(using AssertGiven[util.NotGiven[s.type <:< DFC.Scope.Process], "..."])
    * }}}
    * A bare `Scope` summon resolves to the innermost because Scala prefers a more deeply nested
    * given: a process body's context parameter beats its design's `given TScope`, which in turn
    * beats the implicit-scope givens below.
    */
  sealed trait Scope
  // Low-priority scope givens. `Function` must be summonable at any ED method call site (an ED
  // function is callable from design scope, processes, initial blocks, and other method bodies
  // alike), so it gets an ambient given here. It is defined in a base trait of `object Scope` so
  // that givens declared directly in `object Scope` (e.g. `Global`) always win a generic `Scope`
  // summon: that is what keeps a top-level `<> VAR` rejected even though `Function` is a `Local`.
  sealed trait ScopeLP:
    given Scope.Function = Scope.Function
  object Scope extends ScopeLP:
    // ~~~ CAPABILITY BUILDING BLOCKS: one trait per construct. Mixins only, NEVER given. ~~~
    //
    // Each names exactly ONE thing a scope may do, so a guard can summon precisely the capability
    // it needs and nothing more. This granularity is what makes the ambient `Scope.Function` given
    // safe to summon against: a guard for a capability that `Function` does NOT have (text output,
    // ports, connections, processes...) will not be satisfied by the ambient given, so it fails at
    // global scope exactly as it should. Bundling those constructs together under one coarse
    // `Local` is what previously made `case given Scope.Local` match in plain Scala code.
    /** `<> VAR` declarations (which modifier variants are allowed is decided by the domain). */
    sealed trait HasVars extends Scope
    /** `:=` blocking assignment (further limited by the domain). */
    sealed trait HasAssign extends Scope
    /** `:==` non-blocking assignment. */
    sealed trait HasNBAssign extends Scope
    /** `<> IN/OUT/INOUT` port declarations, and interface/view instantiation. */
    sealed trait HasPorts extends Scope
    /** Domain declarations. Not on `HasPorts`: an interface declares ports but not domains. */
    sealed trait HasDomains extends Scope
    /** `<>` connections. */
    sealed trait HasConnect extends Scope
    /** `process` and `initial` declarations. */
    sealed trait HasProcesses extends Scope
    /** `.reg` / `.prev` / `.rising` / `.falling` (further limited by the domain). */
    sealed trait HasHistory extends Scope
    /** `for` and `while` loops. */
    sealed trait HasLoops extends Scope
    /** `wait` statements, and calls to procedural ED methods (tasks). */
    sealed trait HasWait extends Scope
    /** Assertions and text printing. NOT granted by `Function`: a function is pure by definition
      * (see devdocs/static-domain-plan.md §8.1), so this exclusion is load-bearing.
      */
    sealed trait HasTextOut extends Scope
    /** RT step blocks. */
    sealed trait HasSteps extends Scope
    /** Fork-join blocks. */
    sealed trait HasFork extends Scope
    /** `locally` local blocks. */
    sealed trait HasLocalBlocks extends Scope

    // ~~~ BUNDLES: named groupings of the blocks above. Still capabilities, still never given. ~~~
    /** The classic "some local DFHDL scope" bundle: variables and blocking assignment.
      *
      * `Function` deliberately does NOT extend this. It picks up `HasVars`/`HasAssign`/`HasLoops`
      * DIRECTLY instead, so that its ambient given is not eligible for a `Local` summon. That is
      * what lets `TextOut` and `DFRange` keep asking `case given DFC.Scope.Local` (the broad
      * "is this DFHDL code, or plain Scala?" question) without the ambient given making it true
      * everywhere.
      */
    sealed trait Local extends HasVars, HasAssign, HasLocalBlocks
    /** Ports and views: a design, a domain, or an interface. */
    sealed trait PublicDcl extends HasPorts
    /** A concurrent (structural) body: connections, process/initial declarations, history ops. */
    sealed trait Concurrent extends Local, HasConnect, HasProcesses, HasHistory
    /** A sequential body: loops on top of the local capabilities. */
    sealed trait Sequence extends Local, HasLoops
    /** A sequential body that can also block on time. */
    sealed trait TimedSequence extends Sequence, HasWait

    // ~~~ places: the only traits with givens ~~~
    /** The ambient default. Adds nothing beyond the base `Scope`: it is purely the "no enclosing
      * container" marker, so only the constant capabilities are available.
      */
    /** The ambient default. Adds nothing: only the base `Scope` constant capabilities. */
    sealed trait Global extends Scope
    object Global extends Global
    given Global = Global
    sealed trait Design extends PublicDcl, Concurrent, HasDomains, HasTextOut
    object Design extends Design
    sealed trait Domain extends PublicDcl, Concurrent, HasDomains, HasTextOut
    object Domain extends Domain
    /** Purely structural: ports and views, no variables, no initialization, no domains. */
    sealed trait Interface extends PublicDcl
    object Interface extends Interface
    /** A procedural ED method body (Verilog task / VHDL procedure). */
    @implicitNotFound(
      "A procedural ED method (`Unit <> EDRET`) can only be invoked inside a process or another procedural ED method body"
    )
    sealed trait Procedural extends TimedSequence, HasTextOut
    object Procedural extends Procedural
    /** A process body. Everything a procedural body can do, plus `:==`, step blocks and fork-join.
      * Since `Process` IS a `Procedural`, a task is callable here by plain subtyping, with no extra
      * given needed.
      */
    sealed trait Process extends Procedural, HasNBAssign, HasSteps, HasFork:
      // will include the step cache according to the name of the step block
      // (the plugin will make sure that the name is unique)
      private[core] val stepCache = mutable.Map.empty[String, ir.StepBlock]
    object Process extends Process
    /** An `initial` block body. A `Sequence`, NOT a `TimedSequence`: no `wait` statements and no
      * task calls. This agrees with the existing elaboration check in `DB.initialCheck`, which
      * already rejects every `Wait` inside an `initial` block.
      *
      * NOTE: `wait` does not yet carry a `HasWait` guard, so the rejection is still only an
      * elaboration error, not a compile error. Adding that guard is blocked on the RT question
      * (devdocs/scope-lattice-plan.md §6): an RT DESIGN body is a `Concurrent`, which has no
      * `HasWait`, yet `wait(1.cy)` is legitimate there for FSM steps. Guarding `wait` on `HasWait`
      * today would break every RT design.
      */
    sealed trait Initial extends Sequence, HasTextOut
    object Initial extends Initial
    /** An ED (or, later, static) function method body.
      *
      * It mixes the capability BLOCKS it needs directly rather than inheriting the `Local` or
      * `Sequence` bundles, and this is the load-bearing detail of the whole lattice. `Function`'s
      * given is AMBIENT (`ScopeLP` above), so it is eligible for any summon of one of its
      * supertypes, ANYWHERE, global scope included. Keeping it out of `Local` therefore keeps
      * `case given DFC.Scope.Local` honest: in plain Scala code the summon finds nothing and the
      * DFHDL constructs correctly fall back to their Scala counterparts.
      *
      * It has no `HasTextOut` (a function is pure), no `HasConnect`, no `HasPorts`, no
      * `HasProcesses`, and no `HasWait`.
      */
    @implicitNotFound(
      "An ED function method can only be invoked inside an event-driven (ED) domain."
    )
    sealed trait Function extends HasVars, HasAssign, HasLoops
    object Function extends Function
  end Scope

end DFC

into opaque type DFCG <: DFC = DFC
protected trait DFCGLP:
  // DFCG given must be inline to force new DFC is generated for every missing DFC summon.
  inline given DFCG = DFCG()
object DFCG extends DFCGLP:
  def apply(): DFCG = DFC.emptyNoEO
  @metaContextIgnore
  given DFCG(using dfc: DFC): DFCG = dfc
  given Conversion[DFC, DFCG] = identity

transparent inline def dfc(using d: DFC): d.type = d

trait HasDFC:
  lazy val dfc: DFC
  protected given DFC = dfc
