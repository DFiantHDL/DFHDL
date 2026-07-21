package dfhdl.sim

import dfhdl.compiler.ir
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.core.*
import dfhdl.internals.*

/** The typed, object-based simulation frontend (locked decision 10): a deferred [[Simulation]]
  * builder entered via `dsn.simulation { dut => ... }`, with `peek`/`poke` extensions on the
  * design's member objects. Values are DFHDL constants (`peek` on a port of type `T` returns
  * `T <> CONST`; `poke` accepts what `:=` accepts) of ANY type — wide values and composites
  * (structs, vectors) cross the boundary as packed bits and convert through the IR's canonical
  * `dataToBitsData`/`bitsDataToData`. The typed layer is a thin wrapper over the raw kernel access
  * ([[Sim]]: name-path handles, packed bits), which is internal-only — the typed surface is the
  * sole public value interface.
  *
  * Executing a [[Simulation]] yields a [[SimulationRun]] — the live context of that execution
  * (state, elapsed cycles, lifecycle status), which can be inspected while not running and
  * continued while paused.
  */
final class Simulation[D <: Design] private[sim] (
    dsn: D,
    block: Option[DFCG ?=> SimCtx ?=> D => Unit],
    tier: SimTier,
    seed: Long,
    // internal: an extra lowering applied to the design's elaborated DB before simulation (the
    // staged-oracle harnesses run compiler stages here). The typed surface is unaffected —
    // member-path and type resolution still go through the design's frontend objects, whose
    // names the stages keep.
    dbTransform: ir.DB => ir.DB = identity,
    policy: SeverityPolicy = SeverityPolicy()
):
  def withTier(tier: SimTier): Simulation[D] =
    new Simulation(dsn, block, tier, seed, dbTransform, policy)
  // seed is carried for API completeness; randomization support lands later
  def withSeed(seed: Long): Simulation[D] =
    new Simulation(dsn, block, tier, seed, dbTransform, policy)

  /** Configure what a `Warning`/`Error` report or failed assertion does to the run (see
    * [[SeverityAction]]). `Fatal` always finishes the run, like `$fatal` / VHDL `failure`.
    */
  def withSeverityPolicy(
      warning: SeverityAction = policy.warning,
      error: SeverityAction = policy.error
  ): Simulation[D] =
    new Simulation(dsn, block, tier, seed, dbTransform, SeverityPolicy(warning, error))

  private def newRun[R <: SimulationRun[D]](
      mk: (
          D,
          Sim,
          ir.DFVal => String,
          ir.DFType => ir.DFType,
          Option[DFCG ?=> SimCtx ?=> D => Unit]
      ) => R,
      limit: Long
  ): R =
    val raw = DFacsimile.simulate(dbTransform(dsn.getDB), tier)
    raw.severityPolicy = policy
    val r = mk(dsn, raw, memberPath, concretize, block)
    r.start(limit)
    r
  end newRun

  /** Execute the simulation on the calling flow: fresh state per run; the host block runs as the
    * main process (on a managed worker thread, so a cycle-limit can pause it). Returns once the run
    * pauses or finishes; host-block failures are rethrown here. Block-less simulations start paused
    * and are driven imperatively via [[SimulationRun.continue]]/[[SimulationRun.inspect]].
    */
  def run(limit: Long = Long.MaxValue): SimulationRun[D] =
    newRun(new SimulationRun[D](_, _, _, _, _), limit)

  /** Execute the simulation in the background: returns immediately with a running handle that can
    * be paused, inspected, continued, and finished. Host-block failures are recorded as
    * `Finished(HostError)` and rethrown by [[SimulationBackgroundRun.finish]].
    */
  def runBackground(limit: Long = Long.MaxValue): SimulationBackgroundRun[D] =
    if block.isEmpty then
      throw new IllegalStateException("a background run requires a simulation host block")
    newRun(new SimulationBackgroundRun[D](_, _, _, _, _), limit)

  // The member-object -> hierarchical-name-path bridge: frontend member objects are
  // elaboration-time IR values; resolving their owner chain through the design's own
  // (mutable-DB) getSet yields the instance-named path the lowering registered.
  private def memberPath(m: ir.DFVal): String =
    given MemberGetSet = dsn.dfc.getSet
    if m.isAnonymous then
      throw new IllegalArgumentException(
        s"peek/poke target must be a named design member, got an anonymous value:\n$m"
      )
    var path = m.getName
    var d = m.getOwnerDesign
    while !d.isTop do
      path = s"${instSegment(d)}.$path"
      d = d.getOwnerDesign
    path

  /** Resolve every param-dependent width/dimension in the type to a literal (host-side constants
    * must carry a concrete type). Recurses through composites.
    */
  private def concretize(t: ir.DFType): ir.DFType =
    given MemberGetSet = dsn.dfc.getSet
    given ir.ConstData.CachePolicy = ir.ConstData.CachePolicy.NoCache
    def lit(ref: ir.IntParamRef): ir.IntParamRef =
      ir.IntParamRef(ref.getIntConstData.toOption.getOrElse(
        throw new IllegalArgumentException(s"cannot resolve a param-dependent width in type:\n$t")
      ))
    def rec(t: ir.DFType): ir.DFType = t match
      case b: ir.DFBits    => b.copy(widthParamRef = lit(b.widthParamRef))
      case d: ir.DFDecimal => d.copy(magnitudeWidthParamRef = lit(d.magnitudeWidthParamRef))
      case v: ir.DFVector  =>
        v.copy(cellType = rec(v.cellType), cellDimParamRefs = v.cellDimParamRefs.map(lit))
      case s: ir.DFStruct => s.copy(fieldMap = s.fieldMap.map((n, ft) => (n, rec(ft))))
      case o: ir.DFOpaque => o.copy(actualType = rec(o.actualType))
      case other          => other
    rec(t)
  end concretize

  // the hierarchical immutable DB, used only by the bridge to enumerate sibling instances
  private lazy val hierDB = dsn.dfc.getSet.designDB

  // Repeated instance names (e.g. `List.fill(n)(SubDesign())`) get indexed path segments
  // (adder_0, adder_1, ...) matching the lowering's naming. The cached (elaboration-time)
  // DFDesignInst maps to its immutable-DB copy by `ownerRef` (preserved through inst
  // unification); the rank among same-named instances of the same parent sub-DB in member
  // order gives the index (all instances within one sub-DB share the parent design).
  private def instSegment(d: ir.DFDesignBlock)(using MemberGetSet): String =
    val cached = d.getCachedDesignInst
    val name = cached.getName
    hierDB.subDBs.values.view.flatMap { sub =>
      val insts = sub.members.collect { case i: ir.DFDesignInst => i }
      insts.find(_.ownerRef == cached.ownerRef).map { myCopy =>
        val siblings = insts.filter(_.getName(using sub.getSet) == name)
        if siblings.sizeIs > 1 then s"${name}_${siblings.indexOf(myCopy)}" else name
      }
    }.headOption.getOrElse(name)
end Simulation

/** What the simulation does when a `Warning`/`Error` report or failed assertion fires: keep running
  * (the default, matching HDL simulators — the message is emitted and counted), pause the run
  * (resumable with `continue`, status `Paused(Warning | Error)`), or finish it (terminal, status
  * `Finished(Warning | Error)`). `Info` always continues; `Fatal` always finishes.
  */
enum SeverityAction derives CanEqual:
  case Continue, Pause, Finish

/** The per-severity [[SeverityAction]] configuration of a [[Simulation]]. */
private[sim] final case class SeverityPolicy(
    warning: SeverityAction = SeverityAction.Continue,
    error: SeverityAction = SeverityAction.Continue
)

/** Why a simulation run is paused. A paused run holds its full context and can be continued. */
enum PausedReason derives CanEqual:
  /** an explicit external `pause()` request (background runs) */
  case User

  /** the granted cycle budget (the `run`/`continue` limit) was exhausted */
  case Limit

  /** an error report/assertion fired under a severity policy configured to pause */
  case Error

  /** a warning report/assertion fired under a severity policy configured to pause */
  case Warning

/** Why a simulation run finished. A finished run is terminal — it cannot be continued. */
enum FinishedReason derives CanEqual:
  /** the host block (and, later, all forked processes) ran to completion — or, for a block-less run
    * of a closed (port-less) design, event starvation: the design reached a state fixpoint with no
    * pending waits and no output, so nothing can ever happen again
    */
  case MainDone

  /** a `finish` statement was reached */
  case Finish

  /** a fatal report/assertion fired */
  case Fatal

  /** an error report/assertion fired under a severity policy configured to finish */
  case Error

  /** a warning report/assertion fired under a severity policy configured to finish */
  case Warning

  /** the host block died with an exception (rethrown by foreground `run`/`continue` and by
    * background `finish`)
    */
  case HostError
end FinishedReason

/** Lifecycle status of a [[SimulationRun]]. */
enum RunStatus derives CanEqual:
  case Running
  case Paused(reason: PausedReason)
  case Finished(reason: FinishedReason)

/** Control-flow unwind of the host block when the simulation ends from within a `step` — a `finish`
  * statement, a fatal report/assertion, or a severity policy configured to finish.
  */
private final class SimFinishedControl(val status: RunStatus)
    extends scala.util.control.ControlThrowable

/** The live context of one simulation execution: the state, the elapsed cycle count, and the
  * lifecycle status. This is the run-closure of locked decision 9 in object form — everything a
  * checkpoint needs is reachable from here. While the run is paused (or finished) it can be
  * [[inspect]]ed with the same typed peek/poke surface as the host block; while paused it can be
  * [[continue]]d with a fresh cycle budget. Once finished, a run is terminal.
  *
  * The host block executes on a managed daemon worker thread, advancing the kernel in bounded
  * slices with a pause/budget gate between them — so limits and external pauses interrupt even a
  * single huge `step(n)` call with slice-level responsiveness.
  */
class SimulationRun[D <: Design] private[sim] (
    dsn: D,
    private[sim] val raw: Sim,
    memberPath: ir.DFVal => String,
    concretizeType: ir.DFType => ir.DFType,
    blockOpt: Option[DFCG ?=> SimCtx ?=> D => Unit]
):
  import RunStatus.*
  private val lock = new Object
  private var statusVar: RunStatus = Running
  private var cyclesVar: Long = 0L
  private var budget: Long = 0L // remaining cycle budget; Long.MaxValue = unbounded
  private var pauseRequested = false
  private var hostError: Option[Throwable] = None
  private var worker: Option[Thread] = None
  // pause/limit responsiveness: the worker advances in slices of this many cycles
  private val sliceSize = 4096L

  /** `run`/`continue` on a foreground run block until the run is no longer running; a background
    * run returns control immediately instead.
    */
  protected def continueSync: Boolean = true

  def getRunStatus: RunStatus = lock.synchronized(statusVar)

  /** Total clock cycles this run has advanced. */
  def cycles: Long = lock.synchronized(cyclesVar)

  /** Grant a paused run a fresh cycle budget and let it proceed. Foreground runs block until the
    * next pause/finish (host-block failures rethrown); background runs return immediately.
    * Block-less runs advance on the calling flow and therefore require a finite limit. A finished
    * run cannot be continued.
    */
  def continue(limit: Long = Long.MaxValue): RunStatus =
    require(limit > 0, "the cycle limit must be positive")
    val hasWorker = lock.synchronized {
      statusVar match
        case Finished(_) =>
          throw new IllegalStateException("a finished simulation run cannot be continued")
        case Running =>
          throw new IllegalStateException("the simulation run is already running")
        case Paused(_) => ()
      budget = limit
      if worker.nonEmpty then
        statusVar = Running
        lock.notifyAll()
      worker.nonEmpty
    }
    if !hasWorker then // block-less: imperative driving on the calling flow
      if limit == Long.MaxValue then
        throw new IllegalArgumentException("a block-less simulation run requires a finite limit")
      blocklessAdvance(limit)
    else if continueSync then
      awaitNotRunning()
      rethrowHostError()
    getRunStatus
  end continue

  /** Enter the run's context while it is not running (paused or finished): peek, poke, and settle
    * with the same typed surface as the host block. Advancing cycles from here is rejected — use
    * [[continue]].
    */
  def inspect[T](f: DFCG ?=> SimCtx ?=> D => T): T =
    lock.synchronized {
      if isRunningLocked then
        throw new IllegalStateException("cannot inspect a running simulation — pause it first")
    }
    f(using DFCG())(using makeCtx())(dsn)

  // ---- run machinery (private[sim]) --------------------------------------------------------

  private def makeCtx(): SimCtx = new SimCtx(raw, memberPath, concretizeType, this)

  private def isRunningLocked: Boolean = statusVar match
    case Running => true
    case _       => false

  private def isPausedLocked: Boolean = statusVar match
    case Paused(_) => true
    case _         => false

  private def isFinishedLocked: Boolean = statusVar match
    case Finished(_) => true
    case _           => false

  private def stopStatus(stop: SimStop): RunStatus = stop match
    case SimStop.Finish              => Finished(FinishedReason.Finish)
    case SimStop.Fatal               => Finished(FinishedReason.Fatal)
    case SimStop.SevFinish(severity) =>
      Finished(severity match
        case ir.TextOut.Severity.Warning => FinishedReason.Warning
        case _                           => FinishedReason.Error)
    case SimStop.SevPause(severity) =>
      Paused(severity match
        case ir.TextOut.Severity.Warning => PausedReason.Warning
        case _                           => PausedReason.Error)

  /** Block-less advance on the calling flow: consumes up to `limit` cycles and maps the raw
    * engine's outcome to the run status — a text-output stop (finish/fatal/severity policy), event
    * starvation of a closed design, or the exhausted budget.
    */
  private def blocklessAdvance(limit: Long): Unit =
    val consumed = raw.step(limit)
    lock.synchronized {
      cyclesVar += consumed
      statusVar = raw.stopCause match
        case Some(stop)          => stopStatus(stop)
        case None if raw.starved => Finished(FinishedReason.MainDone)
        case None                => Paused(PausedReason.Limit)
    }

  private def awaitNotRunning(): Unit =
    lock.synchronized { while isRunningLocked do lock.wait() }

  private def rethrowHostError(): Unit =
    lock.synchronized(hostError) match
      case Some(e) => throw e
      case None    => ()

  private[sim] def start(limit: Long): Unit = blockOpt match
    case Some(block) =>
      val t = new Thread(() => runBlock(block), "dfacsimile-run")
      t.setDaemon(true)
      lock.synchronized {
        budget = limit
        worker = Some(t)
      }
      t.start()
      if continueSync then
        awaitNotRunning()
        rethrowHostError()
    case None => // block-less: start paused; drive via continue(limit)/inspect
      if limit != Long.MaxValue && limit > 0 then blocklessAdvance(limit)
      else lock.synchronized { statusVar = Paused(PausedReason.Limit) }

  private def runBlock(block: DFCG ?=> SimCtx ?=> D => Unit): Unit =
    val (endStatus, error) =
      try
        block(using DFCG())(using makeCtx())(dsn)
        (Finished(FinishedReason.MainDone), None)
      catch
        case e: SimFinishedControl => (e.status, None)
        case e: Throwable          => (Finished(FinishedReason.HostError), Some(e))
    lock.synchronized {
      hostError = error
      statusVar = endStatus
      lock.notifyAll()
    }

  /** The block-side clock driver: advances in slices, parking at the pause/budget gate. Only the
    * worker thread may call this (via [[SimCtx.step]]).
    */
  private[sim] def blockStep(n: Long): Unit =
    require(n >= 0, "negative cycle count")
    val isWorker = lock.synchronized(worker.exists(_ eq Thread.currentThread))
    if !isWorker then
      throw new IllegalStateException(
        "`step` is only available to the simulation's driving block — " +
          "advance a paused run with `continue(...)`"
      )
    var remaining = n
    while remaining > 0 do
      val slice = lock.synchronized {
        while pauseRequested || budget == 0L do
          statusVar = Paused(if pauseRequested then PausedReason.User else PausedReason.Limit)
          pauseRequested = false
          lock.notifyAll()
          lock.wait()
        math.min(sliceSize, math.min(remaining, budget))
      }
      val consumed = raw.step(slice)
      lock.synchronized {
        cyclesVar += consumed
        if budget != Long.MaxValue then budget -= consumed
      }
      remaining -= consumed
      raw.stopCause match
        case Some(stop @ SimStop.SevPause(_)) =>
          // a severity policy configured to pause: park here (mid-`step`) until continued
          lock.synchronized {
            statusVar = stopStatus(stop)
            lock.notifyAll()
            while isPausedLocked do lock.wait()
          }
        case Some(stop) =>
          // finish, fatal, or a severity policy configured to finish: unwind the host block
          throw new SimFinishedControl(stopStatus(stop))
        case None => ()
    end while
  end blockStep

  /** Request a pause and wait until the run is actually paused (or finished) — after this returns,
    * inspection is safe.
    */
  protected final def doPause(): RunStatus = lock.synchronized {
    if isRunningLocked then
      pauseRequested = true
      while isRunningLocked do lock.wait()
    statusVar
  }

  /** Lift the budget, let the run proceed to its natural end, and wait for it; host-block failures
    * are rethrown here.
    */
  protected final def doFinish(): RunStatus =
    lock.synchronized {
      if !isFinishedLocked then
        budget = Long.MaxValue
        pauseRequested = false
        statusVar match
          case Paused(_) =>
            statusVar = Running
            lock.notifyAll()
          case _ => ()
        while !isFinishedLocked do lock.wait()
    }
    rethrowHostError()
    getRunStatus
end SimulationRun

/** A background run handle: the host block keeps running while the caller holds this. `continue`
  * returns immediately (the run keeps going in the background); [[pause]] suspends the run at the
  * next slice boundary for safe inspection; [[finish]] lets it run to its natural end and joins.
  */
final class SimulationBackgroundRun[D <: Design] private[sim] (
    dsn: D,
    raw: Sim,
    memberPath: ir.DFVal => String,
    concretizeType: ir.DFType => ir.DFType,
    blockOpt: Option[DFCG ?=> SimCtx ?=> D => Unit]
) extends SimulationRun[D](dsn, raw, memberPath, concretizeType, blockOpt):
  override protected def continueSync: Boolean = false

  /** Suspend the run at the next slice boundary; blocks until it is actually paused (or has
    * finished on its own). After this returns, [[inspect]] is safe.
    */
  def pause(): RunStatus = doPause()

  /** Let the run proceed unbounded to its natural end and wait for it; host-block failures are
    * rethrown here. Returns the terminal status.
    */
  def finish(): RunStatus = doFinish()
end SimulationBackgroundRun

/** Simulation capability context: passed contextually into the host block (and
  * [[SimulationRun.inspect]]).
  */
final class SimCtx private[sim] (
    // internal kernel access: by-name-path handles, packed bits (not public API —
    // the typed peek/poke surface is the only value interface)
    private[sim] val raw: Sim,
    private[sim] val memberPath: ir.DFVal => String,
    private[sim] val concretize: ir.DFType => ir.DFType,
    private[sim] val run: SimulationRun[?]
):
  def step(cycles: Long = 1L): Unit = run.blockStep(cycles)
  def settle(): Unit = raw.settle()
end SimCtx

/** Summon the current simulation context (e.g. `simCtx.step()`). */
def simCtx(using ctx: SimCtx): SimCtx = ctx

extension [D <: Design](dsn: D)
  /** Create a deferred simulation with a host block (the simulation's main process). */
  def simulation(block: DFCG ?=> SimCtx ?=> D => Unit): Simulation[D] =
    new Simulation(dsn, Some(block), SimTier.Interpreter, 0L)

  /** Create a deferred simulation with no host block (testbench-is-IR designs). */
  def simulation: Simulation[D] =
    new Simulation(dsn, None, SimTier.Interpreter, 0L)

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  /** Read the combinationally settled value of this member as a DFHDL constant. */
  def peek(using ctx: SimCtx, dfc: DFCG): DFConstOf[T] =
    import dfc.getSet
    val irVal = dfVal.asIR
    val bits = ctx.raw.peekBits(ctx.memberPath(irVal))
    val concreteType = ctx.concretize(irVal.dfType)
    val data = concreteType.bitsDataToData((bits, BitVector.low(bits.width)))
    DFVal.Const.forced(concreteType.asFE[T], data)

  /** Deposit a value on this member; accepts exactly what `:=` accepts. */
  def poke(value: DFVal.TC.Exact[T])(using ctx: SimCtx, dfc: DFCG): Unit =
    import dfc.getSet
    val constIR = value(ctx.concretize(dfVal.asIR.dfType).asFE[T]).asIR match
      case c: ir.DFVal if c.isConst => c
      case other                    =>
        throw new IllegalArgumentException(s"poke value must convert to a constant, got:\n$other")
    val t = constIR.dfType
    val (bits, bubble) = t.dataToBitsData(constIR.getConstDataOrDefault[Any].asInstanceOf[t.Data])
    if !bubble.isZeros then
      throw new IllegalArgumentException(s"cannot poke a bubble (?) value:\n$constIR")
    ctx.raw.pokeBits(ctx.memberPath(dfVal.asIR), bits)
end extension
