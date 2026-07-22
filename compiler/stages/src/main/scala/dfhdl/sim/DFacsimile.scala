package dfhdl.sim

import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import scala.collection.mutable

/** Which execution kernel runs the lowered design. */
enum SimTier derives CanEqual:
  case Interpreter, Codegen

/** Minimum-viable DFacsimile: lowers an elaborated DFHDL design DB (new-style hierarchical root DB)
  * into the flat pre-scheduled [[Netlist]] and executes it on a kernel tier ([[Interpreter]] or
  * [[Codegen]]). Values of any width lower through [[WideOps]] into 64-bit lanes (word-slicing),
  * and composite types (structs, vectors, opaques) lower as their packed bits — so the kernels stay
  * pure-`Long` machines.
  *
  * Supported IR subset (fails loudly on anything else):
  *   - Dcls (vars/ports) of DFBits/DFDecimal/DFBool/DFBit/DFEnum of any width, plus DFStruct /
  *     DFVector / DFOpaque compositions (packed-bits representation, nesting included)
  *   - REG Dcls with constant init; register hold semantics when unassigned in a branch
  *   - `.reg`/`.prev` (History State) aliases with any step and constant init, sampling the target
  *     at the alias position (wires sampled inside a conditional branch are rejected)
  *   - funcs: n-ary `+`/`-`/`&`/`|`/`^` (width-extending variants included), `*`/`/`/`%` (up to
  *     64-bit results), `++`, comparisons, `unary_-`/`unary_~`/`unary_!`, `<<`/`>>` by constant or
  *     dynamic amount, `ror`/`rol`, `reverse`/`repeat`, `max`/`min`/`abs`, `sel`
  *   - `AsIs` casts (sign-extending for signed sources), bit-select/range on Bits (constant or
  *     dynamic index), field select on structs, vector indexing (constant or dynamic index;
  *     constant-vector indexing becomes per-lane ROMs)
  *   - assignments to partial targets (bit/range/field/cell, dynamic indices included) as RMW
  *   - conditionals: `if`/`match` chains (statement and expression form) lowered to mux trees with
  *     sequential-assignment semantics per scope
  *   - hierarchy: per-instance elaboration of sub-design instances over their (shared) sub-DBs;
  *     port connections through PortByNameSelect, including partial (bit/range) sinks
  *   - top-level IN ports become pokeable hold-state cells; init applies at time zero (no reset
  *     modeling); design params resolve per instance when constant
  *   - RT processes with steps/waits/loops, lowered to FSM sites directly from the IR (the
  *     ProcLowering walk) with the documented cycle semantics
  *   - text output (`print`/`println`/`debug`/`report`/`assert`/`finish`) in design bodies and
  *     processes: statements fire per committed cycle under their full path condition with the
  *     cycle's settled values; report/assert severities feed the run's severity policy, `Fatal` and
  *     `finish` end the run. A design-body (combinational-context) statement whose condition stays
  *     true fires on every such cycle — the clocked reading of what an event-driven simulator would
  *     report per activation.
  *
  * Known minimum limitations: `**`/`clog2` on non-constants, multiplication/division with results
  * wider than 64 bits, bubble (`?`) values simulate as 0 (2-state), non-constant string message
  * arguments, and per-instance param-dependent *widths* (widths resolve via the sub-DB's canonical
  * instance).
  */
object DFacsimile:
  /** Internal raw entry: lowers a DB and returns the raw kernel access ([[Sim]]). Not public API —
    * the typed [[Simulation]] surface (`dsn.simulation { dut => ... }`) is the sole public
    * simulation interface; this stays reachable only for the engine itself and its in-package
    * harnesses (e.g. staged-oracle lockstep over stage-lowered DBs, which have no frontend object).
    */
  private[sim] def simulate(db: DB, tier: SimTier = SimTier.Interpreter): Sim =
    val builder = new Builder(db)
    builder.build()
    val kernel = tier match
      case SimTier.Interpreter => Interpreter.compile(builder.nl)
      case SimTier.Codegen     =>
        // named values are peekable — force their lanes into the signal array; the scheduler
        // reads wait bounds after a probe, so they must be materialized too; text-output actions
        // read their guards and message values from the signal array after the fired cycle
        val schedObserved = builder.procMetas.iterator.flatMap(_.timers.values.map(_.boundNode))
        val actionObserved = builder.actions.iterator.flatMap { a =>
          Iterator.single(a.guard) ++ a.segs.iterator.flatMap {
            case ActSeg.Arg(wv, _) => wv.lanes
            case _                 => Nil
          }
        } ++ Option.when(builder.watchNode >= 0)(builder.watchNode)
        Codegen.compile(
          builder.nl,
          // debug aid: -Ddfhdl.sim.codegen.dumpSource=true prints the generated kernel Java
          dumpSource = java.lang.Boolean.getBoolean("dfhdl.sim.codegen.dumpSource"),
          observed =
            builder.namedNodes.values.flatMap(_.lanes).toSet ++ schedObserved ++ actionObserved,
          watchNode = builder.watchNode
        )
    new Sim(
      builder.nl,
      kernel,
      builder.namedNodes.toMap,
      builder.procMetas.toVector,
      builder.actions.toVector,
      builder.watchNode,
      builder.topHasInputs
    )
  end simulate
end DFacsimile

/** Scheduler metadata for one cycle-wait park of a lowered RT process: the wait's up-counter cell
  * and its live bound node (`cycles - 1`, re-evaluated per cycle like the equivalent FSM counter
  * would). The parked remaining cycle count is `(bound - counter) mod 2^width`.
  */
private[sim] final case class WaitTimerMeta(counterReg: Int, boundNode: Int, mask: Long)

/** Scheduler metadata for one lowered RT process: its FSM state cell and its cycle-wait timers
  * keyed by the state value that parks on them.
  */
private[sim] final case class ProcMeta(segReg: Int, timers: Map[Long, WaitTimerMeta])

/** Why [[Sim.step]] stopped before exhausting its cycle budget. `Finish`, `Fatal`, and `SevFinish`
  * are terminal (further stepping consumes nothing); `SevPause` clears on the next step.
  */
private[sim] enum SimStop derives CanEqual:
  /** a `finish` statement executed */
  case Finish

  /** a fatal report/assertion fired (always terminal, like `$fatal` / VHDL `failure`) */
  case Fatal

  /** a warning/error report/assertion fired under a severity policy configured to pause */
  case SevPause(severity: TextOut.Severity)

  /** a warning/error report/assertion fired under a severity policy configured to finish */
  case SevFinish(severity: TextOut.Severity)

/** One piece of a text-output action's message: a literal, or a value rendered from the fired
  * cycle's settled signals.
  */
private[sim] enum ActSeg:
  case Lit(text: String)
  case Arg(wv: WV, render: BitVector => String)

/** What a fired text-output action does beyond emitting its message. */
private[sim] enum ActKind derives CanEqual:
  /** plain output: `print`/`println`/`debug` */
  case Output

  /** a `report` statement or a failed assertion: severity prefix + run context, then the severity
    * policy (`Fatal` always finishes)
    */
  case Report(severity: TextOut.Severity)

  /** a `finish` statement */
  case Finish

/** A lowered text-output statement: fires on every committed cycle whose settled `guard` value is
  * nonzero (the full path condition — FSM site dispatch, branch guards, and a failing assertion
  * condition — folded into one 1-bit node). Message values read the fired cycle's settled sweep
  * (register operands are MOV-snapshot). `where` is the instance path for report/assert context.
  */
private[sim] final case class SimAction(
    guard: Int,
    kind: ActKind,
    segs: Vector[ActSeg],
    where: String
)

/** A running simulation instance: one state/signal array + a kernel over it. Values are addressed
  * by name; hierarchy paths use instance names (e.g. "alu0.res"). Values of any width move across
  * this boundary as packed [[BitVector]]s; the `Long` variants are a convenience for values up to
  * 64 bits.
  */
final class Sim private[sim] (
    val nl: Netlist,
    kernel: SimKernel,
    nameToWV: Map[String, WV],
    procMetas: Vector[ProcMeta],
    actions: Vector[SimAction] = Vector.empty,
    watchNode: Int = -1,
    hasTopInputs: Boolean = false
):
  private val sig = nl.initialSig
  // settle-on-peek: peeks always observe combinationally settled state (Amaranth's rule)
  private var needsSettle = true

  /** Cycles the scheduler skipped (not evaluated by a kernel) — observability for skip tests. */
  private var skippedVar = 0L
  def skippedCycles: Long = skippedVar

  // total committed cycles — the sim time stamped on report/assert messages
  private var cyclesVar = 0L
  private[sim] def totalCycles: Long = cyclesVar

  // text-output run state: the output sink, the warning/error severity policy (set by the typed
  // Simulation layer), fired-severity counters, and the stop cause of the last step
  private[sim] var textSink: String => Unit = Console.out.print(_)
  private[sim] var severityPolicy: SeverityPolicy = SeverityPolicy()
  private var warningsVar = 0L
  private var errorsVar = 0L
  private[sim] def warningCount: Long = warningsVar
  private[sim] def errorCount: Long = errorsVar
  private var stopVar: Option[SimStop] = None
  private[sim] def stopCause: Option[SimStop] = stopVar

  /** Event starvation: a closed design (no pokeable top inputs) proved to be at a state fixpoint
    * with no pending cycle-wait timers and no active output — nothing can ever happen again.
    */
  private var starvedVar = false
  private[sim] def starved: Boolean = starvedVar

  /** Advance up to `cycles` clock cycles, returning the count actually consumed. Text-output
    * actions fire per committed cycle; a `finish`, a fatal, or a severity-policy stop cuts the
    * budget short (see [[stopCause]]). A terminal stop makes further stepping consume nothing.
    */
  def step(cycles: Long = 1L): Long =
    stopVar match
      case Some(SimStop.SevPause(_)) => stopVar = None // a severity pause is per-fire
      case _                         => ()
    if stopVar.nonEmpty || cycles <= 0 then 0L
    else
      val consumed =
        if procMetas.nonEmpty then stepScheduled(cycles)
        else if actions.nonEmpty then stepWatched(cycles)
        else
          kernel.run(sig, cycles)
          cyclesVar += cycles
          cycles
      needsSettle = true // post-commit register values invalidate the comb sweep
      consumed
  end step

  /** Renders an action's message from the fired cycle's settled signal values. */
  private def actText(a: SimAction): String =
    val sb = new StringBuilder
    for seg <- a.segs do
      seg match
        case ActSeg.Lit(text)       => sb ++= text
        case ActSeg.Arg(wv, render) =>
          val bits = wv.lanes.zipWithIndex.reverse.map { (n, i) =>
            BitVector.fromLong(sig(n), math.min(64, wv.width - 64 * i))
          }.reduce(_ ++ _)
          sb ++= render(bits)
    sb.result()

  /** Executes the fired actions of the just-committed cycle in program order, reading the cycle's
    * settled values (combinational slots and register MOV snapshots survive the commit). A finish
    * or fatal stops the remaining actions of the cycle; a severity pause lets them complete first.
    * Returns true when stepping must stop.
    */
  private def fireActions(): Boolean =
    var pausePend: Option[TextOut.Severity] = None
    var terminal = false
    var i = 0
    while i < actions.length && !terminal do
      val a = actions(i)
      if sig(a.guard) != 0L then
        a.kind match
          case ActKind.Output           => textSink(actText(a))
          case ActKind.Report(severity) =>
            textSink(
              s"${severity.toString.toUpperCase}: ${actText(a)} [${a.where} @ cycle $cyclesVar]\n"
            )
            severity match
              case TextOut.Severity.Info    => ()
              case TextOut.Severity.Warning =>
                warningsVar += 1
                severityPolicy.warning match
                  case SeverityAction.Continue => ()
                  case SeverityAction.Pause    =>
                    if pausePend.isEmpty then pausePend = Some(severity)
                  case SeverityAction.Finish =>
                    stopVar = Some(SimStop.SevFinish(severity))
                    terminal = true
              case TextOut.Severity.Error =>
                errorsVar += 1
                severityPolicy.error match
                  case SeverityAction.Continue => ()
                  case SeverityAction.Pause    =>
                    if pausePend.isEmpty then pausePend = Some(severity)
                  case SeverityAction.Finish =>
                    stopVar = Some(SimStop.SevFinish(severity))
                    terminal = true
              case TextOut.Severity.Fatal =>
                stopVar = Some(SimStop.Fatal)
                terminal = true
            end match
          case ActKind.Finish =>
            stopVar = Some(SimStop.Finish)
            terminal = true
      end if
      i += 1
    end while
    if !terminal then pausePend.foreach(sev => stopVar = Some(SimStop.SevPause(sev)))
    stopVar.nonEmpty
  end fireActions

  /** Per-cycle watched stepping for process-less designs with text output: the kernel bulk-runs
    * between fires, exiting after any cycle whose aggregated watch value is nonzero.
    */
  private def stepWatched(cycles: Long): Long =
    var remaining = cycles
    var consumed = 0L
    var stop = false
    while remaining > 0 && !stop do
      val ran = kernel.runWatch(sig, remaining, watchNode)
      consumed += ran
      cyclesVar += ran
      remaining -= ran
      if sig(watchNode) != 0L then stop = fireActions()
    consumed

  /** The event-timeline scheduler (single-clock degenerate hyperperiod): probe a cycle with change
    * tracking; a clean probe proves the design is at a state fixpoint except wait counters, so the
    * timeline jumps to the nearest cycle-wait expiry (or the budget end when only condition/endless
    * waits remain) by bulk-advancing the counters. A dirty probe backs off into untracked bulk
    * chunks with exponential growth, so active designs run at full kernel speed. Text-output
    * actions fire per evaluated cycle (the watch aggregate keeps bulk runs cheap); an action active
    * at a fixpoint (e.g. a print inside a condition-wait park) blocks skipping — every one of its
    * cycles must be evaluated and emitted. A clean probe with no pending timers, no active output,
    * and no pokeable inputs is event starvation: the remaining budget is consumed as skipped time
    * and the run is flagged [[starved]].
    */
  private def stepScheduled(cycles: Long): Long =
    val hasWatch = watchNode >= 0
    var remaining = cycles
    var chunk = 1L
    var consumed = 0L
    var stop = false
    while remaining > 0 && !stop do
      val dirty = kernel.stepDirty(sig)
      consumed += 1
      cyclesVar += 1
      remaining -= 1
      val fired = hasWatch && sig(watchNode) != 0L
      if fired then stop = fireActions()
      if !stop then
        if dirty then
          var c = math.min(chunk, remaining)
          while c > 0 && !stop do
            val ran =
              if hasWatch then kernel.runWatch(sig, c, watchNode)
              else
                kernel.run(sig, c)
                c
            consumed += ran
            cyclesVar += ran
            remaining -= ran
            c -= ran
            if hasWatch && sig(watchNode) != 0L then stop = fireActions()
          chunk = math.min(chunk * 2, 65536L)
        else if fired then chunk = 1L // active output at a fixpoint: no skipping, re-probe
        else if remaining > 0 then
          // fixpoint except timers: how far until the nearest active cycle-wait expires?
          var horizon = Long.MaxValue
          for pm <- procMetas do
            pm.timers.get(sig(pm.segReg)) match
              case Some(t) =>
                val rem = (sig(t.boundNode) - sig(t.counterReg)) & t.mask
                if rem < horizon then horizon = rem
              case None => // parked without a timer (condition/endless wait or a held FSM state)
          if horizon == Long.MaxValue && !hasTopInputs then starvedVar = true
          val delta = math.min(horizon, remaining)
          if delta > 0 then
            for pm <- procMetas do
              pm.timers.get(sig(pm.segReg)) match
                case Some(t) => sig(t.counterReg) = (sig(t.counterReg) + delta) & t.mask
                case None    =>
            consumed += delta
            cyclesVar += delta
            remaining -= delta
            skippedVar += delta
          chunk = 1L
        end if
      end if
    end while
    consumed
  end stepScheduled
  def settle(): Unit =
    kernel.settle(sig)
    needsSettle = false
  private def wvOf(name: String): WV =
    nameToWV.getOrElse(
      name,
      throw new NoSuchElementException(
        s"no named value: $name\navailable: ${nameToWV.keySet.toList.sorted.mkString(", ")}"
      )
    )
  def peekBits(name: String): BitVector =
    if needsSettle then settle()
    val wv = wvOf(name)
    wv.lanes.zipWithIndex.reverse.map { (n, i) =>
      BitVector.fromLong(sig(n), math.min(64, wv.width - 64 * i))
    }.reduce(_ ++ _)
  def pokeBits(name: String, bits: BitVector): Unit =
    val wv = wvOf(name)
    require(
      bits.width == wv.width,
      s"poke width mismatch on '$name': got ${bits.width}, expected ${wv.width}"
    )
    for (n, i) <- wv.lanes.zipWithIndex do
      sig(n) = bits.bitsWL(math.min(64, wv.width - 64 * i), 64 * i).toLong(signed = false)
    needsSettle = true
  def peek(name: String): Long =
    if needsSettle then settle()
    val wv = wvOf(name)
    require(wv.width <= 64, s"'$name' is ${wv.width} bits wide — use peekBits")
    sig(wv.lanes(0))
  def poke(name: String, value: Long): Unit =
    val wv = wvOf(name)
    require(wv.width <= 64, s"'$name' is ${wv.width} bits wide — use pokeBits")
    sig(wv.lanes(0)) = value & nl.maskOf(wv.lanes(0))
    needsSettle = true
  def widthOf(name: String): Int = wvOf(name).width
  def names: Set[String] = nameToWV.keySet
  private[sim] def debugSig(node: Int): Long =
    if needsSettle then settle()
    sig(node)
end Sim

private final class Builder(rawDB: DB):
  private[sim] val nl = new Netlist
  private val wide = new WideOps(nl)
  private[sim] val namedNodes = mutable.Map.empty[String, WV]
  private[sim] val procMetas = mutable.ArrayBuffer.empty[ProcMeta]
  // text-output actions in program (elaboration walk) order, and the OR-aggregate of their
  // guards the kernels watch per cycle; pokeable top inputs rule out event starvation
  private[sim] val actions = mutable.ArrayBuffer.empty[SimAction]
  private[sim] var watchNode: Int = -1
  private[sim] var topHasInputs = false
  // the new-style root DB is only a hierarchy container; content lives in per-design sub-DBs
  private val topScopeDB: DB = if rawDB.isRoot then rawDB.topDB else rawDB

  private def unsupported(what: String, m: Any): Nothing =
    throw new UnsupportedOperationException(
      s"DFacsimile (minimum) does not support $what:\n$m"
    )

  def build(): Unit =
    new Scope(topScopeDB, "", None).elaborate()
    if actions.nonEmpty then
      watchNode = actions.map(_.guard).reduce { (a, b) =>
        // a constant-true guard (e.g. an unconditional every-cycle print) makes the whole
        // watch constant; constant-false guards never register
        if nl.isConst(a) then (if nl.constValOf(a) != 0L then a else b)
        else if nl.isConst(b) then (if nl.constValOf(b) != 0L then b else a)
        else nl.or(a, b)
      }

  /** One design *instance*: a sub-DB elaborated with per-instance state (sub-DBs are shared across
    * instances of the same design).
    */
  private final class Scope(
      val db: DB,
      prefix: String,
      parentCtx: Option[(Scope, DFDesignInst)]
  ):
    private given MemberGetSet = db.getSet
    private val isTop = parentCtx.isEmpty
    private val (design, designMembers) = db.designMemberList match
      case (d, ms) :: Nil => (d, ms)
      case other          => unsupported(s"a sub-DB holding ${other.size} designs", db)

    // direct-ownership tree (reconstructs conditional block structure), in member order
    private val childrenOf: Map[DFOwner, Vector[DFMember]] =
      val map = mutable.LinkedHashMap.empty[DFOwner, mutable.ArrayBuffer[DFMember]]
      for m <- designMembers do
        m.ownerRef.get match
          case owner: DFOwner => map.getOrElseUpdate(owner, mutable.ArrayBuffer.empty) += m
          case _              => ()
      map.view.mapValues(_.toVector).toMap

    private val nodeOf = mutable.Map.empty[DFVal, WV]
    private val regNodeOf = mutable.Map.empty[DFVal.Dcl, WV]
    private val inPortMov = mutable.Map.empty[DFVal.Dcl, WV]
    // sequential current-value: wires = current driven value; REG dcls = pending din
    private val env = mutable.Map.empty[DFVal.Dcl, WV]
    private val partialDrivers = mutable.Map.empty[DFVal.Dcl, mutable.ArrayBuffer[(Int, Int, WV)]]
    private val childScopes = mutable.Map.empty[DFDesignInst, Scope]
    // net sink values (raw, pre-dealias) — skipped as reads during the walk
    private val netSinkOf = mutable.Map.empty[DFNet, DFVal]
    // nonzero while walking conditional-branch members (position-sensitive constructs care)
    private var condDepth = 0
    // the walk's path condition as (1-bit node, negated?) frames whose conjunction gates
    // text-output actions: branch walks push their guard, process site programs push their
    // dispatch condition. Kept as frames (not a node) so designs without text output pay nothing.
    private var pathConds: List[(Int, Boolean)] = Nil
    // reset-site fold probing (see `ProcLowering.run`): a dynamic dispatch decision or an emitted
    // text-output statement during the reset program's emission disqualifies the fold, mirroring
    // the stage's gates (static guards, constant full-register assignments only)
    private var foldProbing = false
    private var foldViolation = false

    private def andCond(a: Int, b: Int): Int =
      if nl.isConst(a) then (if nl.constValOf(a) != 0L then b else a)
      else if nl.isConst(b) then (if nl.constValOf(b) != 0L then a else b)
      else nl.and(a, b)

    /** The current path condition as a single 1-bit node: the conjunction of the tracked frames,
      * with an optional extra frame (an assertion's failing condition).
      */
    private def pathGuardNode(extra: Option[(Int, Boolean)]): Int =
      (extra.toList ::: pathConds).foldLeft(nl.const(1, 1L)) { case (acc, (n, neg)) =>
        andCond(acc, if neg then nl.not(n) else n)
      }
    // write-only views: net sinks and the alias chains under them (never built as reads)
    private val writeViews = mutable.Set.empty[DFVal]
    // context-sensitive value bindings during RT process lowering: process-owned values compile
    // per transition context (e.g. a loop guard reads forwarded values at a loop-back edge but
    // committed values inside a control state), so their nodes must not enter the global cache
    private var procOverlay: mutable.Map[DFVal, WV] = null
    // time-zero state overrides computed in the pre-pass from initial blocks and Rule-4-converted
    // process prologues ("initial wins" over a declaration init)
    private val initOverride = mutable.Map.empty[DFVal.Dcl, BitVector]

    /** Value-forwarding state of one process transition context (a site program). Mirrors the FSM
      * fusion's per-goto crossing model: register writes are pending until a conceptual cycle
      * boundary (a dispatch entry) promotes them into the read view; wire writes are blocking
      * within the region and unreadable across a boundary.
      */
    private final class TransCtx(val condBase: Int):
      var fwdRegs = Map.empty[DFVal.Dcl, WV]
      var fwdUnclean = Set.empty[DFVal.Dcl]
      var wireBarrier = Set.empty[DFVal.Dcl]
      var regsSinceCross = Set.empty[DFVal.Dcl]
      var uncleanSinceCross = Set.empty[DFVal.Dcl]
      var wiresSinceCross = Set.empty[DFVal.Dcl]
      def snapshot(): TransSnap =
        TransSnap(fwdRegs, fwdUnclean, wireBarrier, regsSinceCross, uncleanSinceCross,
          wiresSinceCross)
      def restore(s: TransSnap): Unit =
        fwdRegs = s.fwdRegs; fwdUnclean = s.fwdUnclean; wireBarrier = s.wireBarrier
        regsSinceCross = s.regsSinceCross; uncleanSinceCross = s.uncleanSinceCross
        wiresSinceCross = s.wiresSinceCross
    end TransCtx
    private final case class TransSnap(
        fwdRegs: Map[DFVal.Dcl, WV],
        fwdUnclean: Set[DFVal.Dcl],
        wireBarrier: Set[DFVal.Dcl],
        regsSinceCross: Set[DFVal.Dcl],
        uncleanSinceCross: Set[DFVal.Dcl],
        wiresSinceCross: Set[DFVal.Dcl]
    )
    private var transCtx: TransCtx = null

    /** Records an assignment for the transition context's forwarding model. */
    private def recordWrite(dcl: DFVal.Dcl, full: Boolean): Unit =
      val t = transCtx
      if t ne null then
        if regNodeOf.contains(dcl) then
          t.regsSinceCross += dcl
          if !full || condDepth > t.condBase then t.uncleanSinceCross += dcl
          else t.uncleanSinceCross -= dcl // a later clean full write supersedes
        else t.wiresSinceCross += dcl

    /** Crosses a conceptual cycle boundary: pending register writes become the read view. */
    private def crossBoundary(): Unit =
      val t = transCtx
      if t ne null then
        for dcl <- t.regsSinceCross do t.fwdRegs = t.fwdRegs.updated(dcl, env(dcl))
        t.fwdUnclean = t.fwdUnclean -- (t.regsSinceCross -- t.uncleanSinceCross)
          ++ t.uncleanSinceCross
        t.wireBarrier ++= t.wiresSinceCross
        t.regsSinceCross = Set.empty
        t.uncleanSinceCross = Set.empty
        t.wiresSinceCross = Set.empty

    def elaborate(): Unit =
      // pre-pass: net sink direction (connections are continuous — order-free via MOV patching)
      designMembers.foreach {
        case net: DFNet =>
          net.op match
            case DFNet.Op.Assignment                          => netSinkOf(net) = net.lhsRef.get
            case DFNet.Op.Connection | DFNet.Op.ViaConnection =>
              net match
                case DFNet.Connection(_, _, swapped) =>
                  netSinkOf(net) = if swapped then net.rhsRef.get else net.lhsRef.get
                case _ => ()
            case _ => ()
        case _ => ()
      }
      netSinkOf.values.foreach { sink =>
        var v = sink
        writeViews += v
        while v.isInstanceOf[DFVal.Alias] do
          v = v.asInstanceOf[DFVal.Alias].relValRef.get
          v match
            case _: DFVal.Dcl => () // the target itself stays readable
            case _            => writeViews += v
      }
      // pre-pass: time-zero state overrides — initial blocks and Rule-4-convertible process
      // prologues fold statically into register init values ("initial wins" over decl init)
      designMembers.foreach {
        case p: ProcessBlock if p.isInitial =>
          if !p.isInRTDomain then unsupported("an initial block outside the RT domain", p)
          foldInitialStatic(childrenOf.getOrElse(p, Vector.empty).toList, p)
        case p: ProcessBlock if p.isInRTDomain && processHasTime(p) => prepassProcess(p)
        case _                                                      => ()
      }
      // pre-pass: state cells — registers, and IN ports (pokeable hold cells at top,
      // MOV placeholders patched by the parent's connections otherwise)
      designMembers.foreach {
        case dcl: DFVal.Dcl if dcl.modifier.isReg =>
          val w = widthOf(dcl)
          val init = initOverride.get(dcl).getOrElse {
            dcl.initRefList.headOption match
              case Some(initRef) => regInitBits(initRef.get, w)
              case None          => BitVector.low(w)
          }
          regNodeOf(dcl) = wide.reg(w, init)
        case dcl: DFVal.Dcl =>
          dcl.modifier.dir match
            case DFVal.Modifier.Dir.IN =>
              if isTop then
                regNodeOf(dcl) = wide.reg(widthOf(dcl), BitVector.low(widthOf(dcl)))
                topHasInputs = true
              else
                dcl.dfType match
                  // an explicitly-declared Clk/Rst magnet input is not wired as a data port in the
                  // sim: reset stays deasserted (register inits apply at time zero) and the clock
                  // is modeled by the cycle stepping. Bind it to its deasserted (low) constant so
                  // combinational reads such as `!i_rst.actual` resolve. (Assumes active-high reset.)
                  case o: DFOpaque if o.isMagnet =>
                    env(dcl) = wide.const(widthOf(dcl), BitVector.low(widthOf(dcl)))
                  case _ =>
                    inPortMov(dcl) = wide.mov(widthOf(dcl))
            case _ => // wires/OUT ports bind at their driving net
        case _ =>
      }
      // globals closure of this sub-DB (constants incl. ROM data)
      processMembers(db.membersGlobals)
      // ordered walk of the design body
      processMembers(childrenOf.getOrElse(design, Vector.empty))
      finalizeScope()
    end elaborate

    private def processMembers(ms: Iterable[DFMember]): Unit = ms.foreach(processMember)

    private def processMember(m: DFMember): Unit = m match
      case _: DFVal.Dcl            => () // declarations: state in pre-pass, wires at their net
      case p: DFVal.DesignParam    => bindParam(p)
      case _: DFConditional.Block  => () // processed by its header's chain
      case h: DFConditional.Header => processConditionalChain(h)
      case v: DFVal if writeViews.contains(v) => () // write-only view of a sink
      case v: DFVal if isConstVector(v)       => () // ROM data, materialized at its use site
      case v: DFVal                           =>
        // may already be bound by a lazy forward-reference read
        val bound = nodeOf.contains(v) || ((procOverlay ne null) && procOverlay.contains(v))
        if !bound then bindVal(v, buildValWV(v))
      case net: DFNet         => buildNet(net)
      case t: TextOut         => buildTextOut(t)
      case inst: DFDesignInst => elaborateChild(inst)
      case _: DFRange         => () // loop-range bookkeeping — read at its loop
      case lb: LocalBlock     => processMembers(childrenOf.getOrElse(lb, Vector.empty))
      case dmn: DomainBlock if isRelatedRTDomain(dmn) =>
        // an RT domain marked `@timing.related` (only) shares its target's clock and adds no new
        // clk/rst ports, so its members live in the same timing domain and are walked inline (like
        // a LocalBlock). Reset inclusion (`includeReset`) is irrelevant here: DFacsimile applies
        // register inits at time zero and never asserts reset at runtime.
        processMembers(childrenOf.getOrElse(dmn, Vector.empty))
      case fb: DFLoop.DFForBlock if fb.isCombinational => unrollCombFor(fb)
      case pb: ProcessBlock                            =>
        if pb.isInitial then () // folded into time-zero state in the pre-pass
        else if pb.isInRTDomain then buildProcess(pb)
        else unsupported("a process outside the RT domain", pb)
      case m => unsupported("member kind", m)
    end processMember

    /** An RT domain marked `@timing.related` (and nothing else) shares its target's clock, adding no
      * new clock: its members belong to the same timing domain and are walked inline. A domain that
      * introduces its own clock (a genuinely separate clock domain) stays unsupported.
      */
    private def isRelatedRTDomain(dmn: DomainBlock): Boolean =
      dmn.domainType == DomainType.RT &&
        dmn.meta.annotations.exists(_.isInstanceOf[constraints.Timing.Related]) &&
        !dmn.meta.annotations.exists(_.isInstanceOf[constraints.Timing.Clock])

    /** Combinational (`COMB_LOOP`) for loop: unrolled at build time over its constant range, the
      * iterator bound to a constant per pass.
      */
    private def unrollCombFor(fb: DFLoop.DFForBlock): Unit =
      val iter = fb.iteratorRef.get
      val body = childrenOf.getOrElse(fb, Vector.empty)
      for i <- combForRange(fb) do
        env(iter) = wide.const(widthOf(iter), BitVector.fromLong(i, widthOf(iter)))
        processMembers(body)
      env.remove(iter)

    private def combForRange(fb: DFLoop.DFForBlock): Seq[Long] =
      val range = fb.rangeRef.get
      def cint(v: DFVal): Long =
        constOpt[Option[BigInt]](v) match
          case Some(Some(i)) => i.toLong
          case _             => unsupported("non-constant combinational loop bound", fb)
      val start = cint(range.startRef.get)
      val end = cint(range.endRef.get)
      val step = cint(range.stepRef.get)
      if step == 0 then unsupported("zero loop step", fb)
      val untilEnd = range.op match
        case DFRange.Op.Until => end
        case DFRange.Op.To    => if step > 0 then end + 1 else end - 1
      start.until(untilEnd, step)

    /** Computes a value's lowering (without binding it). */
    private def buildValWV(v: DFVal): WV =
      tryFoldConst(v).getOrElse(buildValWVCore(v))

    /** The netlist build of a value, bypassing the whole-value constant fold. The per-instance
      * const path enters here directly: it folds THROUGH the netlist build (where design params are
      * bound to their per-instance values) instead of through the IR const resolution.
      */
    private def buildValWVCore(v: DFVal): WV =
      v match
        case pbns: DFVal.PortByNameSelect => pbnsReadWV(pbns)
        case f: DFVal.Func                => buildFunc(f)
        case a: DFVal.Alias.AsIs          =>
          val rel = a.relValRef.get
          wide.resize(readWV(rel), widthOf(a), isSignedType(rel.dfType))
        case a: DFVal.Alias.ApplyIdx     => buildApplyIdx(a)
        case a: DFVal.Alias.ApplyRange   => buildApplyRange(a)
        case sf: DFVal.Alias.SelectField => buildSelectField(sf)
        case h: DFVal.Alias.History      => buildHistory(h)
        case c: DFVal.Const              =>
          // reached for bubble (don't-care) constants (`?`, simulated as 0, 2-state) and by the
          // per-instance const path (whole-value folds are bypassed there)
          wide.const(widthOf(c), lenientDataToBits(c.dfType, c.data, c))
        case m => unsupported("value kind", m)

    private def bindVal(v: DFVal, wv: WV): Unit =
      val po = procOverlay
      if po ne null then po(v) = wv
      else nodeOf(v) = wv
      // register only cost-free names, i.e. values whose lanes are state/const (already resident
      // in the signal array — e.g. a named `.reg` history alias or a named constant); a named
      // comb expression is not peekable, matching the ports-and-registers observability surface
      if !v.isAnonymous && wv.lanes.forall(n => nl.opcodes(n) == Op.REG || nl.isConst(n)) then
        namedNodes(prefix + v.getName) = wv

    // ---- reads ----------------------------------------------------------------------------

    private def readWV(v: DFVal): WV = v match
      case dcl: DFVal.Dcl =>
        val t = transCtx
        if (t ne null) && regNodeOf.contains(dcl) then
          // register reads inside a process transition context: committed state, except values
          // promoted across a conceptual cycle boundary (fusion's value forwarding — e.g. a
          // loop-back guard evaluating `(i + 1) < N` after the pending `i.din := i + 1`)
          if t.fwdUnclean.contains(dcl) then
            unsupported(
              "a forwarded read of a register with a conditional or partial pending assignment",
              dcl
            )
          t.fwdRegs.getOrElse(dcl, regNodeOf(dcl))
        else if (t ne null) && t.wireBarrier.contains(dcl) then
          unsupported("a wire read across a process transition boundary", dcl)
        else
          regNodeOf.get(dcl).orElse(env.get(dcl)).orElse(inPortMov.get(dcl))
            .getOrElse(unsupported("reading a value before it is driven", dcl))
        end if
      case v =>
        val po = procOverlay
        val overlayHit = if po ne null then po.get(v) else None
        overlayHit.orElse(nodeOf.get(v)) match
          case Some(wv) => wv
          case None     =>
            // lazy compilation: in-process values compile per transition context; design-level
            // reads cover forward references (e.g. chain guards interleaved between blocks in
            // stage-produced IR) and constants skipped in the walk (ROM vectors)
            val wv = buildValWV(v)
            bindVal(v, wv)
            wv

    /** Const resolution for simulation: the NoCache policy recomputes through design params, immune
      * to previously-cached symbolic (Always-policy) results.
      *
      * In a child scope the IR-level resolution of anything that transitively reads a design param
      * is instance-ambiguous: the shared sub-DB bakes the FIRST elaboration's applied snapshot into
      * the param member, and ref-based resolution otherwise goes through an arbitrary instance's
      * `paramMap`. Such values are recomputed per instance here: a bare param through THIS
      * instance's `paramMap` (in the parent scope), and an expression over params through the
      * netlist build, whose build-time const folding sees the per-instance param bindings.
      */
    private def constOpt[T](v: DFVal): Option[T] =
      def raw =
        v.getConstData[T](using summon[MemberGetSet], ConstData.CachePolicy.NoCache).toOption
      v match
        case p: DFVal.DesignParam if !isTop =>
          perInstanceParamData(p).asInstanceOf[Option[T]].orElse(raw)
        case _ =>
          val rawData = raw
          if isTop || rawData.isEmpty || !paramDependent(v) then rawData
          else perInstanceConstData(v).asInstanceOf[Option[T]].orElse(rawData)

    /** This instance's applied value of a design param, resolved in the parent scope's context. */
    private def perInstanceParamData(p: DFVal.DesignParam): Option[Any] =
      parentCtx.flatMap((parentScope, inst) => parentScope.paramDataOf(inst, p.getName))

    // whether a value transitively references a design param through its refs; the walk stops at
    // Dcls (runtime state is never constant) and at params (their default ref is irrelevant)
    private val paramDepMemo = mutable.Map.empty[DFVal, Boolean]
    private def paramDependent(v: DFVal): Boolean =
      paramDepMemo.get(v) match
        case Some(b) => b
        case None    =>
          val b = v match
            case _: DFVal.DesignParam => true
            case _: DFVal.Dcl         => false
            case _                    =>
              v.getRefs.exists { r =>
                r.get match
                  case dv: DFVal => paramDependent(dv)
                  case _         => false
              }
          paramDepMemo(v) = b
          b

    /** Per-instance recomputation of a param-dependent constant: build through the netlist (design
      * params bind to their per-instance values there, and constant inputs fold at build time) and
      * read the folded constant lanes back as data. `None` when the type is not packed-bits
      * representable or the build does not fold to a constant — the caller then falls back to the
      * (instance-ambiguous) IR resolution.
      */
    private def perInstanceConstData(v: DFVal): Option[Any] =
      val packed = v.dfType match
        case _: DFBits | _: DFDecimal | DFBool | DFBit | _: DFEnum | _: DFStruct | _: DFVector |
            _: DFOpaque => true
        case _ => false
      if !packed then None
      else
        val wv = buildValWVCore(v)
        Option.when(wv.lanes.forall(nl.isConst))(
          v.dfType.bitsDataToData((wide.constBits(wv), BitVector.low(wv.width)))
        )

    private def constDataOf(v: DFVal): Any =
      constOpt[Any](v).getOrElse(unsupported("non-constant data here", v))

    private def constIntOf(v: DFVal): Int =
      constOpt[Option[BigInt]](v) match
        case Some(Some(i)) => i.toInt
        case _             => unsupported("non-constant index/amount", v)

    private def constIdxOpt(v: DFVal): Option[Int] =
      constOpt[Option[BigInt]](v) match
        case Some(Some(i)) => Some(i.toInt)
        case _             => None

    // ---- data -> packed bits ----------------------------------------------------------------

    private def dataToBitsOpt(t: DFType, data: Any): Option[BitVector] =
      scala.util.Try(t.dataToBitsData(data.asInstanceOf[t.Data])).toOption.flatMap {
        (value, bubble) => if bubble.isZeros then Some(value) else None
      }

    /** Like [[dataToBitsOpt]], but bubble (don't-care) bits become 0 — 2-state minimum. */
    private def lenientDataToBits(t: DFType, data: Any, where: Any): BitVector =
      scala.util.Try(t.dataToBitsData(data.asInstanceOf[t.Data])).toOption match
        case Some((value, bubble)) =>
          if bubble.isZeros then value else value.and(bubble.not)
        case None => unsupported(s"constant of type $t", where)

    private def constBitsOpt(v: DFVal): Option[BitVector] =
      constOpt[Any](v).flatMap(dataToBitsOpt(v.dfType, _))

    /** Register init as packed bits; bubble bits lower to 0, and value-preserving casts whose
      * data-level fold fails on param-dependent widths fall through to their source.
      */
    private def regInitBits(v: DFVal, w: Int): BitVector =
      constOpt[Any](v) match
        case Some(data) => lenientDataToBits(v.dfType, data, v)
        case None       =>
          v match
            case a: DFVal.Alias.AsIs =>
              resizeBits(regInitBits(a.relValRef.get, w), w, isSignedType(a.relValRef.get.dfType))
            case _ => unsupported("non-constant register init", v)

    private def resizeBits(bits: BitVector, w: Int, signed: Boolean): BitVector =
      if bits.width == w then bits
      else if w < bits.width then bits.drop(bits.width.toLong - w)
      else
        BitVector.fill(w.toLong - bits.width)(signed && bits.width > 0 &&
          bits.bit(bits.width - 1)) ++ bits

    private def isSignedType(t: DFType): Boolean = t match
      case d: DFDecimal => d.signed
      case _            => false

    // ---- value builders -------------------------------------------------------------------

    private def isConstVector(v: DFVal): Boolean =
      v.dfType.isInstanceOf[DFVector] && v.isConst

    private def tryFoldConst(v: DFVal): Option[WV] =
      constBitsOpt(v).map(bits => wide.const(widthOf(v), bits))

    private def buildFunc(f: DFVal.Func): WV =
      import DFVal.Func.Op as FO
      val args = f.args.map(_.get)
      val resW = widthOf(f)
      def signedArgs: Boolean = isSignedType(args.head.dfType)
      def rd(a: DFVal): WV = readWV(a)
      def rdAt(a: DFVal, w: Int): WV = wide.resize(rd(a), w, isSignedType(a.dfType))
      def cmpArgs: (WV, WV) =
        val w = math.max(widthOf(args.head), widthOf(args(1)))
        (rdAt(args.head, w), rdAt(args(1), w))
      def bool(n: Int): WV = WV(Vector(n), 1)
      def ltN(x: DFVal, y: DFVal): Int =
        val w = math.max(widthOf(x), widthOf(y))
        wide.ltNode(rdAt(x, w), rdAt(y, w), signedArgs)
      def singleLaneAt(w: Int)(mk: (Int, Int) => Int): WV =
        if w > 64 then unsupported(s"result width $w for op ${f.op} (only 1..64)", f)
        WV(Vector(mk(rdAt(args.head, w).lanes(0), rdAt(args(1), w).lanes(0))), w)
      def constAmountOpt: Option[Int] = constOpt[Option[BigInt]](args(1)) match
        case Some(Some(v)) => Some(v.toInt)
        case _             => None
      val res = f.op match
        case FO.+ => args.map(rdAt(_, resW)).reduce(wide.add)
        case FO.- => args.map(rdAt(_, resW)).reduce(wide.sub)
        // single-arg |/&/^ are bit reductions (Bits/UInt -> Bit), not binary ops
        case FO.| if args.sizeIs == 1 =>
          bool(wide.neqNode(rd(args.head), wide.zero(widthOf(args.head))))
        case FO.& if args.sizeIs == 1 =>
          bool(wide.eqNode(rd(args.head), wide.ones(widthOf(args.head))))
        case FO.^ if args.sizeIs == 1 =>
          // parity: xor the (64-bit-normalized) lanes together, then fold down to bit 0
          var x = rd(args.head).lanes.map(nl.resize(_, 64)).reduce(nl.xor)
          for sh <- List(32, 16, 8, 4, 2, 1) do x = nl.xor(x, nl.shr(x, sh))
          bool(nl.resize(x, 1))
        case FO.^         => args.map(rdAt(_, resW)).reduce(wide.xor)
        case FO.&         => args.map(rdAt(_, resW)).reduce(wide.and)
        case FO.|         => args.map(rdAt(_, resW)).reduce(wide.or)
        case FO.*         => singleLaneAt(resW)(nl.mul)
        case FO./         => singleLaneAt(resW)(if signedArgs then nl.sdiv else nl.udiv)
        case FO.%         => singleLaneAt(resW)(if signedArgs then nl.srem else nl.urem)
        case FO.++        => wide.concat(args.map(rd))
        case FO.===       => val (a, b) = cmpArgs; bool(wide.eqNode(a, b))
        case FO.=!=       => val (a, b) = cmpArgs; bool(wide.neqNode(a, b))
        case FO.<         => bool(ltN(args.head, args(1)))
        case FO.>         => bool(ltN(args(1), args.head))
        case FO.<=        => bool(nl.not(ltN(args(1), args.head)))
        case FO.>=        => bool(nl.not(ltN(args.head, args(1))))
        case FO.`unary_-` => wide.neg(rdAt(args.head, resW))
        case FO.`unary_~` => wide.not(rd(args.head))
        case FO.`unary_!` => wide.not(rd(args.head))
        case FO.max       => val (a, b) = cmpArgs; wide.mux(wide.ltNode(a, b, signedArgs), b, a)
        case FO.min       => val (a, b) = cmpArgs; wide.mux(wide.ltNode(a, b, signedArgs), a, b)
        case FO.abs       =>
          val a = rdAt(args.head, resW)
          if signedArgs then
            wide.mux(wide.ltNode(a, wide.zero(resW), signed = true), wide.neg(a), a)
          else a
        case FO.sel => wide.mux(rd(args.head).lanes(0), rd(args(1)), rd(args(2)))
        // edge detection over a 1-cycle sampling register; the init biases match the RT lowering
        // (no spurious edge at time zero: rising samples 1, falling samples 0)
        case FO.rising =>
          val a = rd(args.head)
          val prev = wide.reg(1, BitVector.high(1))
          wide.setNext(prev, a)
          WV(Vector(nl.and(nl.not(prev.lanes(0)), a.lanes(0))), 1)
        case FO.falling =>
          val a = rd(args.head)
          val prev = wide.reg(1, BitVector.low(1))
          wide.setNext(prev, a)
          WV(Vector(nl.and(prev.lanes(0), nl.not(a.lanes(0)))), 1)
        case FO.<< =>
          val a = rdAt(args.head, resW)
          constAmountOpt match
            case Some(amt) => wide.shlConst(a, amt)
            case None      => wide.shlDyn(a, rd(args(1)))
        case FO.>> =>
          val a = rdAt(args.head, resW)
          constAmountOpt match
            case Some(amt) => wide.shrConst(a, amt, arith = signedArgs)
            case None      => wide.shrDyn(a, rd(args(1)), arith = signedArgs)
        case FO.ror     => wide.rotr(rd(args.head), constIntOf(args(1)))
        case FO.rol     => wide.rotl(rd(args.head), constIntOf(args(1)))
        case FO.reverse =>
          args.head.dfType match
            case vt: DFVector => // vector reversal reverses the cell order, not the bits
              val cellW = widthOfType(vt.cellType, f)
              val a = rd(args.head)
              wide.concat(Vector.tabulate(a.width / cellW)(i => wide.extract(a, i * cellW, cellW)))
            case _ => wide.reverse(rd(args.head))
        case FO.repeat => wide.repeat(rd(args.head), constIntOf(args(1)))
        case op        => unsupported(s"func op $op", f)
      if res.width != resW then unsupported("width-changing func result", f)
      res
    end buildFunc

    private def buildApplyIdx(a: DFVal.Alias.ApplyIdx): WV =
      val rel = a.relValRef.get
      rel.dfType match
        case vt: DFVector =>
          val cellW = widthOfType(vt.cellType, a)
          constIdxOpt(a.relIdx.get) match
            case Some(i) =>
              val relWV = readWV(rel)
              wide.extract(relWV, (relWV.width / cellW - 1 - i) * cellW, cellW)
            case None =>
              if rel.isConst then // constant table with a dynamic address — per-lane ROMs
                val data = constOpt[Vector[Any]](rel)
                  .getOrElse(unsupported("dynamic indexing of this vector", a))
                val cells = data.map(cell => lenientDataToBits(vt.cellType, cell, a))
                wide.rom(cells, cellW, wide.bitField(readWV(a.relIdx.get), 0, 32))
              else
                val relWV = readWV(rel)
                val off = dynCellOffset(relWV.width / cellW, cellW, a.relIdx.get)
                wide.dynExtract(relWV, off, cellW)
        case _: DFBits =>
          constIdxOpt(a.relIdx.get) match
            case Some(i) => wide.extract(readWV(rel), i, 1)
            case None    => wide.dynExtract(readWV(rel), dynBitOffset(a.relIdx.get), 1)
        case t => unsupported(s"indexing into $t", a)
      end match
    end buildApplyIdx

    private def buildApplyRange(a: DFVal.Alias.ApplyRange): WV =
      val rel = a.relValRef.get
      val hi = a.idxHighRef.getIntOpt.getOrElse(unsupported("non-constant range", a))
      val lo = a.idxLowRef.getIntOpt.getOrElse(unsupported("non-constant range", a))
      rel.dfType match
        case _: DFBits => wide.extract(readWV(rel), lo, hi - lo + 1)
        case t         => unsupported(s"range selection on $t", a)

    private def buildSelectField(sf: DFVal.Alias.SelectField): WV =
      val rel = sf.relValRef.get
      rel.dfType match
        case st: DFStruct =>
          wide.extract(readWV(rel), st.fieldRelBitLow(sf.fieldName), widthOf(sf))
        case t => unsupported(s"field selection on $t", sf)

    /** `.reg`/`.prev` (History State) alias: a chain of `step` registers sampling the target's
      * value at the alias position (matching NameRegAliases' versioned-sampling semantics for
      * mutable wires), with the same init on every stage.
      */
    private def buildHistory(h: DFVal.Alias.History): WV =
      h.op match
        case DFVal.Alias.History.Op.State =>
          val relVal = h.relValRef.get
          relVal match
            // a wire sampled inside a conditional branch would need a conditional (hold-when-
            // untaken) din per NameRegAliases' versioning — out of the minimum's scope
            case dcl: DFVal.Dcl if condDepth > 0 && !regNodeOf.contains(dcl) =>
              unsupported("`.reg` of a mutable wire inside a conditional block", h)
            case _ => ()
          val w = widthOf(h)
          val init = h.initRefOption match
            case Some(ref) => regInitBits(ref.get, w)
            case None      => BitVector.low(w)
          var out = readWV(relVal)
          if out.width != w then unsupported("width-changing history alias", h)
          for _ <- 0 until h.step do
            val stage = wide.reg(w, init)
            wide.setNext(stage, out)
            out = stage
          out
        case op => unsupported(s"history op $op", h)

    /** The dynamic index value as a 32-bit lane (bit offsets always fit 32 bits). */
    private def dynBitOffset(idx: DFVal): WV =
      WV(Vector(wide.bitField(readWV(idx), 0, 32)), 32)

    /** Bit offset of a dynamically indexed vector cell: `(len-1-idx) * cellW` (cell 0 packs at the
      * MSBs).
      */
    private def dynCellOffset(len: Int, cellW: Int, idx: DFVal): WV =
      val idxNode = wide.bitField(readWV(idx), 0, 32)
      val rev = nl.sub(nl.const(32, (len - 1).toLong), idxNode)
      WV(Vector(nl.mul(rev, nl.const(32, cellW.toLong))), 32)

    // ---- conditionals ---------------------------------------------------------------------

    /** A conditional-chain block's activation condition (pattern and/or guard) as a 1-bit node;
      * None = always taken (an else branch / unguarded catch-all case).
      */
    private def blockCondNode(block: DFConditional.Block, selectorWV: Option[WV]): Option[Int] =
      import DFConditional.DFCaseBlock.Pattern
      def patternCond(p: Pattern): Int = p match
        case Pattern.Singleton(ref)    => wide.eqNode(selectorWV.get, readWV(ref.get))
        case Pattern.Alternative(list) => list.map(patternCond).reduce(nl.or)
        case p                         => unsupported(s"match pattern $p", block)
      val guardCond = block.guardRef.get match
        case g: DFVal => Some(readWV(g).lanes(0))
        case _        => None
      block match
        case cb: DFConditional.DFCaseBlock =>
          val patCond = cb.pattern match
            case Pattern.CatchAll => None
            case p                => Some(patternCond(p))
          (patCond, guardCond) match
            case (Some(p), Some(g)) => Some(nl.and(p, g))
            case (p, g)             => p.orElse(g)
        case _ => guardCond
    end blockCondNode

    /** Sequential-assignment merge of per-branch results over one key space, with mux trees built
      * right (else/default) to left. `holdOf` supplies the committed fallback of a state cell
      * (registers hold when unassigned); wires fall back to their pre-chain value or poison.
      */
    private def mergeKeyed[K](
        base: Map[K, WV],
        condBranches: List[(Int, Map[K, WV])],
        elseOpt: Option[Map[K, WV]],
        holdOf: K => Option[WV]
    )(sink: (K, Option[WV]) => Unit): Unit =
      val allResults = condBranches.map(_._2) ++ elseOpt
      val assignedKeys = allResults.iterator.flatMap(_.keys).toSet
        .filter(k => allResults.exists(m => m.get(k) != base.get(k)))
      for k <- assignedKeys do
        val default: Option[WV] = holdOf(k) match
          case Some(hold) => Some(base.getOrElse(k, hold))
          case None       => base.get(k)
        val start: Option[WV] = elseOpt match
          case Some(e) => e.get(k).orElse(default)
          case None    => default
        val merged = condBranches.foldRight(start) { case ((cond, m), acc) =>
          (m.get(k).orElse(default), acc) match
            case (Some(t), Some(f)) => Some(wide.mux(cond, t, f))
            case _                  => None
        }
        sink(k, merged)
    end mergeKeyed

    private def envHoldOf(dcl: DFVal.Dcl): Option[WV] = regNodeOf.get(dcl)

    private def processConditionalChain(header: DFConditional.Header): Unit =
      val blocks = db.conditionalChainTable.getOrElse(header, Nil)
      if blocks.isEmpty then unsupported("conditional header without blocks", header)
      val selectorWV = header match
        case mh: DFConditional.DFMatchHeader => Some(readWV(mh.selectorRef.get))
        case _                               => None
      val isExpr = header.dfType match
        case DFUnit => false
        case _      => true

      val baseEnv = env.toMap
      val po = procOverlay
      val baseOverlay = if po ne null then po.toMap else Map.empty[DFVal, WV]
      val basePath = pathConds
      var negs = List.empty[(Int, Boolean)] // not-taken frames of the branches walked so far
      case class Branch(condOpt: Option[Int], resultEnv: Map[DFVal.Dcl, WV], yieldOpt: Option[WV])
      val branches = blocks.map { block =>
        env.clear(); env ++= baseEnv
        if po ne null then
          po.clear(); po ++= baseOverlay
        val condOpt = blockCondNode(block, selectorWV)
        val blockMembers = childrenOf.getOrElse(block, Vector.empty)
        pathConds = condOpt.map((_, false)).toList ::: negs ::: basePath
        condOpt.foreach(c => negs ::= (c, true))
        condDepth += 1
        processMembers(blockMembers)
        condDepth -= 1
        pathConds = basePath
        val yieldOpt =
          if isExpr then
            blockMembers.lastOption match
              case Some(v: DFVal) => Some(readWV(v))
              case _ => unsupported("expression conditional block without a yield value", block)
          else None
        Branch(condOpt, env.toMap, yieldOpt)
      }
      env.clear(); env ++= baseEnv

      val hasElse = branches.last.condOpt.isEmpty
      val (condBranches, elseBranch) =
        if hasElse then (branches.init, Some(branches.last)) else (branches, None)
      mergeKeyed(
        baseEnv,
        condBranches.map(b => (b.condOpt.get, b.resultEnv)),
        elseBranch.map(_.resultEnv),
        envHoldOf
      ) {
        case (dcl, Some(n)) => env(dcl) = n
        case (dcl, None)    => env.remove(dcl) // partially driven wire: poison until re-driven
      }
      // expression form: merge the block yields into the header's value
      if isExpr then
        if !hasElse then unsupported("expression conditional without a default branch", header)
        val start = elseBranch.get.yieldOpt
        val merged = condBranches.foldRight(start) { (b, acc) =>
          (b.yieldOpt, acc) match
            case (Some(t), Some(f)) => Some(wide.mux(b.condOpt.get, t, f))
            case _                  => None
        }
        bindVal(header, merged.getOrElse(unsupported("unmergeable conditional expression", header)))
    end processConditionalChain

    // ---- text output ------------------------------------------------------------------------

    /** Registers a text-output statement as a simulation action: it fires on every committed cycle
      * whose path condition holds — the enclosing branch guards, and in a process the FSM site
      * dispatch, so a print between two waits fires exactly on the transition cycle, per the
      * documented cycle semantics (statements fuse into the transition they belong to). Message
      * arguments compile as reads in the current context and render from the fired cycle's settled
      * values.
      */
    private def buildTextOut(t: TextOut): Unit =
      val extra = t.op match
        case TextOut.Op.Assert(assertionRef, _) =>
          Some((readWV(assertionRef.get).lanes(0), true)) // fires when the assertion is false
        case _ => None
      val guard = pathGuardNode(extra)
      if nl.isConst(guard) && nl.constValOf(guard) == 0L then () // statically dead path
      else
        // the stage's reset-site fold emits constant register assignments only — an emitted
        // text output keeps the reset state as a real bootstrap
        if foldProbing then foldViolation = true
        lazy val msgSegs: Vector[ActSeg] =
          t.msgParts.coalesce(t.msgArgs.map(_.get)).iterator.map {
            case s: String => ActSeg.Lit(s)
            case v: DFVal  => argSeg(v)
          }.toVector
        val where = if prefix.isEmpty then design.getName else prefix.dropRight(1)
        val (kind, segs) = t.op match
          case TextOut.Op.Print   => (ActKind.Output, msgSegs)
          case TextOut.Op.Println => (ActKind.Output, msgSegs :+ ActSeg.Lit("\n"))
          case TextOut.Op.Debug   =>
            import t.meta.position as pos
            val header = s"Debug at $prefix${t.getOwnerDomain.getFullName}\n" +
              s"${pos.fileUnixPath}:${pos.lineStart}:${pos.columnStart}\n"
            val argSegs = t.msgArgs.map(_.get).flatMap { v =>
              val nm = if v.isAnonymous then "?" else v.getName
              Vector(ActSeg.Lit(s"$nm = "), argSeg(v), ActSeg.Lit("\n"))
            }
            (ActKind.Output, ActSeg.Lit(header) +: argSegs.toVector)
          case TextOut.Op.Finish              => (ActKind.Finish, Vector.empty[ActSeg])
          case TextOut.Op.Report(severity)    => (ActKind.Report(severity), msgSegs)
          case TextOut.Op.Assert(_, severity) =>
            val body = if msgSegs.isEmpty then Vector(ActSeg.Lit("assertion failed")) else msgSegs
            (ActKind.Report(severity), body)
        actions += SimAction(nl.snap(guard), kind, segs, where)
      end if
    end buildTextOut

    /** One message-argument segment: constants (strings included) render at build time; runtime
      * values compile as reads in the current context (register operands MOV-snapshot for the
      * post-commit fire) and render per fire, following the backends' display conventions — decimal
      * for integers, `true`/`false` for booleans, `0`/`1` for bits, entry names for enums, and
      * zero-padded hex for bits vectors (and, as a packed-bits fallback, composites).
      */
    private def argSeg(v: DFVal): ActSeg =
      v.dfType match
        case _: DFString =>
          val s = constOpt[Option[String]](v).flatten
            .getOrElse(unsupported("a non-constant string message argument", v))
          ActSeg.Lit(s)
        case t =>
          val render = renderOf(t, widthOf(v))
          val wv = readWV(v)
          if wv.lanes.forall(nl.isConst) then ActSeg.Lit(render(wide.constBits(wv)))
          else ActSeg.Arg(wide.snap(wv), render)

    private def renderOf(t: DFType, w: Int): BitVector => String = t match
      case DFBool    => bits => if bits.bit(0) then "true" else "false"
      case DFBit     => bits => if bits.bit(0) then "1" else "0"
      case e: DFEnum =>
        bits =>
          val value = bits.toBigInt(signed = false)
          e.entries.collectFirst { case (name, ev) if ev.compare(value) == 0 => name }
            .getOrElse(s"?($value)")
      case d: DFDecimal => bits => bits.toBigInt(signed = d.signed).toString
      case _            => // DFBits and packed composites: zero-padded hex, one digit per nibble
        val digits = (w + 3) / 4
        bits => ("%0" + digits + "x").format(bits.toBigInt(signed = false).bigInteger)

    // ---- nets & hierarchy -----------------------------------------------------------------

    private def buildNet(net: DFNet): Unit = net.op match
      case DFNet.Op.Assignment =>
        net.lhsRef.get match
          case dcl: DFVal.Dcl =>
            env(dcl) = readWV(net.rhsRef.get)
            recordWrite(dcl, full = true)
          case alias: DFVal.Alias => assignPartial(alias, readWV(net.rhsRef.get), net)
          case other              => unsupported("assignment target", net)
      case DFNet.Op.Connection | DFNet.Op.ViaConnection =>
        val sink = netSinkOf.getOrElse(net, unsupported("connection direction resolution", net))
        val src = if sink eq net.lhsRef.get then net.rhsRef.get else net.lhsRef.get
        connectSink(sink, readWV(src), net)
      case op => unsupported(s"net op $op", net)

    /** RMW lowering of an assignment through a partial view (bit/range/field/cell, dynamic indices
      * included) into a whole-value update of the underlying declaration.
      */
    private def assignPartial(alias: DFVal.Alias, part: WV, net: DFNet): Unit =
      val (dcl, staticLo, dynOffOpt) = assignTarget(alias, net)
      val base = env.get(dcl).orElse(regNodeOf.get(dcl))
        .getOrElse(unsupported("partial assignment to an undriven value", net))
      recordWrite(dcl, full = false)
      env(dcl) = dynOffOpt match
        case None      => wide.insert(base, part, staticLo)
        case Some(dyn) =>
          val off =
            if staticLo == 0 then dyn
            else WV(Vector(nl.add(dyn.lanes(0), nl.const(32, staticLo.toLong))), 32)
          wide.dynInsert(base, part, off)

    /** Resolve a write-view alias chain to its declaration + bit offset (static part + optional
      * dynamic part).
      */
    private def assignTarget(v: DFVal, net: DFNet): (DFVal.Dcl, Int, Option[WV]) =
      def addDyn(acc: Option[WV], more: WV): Option[WV] = acc match
        case None    => Some(more)
        case Some(a) => Some(WV(Vector(nl.add(a.lanes(0), more.lanes(0))), 32))
      v match
        case dcl: DFVal.Dcl             => (dcl, 0, None)
        case ar: DFVal.Alias.ApplyRange =>
          val (dcl, lo0, dyn) = assignTarget(ar.relValRef.get, net)
          val lo = ar.idxLowRef.getIntOpt.getOrElse(unsupported("non-constant range", net))
          (dcl, lo0 + lo, dyn)
        case ai: DFVal.Alias.ApplyIdx =>
          val rel = ai.relValRef.get
          val (dcl, lo0, dyn) = assignTarget(rel, net)
          rel.dfType match
            case vt: DFVector =>
              val cellW = widthOfType(vt.cellType, net)
              val len = widthOfType(rel.dfType, net) / cellW
              constIdxOpt(ai.relIdx.get) match
                case Some(i) => (dcl, lo0 + (len - 1 - i) * cellW, dyn)
                case None    => (dcl, lo0, addDyn(dyn, dynCellOffset(len, cellW, ai.relIdx.get)))
            case _: DFBits =>
              constIdxOpt(ai.relIdx.get) match
                case Some(i) => (dcl, lo0 + i, dyn)
                case None    => (dcl, lo0, addDyn(dyn, dynBitOffset(ai.relIdx.get)))
            case t => unsupported(s"assignment through indexing into $t", net)
        case sf: DFVal.Alias.SelectField =>
          val rel = sf.relValRef.get
          val (dcl, lo0, dyn) = assignTarget(rel, net)
          rel.dfType match
            case st: DFStruct => (dcl, lo0 + st.fieldRelBitLow(sf.fieldName), dyn)
            case t            => unsupported(s"assignment through field selection on $t", net)
        case other => unsupported("assignment target", other)
      end match
    end assignTarget

    private def connectSink(sink: DFVal, srcWV: WV, net: DFNet): Unit = sink match
      case pbns: DFVal.PortByNameSelect =>
        val inst = pbns.designInstRef.get
        val child =
          childScopes.getOrElse(inst, unsupported("connection before instance elaboration", net))
        child.connectInPort(pbns.portNamePath, srcWV, net)
      case dcl: DFVal.Dcl =>
        if env.contains(dcl) || partialDrivers.contains(dcl) then
          unsupported("multiple drivers of a value", net)
        env(dcl) = srcWV
      case alias: DFVal.Alias =>
        val (dcl, lo, dynOpt) = assignTarget(alias, net)
        if dynOpt.nonEmpty then unsupported("dynamic partial connection target", net)
        if env.contains(dcl) then unsupported("mixed whole and partial drivers", net)
        val hi = lo + widthOf(alias) - 1
        partialDrivers.getOrElseUpdate(dcl, mutable.ArrayBuffer.empty) += ((hi, lo, srcWV))
      case other => unsupported("connection sink", other)

    // repeated instance names (e.g. `List.fill(n)(SubDesign())`) get indexed path segments
    // (adder_0, adder_1, ...) — matching the uniqueNames convention of the compiler stages
    private val instNameCounts: Map[String, Int] = designMembers
      .collect { case i: DFDesignInst => i.getName }
      .groupBy(identity).view.mapValues(_.size).toMap
    private val instNameNextIdx = mutable.Map.empty[String, Int]

    private def elaborateChild(inst: DFDesignInst): Unit =
      if !rawDB.isRoot then unsupported("design instance without a hierarchical root DB", inst)
      val childDB = rawDB.subDBs
        .getOrElse(inst.designRef, unsupported("missing sub-DB for design instance", inst))
      val baseName = inst.getName
      val pathName =
        if instNameCounts.getOrElse(baseName, 1) > 1 then
          val idx = instNameNextIdx.getOrElse(baseName, 0)
          instNameNextIdx(baseName) = idx + 1
          s"${baseName}_$idx"
        else baseName
      val child = new Scope(childDB, prefix + pathName + ".", Some((this, inst)))
      childScopes(inst) = child
      child.elaborate()

    private def bindParam(p: DFVal.DesignParam): Unit =
      val data = parentCtx match
        case Some((parentScope, inst)) =>
          parentScope.paramDataOf(inst, p.getName)
            .orElse(constOpt[Any](p))
            .getOrElse(unsupported("non-constant design param", p))
        case None => constDataOf(p)
      bindVal(p, wide.const(widthOf(p), lenientDataToBits(p.dfType, data, p)))

    /** Resolve a child instance's param value in THIS (parent) scope's context. */
    private def paramDataOf(inst: DFDesignInst, name: String): Option[Any] =
      inst.paramMap.get(name).flatMap(ref => constOpt[Any](ref.get))

    /** Parent-side read of a child port (child is fully elaborated at this point). */
    private def pbnsReadWV(pbns: DFVal.PortByNameSelect): WV =
      val inst = pbns.designInstRef.get
      val child =
        childScopes.getOrElse(inst, unsupported("port select before instance elaboration", pbns))
      child.portReadWV(pbns.portNamePath, pbns)

    private def portByName(path: String, where: Any): DFVal.Dcl =
      if path.contains('.') then unsupported(s"nested port path '$path'", where)
      designMembers.collectFirst {
        case dcl: DFVal.Dcl if !dcl.isAnonymous && dcl.getName == path => dcl
      }.getOrElse(unsupported(s"port '$path' of design '${design.dclName}'", where))

    private def portReadWV(path: String, where: Any): WV =
      val dcl = portByName(path, where)
      regNodeOf.get(dcl).orElse(inPortMov.get(dcl)).orElse(env.get(dcl))
        .getOrElse(unsupported("reading an undriven port", dcl))

    private def connectInPort(path: String, srcWV: WV, where: Any): Unit =
      val dcl = portByName(path, where)
      val movWV = inPortMov.getOrElse(dcl, unsupported("connection to a non-input port", dcl))
      wide.patchMov(movWV, srcWV)

    // ==================== RT processes (FSM simulation) ====================================

    /** A process-internal state cell (FSM state / wait counter): merged and committed like a
      * register but not backed by an IR declaration.
      */
    private final class PCell(val regWV: WV)

    private val procBootNeeded = mutable.Map.empty[ProcessBlock, Boolean]

    private def flattenedOf(o: DFOwner): Iterator[DFMember] =
      childrenOf.getOrElse(o, Vector.empty).iterator.flatMap {
        case owner: DFOwner => Iterator.single[DFMember](owner) ++ flattenedOf(owner)
        case m              => Iterator.single(m)
      }

    /** A construct that consumes cycles (a park, or a region guaranteed to contain parks). */
    private def isTimeConstructM(m: DFMember): Boolean = m match
      case _: Wait          => true
      case _: StepBlock     => true
      case lb: DFLoop.Block => !lb.isCombinational
      case _                => false

    private def processHasTime(pb: ProcessBlock): Boolean =
      flattenedOf(pb).exists(isTimeConstructM)

    private enum WaitKind derives CanEqual:
      case Cycles1
      case CyclesN(nVal: DFVal) // cycle count sampled live, like the equivalent FSM counter
      case CyclesLit(n: Long) // timed wait, converted through the domain clock rate
      case CondW(trigger: DFVal) // resume when the trigger is true (sampled once per cycle)
      case Endless

    private def waitKindOf(wt: Wait): WaitKind =
      val trigger = wt.triggerRef.get
      if wt.isEndless then WaitKind.Endless
      else
        trigger.dfType match
          case DFBool | DFBit => WaitKind.CondW(trigger)
          case DFTime         =>
            val clkRate = rawDB.resolvedClkRstMap
              .get(wt.getOwnerDomain)
              .flatMap(_._1)
              .flatMap(_.rate.toOption)
              .getOrElse(unsupported("a timed wait without a resolved clock rate", wt))
            val waitTime = constOpt[TimeNumber](trigger)
              .getOrElse(unsupported("a non-constant timed wait", wt))
            val n = (waitTime / clkRate.to_ps).value.toLong
            if n <= 0 then unsupported("a non-positive timed wait", wt)
            else if n == 1 then WaitKind.Cycles1
            else WaitKind.CyclesLit(n)
          case _: DFDecimal =>
            constOpt[Option[BigInt]](trigger) match
              case Some(Some(n)) if n == 1 => WaitKind.Cycles1
              case _                       => WaitKind.CyclesN(trigger)
          case t => unsupported(s"wait trigger type $t", wt)
      end if
    end waitKindOf

    /** Statically evaluates initial-convertible content (initial blocks and Rule-4-converted
      * process prologues) into time-zero packed-bits state per REG declaration.
      */
    private def foldInitialStatic(topMembers: List[DFMember], where: Any): Unit =
      val iterBind = mutable.Map.empty[DFVal.Dcl, BigInt]
      def constBitsOf(v: DFVal): BitVector =
        constOpt[Any](v) match
          case Some(d) => lenientDataToBits(v.dfType, d, where)
          case None    => unsupported("a non-constant value in initial content", v)
      def constIdxOf(v: DFVal): Int = v match
        case dcl: DFVal.Dcl if iterBind.contains(dcl) => iterBind(dcl).toInt
        case a: DFVal.Alias.AsIs                      => constIdxOf(a.relValRef.get)
        case _                                        =>
          constOpt[Option[BigInt]](v) match
            case Some(Some(i)) => i.toInt
            case _             => unsupported("a non-constant index in initial content", v)
      def lhsTarget(v: DFVal): (DFVal.Dcl, Int) = v match
        case dcl: DFVal.Dcl             => (dcl, 0)
        case ar: DFVal.Alias.ApplyRange =>
          val (dcl, lo0) = lhsTarget(ar.relValRef.get)
          val lo = ar.idxLowRef.getIntOpt.getOrElse(unsupported("a non-constant range", ar))
          (dcl, lo0 + lo)
        case ai: DFVal.Alias.ApplyIdx =>
          val rel = ai.relValRef.get
          val (dcl, lo0) = lhsTarget(rel)
          rel.dfType match
            case vt: DFVector =>
              val cellW = widthOfType(vt.cellType, ai)
              val len = widthOfType(vt, ai) / cellW
              (dcl, lo0 + (len - 1 - constIdxOf(ai.relIdx.get)) * cellW)
            case _: DFBits => (dcl, lo0 + constIdxOf(ai.relIdx.get))
            case t         => unsupported(s"initial assignment through indexing into $t", ai)
        case sf: DFVal.Alias.SelectField =>
          val rel = sf.relValRef.get
          val (dcl, lo0) = lhsTarget(rel)
          rel.dfType match
            case st: DFStruct => (dcl, lo0 + st.fieldRelBitLow(sf.fieldName))
            case t => unsupported(s"initial assignment through field selection on $t", sf)
        case other => unsupported("initial assignment target", other)
      def baseBitsOf(dcl: DFVal.Dcl): BitVector =
        initOverride.getOrElse(
          dcl, {
            val w = widthOf(dcl)
            dcl.initRefList.headOption match
              case Some(r) => regInitBits(r.get, w)
              case None    => BitVector.low(w)
          }
        )
      def splice(base: BitVector, part: BitVector, lo: Int): BitVector =
        val hiW = base.width - lo - part.width
        val parts = List.newBuilder[BitVector]
        if hiW > 0 then parts += base.bitsWL(hiW, lo + part.width)
        parts += part
        if lo > 0 then parts += base.bitsWL(lo, 0)
        parts.result().reduce(_ ++ _)
      def foldStmts(ms: List[DFMember]): Unit =
        var skip = Set.empty[DFMember]
        ms.foreach {
          case m if skip.contains(m)                       => ()
          case net: DFNet if net.op == DFNet.Op.Assignment =>
            val lhsVal = net.lhsRef.get
            val (dcl, lo) = lhsTarget(lhsVal)
            if !dcl.modifier.isReg then
              unsupported("initial content assigning a non-register (lands with M3)", net)
            val rhs = net.rhsRef.get
            val part = resizeBits(constBitsOf(rhs), widthOf(lhsVal), isSignedType(rhs.dfType))
            initOverride(dcl) = splice(baseBitsOf(dcl), part, lo)
          case h: DFConditional.Header =>
            import DFConditional.DFCaseBlock.Pattern
            val blocks = db.conditionalChainTable.getOrElse(h, Nil)
            skip ++= blocks
            val selBits = h match
              case mh: DFConditional.DFMatchHeader => Some(constBitsOf(mh.selectorRef.get))
              case _                               => None
            def patternHit(p: Pattern): Boolean = p match
              case Pattern.Singleton(ref)    => constBitsOf(ref.get).equals(selBits.get)
              case Pattern.Alternative(list) => list.exists(patternHit)
              case p                         => unsupported(s"initial match pattern $p", h)
            val taken = blocks.find { b =>
              val guardOk = b.guardRef.get match
                case g: DFVal => constBitsOf(g).bit(0)
                case _        => true
              val patOk = b match
                case cb: DFConditional.DFCaseBlock =>
                  cb.pattern match
                    case Pattern.CatchAll => true
                    case p                => patternHit(p)
                case _ => true
              guardOk && patOk
            }
            taken.foreach(b => foldStmts(childrenOf.getOrElse(b, Vector.empty).toList))
          case _: DFConditional.Block => () // handled at its header
          case fb: DFLoop.DFForBlock  =>
            val iter = fb.iteratorRef.get
            val body = childrenOf.getOrElse(fb, Vector.empty).toList
            for i <- combForRange(fb) do
              iterBind(iter) = BigInt(i)
              foldStmts(body)
            iterBind.remove(iter)
          // unreachable in valid IR: RT `initial` blocks reject text output at elaboration, and
          // a printing prologue is not initial-convertible (it keeps the bootstrap state)
          case t: TextOut => unsupported("text output in statically folded initial content", t)
          case _: DFVal   => () // anonymous dependencies / iterator declarations
          case _: DFRange => ()
          case m          => unsupported("initial content member", m)
        }
      end foldStmts
      foldStmts(topMembers)
    end foldInitialStatic

    /** Pre-pass per RT process with time constructs: M1 validation, the Rule-4 gate (mirroring
      * `DropRTWaits` Rule 6: bootstrap skipped when the prologue is initial-convertible and no
      * trailing statement shares a prologue-assigned declaration), and the static fold of the
      * converted prologue into time-zero state.
      */
    private def prepassProcess(pb: ProcessBlock): Unit =
      flattenedOf(pb).foreach {
        case f: ForkBlock                   => unsupported("fork/join in processes", f)
        case sb: StepBlock if !sb.isRegular =>
          unsupported("onEntry/onExit/fallThrough step blocks (land with M3)", sb)
        case lb: DFLoop.Block if !lb.isCombinational && lb.isFallThrough =>
          unsupported("FALL_THROUGH loops (land with M3)", lb)
        case w: Wait => waitKindOf(w) // validates the trigger form
        case _       => ()
      }
      if !processHasTime(pb) then () // every-cycle combinational body — nothing to plan
      else
        val top = childrenOf.getOrElse(pb, Vector.empty).toList
        def expandOwners(list: List[DFMember]): List[DFMember] = list.flatMap {
          case owner: DFOwner => owner :: flattenedOf(owner).toList
          case m              => List(m)
        }
        val prologueTop = top.takeWhile(m => !isTimeConstructM(m))
        val prologue = expandOwners(prologueTop)
        val startsWithGen = top.dropWhile {
          case v: DFVal => v.isAnonymous
          case _        => false
        }.headOption.exists(isTimeConstructM)
        val trailingTop =
          if startsWithGen || prologueTop.sizeIs >= top.size then Nil
          else top.reverse.takeWhile(m => !isTimeConstructM(m)).reverse
        val trailing = expandOwners(trailingTop)
        val prologueDcls = assignedDcls(prologue).toSet
        val shares = prologueDcls.nonEmpty && assignedDcls(trailing).exists(prologueDcls.contains)
        val needsBoot =
          if startsWithGen then false // M1 rejects onEntry, so a leading step never needs a boot
          else !(isInitialConvertible(prologue) && !shares)
        procBootNeeded(pb) = needsBoot
        if !needsBoot then
          foldInitialStatic(prologueTop, pb)
          // a process-leading for loop's iterator initialization is prologue content in the FSM
          // lowering, so it lands in the generated initial state: the iterator holds its start
          // value at time zero (whether the loop control fuses or keeps the reset-entry state)
          top.find(isTimeConstructM) match
            case Some(fb: DFLoop.DFForBlock) =>
              val iter = fb.iteratorRef.get
              val start = constOpt[Option[BigInt]](fb.rangeRef.get.startRef.get).flatten
                .getOrElse(unsupported("a non-constant start of a process-leading for loop", fb))
              initOverride(iter) = BitVector.fromLong(start.toLong, widthOf(iter))
            case _ => ()
      end if
    end prepassProcess

    private def buildProcess(pb: ProcessBlock): Unit =
      if !processHasTime(pb) then
        // a process without steps/waits/loops is purely combinational, every-cycle logic
        processMembers(childrenOf.getOrElse(pb, Vector.empty))
      else new ProcLowering(pb).run()

    /** Lowers one clock-bound RT process into FSM sites over an implicit state cell, directly from
      * the elaborated IR, following the documented cycle semantics with the same fusion/fallback
      * decisions as the FSM lowering stages:
      *   - waits and pure-dispatch steps park; loops with control-free bodies park per iteration
      *   - control flow fuses into transition cycles, with value forwarding across conceptual cycle
      *     boundaries (a loop-back guard evaluates on next-cycle register values); the
      *     process-leading construct fuses at the forever wrap-around too (the re-executed prologue
      *     re-initializes the values its dispatch guards read)
      *   - a fused process-leading construct keeps a state for the reset entry only; when its
      *     dispatch const-folds under the prologue values, the folded assignments join the
      *     time-zero state and the FSM resets directly into the fold's target park (the reset-site
      *     fold — zero bootstrap cycles)
      *   - fallback control states: match dispatch, guards reading conditionally/partially assigned
      *     state or history aliases, and dispatch cycles that do not fold (e.g. dynamic-nest or
      *     dynamic wrap-around re-entry), detected with the same visit-capped expansion and
      *     first-victim-restart discipline as `FirstStepFusion`
      */
    private final class ProcLowering(pb: ProcessBlock):
      private val top: List[DFMember] = childrenOf.getOrElse(pb, Vector.empty).toList
      private val needsBoot = procBootNeeded(pb)

      private enum PCont:
        case SeqC(rest: List[DFMember], outer: PCont)
        case LoopBack(loop: DFLoop.Block, outer: PCont)
        case Wrap

      // ---- classification -----------------------------------------------------------------
      private def bodyOf(o: DFOwner): List[DFMember] = childrenOf.getOrElse(o, Vector.empty).toList
      private def hasTimeIn(o: DFOwner): Boolean = flattenedOf(o).exists(isTimeConstructM)
      private def hasControlIn(o: DFOwner): Boolean = flattenedOf(o).exists {
        case m if isTimeConstructM(m) => true
        case _: Goto                  => true
        case _                        => false
      }
      private def isParkLoop(lb: DFLoop.Block): Boolean = !hasControlIn(lb)
      private def isParkStep(sb: StepBlock): Boolean = !hasTimeIn(sb)
      private def chainBlocksOf(h: DFConditional.Header): List[DFConditional.Block] =
        db.conditionalChainTable.getOrElse(h, Nil)
      private def chainHasControl(h: DFConditional.Header): Boolean =
        chainBlocksOf(h).exists(hasControlIn)
      private def isFusable(m: DFMember): Boolean = m match
        case lb: DFLoop.Block if !lb.isCombinational => !isParkLoop(lb)
        case sb: StepBlock                           => !isParkStep(sb)
        case _                                       => false

      private def enclosingStep(m: DFMember): StepBlock =
        var o: DFBlock = m.getOwnerBlock
        while !o.isInstanceOf[StepBlock] do
          o match
            case _: ProcessBlock => unsupported("a relative goto outside a step", m)
            case _               => o = o.getOwnerBlock
        o.asInstanceOf[StepBlock]
      private lazy val firstRegularStep: StepBlock =
        flattenedOf(pb).collectFirst { case sb: StepBlock => sb }
          .getOrElse(unsupported("FirstStep without any step", pb))

      // ---- structure scan: parks/controls in order, with their exit continuations -----------
      private val stepExitConts = mutable.Map.empty[StepBlock, PCont]
      private def parkPositions(): List[(DFMember, PCont)] =
        stepExitConts.clear()
        val acc = List.newBuilder[(DFMember, PCont)]
        def scan(items: List[DFMember], cont: PCont): Unit = items match
          case Nil       => ()
          case m :: rest =>
            val myCont = PCont.SeqC(rest, cont)
            m match
              case wt: Wait =>
                acc += ((wt, myCont))
                scan(rest, cont)
              case lb: DFLoop.Block if !lb.isCombinational =>
                if isParkLoop(lb) || fallback.contains(lb) then acc += ((lb, myCont))
                scan(bodyOf(lb), PCont.LoopBack(lb, myCont))
                scan(rest, cont)
              case sb: StepBlock =>
                stepExitConts(sb) = myCont
                if isParkStep(sb) || fallback.contains(sb) then acc += ((sb, myCont))
                scan(bodyOf(sb), myCont)
                scan(rest, cont)
              case h: DFConditional.Header if chainHasControl(h) =>
                val blocks = chainBlocksOf(h)
                val blockSet = blocks.toSet[DFMember]
                val after = rest.filterNot(blockSet)
                for b <- blocks do scan(bodyOf(b), PCont.SeqC(after, cont))
                scan(after, cont)
              case _ => scan(rest, cont)
            end match
        scan(top, PCont.Wrap)
        acc.result()
      end parkPositions

      // ---- fallback fixpoint (which fusable regions keep a control state) -------------------
      private val fallback = mutable.Set.empty[DFOwner]
      private final class AbortWalk(val victim: DFOwner) extends Exception
      private lazy val firstConstructOpt: Option[DFMember] = top.find(isTimeConstructM)

      private def valTreeReads(v: DFVal): (Set[DFVal.Dcl], Boolean) =
        var dcls = Set.empty[DFVal.Dcl]
        var hist = false
        def walk(x: DFVal): Unit = x match
          case dcl: DFVal.Dcl         => dcls += dcl
          case h: DFVal.Alias.History => hist = true
          case _                      =>
            x.getRefs.foreach { r =>
              r.get match
                case dep: DFVal => walk(dep)
                case _          => ()
            }
        walk(v)
        (dcls, hist)

      private def loopGuardReads(lb: DFLoop.Block): (Set[DFVal.Dcl], Boolean) = lb match
        case fb: DFLoop.DFForBlock =>
          val range = fb.rangeRef.get
          val (d1, h1) = valTreeReads(range.endRef.get)
          val (d2, h2) = valTreeReads(range.stepRef.get)
          (d1 ++ d2, h1 || h2)
        case wb: DFLoop.DFWhileBlock => valTreeReads(wb.guardRef.get)

      private def loopTailRegion(lb: DFLoop.Block): List[DFMember] =
        bodyOf(lb).reverse.takeWhile(m => !isTimeConstructM(m)).reverse.flatMap {
          case o: DFOwner => o :: flattenedOf(o).toList
          case m          => List(m)
        }

      private def dirtyAssigned(ms: List[DFMember]): Set[DFVal.Dcl] =
        ms.flatMap {
          case net: DFNet if net.op == DFNet.Op.Assignment =>
            net.lhsRef.get match
              case dcl: DFVal.Dcl =>
                net.getOwnerBlock match
                  case _: DFConditional.Block => Some(dcl) // conditionally assigned
                  case _                      => None
              case alias: DFVal.Alias => alias.departialDcl.map(_._1) // partially assigned
              case _                  => None
          case _ => None
        }.toSet

      private def constBoolOf(v: DFVal): Option[Boolean] =
        constOpt[Any](v).flatMap(dataToBitsOpt(v.dfType, _)).map(_.bit(0))

      private def staticTakenOf(b: DFConditional.Block): Option[Boolean] = b match
        case cb: DFConditional.DFCaseBlock =>
          cb.pattern match
            case DFConditional.DFCaseBlock.Pattern.CatchAll =>
              cb.guardRef.get match
                case g: DFVal => constBoolOf(g)
                case _        => Some(true)
            case _ => None
        case _ =>
          b.guardRef.get match
            case g: DFVal => constBoolOf(g)
            case _        => Some(true)

      private def staticEntryGuard(lb: DFLoop.Block): Option[Boolean] = lb match
        case fb: DFLoop.DFForBlock =>
          val r = fb.rangeRef.get
          for
            start <- constOpt[Option[BigInt]](r.startRef.get).flatten
            end <- constOpt[Option[BigInt]](r.endRef.get).flatten
            step <- constOpt[Option[BigInt]](r.stepRef.get).flatten
          yield (r.op, step.signum >= 0) match
            case (DFRange.Op.Until, true)  => start < end
            case (DFRange.Op.To, true)     => start <= end
            case (DFRange.Op.Until, false) => start > end
            case (DFRange.Op.To, false)    => start >= end
        case wb: DFLoop.DFWhileBlock => constBoolOf(wb.guardRef.get)

      private def computeFallbacks(): Unit =
        // The process-leading construct is fused like any other candidate: the wrap-around
        // re-entry (its self-goto in the FSM lowering) resolves by constant pruning on the
        // re-executed prologue's values, and a genuinely dynamic re-entry falls back through
        // the visit-capped walks below (Rule C), keeping a control state.
        parkPositions() // populates stepExitConts for the walks below
        // Rule B: syntactic fusion blockers
        flattenedOf(pb).foreach {
          // a match chain carrying control cannot be inlined — its nearest fusable region
          // keeps a control state (at a park's own dispatch it is fine)
          case h: DFConditional.DFMatchHeader if chainHasControl(h) =>
            var o: DFBlock = h.getOwnerBlock
            var done = false
            while !done do
              o match
                case lb: DFLoop.Block if isFusable(lb) =>
                  fallback += lb; done = true
                case sb: StepBlock if isFusable(sb) =>
                  fallback += sb; done = true
                case _: ProcessBlock | _: StepBlock | _: DFLoop.Block => done = true
                case _                                                => o = o.getOwnerBlock
          case _ => ()
        }
        // forwarded loop-back guards that cannot be soundly evaluated at the boundary
        flattenedOf(pb).foreach {
          case lb: DFLoop.Block
              if !lb.isCombinational && isFusable(lb) && !fallback.contains(lb) =>
            val (guardDcls, hasHistory) = loopGuardReads(lb)
            if hasHistory then fallback += lb
            else
              val tail = loopTailRegion(lb)
              val tailAssigned = assignedDcls(tail).toSet
              if dirtyAssigned(tail).exists(guardDcls.contains) then fallback += lb
              else if guardDcls.exists(d => !d.modifier.isReg && tailAssigned.contains(d)) then
                fallback += lb
          case _ => ()
        }
        // Rule C: dispatch cycles that cannot fold, with the stage's visit-capped expansion and
        // first-victim-restart discipline
        var changed = true
        var iterGuard = 0
        while changed && iterGuard < 1000 do
          iterGuard += 1
          changed = ruleCPass()
      end computeFallbacks

      private def ruleCPass(): Boolean =
        try
          val positions = parkPositions()
          if needsBoot then walkSeq(top, PCont.Wrap, Map.empty)
          for (m, cont) <- positions do
            m match
              case wt: Wait =>
                waitKindOf(wt) match
                  case WaitKind.Endless => ()
                  case _                => walkCont(cont, Map.empty)
              case lb: DFLoop.Block =>
                if isParkLoop(lb) then walkCont(cont, Map.empty)
                else // control state: dispatches into its body and its exit
                  walkSeq(bodyOf(lb), PCont.LoopBack(lb, cont), Map.empty)
                  walkCont(cont, Map.empty)
              case sb: StepBlock => walkSeq(bodyOf(sb), stepExitConts(sb), Map.empty)
              case _             => ()
          false
        catch
          case a: AbortWalk =>
            fallback += a.victim
            true

      private def walkSeq(items: List[DFMember], cont: PCont, visits: Map[DFOwner, Int]): Unit =
        items match
          case Nil       => walkCont(cont, visits)
          case m :: rest =>
            val myCont = PCont.SeqC(rest, cont)
            m match
              case _: Wait                                 => () // parked — terminal
              case lb: DFLoop.Block if !lb.isCombinational =>
                if isParkLoop(lb) || fallback.contains(lb) then () // parked
                else walkLoopEntry(lb, myCont, visits)
              case sb: StepBlock                                 => walkStepEntry(sb, visits)
              case h: DFConditional.Header if chainHasControl(h) =>
                val blocks = chainBlocksOf(h)
                val blockSet = blocks.toSet[DFMember]
                val after = rest.filterNot(blockSet)
                var terminal = false
                for b <- blocks if !terminal do
                  staticTakenOf(b) match
                    case Some(true) =>
                      walkSeq(bodyOf(b) ::: after, cont, visits)
                      terminal = true
                    case Some(false) => ()
                    case None        => walkSeq(bodyOf(b) ::: after, cont, visits)
                if !terminal then walkSeq(after, cont, visits)
              case g: Goto => walkGoto(g, visits)
              case _       => walkSeq(rest, cont, visits)
            end match

      private def walkCont(cont: PCont, visits: Map[DFOwner, Int]): Unit = cont match
        case PCont.SeqC(rest, outer)   => walkSeq(rest, outer, visits)
        case PCont.LoopBack(lb, outer) =>
          if fallback.contains(lb) then () // parked at the control state
          else
            val c = visits.getOrElse(lb, 0)
            if c >= 2 then throw new AbortWalk(lb)
            val v2 = visits.updated(lb, c + 1)
            walkSeq(bodyOf(lb), PCont.LoopBack(lb, outer), v2) // stay
            walkCont(outer, v2) // exit
        case PCont.Wrap =>
          if needsBoot then () // parked at the boot state
          else walkSeq(top, PCont.Wrap, visits)

      private def walkLoopEntry(lb: DFLoop.Block, exitCont: PCont, visits: Map[DFOwner, Int])
          : Unit =
        val c = visits.getOrElse(lb, 0)
        if c >= 2 then throw new AbortWalk(lb)
        val v2 = visits.updated(lb, c + 1)
        staticEntryGuard(lb) match
          case Some(true)  => walkSeq(bodyOf(lb), PCont.LoopBack(lb, exitCont), v2)
          case Some(false) => walkCont(exitCont, v2)
          case None        =>
            walkSeq(bodyOf(lb), PCont.LoopBack(lb, exitCont), v2)
            walkCont(exitCont, v2)

      private def walkStepEntry(sb: StepBlock, visits: Map[DFOwner, Int]): Unit =
        if isParkStep(sb) || fallback.contains(sb) then () // parked
        else
          val c = visits.getOrElse(sb, 0)
          if c >= 2 then throw new AbortWalk(sb)
          walkSeq(bodyOf(sb), stepExitConts(sb), visits.updated(sb, c + 1))

      private def walkGoto(g: Goto, visits: Map[DFOwner, Int]): Unit =
        g.stepRef.get match
          case sb: StepBlock  => walkStepEntry(sb, visits)
          case Goto.ThisStep  => walkStepEntry(enclosingStep(g), visits)
          case Goto.NextStep  => walkCont(stepExitConts(enclosingStep(g)), visits)
          case Goto.FirstStep => walkStepEntry(firstRegularStep, visits)

      // ---- sites & cells --------------------------------------------------------------------
      private val sitePrograms = mutable.ArrayBuffer.empty[() => Unit]
      private val siteOf = mutable.Map.empty[DFMember, Int]
      private var bootSite = -1
      private val allCells = mutable.ArrayBuffer.empty[PCell]
      private val waitCells = mutable.Map.empty[Wait, PCell]
      private val cellEnv = mutable.Map.empty[PCell, WV]
      private var segCellVar: PCell = null
      private var segW = 0
      private val timers = mutable.Map.empty[Long, WaitTimerMeta]

      private def addSite(program: () => Unit): Int =
        sitePrograms += program
        sitePrograms.length - 1

      private def newCell(w: Int, init: BitVector, tracked: Boolean): PCell =
        val wv = wide.reg(w, init)
        if !tracked then wv.lanes.foreach(nl.markUntracked)
        val c = new PCell(wv)
        allCells += c
        c

      private def clog2(n: Int): Int =
        if n <= 1 then 1 else 32 - Integer.numberOfLeadingZeros(n - 1)

      private def jump(k: Int): Unit =
        cellEnv(segCellVar) = WV(Vector(nl.const(segW, k.toLong)), segW)

      private def sinkEnv(dcl: DFVal.Dcl, wvOpt: Option[WV]): Unit = wvOpt match
        case Some(wv) => env(dcl) = wv
        case None     => env.remove(dcl)
      private def sinkCell(c: PCell, wvOpt: Option[WV]): Unit = wvOpt match
        case Some(wv) => cellEnv(c) = wv
        case None     => cellEnv.remove(c)
      private def cellHold(c: PCell): Option[WV] = Some(c.regWV)

      // ---- emission -------------------------------------------------------------------------

      /** Recompiles a value fresh in the current transition context (dropping its anonymous
        * dependency tree from the context cache), so guards re-evaluate against the current
        * forwarding state at every boundary they are inlined into.
        */
      private def freshWV(v: DFVal): WV =
        val po = procOverlay
        if po ne null then
          v.collectRelMembers(true).foreach {
            case x: DFVal if x.isAnonymous => po.remove(x)
            case _                         => ()
          }
        readWV(v)
      private def compileGuardFresh(v: DFVal): Int = freshWV(v).lanes(0)

      private def emitPayload(items: List[DFMember]): Unit = items.foreach {
        case _: DFVal => () // values compile lazily on read, per context
        case m        => processMember(m)
      }

      private def emitBranch2(cond: Int, thenFn: () => Unit, elseFn: () => Unit): Unit =
        if nl.isConst(cond) then (if nl.constValOf(cond) != 0L then thenFn() else elseFn())
        else
          if foldProbing then foldViolation = true
          val baseEnv = env.toMap
          val baseCells = cellEnv.toMap
          val po = procOverlay
          val baseOverlay = po.toMap
          val t = transCtx
          val snap = t.snapshot()
          val basePath = pathConds
          def restoreBase(): Unit =
            env.clear(); env ++= baseEnv
            cellEnv.clear(); cellEnv ++= baseCells
            po.clear(); po ++= baseOverlay
            t.restore(snap)
          // dispatch branches are execution paths, not payload conditionals: a full register
          // write on the taken path stays forwardable (the stage's per-path expansion state)
          pathConds = (cond, false) :: basePath
          thenFn()
          val tEnv = env.toMap
          val tCells = cellEnv.toMap
          restoreBase()
          pathConds = (cond, true) :: basePath
          elseFn()
          val eEnv = env.toMap
          val eCells = cellEnv.toMap
          restoreBase()
          pathConds = basePath
          mergeKeyed(baseEnv, List((cond, tEnv)), Some(eEnv), envHoldOf)(sinkEnv)
          mergeKeyed(baseCells, List((cond, tCells)), Some(eCells), cellHold)(sinkCell)
      end emitBranch2

      private def emitDispatchChain(
          h: DFConditional.Header,
          rest: List[DFMember],
          cont: PCont
      ): Unit =
        val blocks = chainBlocksOf(h)
        val blockSet = blocks.toSet[DFMember]
        val after = rest.filterNot(blockSet)
        val selectorWV = h match
          case mh: DFConditional.DFMatchHeader =>
            // the stage's reset-site fold never folds a match dispatch (if-chains only)
            if foldProbing then foldViolation = true
            Some(freshWV(mh.selectorRef.get))
          case _ => None
        val baseEnv = env.toMap
        val baseCells = cellEnv.toMap
        val po = procOverlay
        val baseOverlay = po.toMap
        val t = transCtx
        val snap = t.snapshot()
        val basePath = pathConds
        var negs = List.empty[(Int, Boolean)] // not-taken frames of the branches walked so far
        def restoreBase(): Unit =
          env.clear(); env ++= baseEnv
          cellEnv.clear(); cellEnv ++= baseCells
          po.clear(); po ++= baseOverlay
          t.restore(snap)
        var condBranches = List.empty[(Int, Map[DFVal.Dcl, WV], Map[PCell, WV])]
        var elseResult = Option.empty[(Map[DFVal.Dcl, WV], Map[PCell, WV])]
        var done = false
        for b <- blocks if !done do
          restoreBase()
          blockCondNode(b, selectorWV) match
            case Some(c) if nl.isConst(c) =>
              if nl.constValOf(c) != 0L then
                // statically taken — the rest of the chain is unreachable
                restoreBase()
                pathConds = negs ::: basePath
                emitFrom(bodyOf(b) ::: after, cont)
                elseResult = Some((env.toMap, cellEnv.toMap))
                done = true
            case Some(c) =>
              if foldProbing then foldViolation = true
              pathConds = (c, false) :: negs ::: basePath
              emitFrom(bodyOf(b) ::: after, cont)
              negs ::= (c, true)
              condBranches :+= ((c, env.toMap, cellEnv.toMap))
            case None => // else branch / unguarded catch-all — terminal
              pathConds = negs ::: basePath
              emitFrom(bodyOf(b) ::: after, cont)
              elseResult = Some((env.toMap, cellEnv.toMap))
              done = true
          end match
        end for
        if !done && elseResult.isEmpty then
          // no else: the fall-through path continues past the chain
          restoreBase()
          pathConds = negs ::: basePath
          emitFrom(after, cont)
          elseResult = Some((env.toMap, cellEnv.toMap))
        restoreBase()
        pathConds = basePath
        mergeKeyed(
          baseEnv,
          condBranches.map(t3 => (t3._1, t3._2)),
          elseResult.map(_._1),
          envHoldOf
        )(
          sinkEnv
        )
        mergeKeyed(
          baseCells,
          condBranches.map(t3 => (t3._1, t3._3)),
          elseResult.map(_._2),
          cellHold
        )(sinkCell)
      end emitDispatchChain

      private def emitForIncrement(fb: DFLoop.DFForBlock): Unit =
        val iter = fb.iteratorRef.get
        val w = widthOf(iter)
        val cur = readWV(iter)
        val stepV = constOpt[Option[BigInt]](fb.rangeRef.get.stepRef.get).flatten
          .getOrElse(unsupported("a non-constant loop step", fb))
        env(iter) = WV(Vector(nl.add(cur.lanes(0), nl.const(w, stepV.toLong))), w)
        recordWrite(iter, full = true)

      private def compileForGuard(fb: DFLoop.DFForBlock): Int =
        val range = fb.rangeRef.get
        val iterWV = readWV(fb.iteratorRef.get)
        val endWV0 = freshWV(range.endRef.get)
        val w = math.max(iterWV.width, endWV0.width)
        val a = wide.resize(iterWV, w, signed = true)
        val b = wide.resize(endWV0, w, signed = isSignedType(range.endRef.get.dfType))
        val stepSign = constOpt[Option[BigInt]](range.stepRef.get).flatten
          .map(_.signum).getOrElse(1)
        (range.op, stepSign >= 0) match
          case (DFRange.Op.Until, true)  => wide.ltNode(a, b, signed = true)
          case (DFRange.Op.To, true)     => nl.not(wide.ltNode(b, a, signed = true))
          case (DFRange.Op.Until, false) => wide.ltNode(b, a, signed = true)
          case (DFRange.Op.To, false)    => nl.not(wide.ltNode(a, b, signed = true))

      private def loopGuardNode(lb: DFLoop.Block): Int = lb match
        case fb: DFLoop.DFForBlock   => compileForGuard(fb)
        case wb: DFLoop.DFWhileBlock => compileGuardFresh(wb.guardRef.get)

      private def enterWait(wt: Wait): Unit =
        waitCells.get(wt).foreach { cell =>
          cellEnv(cell) = wide.zero(cell.regWV.width) // counter reset on entry
        }
        jump(siteOf(wt))

      private def enterLoop(lb: DFLoop.Block, exitCont: PCont): Unit =
        lb match
          case fb: DFLoop.DFForBlock =>
            // iterator initialization on the entry edge
            val iter = fb.iteratorRef.get
            val startWV = freshWV(fb.rangeRef.get.startRef.get)
            env(iter) = wide.resize(startWV, widthOf(iter), signed = true)
            recordWrite(iter, full = true)
          case _ => ()
        siteOf.get(lb) match
          case Some(k) => jump(k) // an iteration park or a control state
          case None    => // fused: the entry guard evaluates combinationally on this edge
            crossBoundary()
            emitBranch2(
              loopGuardNode(lb),
              () => emitFrom(bodyOf(lb), PCont.LoopBack(lb, exitCont)),
              () => emitCont(exitCont)
            )
      end enterLoop

      private def enterStep(sb: StepBlock): Unit =
        siteOf.get(sb) match
          case Some(k) => jump(k)
          case None    => // fused entry: the step's leading payload joins this transition cycle
            crossBoundary()
            emitFrom(bodyOf(sb), stepExitConts(sb))

      private def emitGoto(g: Goto): Unit =
        g.stepRef.get match
          case sb: StepBlock  => enterStep(sb)
          case Goto.ThisStep  => enterStep(enclosingStep(g))
          case Goto.NextStep  => emitCont(stepExitConts(enclosingStep(g)))
          case Goto.FirstStep => enterStep(firstRegularStep)

      private def emitFrom(items: List[DFMember], cont: PCont): Unit = items match
        case Nil       => emitCont(cont)
        case m :: rest =>
          m match
            case wt: Wait                                => enterWait(wt)
            case lb: DFLoop.Block if !lb.isCombinational =>
              enterLoop(lb, PCont.SeqC(rest, cont))
            case sb: StepBlock                                 => enterStep(sb)
            case g: Goto                                       => emitGoto(g)
            case h: DFConditional.Header if chainHasControl(h) =>
              emitDispatchChain(h, rest, cont)
            case h: DFConditional.Header => // payload chain (headers are values — check first)
              processMember(h)
              emitFrom(rest, cont)
            case _: DFConditional.Block => emitFrom(rest, cont) // handled at its header
            case _: DFVal               => emitFrom(rest, cont) // lazily compiled on read
            case other                  =>
              processMember(other)
              emitFrom(rest, cont)

      private def emitCont(cont: PCont): Unit = cont match
        case PCont.SeqC(rest, outer)   => emitFrom(rest, outer)
        case PCont.LoopBack(lb, outer) =>
          lb match
            case fb: DFLoop.DFForBlock => emitForIncrement(fb)
            case _                     => ()
          siteOf.get(lb) match
            case Some(k) => jump(k) // the control state re-evaluates the guard next cycle
            case None    => // fused loop-back: forwarded guard in this transition cycle
              crossBoundary()
              emitBranch2(
                loopGuardNode(lb),
                () => emitFrom(bodyOf(lb), cont),
                () => emitCont(outer)
              )
        case PCont.Wrap =>
          // forever wrap-around: re-execute the prologue payload and re-enter the process
          if needsBoot then jump(bootSite)
          else emitFrom(top, PCont.Wrap)

      // ---- site programs ----------------------------------------------------------------------

      private def emitWait(wt: Wait, cont: PCont): Unit =
        waitKindOf(wt) match
          case WaitKind.Cycles1       => emitCont(cont) // parks exactly one cycle
          case WaitKind.CyclesN(nVal) =>
            val cell = waitCells(wt)
            val w = cell.regWV.width
            val cnt = cell.regWV.lanes(0)
            val nNode = freshWV(nVal).lanes(0)
            val bound = nl.sub(nNode, nl.const(w, 1L))
            timers(siteOf(wt).toLong) = WaitTimerMeta(cnt, bound, SimOps.maskFor(w))
            emitBranch2(
              nl.neq(cnt, bound),
              () => cellEnv(cell) = WV(Vector(nl.add(cnt, nl.const(w, 1L))), w),
              () => emitCont(cont)
            )
          case WaitKind.CyclesLit(n) =>
            val cell = waitCells(wt)
            val w = cell.regWV.width
            val cnt = cell.regWV.lanes(0)
            val bound = nl.const(w, n - 1)
            timers(siteOf(wt).toLong) = WaitTimerMeta(cnt, bound, SimOps.maskFor(w))
            emitBranch2(
              nl.neq(cnt, bound),
              () => cellEnv(cell) = WV(Vector(nl.add(cnt, nl.const(w, 1L))), w),
              () => emitCont(cont)
            )
          case WaitKind.CondW(trigger) =>
            emitBranch2(compileGuardFresh(trigger), () => emitCont(cont), () => ())
          case WaitKind.Endless => () // parked forever (the state cell holds by default)

      private def emitParkLoop(lb: DFLoop.Block, exitCont: PCont): Unit = lb match
        case fb: DFLoop.DFForBlock =>
          emitBranch2(
            compileForGuard(fb),
            () =>
              emitPayload(bodyOf(fb))
              emitForIncrement(fb) // stays parked (the state cell holds by default)
            ,
            () => emitCont(exitCont)
          )
        case wb: DFLoop.DFWhileBlock =>
          emitBranch2(
            compileGuardFresh(wb.guardRef.get),
            () => emitPayload(bodyOf(wb)),
            () => emitCont(exitCont)
          )

      private def emitCtrlLoop(lb: DFLoop.Block, exitCont: PCont): Unit =
        lb match
          case fb: DFLoop.DFForBlock =>
            emitBranch2(
              compileForGuard(fb),
              () => emitFrom(bodyOf(fb), PCont.LoopBack(fb, exitCont)),
              () => emitCont(exitCont)
            )
          case wb: DFLoop.DFWhileBlock =>
            emitBranch2(
              compileGuardFresh(wb.guardRef.get),
              () => emitFrom(bodyOf(wb), PCont.LoopBack(wb, exitCont)),
              () => emitCont(exitCont)
            )

      private def emitStepPark(sb: StepBlock): Unit =
        emitFrom(bodyOf(sb), stepExitConts(sb))

      // ---- top-level --------------------------------------------------------------------------

      def run(): Unit =
        // sequential-loop iterators become state cells (registers keyed by their declaration)
        flattenedOf(pb).foreach {
          case fb: DFLoop.DFForBlock if !fb.isCombinational =>
            val iter = fb.iteratorRef.get
            val w = widthOf(iter)
            regNodeOf(iter) = wide.reg(w, initOverride.getOrElse(iter, BitVector.low(w)))
          case _ => ()
        }
        computeFallbacks()
        // site allocation (the boot state first, matching the synthetic S_0 of the FSM lowering)
        if needsBoot then bootSite = addSite(() => emitFrom(top, PCont.Wrap))
        // a fused process-leading construct still needs a state for the reset entry (there is no
        // jump site to inline its dispatch into at reset). It is allocated up front and its
        // program probed during emission: when the dispatch const-folds under the prologue values
        // (the reset-site fold), the folded assignments become time-zero state, the FSM resets
        // directly into the fold's target park, and this site stays allocated but unreachable
        val firstFused = !needsBoot && firstConstructOpt.exists {
          case o: DFOwner => isFusable(o) && !fallback.contains(o)
          case _          => false
        }
        val resetSite = if firstFused then addSite(() => emitFrom(top, PCont.Wrap)) else -1
        val positions = parkPositions()
        for (m, cont) <- positions do
          m match
            case wt: Wait =>
              waitKindOf(wt) match
                case WaitKind.CyclesN(nVal) =>
                  val w = widthOf(nVal)
                  if w > 64 then unsupported("a cycle count wider than 64 bits", wt)
                  waitCells(wt) = newCell(w, BitVector.low(w), tracked = false)
                case WaitKind.CyclesLit(n) =>
                  val w = math.max(1, 64 - java.lang.Long.numberOfLeadingZeros(n - 1))
                  waitCells(wt) = newCell(w, BitVector.low(w), tracked = false)
                case _ => ()
              siteOf(wt) = addSite(() => emitWait(wt, cont))
            case lb: DFLoop.Block =>
              if isParkLoop(lb) then siteOf(lb) = addSite(() => emitParkLoop(lb, cont))
              else siteOf(lb) = addSite(() => emitCtrlLoop(lb, cont))
            case sb: StepBlock => siteOf(sb) = addSite(() => emitStepPark(sb))
            case m             => unsupported("park construct", m)
        end for
        segW = clog2(sitePrograms.length)
        segCellVar = newCell(segW, BitVector.low(segW), tracked = true)
        val segNode = segCellVar.regWV.lanes(0)
        // the per-site dispatch conditions: the base path frame of each program's text-output
        // actions during emission, and the merge keys afterwards
        val siteConds =
          sitePrograms.indices.toList.map(k => nl.eq(segNode, nl.const(segW, k.toLong)))
        // per-site program emission, each in a fresh transition context
        val envAtStart = env.toMap
        val basePath = pathConds
        val progEnvs = mutable.ArrayBuffer.empty[Map[DFVal.Dcl, WV]]
        val progCells = mutable.ArrayBuffer.empty[Map[PCell, WV]]
        for k <- sitePrograms.indices do
          env.clear(); env ++= envAtStart
          cellEnv.clear()
          procOverlay = mutable.Map.empty
          transCtx = new TransCtx(condDepth)
          pathConds = (siteConds(k), false) :: basePath
          foldProbing = firstFused && k == resetSite
          if foldProbing then foldViolation = false
          sitePrograms(k)()
          foldProbing = false
          progEnvs += env.toMap
          progCells += cellEnv.toMap
        procOverlay = null
        transCtx = null
        pathConds = basePath
        env.clear(); env ++= envAtStart
        cellEnv.clear()
        // the reset-site fold, with the stage's gates: no dynamic dispatch decision, a single
        // constant jump landing exactly on the first park in member order, and only constant
        // full-register values assigned along the way. Any miss keeps the reset state as a real
        // (one-cycle) bootstrap.
        val foldedEntry: Option[Int] =
          if !firstFused || foldViolation then None
          else
            val fEnv = progEnvs(resetSite)
            val fCells = progCells(resetSite)
            val changed = fEnv.filter((dcl, wv) => !envAtStart.get(dcl).contains(wv))
            val jumpConst = fCells.get(segCellVar)
              .filter(_.lanes.forall(nl.isConst))
              .map(wv => nl.constValOf(wv.lanes(0)).toInt)
            val firstParkSite = positions.headOption.map((m, _) => siteOf(m))
            val assignsOk = changed.forall { (dcl, wv) =>
              regNodeOf.contains(dcl) && wv.lanes.forall(nl.isConst)
            }
            val cellsOk = fCells.forall { (c, wv) =>
              (c eq segCellVar) || wv.lanes.forall(nl.isConst)
            }
            if jumpConst.exists(firstParkSite.contains) && assignsOk && cellsOk then
              for (dcl, wv) <- changed do wide.setRegInit(regNodeOf(dcl), wide.constBits(wv))
              jumpConst
            else None
        val entryIdx =
          if needsBoot then bootSite
          else
            foldedEntry.getOrElse {
              if firstFused then resetSite // the kept one-time reset bootstrap state
              else
                val fc = top.find(isTimeConstructM).getOrElse(unsupported("process entry", pb))
                siteOf.getOrElse(fc, unsupported("an unresolvable process entry site", fc))
            }
        nl.setRegInit(segNode, entryIdx.toLong)
        // merge all site results keyed by the state value
        mergeKeyed(envAtStart, siteConds.zip(progEnvs), None, envHoldOf)(sinkEnv)
        val mergedCells = mutable.Map.empty[PCell, WV]
        mergeKeyed(Map.empty[PCell, WV], siteConds.zip(progCells), None, cellHold) { (c, wvOpt) =>
          wvOpt.foreach(mergedCells(c) = _)
        }
        for c <- allCells do wide.setNext(c.regWV, mergedCells.getOrElse(c, c.regWV))
        procMetas += ProcMeta(segNode, timers.toMap)
      end run
    end ProcLowering

    // ---- finalize ------------------------------------------------------------------------

    private def finalizeScope(): Unit =
      // compose partially-driven values (must fully cover the target, no overlaps)
      for (dcl, parts) <- partialDrivers do
        val w = widthOf(dcl)
        val sorted = parts.sortBy(_._2) // by lo
        var expectedLo = 0
        for (hi, lo, _) <- sorted do
          if lo != expectedLo || hi < lo then
            unsupported(s"partial drivers with gaps/overlaps at bit $lo", dcl)
          expectedLo = hi + 1
        if expectedLo != w then unsupported("partial drivers not covering the full value", dcl)
        env(dcl) = wide.assemble(sorted.toSeq.map((_, lo, wv) => wv -> lo), w)
      // registers commit their pending value; unassigned registers (incl. top IN hold cells) hold
      for (dcl, regWV) <- regNodeOf do wide.setNext(regWV, env.getOrElse(dcl, regWV))
      // names for peek/poke: ports and registered declarations only — registering a named comb
      // wire would force a per-cycle signal-array store in the codegen tier just to keep it
      // peekable, so combinational VAR wires stay unobserved (like any named comb expression)
      designMembers.foreach {
        case dcl: DFVal.Dcl
            if !dcl.isAnonymous &&
              (dcl.modifier.isReg || dcl.modifier.dir != DFVal.Modifier.Dir.VAR) =>
          regNodeOf.get(dcl).orElse(env.get(dcl)).orElse(inPortMov.get(dcl)).foreach { wv =>
            namedNodes(prefix + dcl.getName) = wv
          }
        case _ =>
      }
    end finalizeScope

    // ---- width helpers ---------------------------------------------------------------------

    private def widthOf(v: DFVal): Int = widthOfType(v.dfType, v)
    private def widthOfType(t: DFType, where: Any): Int =
      val w = t.widthIntOpt.orElse(widthThroughParams(t))
        .getOrElse(unsupported("unresolvable (param-dependent) width", where))
      if w < 1 then unsupported(s"width $w", where)
      w

    /** `widthIntOpt` resolves param refs with the default `Always` cache policy, under which a
      * non-device-top DesignParam stays symbolic — re-resolve through design params instead.
      */
    private def widthThroughParams(t: DFType): Option[Int] =
      given ConstData.CachePolicy = ConstData.CachePolicy.NoCache
      t match
        case b: DFBits    => b.widthParamRef.getIntConstData.toOption
        case d: DFDecimal =>
          d.magnitudeWidthParamRef.getIntConstData.toOption.map(_ + d.fractionWidth)
        case v: DFVector =>
          val cellW = v.cellType.widthIntOpt.orElse(widthThroughParams(v.cellType))
          val dims = v.cellDimParamRefs.map(_.getIntConstData.toOption)
          if dims.exists(_.isEmpty) then None
          else cellW.map(_ * dims.flatten.product)
        case s: DFStruct =>
          val ws = s.fieldMap.values.map(ft => ft.widthIntOpt.orElse(widthThroughParams(ft)))
          if ws.exists(_.isEmpty) then None else Some(ws.flatten.sum)
        case o: DFOpaque => o.actualType.widthIntOpt.orElse(widthThroughParams(o.actualType))
        case _           => None
    end widthThroughParams
  end Scope
end Builder
