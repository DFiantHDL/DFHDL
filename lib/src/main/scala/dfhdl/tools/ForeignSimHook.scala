package dfhdl.tools

import dfhdl.options.SimulatorOptions

/** The generic context DFHDL provides to every foreign IP simulation hook:
  *
  *   - [[ipName]]: the IP name (its `dfhdl-ips/<ipName>` subfolder)
  *   - [[ipDir]]: the committed IP folder in the project (`<project>/dfhdl-ips/<ipName>`)
  *   - [[topName]]: the top design name being simulated
  *   - [[platformID]]: the top design's `@platformID(...)` name, if annotated (e.g. `ulx3s`)
  *   - [[simInDFTools]]: whether the simulator itself runs from a DFTools image. This is the tool's
  *     *effective* location — under `tools-location = auto` it cannot be derived from the option
  *     alone — and is what a hook must key its own launches on (e.g. a viewer sharing the
  *     simulator's loopback must run inside the DFTools network too, or on the host with it).
  *
  * An IP that needs more (test/capture config, per-run state, …) extends this with its own context
  * type and carries it through the whole lifecycle — DFHDL never needs to know those specifics.
  */
open class ForeignSimContext(
    val ipName: String,
    val ipDir: os.Path,
    val topName: String,
    val platformID: Option[String] = None,
    val simInDFTools: Boolean = false
)

/** A hook a foreign IP can register to run code around a DFHDL-driven simulation (e.g. launch a
  * viewer). The IP names a Scala `object` extending this trait via its `EDBlackBox.ForeignIP`
  * `simHookClass` FQN; DFHDL reflectively loads it, builds its [[context]] from the generic
  * [[ForeignSimContext]], then drives the lifecycle: [[onSimStart]] before the simulator process
  * starts, [[simEnv]] to contribute environment for the run, and [[onSimEnd]] after it finishes
  * (even on failure).
  *
  * `Ctx` lets the IP carry a richer, IP-specific context (its own config + per-run state) through
  * the whole lifecycle, keeping all IP specifics out of the generic mechanism.
  */
trait ForeignSimHook[Ctx <: ForeignSimContext]:
  /** Build this hook's context from the generic one DFHDL provides (read IP-specific config here).
    */
  def context(base: ForeignSimContext): Ctx

  /** Run before the simulator process starts. The [[SimulatorOptions]] are provided so a hook can
    * honor the active `tools-location` — e.g. launch its viewer locally (host PATH) or inside a
    * DFTools image to match wherever the simulator itself runs.
    */
  def onSimStart(ctx: Ctx)(using SimulatorOptions): Unit = {}
  def onSimEnd(ctx: Ctx)(using SimulatorOptions): Unit = {}

  /** Environment variables the simulator process must see for this IP to work (e.g. a viewer
    * rendezvous address). Queried after [[onSimStart]] (so a hook can report e.g. a port it just
    * bound) and merged into the spawned simulator's environment. Defaults to none.
    */
  def simEnv(ctx: Ctx): Map[String, String] = Map.empty
end ForeignSimHook

object ForeignSimHook:
  /** A hook with its context already bound, letting the tools layer drive the lifecycle without
    * carrying the (existential) `Ctx` type around.
    */
  trait Bound:
    def onSimStart(): Unit
    def simEnv(): Map[String, String]
    def onSimEnd(): Unit

  /** Build a hook's context and bind it, capturing the existential `Ctx` of a reflectively-loaded
    * hook in one place so call sites stay untyped. The current [[SimulatorOptions]] are captured
    * here and replayed into the lifecycle methods, so [[Bound]] stays no-arg for the tools layer.
    */
  def bind[Ctx <: ForeignSimContext](hook: ForeignSimHook[Ctx], base: ForeignSimContext)(using
      SimulatorOptions
  ): Bound =
    val ctx = hook.context(base)
    new Bound:
      def onSimStart(): Unit = hook.onSimStart(ctx)
      def simEnv(): Map[String, String] = hook.simEnv(ctx)
      def onSimEnd(): Unit = hook.onSimEnd(ctx)
end ForeignSimHook
