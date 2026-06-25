package dfhdl.tools

/** The generic context DFHDL provides to every foreign IP simulation hook:
  *
  *   - [[ipName]]: the IP name (its `dfhdl-ips/<ipName>` subfolder)
  *   - [[ipDir]]: the committed IP folder in the project (`<project>/dfhdl-ips/<ipName>`)
  *   - [[topName]]: the top design name being simulated
  *
  * An IP that needs more (test/capture config, per-run state, …) extends this with its own context
  * type and carries it through the whole lifecycle — DFHDL never needs to know those specifics.
  */
open class ForeignSimContext(
    val ipName: String,
    val ipDir: os.Path,
    val topName: String
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
  def onSimStart(ctx: Ctx): Unit = {}
  def onSimEnd(ctx: Ctx): Unit = {}

  /** Environment variables the simulator process must see for this IP to work (e.g. a viewer
    * rendezvous address). Queried after [[onSimStart]] (so a hook can report e.g. a port it just
    * bound) and merged into the spawned simulator's environment. Defaults to none.
    */
  def simEnv(ctx: Ctx): Map[String, String] = Map.empty

object ForeignSimHook:
  /** A hook with its context already bound, letting the tools layer drive the lifecycle without
    * carrying the (existential) `Ctx` type around.
    */
  trait Bound:
    def onSimStart(): Unit
    def simEnv(): Map[String, String]
    def onSimEnd(): Unit

  /** Build a hook's context and bind it, capturing the existential `Ctx` of a reflectively-loaded
    * hook in one place so call sites stay untyped.
    */
  def bind[Ctx <: ForeignSimContext](hook: ForeignSimHook[Ctx], base: ForeignSimContext): Bound =
    val ctx = hook.context(base)
    new Bound:
      def onSimStart(): Unit = hook.onSimStart(ctx)
      def simEnv(): Map[String, String] = hook.simEnv(ctx)
      def onSimEnd(): Unit = hook.onSimEnd(ctx)
end ForeignSimHook
