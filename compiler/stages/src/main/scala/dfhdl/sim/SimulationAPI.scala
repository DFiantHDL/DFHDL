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
  */
final class Simulation[D <: Design] private[sim] (
    dsn: D,
    block: Option[DFCG ?=> SimCtx ?=> D => Unit],
    tier: SimTier,
    seed: Long
):
  def withTier(tier: SimTier): Simulation[D] = new Simulation(dsn, block, tier, seed)
  // seed is carried for API completeness; randomization support lands later
  def withSeed(seed: Long): Simulation[D] = new Simulation(dsn, block, tier, seed)

  /** Execute the simulation: fresh state per run; the host block runs as the main process. */
  def run(): Unit =
    val raw = DFacsimile.simulate(dsn.getDB, tier)
    val ctx = new SimCtx(raw, memberPath, concretize)
    val dfc = DFCG()
    block.foreach(b => b(using dfc)(using ctx)(dsn))

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

  // flat immutable DB used only by the bridge to enumerate sibling instances
  private lazy val flatDB = dsn.dfc.getSet.designDB
  private lazy val flatInsts = flatDB.members.collect { case i: ir.DFDesignInst => i }

  // Repeated instance names (e.g. `List.fill(n)(SubDesign())`) get indexed path segments
  // (adder_0, adder_1, ...) — matching the lowering's naming. The cached (elaboration-time)
  // DFDesignInst maps to its immutable-DB copy by `ownerRef` (preserved through unifyInst);
  // the rank among same-named same-parent instances in member order gives the index.
  private def instSegment(d: ir.DFDesignBlock)(using MemberGetSet): String =
    val cached = d.getCachedDesignInst
    val name = cached.getName
    val flatGS = flatDB.getSet
    flatInsts.find(_.ownerRef == cached.ownerRef) match
      case Some(myCopy) =>
        val parent = myCopy.getOwnerDesign(using flatGS)
        val siblings = flatInsts.filter { i =>
          (i.getName(using flatGS) == name) && (i.getOwnerDesign(using flatGS) eq parent)
        }
        if siblings.sizeIs > 1 then s"${name}_${siblings.indexOf(myCopy)}" else name
      case None => name
end Simulation

/** Simulation capability context: passed contextually into the host block. */
final class SimCtx private[sim] (
    // internal kernel access: by-name-path handles, packed bits (not public API —
    // the typed peek/poke surface is the only value interface)
    private[sim] val raw: Sim,
    private[sim] val memberPath: ir.DFVal => String,
    private[sim] val concretize: ir.DFType => ir.DFType
):
  def step(cycles: Long = 1L): Unit = raw.step(cycles)
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
