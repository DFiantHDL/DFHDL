package dfhdl.sim

import dfhdl.compiler.ir
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.core.*
import dfhdl.internals.*

/** The typed, object-based simulation frontend (locked decision 10): a deferred [[Simulation]]
  * builder entered via `dsn.simulation { dut => ... }`, with `peek`/`poke` extensions on the
  * design's member objects. Values are DFHDL constants (`peek` on a port of type `T` returns
  * `T <> CONST`; `poke` accepts what `:=` accepts). The typed layer is a thin wrapper over the raw
  * kernel access ([[Sim]]: name-path handles, packed `Long` values), which is internal-only — the
  * typed surface is the sole public value interface.
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
    val ctx = new SimCtx(raw, memberPath)
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
    // internal kernel access: by-name-path handles, packed Long values (not public API —
    // the typed peek/poke surface is the only value interface)
    private[sim] val raw: Sim,
    private[sim] val memberPath: ir.DFVal => String
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

// host-side constants must carry a concrete (non-parametric) type; the netlist knows the
// param-resolved width even when the member's own type references design params
private def concreteFE[T <: DFTypeAny](irType: ir.DFType, width: Int): T =
  val concreteIR = irType match
    case b: ir.DFBits    => b.copy(widthParamRef = ir.IntParamRef(width))
    case d: ir.DFDecimal =>
      d.copy(magnitudeWidthParamRef = ir.IntParamRef(width - d.fractionWidth))
    case other => other
  concreteIR.asFE[T]

extension [T <: DFTypeAny, M <: ModifierAny](dfVal: DFVal[T, M])
  /** Read the combinationally settled value of this member as a DFHDL constant. */
  def peek(using ctx: SimCtx, dfc: DFCG): DFConstOf[T] =
    val irVal = dfVal.asIR
    val path = ctx.memberPath(irVal)
    val value = ctx.raw.peek(path)
    val width = ctx.raw.widthOf(path)
    val data = DataConv.longToData(irVal.dfType, width, value)
    DFVal.Const.forced(concreteFE(irVal.dfType, width), data)

  /** Convert anything `:=` accepts into a constant of this member's concrete type — the
    * expected-value counterpart of `poke` (e.g. `dut.sum.constOf(5)`).
    */
  def constOf(value: DFVal.TC.Exact[T])(using ctx: SimCtx, dfc: DFCG): DFConstOf[T] =
    import dfc.getSet
    val irVal = dfVal.asIR
    val path = ctx.memberPath(irVal)
    val converted = value(concreteFE(irVal.dfType, ctx.raw.widthOf(path)))
    converted.asIR match
      case c: ir.DFVal if c.isConst => converted.asInstanceOf[DFConstOf[T]]
      case other                    =>
        throw new IllegalArgumentException(s"value must convert to a constant, got:\n$other")

  /** Deposit a value on this member; accepts exactly what `:=` accepts. */
  def poke(value: DFVal.TC.Exact[T])(using ctx: SimCtx, dfc: DFCG): Unit =
    import dfc.getSet
    val constIR = constOf(value).asIR
    val packed =
      DataConv.dataToLongOpt(constIR.dfType, constIR.getConstDataOrDefault[Any]).getOrElse(
        throw new IllegalArgumentException(s"unsupported poke value data:\n$constIR")
      )
    ctx.raw.poke(ctx.memberPath(dfVal.asIR), packed)
end extension

/** Packed-Long <-> `Data` conversion at the typed/raw boundary (widths <= 64). */
private[sim] object DataConv:
  def longToData(dfType: ir.DFType, width: Int, value: Long): Any = dfType match
    case _: ir.DFBits =>
      (BitVector.fromLong(value, width), BitVector.low(width))
    case d: ir.DFDecimal =>
      val bigVal =
        if d.signed && width < 64 && ((value >> (width - 1)) & 1L) == 1L then
          BigInt(value - (1L << width))
        else if !d.signed && width == 64 && value < 0 then
          (BigInt(value >>> 1) << 1) | (value & 1L)
        else BigInt(value)
      Some(bigVal)
    case _: ir.DFBoolOrBit => Some(value != 0L)
    case _: ir.DFEnum      => Some(BigInt(value))
    case t                 => throw new UnsupportedOperationException(s"unsupported peek type $t")

  def dataToLongOpt(dfType: ir.DFType, data: Any): Option[Long] = dfType match
    case _: ir.DFBits =>
      val (value, bubble) = data.asInstanceOf[(BitVector, BitVector)]
      if bubble.isZeros then Some(value.toLong(signed = false)) else None
    case _: ir.DFDecimal | _: ir.DFEnum =>
      data.asInstanceOf[Option[BigInt]].map(_.toLong)
    case _: ir.DFBoolOrBit =>
      data.asInstanceOf[Option[Boolean]].map(v => if v then 1L else 0L)
    case _ => None
end DataConv
