package dfhdl.sim

import dfhdl.compiler.ir.*
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
  *
  * Known minimum limitations: processes/domains, `**`/`clog2` on non-constants,
  * multiplication/division with results wider than 64 bits, bubble (`?`) values simulate as 0
  * (2-state), and per-instance param-dependent *widths* (widths resolve via the sub-DB's canonical
  * instance).
  */
object DFacsimile:
  def simulate(db: DB, tier: SimTier = SimTier.Interpreter): Sim =
    val builder = new Builder(db)
    builder.build()
    val kernel = tier match
      case SimTier.Interpreter => Interpreter.compile(builder.nl)
      case SimTier.Codegen     =>
        // named values are peekable — force their lanes into the signal array
        Codegen.compile(builder.nl, observed = builder.namedNodes.values.flatMap(_.lanes).toSet)
    new Sim(builder.nl, kernel, builder.namedNodes.toMap)
end DFacsimile

/** A running simulation instance: one state/signal array + a kernel over it. Values are addressed
  * by name; hierarchy paths use instance names (e.g. "alu0.res"). Values of any width move across
  * this boundary as packed [[BitVector]]s; the `Long` variants are a convenience for values up to
  * 64 bits.
  */
final class Sim private[sim] (
    val nl: Netlist,
    kernel: SimKernel,
    nameToWV: Map[String, WV]
):
  private val sig = nl.initialSig
  // settle-on-peek: peeks always observe combinationally settled state (Amaranth's rule)
  private var needsSettle = true
  def step(cycles: Long = 1L): Unit =
    kernel.run(sig, cycles)
    needsSettle = true // post-commit register values invalidate the comb sweep
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
end Sim

private final class Builder(rawDB: DB):
  private[sim] val nl = new Netlist
  private val wide = new WideOps(nl)
  private[sim] val namedNodes = mutable.Map.empty[String, WV]
  // the new-style root DB is only a hierarchy container; content lives in per-design sub-DBs
  private val topScopeDB: DB = if rawDB.isRoot then rawDB.topDB else rawDB

  private def unsupported(what: String, m: Any): Nothing =
    throw new UnsupportedOperationException(
      s"DFacsimile (minimum) does not support $what:\n$m"
    )

  def build(): Unit =
    new Scope(topScopeDB, "", None).elaborate()

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
    // write-only views: net sinks and the alias chains under them (never built as reads)
    private val writeViews = mutable.Set.empty[DFVal]

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
      // pre-pass: state cells — registers, and IN ports (pokeable hold cells at top,
      // MOV placeholders patched by the parent's connections otherwise)
      designMembers.foreach {
        case dcl: DFVal.Dcl if dcl.modifier.isReg =>
          val w = widthOf(dcl)
          val init = dcl.initRefList.headOption match
            case Some(initRef) => regInitBits(initRef.get, w)
            case None          => BitVector.low(w)
          regNodeOf(dcl) = wide.reg(w, init)
        case dcl: DFVal.Dcl =>
          dcl.modifier.dir match
            case DFVal.Modifier.Dir.IN =>
              if isTop then regNodeOf(dcl) = wide.reg(widthOf(dcl), BitVector.low(widthOf(dcl)))
              else inPortMov(dcl) = wide.mov(widthOf(dcl))
            case _ => // wires/OUT ports bind at their driving net
        case _ =>
      }
      // globals closure of this sub-DB (constants incl. ROM data)
      processMembers(db.membersGlobals)
      // ordered walk of the design body
      processMembers(childrenOf.getOrElse(design, Vector.empty))
      finalizeScope()
    end elaborate

    private def processMembers(ms: Iterable[DFMember]): Unit = ms.foreach {
      case _: DFVal.Dcl            => () // declarations: state in pre-pass, wires at their net
      case p: DFVal.DesignParam    => bindParam(p)
      case _: DFConditional.Block  => () // processed by its header's chain
      case h: DFConditional.Header => processConditionalChain(h)
      case v: DFVal if writeViews.contains(v) => () // write-only view of a sink
      case v: DFVal if isConstVector(v)       => () // ROM data, materialized at its use site
      case v: DFVal                           =>
        tryFoldConst(v) match
          case Some(wv) => bindVal(v, wv)
          case None     =>
            v match
              case pbns: DFVal.PortByNameSelect => bindVal(pbns, pbnsReadWV(pbns))
              case f: DFVal.Func                => bindVal(f, buildFunc(f))
              case a: DFVal.Alias.AsIs          =>
                val rel = a.relValRef.get
                bindVal(a, wide.resize(readWV(rel), widthOf(a), isSignedType(rel.dfType)))
              case a: DFVal.Alias.ApplyIdx     => bindVal(a, buildApplyIdx(a))
              case a: DFVal.Alias.ApplyRange   => bindVal(a, buildApplyRange(a))
              case sf: DFVal.Alias.SelectField => bindVal(sf, buildSelectField(sf))
              case h: DFVal.Alias.History      => bindVal(h, buildHistory(h))
              case c: DFVal.Const              =>
                // reached only for bubble (don't-care) constants (`?`) — simulate as 0 (2-state)
                bindVal(c, wide.const(widthOf(c), lenientDataToBits(c.dfType, c.data, c)))
              case m => unsupported("value kind", m)
      case net: DFNet         => buildNet(net)
      case inst: DFDesignInst => elaborateChild(inst)
      case m                  => unsupported("member kind", m)
    }

    private def bindVal(v: DFVal, wv: WV): Unit =
      nodeOf(v) = wv
      if !v.isAnonymous then namedNodes(prefix + v.getName) = wv

    // ---- reads ----------------------------------------------------------------------------

    private def readWV(v: DFVal): WV = v match
      case dcl: DFVal.Dcl =>
        regNodeOf.get(dcl).orElse(env.get(dcl)).orElse(inPortMov.get(dcl))
          .getOrElse(unsupported("reading a value before it is driven", dcl))
      case v =>
        nodeOf.get(v) match
          case Some(wv) => wv
          case None     =>
            // whole-value reads of constants that were skipped in the walk (e.g. ROM vectors)
            tryFoldConst(v) match
              case Some(wv) =>
                nodeOf(v) = wv
                wv
              case None => unsupported("reading a value before it is built", v)

    /** Const resolution for simulation: the NoCache policy recomputes through design params, immune
      * to previously-cached symbolic (Always-policy) results.
      */
    private def constOpt[T](v: DFVal): Option[T] =
      v.getConstData[T](using summon[MemberGetSet], ConstData.CachePolicy.NoCache).toOption

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
        case FO.+         => args.map(rdAt(_, resW)).reduce(wide.add)
        case FO.-         => args.map(rdAt(_, resW)).reduce(wide.sub)
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
        case FO.<<  =>
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

    private def processConditionalChain(header: DFConditional.Header): Unit =
      import DFConditional.DFCaseBlock.Pattern
      val blocks = db.conditionalChainTable.getOrElse(header, Nil)
      if blocks.isEmpty then unsupported("conditional header without blocks", header)
      val selectorWV = header match
        case mh: DFConditional.DFMatchHeader => Some(readWV(mh.selectorRef.get))
        case _                               => None
      def patternCond(p: Pattern): Int = p match
        case Pattern.Singleton(ref)    => wide.eqNode(selectorWV.get, readWV(ref.get))
        case Pattern.Alternative(list) => list.map(patternCond).reduce(nl.or)
        case p                         => unsupported(s"match pattern $p", header)
      val isExpr = header.dfType match
        case DFUnit => false
        case _      => true

      val baseEnv = env.toMap
      case class Branch(condOpt: Option[Int], resultEnv: Map[DFVal.Dcl, WV], yieldOpt: Option[WV])
      val branches = blocks.map { block =>
        env.clear(); env ++= baseEnv
        val guardCond = block.guardRef.get match
          case g: DFVal => Some(readWV(g).lanes(0))
          case _        => None
        val condOpt = block match
          case cb: DFConditional.DFCaseBlock =>
            val patCond = cb.pattern match
              case Pattern.CatchAll => None
              case p                => Some(patternCond(p))
            (patCond, guardCond) match
              case (Some(p), Some(g)) => Some(nl.and(p, g))
              case (p, g)             => p.orElse(g)
          case _ => guardCond
        val blockMembers = childrenOf.getOrElse(block, Vector.empty)
        condDepth += 1
        processMembers(blockMembers)
        condDepth -= 1
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
      // merge assigned values with mux trees, right (else/default) to left
      val assignedKeys = branches.iterator.flatMap(_.resultEnv.keys).toSet
        .filter(dcl => branches.exists(b => b.resultEnv.get(dcl) != baseEnv.get(dcl)))
      for dcl <- assignedKeys do
        // registers hold their value when unassigned; wires fall back to their prior value
        val default: Option[WV] =
          if regNodeOf.contains(dcl) then Some(baseEnv.getOrElse(dcl, regNodeOf(dcl)))
          else baseEnv.get(dcl)
        val start: Option[WV] = elseBranch match
          case Some(b) => b.resultEnv.get(dcl).orElse(default)
          case None    => default
        val merged = condBranches.foldRight(start) { (b, acc) =>
          (b.resultEnv.get(dcl).orElse(default), acc) match
            case (Some(t), Some(f)) => Some(wide.mux(b.condOpt.get, t, f))
            case _                  => None
        }
        merged match
          case Some(n) => env(dcl) = n
          case None    => env.remove(dcl) // partially driven wire: poison until re-driven
      end for
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

    // ---- nets & hierarchy -----------------------------------------------------------------

    private def buildNet(net: DFNet): Unit = net.op match
      case DFNet.Op.Assignment =>
        net.lhsRef.get match
          case dcl: DFVal.Dcl     => env(dcl) = readWV(net.rhsRef.get)
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
      // names for state cells and driven wires
      designMembers.foreach {
        case dcl: DFVal.Dcl if !dcl.isAnonymous =>
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
