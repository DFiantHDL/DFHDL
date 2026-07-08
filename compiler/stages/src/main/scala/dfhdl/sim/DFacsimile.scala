package dfhdl.sim

import dfhdl.compiler.ir.*
import dfhdl.internals.*
import scala.collection.mutable

/** Which execution kernel runs the lowered design. */
enum SimTier derives CanEqual:
  case Interpreter, Codegen

/** Minimum-viable DFacsimile: lowers an elaborated DFHDL design DB (new-style hierarchical root DB)
  * into the flat pre-scheduled [[Netlist]] and executes it on a kernel tier ([[Interpreter]] or
  * [[Codegen]]).
  *
  * Supported IR subset (fails loudly on anything else):
  *   - scalar Dcls (vars/ports) of DFBits/DFDecimal/DFBool/DFBit up to 64 bits
  *   - REG Dcls with constant init; register hold semantics when unassigned in a branch
  *   - funcs: n-ary `+`/`&`/`|`/`^`, `++`, `===`/`=!=`, `unary_~`/`unary_!`, `<<`/`>>` by constant
  *     amount, `ror`/`rol`, `sel`
  *   - `AsIs` casts, bit-select/range on Bits (constant index), constant-vector indexing (ROM)
  *   - conditionals: `if`/`match` chains (statement and expression form) lowered to mux trees with
  *     sequential-assignment semantics per scope
  *   - hierarchy: per-instance elaboration of sub-design instances over their (shared) sub-DBs;
  *     port connections through PortByNameSelect, including partial (bit/range) sinks
  *   - top-level IN ports become pokeable hold-state cells; init applies at time zero (no reset
  *     modeling); design params resolve per instance when constant
  *
  * Known minimum limitations: dynamic shift amounts, processes/domains, `.reg`/`.prev` aliases,
  * assignment to partial targets, and per-instance param-dependent *widths* (widths resolve via the
  * sub-DB's canonical instance).
  */
object DFacsimile:
  def simulate(db: DB, tier: SimTier = SimTier.Interpreter): Sim =
    val builder = new Builder(db)
    builder.build()
    val kernel = tier match
      case SimTier.Interpreter => Interpreter.compile(builder.nl)
      case SimTier.Codegen     =>
        // named values are peekable — force them into the signal array
        Codegen.compile(builder.nl, observed = builder.namedNodes.values.toSet)
    new Sim(builder.nl, kernel, builder.namedNodes.toMap)
end DFacsimile

/** A running simulation instance: one state/signal array + a kernel over it. Values are addressed
  * by name; hierarchy paths use instance names (e.g. "alu0.res").
  */
final class Sim private[sim] (
    val nl: Netlist,
    kernel: SimKernel,
    nameToNode: Map[String, Int]
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
  private def nodeOf(name: String): Int =
    nameToNode.getOrElse(
      name,
      throw new NoSuchElementException(
        s"no named value: $name\navailable: ${nameToNode.keySet.toList.sorted.mkString(", ")}"
      )
    )
  def peek(name: String): Long =
    if needsSettle then settle()
    sig(nodeOf(name))
  def poke(name: String, value: Long): Unit =
    val node = nodeOf(name)
    sig(node) = value & nl.maskOf(node)
    needsSettle = true
  def widthOf(name: String): Int = nl.widthOf(nodeOf(name))
  def names: Set[String] = nameToNode.keySet
end Sim

private final class Builder(rawDB: DB):
  private[sim] val nl = new Netlist
  private[sim] val namedNodes = mutable.Map.empty[String, Int]
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

    private val nodeOf = mutable.Map.empty[DFVal, Int]
    private val regNodeOf = mutable.Map.empty[DFVal.Dcl, Int]
    private val inPortMov = mutable.Map.empty[DFVal.Dcl, Int]
    // sequential current-value: wires = current driven value; REG dcls = pending din
    private val env = mutable.Map.empty[DFVal.Dcl, Int]
    private val partialDrivers = mutable.Map.empty[DFVal.Dcl, mutable.ArrayBuffer[(Int, Int, Int)]]
    private val childScopes = mutable.Map.empty[DFDesignInst, Scope]
    // net sink values (raw, pre-dealias) — skipped as reads during the walk
    private val netSinkOf = mutable.Map.empty[DFNet, DFVal]

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
      // pre-pass: state cells — registers, and IN ports (pokeable hold cells at top,
      // MOV placeholders patched by the parent's connections otherwise)
      designMembers.foreach {
        case dcl: DFVal.Dcl if dcl.modifier.isReg =>
          val init = dcl.initRefList.headOption match
            case Some(initRef) => constLongOf(initRef.get)
            case None          => 0L
          regNodeOf(dcl) = nl.reg(widthOf(dcl), init)
        case dcl: DFVal.Dcl =>
          dcl.modifier.dir match
            case DFVal.Modifier.Dir.IN =>
              if isTop then regNodeOf(dcl) = nl.reg(widthOf(dcl), 0L)
              else inPortMov(dcl) = nl.mov(widthOf(dcl))
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
      case v: DFVal if netSinkOf.valuesIterator.contains(v) => () // write-only view of a sink
      case v: DFVal if isConstVector(v) => () // ROM data, materialized at its use site
      case v: DFVal                     =>
        tryFoldConst(v) match
          case Some(node) => bindVal(v, node)
          case None       =>
            v match
              case pbns: DFVal.PortByNameSelect => bindVal(pbns, pbnsReadNode(pbns))
              case f: DFVal.Func                => bindVal(f, buildFunc(f))
              case a: DFVal.Alias.AsIs          =>
                bindVal(a, nl.resize(readNode(a.relValRef.get), widthOf(a)))
              case a: DFVal.Alias.ApplyIdx   => bindVal(a, buildApplyIdx(a))
              case a: DFVal.Alias.ApplyRange => bindVal(a, buildApplyRange(a))
              case c: DFVal.Const            =>
                // reached only for bubble (don't-care) constants (`?`) — simulate as 0 (2-state)
                bindVal(c, nl.const(widthOf(c), lenientDataToLong(c.dfType, c.data, c)))
              case m => unsupported("value kind", m)
      case net: DFNet         => buildNet(net)
      case inst: DFDesignInst => elaborateChild(inst)
      case m                  => unsupported("member kind", m)
    }

    private def bindVal(v: DFVal, node: Int): Unit =
      nodeOf(v) = node
      if !v.isAnonymous then namedNodes(prefix + v.getName) = node

    // ---- reads ----------------------------------------------------------------------------

    private def readNode(v: DFVal): Int = v match
      case dcl: DFVal.Dcl =>
        regNodeOf.get(dcl).orElse(env.get(dcl)).orElse(inPortMov.get(dcl))
          .getOrElse(unsupported("reading a value before it is driven", dcl))
      case v =>
        nodeOf.getOrElse(v, unsupported("reading a value before it is built", v))

    /** Const resolution for simulation: the NoCache policy recomputes through design params, immune
      * to previously-cached symbolic (Always-policy) results.
      */
    private def constOpt[T](v: DFVal): Option[T] =
      v.getConstData[T](using summon[MemberGetSet], ConstData.CachePolicy.NoCache).toOption

    private def constDataOf(v: DFVal): Any =
      constOpt[Any](v).getOrElse(unsupported("non-constant data here", v))

    /** Constant value as a Long. Falls through value-preserving casts whose data-level fold fails
      * on param-dependent widths (the caller masks to the target width).
      */
    private def constLongOf(v: DFVal): Long =
      constOpt[Any](v).flatMap(dataToLongOpt(v.dfType, _)) match
        case Some(value) => value
        case None        =>
          v match
            case a: DFVal.Alias.AsIs => constLongOf(a.relValRef.get)
            case _                   => unsupported("non-constant data here", v)

    private def constIntOf(v: DFVal): Int =
      constOpt[Option[BigInt]](v) match
        case Some(Some(i)) => i.toInt
        case _             => unsupported("non-constant index/amount", v)

    // ---- value builders -------------------------------------------------------------------

    private def isConstVector(v: DFVal): Boolean =
      v.dfType.isInstanceOf[DFVector] && v.isConst

    private def tryFoldConst(v: DFVal): Option[Int] = v.dfType match
      case t @ (_: DFBits | _: DFDecimal | _: DFEnum | _: DFBoolOrBit) =>
        for
          data <- constOpt[Any](v)
          value <- dataToLongOpt(t, data)
        yield nl.const(widthOf(v), value)
      case _ => None

    private def buildFunc(f: DFVal.Func): Int =
      import DFVal.Func.Op as FO
      val args = f.args.map(_.get)
      def chain(op: (Int, Int) => Int): Int = args.map(readNode).reduce(op)
      def signedArgs: Boolean = args.head.dfType match
        case d: DFDecimal => d.signed
        case _            => false
      def lt(x: DFVal, y: DFVal): Int =
        if signedArgs then nl.slt(readNode(x), readNode(y)) else nl.ult(readNode(x), readNode(y))
      def constAmountOpt: Option[Int] = constOpt[Option[BigInt]](args(1)) match
        case Some(Some(v)) => Some(v.toInt)
        case _             => None
      val n = f.op match
        case FO.+         => chain(nl.add)
        case FO.-         => chain(nl.sub)
        case FO.^         => chain(nl.xor)
        case FO.&         => chain(nl.and)
        case FO.|         => chain(nl.or)
        case FO.++        => buildConcat(args)
        case FO.===       => nl.eq(readNode(args.head), readNode(args(1)))
        case FO.=!=       => nl.neq(readNode(args.head), readNode(args(1)))
        case FO.<         => lt(args.head, args(1))
        case FO.>         => lt(args(1), args.head)
        case FO.<=        => nl.not(lt(args(1), args.head))
        case FO.>=        => nl.not(lt(args.head, args(1)))
        case FO.`unary_~` => nl.not(readNode(args.head))
        case FO.`unary_!` => nl.not(readNode(args.head))
        case FO.sel       => nl.mux(readNode(args.head), readNode(args(1)), readNode(args(2)))
        case FO.<<        =>
          constAmountOpt match
            case Some(amt) => nl.shl(readNode(args.head), amt)
            case None      => nl.shlv(readNode(args.head), readNode(args(1)))
        case FO.>> =>
          if signedArgs then nl.srav(readNode(args.head), readNode(args(1)))
          else
            constAmountOpt match
              case Some(amt) => nl.shr(readNode(args.head), amt)
              case None      => nl.shrv(readNode(args.head), readNode(args(1)))
        case FO.ror => nl.rotr(readNode(args.head), constIntOf(args(1)))
        case FO.rol =>
          val a = readNode(args.head)
          nl.rotr(a, nl.widthOf(a) - constIntOf(args(1)))
        case op => unsupported(s"func op $op", f)
      if nl.widthOf(n) != widthOf(f) then unsupported("width-changing func result", f)
      n
    end buildFunc

    private def buildConcat(args: List[DFVal]): Int =
      // args.head holds the MSBs
      val totalWidth = args.map(widthOf).sum
      args.map(a => (readNode(a), widthOf(a))).reduceLeft { case ((accN, accW), (n, w)) =>
        (nl.or(nl.shl(nl.resize(accN, totalWidth), w), nl.resize(n, totalWidth)), accW + w)
      }(0)

    private def buildApplyIdx(a: DFVal.Alias.ApplyIdx): Int =
      val rel = a.relValRef.get
      rel.dfType match
        case vt: DFVector =>
          val data = constOpt[Vector[Any]](rel)
            .getOrElse(unsupported("dynamic indexing of a non-constant vector", a))
          val cellType = vt.cellType
          val cellWidth = cellType.widthUNSAFE
          if cellWidth > 64 then unsupported(s"ROM cell width $cellWidth", a)
          val table = data.map(cell => dataToLong(cellType, cell, a)).toArray
          nl.rom(table, cellWidth, readNode(a.relIdx.get))
        case _: DFBits =>
          nl.resize(nl.shr(readNode(rel), constIntOf(a.relIdx.get)), 1)
        case t => unsupported(s"indexing into $t", a)

    private def buildApplyRange(a: DFVal.Alias.ApplyRange): Int =
      val rel = a.relValRef.get
      val hi = a.idxHighRef.getIntOpt.getOrElse(unsupported("non-constant range", a))
      val lo = a.idxLowRef.getIntOpt.getOrElse(unsupported("non-constant range", a))
      nl.resize(nl.shr(readNode(rel), lo), hi - lo + 1)

    // ---- conditionals ---------------------------------------------------------------------

    private def processConditionalChain(header: DFConditional.Header): Unit =
      import DFConditional.DFCaseBlock.Pattern
      val blocks = db.conditionalChainTable.getOrElse(header, Nil)
      if blocks.isEmpty then unsupported("conditional header without blocks", header)
      val selectorNode = header match
        case mh: DFConditional.DFMatchHeader => Some(readNode(mh.selectorRef.get))
        case _                               => None
      def patternCond(p: Pattern): Int = p match
        case Pattern.Singleton(ref)    => nl.eq(selectorNode.get, readNode(ref.get))
        case Pattern.Alternative(list) => list.map(patternCond).reduce(nl.or)
        case p                         => unsupported(s"match pattern $p", header)
      val isExpr = header.dfType match
        case DFUnit => false
        case _      => true

      val baseEnv = env.toMap
      case class Branch(condOpt: Option[Int], resultEnv: Map[DFVal.Dcl, Int], yieldOpt: Option[Int])
      val branches = blocks.map { block =>
        env.clear(); env ++= baseEnv
        val guardCond = block.guardRef.get match
          case g: DFVal => Some(readNode(g))
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
        processMembers(blockMembers)
        val yieldOpt =
          if isExpr then
            blockMembers.lastOption match
              case Some(v: DFVal) => Some(readNode(v))
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
        val default: Option[Int] =
          if regNodeOf.contains(dcl) then Some(baseEnv.getOrElse(dcl, regNodeOf(dcl)))
          else baseEnv.get(dcl)
        val start: Option[Int] = elseBranch match
          case Some(b) => b.resultEnv.get(dcl).orElse(default)
          case None    => default
        val merged = condBranches.foldRight(start) { (b, acc) =>
          (b.resultEnv.get(dcl).orElse(default), acc) match
            case (Some(t), Some(f)) => Some(nl.mux(b.condOpt.get, t, f))
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
            case (Some(t), Some(f)) => Some(nl.mux(b.condOpt.get, t, f))
            case _                  => None
        }
        bindVal(header, merged.getOrElse(unsupported("unmergeable conditional expression", header)))
    end processConditionalChain

    // ---- nets & hierarchy -----------------------------------------------------------------

    private def buildNet(net: DFNet): Unit = net.op match
      case DFNet.Op.Assignment =>
        net.lhsRef.get match
          case dcl: DFVal.Dcl => env(dcl) = readNode(net.rhsRef.get)
          case other          => unsupported("assignment to a partial/alias target", net)
      case DFNet.Op.Connection | DFNet.Op.ViaConnection =>
        val sink = netSinkOf.getOrElse(net, unsupported("connection direction resolution", net))
        val src = if sink eq net.lhsRef.get then net.rhsRef.get else net.lhsRef.get
        connectSink(sink, readNode(src), net)
      case op => unsupported(s"net op $op", net)

    private def connectSink(sink: DFVal, srcNode: Int, net: DFNet): Unit = sink match
      case pbns: DFVal.PortByNameSelect =>
        val inst = pbns.designInstRef.get
        val child =
          childScopes.getOrElse(inst, unsupported("connection before instance elaboration", net))
        child.connectInPort(pbns.portNamePath, srcNode, net)
      case dcl: DFVal.Dcl =>
        if env.contains(dcl) || partialDrivers.contains(dcl) then
          unsupported("multiple drivers of a value", net)
        env(dcl) = srcNode
      case alias: DFVal.Alias =>
        val (dcl, hi, lo) = partialTarget(alias)
        if env.contains(dcl) then unsupported("mixed whole and partial drivers", net)
        partialDrivers.getOrElseUpdate(dcl, mutable.ArrayBuffer.empty) += ((hi, lo, srcNode))
      case other => unsupported("connection sink", other)

    private def partialTarget(alias: DFVal.Alias): (DFVal.Dcl, Int, Int) =
      def relDcl(rel: DFVal): DFVal.Dcl = rel match
        case dcl: DFVal.Dcl => dcl
        case other          => unsupported("nested partial connection target", alias)
      alias match
        case ai: DFVal.Alias.ApplyIdx if ai.relValRef.get.dfType.isInstanceOf[DFBits] =>
          val idx = constIntOf(ai.relIdx.get)
          (relDcl(ai.relValRef.get), idx, idx)
        case ar: DFVal.Alias.ApplyRange =>
          val hi = ar.idxHighRef.getIntOpt.getOrElse(unsupported("non-constant range", alias))
          val lo = ar.idxLowRef.getIntOpt.getOrElse(unsupported("non-constant range", alias))
          (relDcl(ar.relValRef.get), hi, lo)
        case other => unsupported("partial connection target", other)

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
      bindVal(p, nl.const(widthOf(p), dataToLong(p.dfType, data, p)))

    /** Resolve a child instance's param value in THIS (parent) scope's context. */
    private def paramDataOf(inst: DFDesignInst, name: String): Option[Any] =
      inst.paramMap.get(name).flatMap(ref => constOpt[Any](ref.get))

    /** Parent-side read of a child port (child is fully elaborated at this point). */
    private def pbnsReadNode(pbns: DFVal.PortByNameSelect): Int =
      val inst = pbns.designInstRef.get
      val child =
        childScopes.getOrElse(inst, unsupported("port select before instance elaboration", pbns))
      child.portReadNode(pbns.portNamePath, pbns)

    private def portByName(path: String, where: Any): DFVal.Dcl =
      if path.contains('.') then unsupported(s"nested port path '$path'", where)
      designMembers.collectFirst {
        case dcl: DFVal.Dcl if !dcl.isAnonymous && dcl.getName == path => dcl
      }.getOrElse(unsupported(s"port '$path' of design '${design.dclName}'", where))

    private def portReadNode(path: String, where: Any): Int =
      val dcl = portByName(path, where)
      regNodeOf.get(dcl).orElse(inPortMov.get(dcl)).orElse(env.get(dcl))
        .getOrElse(unsupported("reading an undriven port", dcl))

    private def connectInPort(path: String, srcNode: Int, where: Any): Unit =
      val dcl = portByName(path, where)
      val mov = inPortMov.getOrElse(dcl, unsupported("connection to a non-input port", dcl))
      nl.patchMov(mov, srcNode)

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
        val composed = sorted.map { (_, lo, n) =>
          if lo == 0 then nl.resize(n, w) else nl.shl(nl.resize(n, w), lo)
        }.reduce(nl.or)
        env(dcl) = composed
      // registers commit their pending value; unassigned registers (incl. top IN hold cells) hold
      for (dcl, regId) <- regNodeOf do nl.setNext(regId, env.getOrElse(dcl, regId))
      // names for state cells and driven wires
      designMembers.foreach {
        case dcl: DFVal.Dcl if !dcl.isAnonymous =>
          regNodeOf.get(dcl).orElse(env.get(dcl)).orElse(inPortMov.get(dcl)).foreach { n =>
            namedNodes(prefix + dcl.getName) = n
          }
        case _ =>
      }
    end finalizeScope

    // ---- data helpers ---------------------------------------------------------------------

    private def widthOf(v: DFVal): Int = widthOfType(v.dfType, v)
    private def widthOfType(t: DFType, where: Any): Int =
      val w = t.widthIntOpt.orElse(widthThroughParams(t))
        .getOrElse(unsupported("unresolvable (param-dependent) width", where))
      if w < 1 || w > 64 then unsupported(s"width $w (only 1..64)", where)
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
        case _ => None

    private def dataToLongOpt(dfType: DFType, data: Any): Option[Long] = dfType match
      case _: DFBits =>
        val (value, bubble) = data.asInstanceOf[(BitVector, BitVector)]
        if bubble.isZeros then Some(value.toLong(signed = false)) else None
      case _: DFDecimal | _: DFEnum =>
        data.asInstanceOf[Option[BigInt]].map(_.toLong)
      case _: DFBoolOrBit =>
        data.asInstanceOf[Option[Boolean]].map(v => if v then 1L else 0L)
      case _ => None

    private def dataToLong(dfType: DFType, data: Any, where: Any): Long =
      dataToLongOpt(dfType, data)
        .getOrElse(unsupported(s"constant data of type $dfType (or bubble data)", where))

    /** Like [[dataToLong]], but bubble (don't-care) data becomes 0 — 2-state minimum. */
    private def lenientDataToLong(dfType: DFType, data: Any, where: Any): Long = dfType match
      case _: DFBits =>
        val (value, bubble) = data.asInstanceOf[(BitVector, BitVector)]
        (value.toLong(signed = false)) & ~bubble.toLong(signed = false)
      case _: DFDecimal | _: DFEnum =>
        data.asInstanceOf[Option[BigInt]].map(_.toLong).getOrElse(0L)
      case _: DFBoolOrBit =>
        data.asInstanceOf[Option[Boolean]].map(v => if v then 1L else 0L).getOrElse(0L)
      case t => unsupported(s"constant of type $t", where)
  end Scope
end Builder
