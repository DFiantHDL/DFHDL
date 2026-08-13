package dfhdl.compiler.ir
import dfhdl.compiler.analysis.stripTypePreservingAliases
import scala.collection.mutable
import DFVal.Func.Op as FuncOp

/** Shared symbolic calculus for integer `DFVal` expressions.
  *
  * Every expression is decomposed into a linear combination `sum(coeff_i * base_i) + offset`, where
  * the bases are non-constant terms kept opaque (design params, ports, non-linear functions such as
  * `clog2`). Two expressions are equivalent when their bases cancel term-by-term and their offsets
  * match, so e.g. `2 * W` matches `W + W`, `(W + 5) - W` matches `5`, `v1 * v2` matches `v2 * v1`,
  * and `max(W, W + 1)` matches `W + 1`.
  *
  * Used by `IntParamRef.compare` for post-elaboration parametric width equivalence, and by the
  * core-library elaboration constant folding (`SimplifyFunc`) through [[constDiff]].
  */
object IntExprCalc:
  /** `terms.map((c, b) => c * b).sum + offset`, with pairwise-inequivalent bases and non-zero
    * coefficients. An empty `terms` list means the expression is the constant `offset`.
    */
  final case class Linear(terms: List[(Int, DFVal)], offset: Int)

  /** Decomposes `v` into its linear form. */
  def linearOf(v: DFVal, resolveDesignParams: Boolean)(using MemberGetSet): Linear =
    Calc(if (resolveDesignParams) ParamResolve.AppliedExpr else ParamResolve.Opaque).linear(v)

  /** If `a - b` reduces to a constant (all symbolic terms cancel), returns it.
    *
    * With `elimSymbolicMaxMin` enabled, a `max`/`min` whose operands are partly symbolic and partly
    * constant additionally reduces to its constant operands, ELIMINATING the symbolic dependency:
    * `max(W, 16)` reads as `16`. This is a deliberate semantic choice for width-fit decisions (a
    * comparison such as `16 >= max(W, 16)` then decides as `16 >= 16`), not an equivalence: never
    * enable it for equality/similarity queries (`=~`, `isSimilarTo`), where `max(W, 16)` and `16`
    * must stay distinct.
    */
  def constDiff(
      a: DFVal,
      b: DFVal,
      resolveDesignParams: Boolean,
      elimSymbolicMaxMin: Boolean = false
  )(using
      MemberGetSet
  ): Option[Int] =
    Calc(
      if (resolveDesignParams) ParamResolve.AppliedExpr else ParamResolve.Opaque,
      elimSymbolicMaxMin
    ).constDiff(a, b)

  /** Decides the width-fit acceptance `a >= b` between two width expressions. A constant difference
    * (with the max/min symbolic elimination of [[constDiff]]) decides directly; an undecidable
    * difference falls back to a non-negativity proof over the validity domain, using the fact that
    * both sides are widths and hence `>= 1` for every valid elaboration (`Arg.Width` rejects
    * non-positive widths). So `2 * W >= W` is accepted for a free parameter `W`, `W >= 2 * W` is
    * definitively rejected, and `16 >= W` stays undecidable (`W` may exceed 16). Width-fit check
    * sites only: like the max/min elimination, the proof rules must never back equality/similarity
    * queries (`=~`, `isSimilarTo`).
    */
  def widthFitCompare(a: DFVal, b: DFVal)(using MemberGetSet): Option[Boolean] =
    val calc = Calc(ParamResolve.AppliedExpr, elimSymbolicMaxMin = true)
    val la = calc.linear(a)
    val lb = calc.linear(b)
    val diff = calc.sub(la, lb)
    if (diff.terms.isEmpty) Some(diff.offset >= 0)
    else
      // both sides are widths: `>= 1` on the valid domain
      val facts = List(la, lb)
      if (calc.proveNonNeg(diff, facts)) Some(true)
      else
        // the negative direction: `b - a - 1 >= 0` proves `b > a`, deciding `a >= b` as false
        val negDiffM1 = Linear(diff.terms.map((c, b) => (-c, b)), -diff.offset - 1)
        if (calc.proveNonNeg(negDiffM1, facts)) Some(false)
        // last, so that it only ever turns an undecided answer into a decided one and never
        // overrides the max/min elimination above, which reads a mixed chain by its constants
        else if (calc.dominatesByBranch(a, b)) Some(true)
        else None
  end widthFitCompare

  /** The linear form of `a - b`, the canonical shape of a relation between two integer expressions:
    * `a >= b` is `linearDiff(a, b) >= 0`, and every other comparison normalizes onto the same
    * shape. So `W + W >= 8` and `2 * W >= 8` state one relation rather than two.
    *
    * Design parameters stay OPAQUE, which is what makes it a statement about the DESIGN: two
    * relations that coincide only for the values of one instantiation stay distinct.
    */
  def linearDiff(a: DFVal, b: DFVal)(using MemberGetSet): Linear =
    val calc = Calc(ParamResolve.Opaque)
    calc.sub(calc.linear(a), calc.linear(b))

  /** The constant `x - y`, or `None` when their symbolic terms do not cancel.
    *
    * This is what compares two relations in the [[linearDiff]] form: an answer at all means they
    * constrain the same expression, and `x - y >= 0` means `y >= 0` implies `x >= 0`, i.e. `y` is
    * the stronger of the two and `x` states nothing further.
    */
  def constOffsetDiff(x: Linear, y: Linear)(using MemberGetSet): Option[Int] =
    val d = Calc(ParamResolve.Opaque).sub(x, y)
    Option.when(d.terms.isEmpty)(d.offset)

  /** How the calculus treats a [[DFVal.DesignParam]] it reaches. */
  private enum ParamResolve derives CanEqual:
    /** Stays an opaque base, so a decision holds for any parameter assignment (elaboration-time
      * folding, `SimplifyFunc`).
      */
    case Opaque

    /** Substituted by the APPLIED value expression, and only where an instantiation actually
      * supplies one. A parameter with none stays an opaque base, so a decision made about it holds
      * for every assignment: while its own design is still elaborating there is no instance yet,
      * and the elaboration root never has one. The exception is a parameter the design read
      * (`ForcedParamTag`), which folds to the value it was read at, that design being specialized
      * to it and stating so. Used by width equivalence (`IntParamRef.compare`) and the width-fit
      * proof ([[widthFitCompare]]).
      */
    case AppliedExpr

    /** Folded to the applied constant DATA, resolved only through an instantiation site
      * (`DesignParam.instAppliedConstDataOpt`), which works under any getSet (elaboration-time
      * cached instance, hierarchical sub-DB walk-up, or flat DB). A parameter with no instantiation
      * site (the elaboration root's own parameters, overridable in the generated HDL) or one that
      * does not fold to a constant stays an opaque base, so any decision made with this mode holds
      * for every assignment of the root parameters. Used by the slice calculus ([[DataCalc]]).
      */
    case AppliedData
  end ParamResolve

  /** Linear calculus over slice bounds (parameter-dependent bit-range endpoints), used by
    * [[DFMember.departial]] and the connectivity slice-overlap analysis. See
    * [[ParamResolve.AppliedData]] for the design-parameter resolution semantics.
    */
  object DataCalc:
    private def calc(using MemberGetSet): Calc = Calc(ParamResolve.AppliedData)
    def const(i: Int): Linear = Linear(Nil, i)
    def isConst(l: Linear): Boolean = l.terms.isEmpty
    def linearOfVal(v: DFVal)(using MemberGetSet): Linear = calc.linear(v)
    def linearOfParamRef(ref: IntParamRef)(using MemberGetSet): Linear =
      calc.linearOfParamRef(ref)
    def add(a: Linear, b: Linear)(using MemberGetSet): Linear = calc.add(a, b)
    def sub(a: Linear, b: Linear)(using MemberGetSet): Linear = calc.add(a, negate(b))
    def negate(l: Linear): Linear = Linear(l.terms.map((c, b) => (-c, b)), -l.offset)
    def addConst(l: Linear, k: Int): Linear = l.copy(offset = l.offset + k)
    def scale(l: Linear, k: Int): Linear =
      if (k == 0) Linear(Nil, 0)
      else Linear(l.terms.map((c, b) => (c * k, b)), l.offset * k)

    /** Product of two linear forms; defined only when at least one side is a constant. */
    def mulOpt(a: Linear, b: Linear): Option[Linear] =
      if (a.terms.isEmpty) Some(scale(b, a.offset))
      else if (b.terms.isEmpty) Some(scale(a, b.offset))
      else None

    /** Total bit width of a type as a linear form, when expressible. */
    def linearOfTypeWidth(t: DFType)(using MemberGetSet): Option[Linear] =
      calc.linearOfTypeWidth(t)

    /** Proves `e >= 0` for every valid parameter assignment. Each fact in `facts` is a linear form
      * known to be `>= 1` on the valid domain (slice widths: a slice of zero or negative width is
      * never a valid elaboration). This covers the equal-bin pattern (`k*W`-based slices of width
      * `W`) at any distance. See [[Calc.proveNonNeg]] for the proof rules.
      */
    def proveNonNeg(e: Linear, facts: List[Linear])(using MemberGetSet): Boolean =
      calc.proveNonNeg(e, facts)

    /** Folds a linear form to a single integer by resolving every remaining opaque base through the
      * design-parameter chain, which includes an elaboration ROOT's own parameters via their
      * defaults (`AppliedData` deliberately keeps those symbolic). `None` when any base does not
      * resolve to an integer.
      *
      * A decision made on folded values holds for the parameters actually elaborated, NOT for every
      * HDL override, so this may only refine a legality verdict. It must never reach a
      * directionality decision: the flow of a connection is a structural property that has to hold
      * for every parameter assignment.
      */
    def foldConst(l: Linear)(using MemberGetSet): Option[Int] =
      l.terms.foldLeft(Option(l.offset)) { case (accOpt, (c, base)) =>
        accOpt.flatMap { acc =>
          base.getConstDataThroughParams[Option[BigInt]].flatten
            .filter(_.isValidInt).map(v => acc + c * v.toInt)
        }
      }
  end DataCalc

  private object ConstInt:
    def unapply(v: DFVal): Option[Int] = v match
      case c: DFVal.Const =>
        c.data match
          case Some(i: BigInt) => Some(i.toInt)
          case _               => None
      case _ => None

  private final class Calc(mode: ParamResolve, elimSymbolicMaxMin: Boolean = false)(using
      getSet: MemberGetSet
  ):
    // Strip type-preserving AsIs wrappers and, under `AppliedExpr`, DesignParams that an
    // instantiation actually supplies a value for.
    //
    // A parameter's DEFAULT is never that value. It is what the parameter is when nothing says
    // otherwise, and substituting it decides a relation on a value the design may well not
    // have: the generated module keeps the parameter overridable, from a DFHDL parent or from
    // hand-written HDL, so a decision about it either holds symbolically or is not a decision
    // about the design at all. So a parameter with no applied value stays an opaque base, which
    // covers both the design that is still elaborating its own body (no instance exists yet) and
    // the elaboration root (whose parameters are the free variables of the compilation).
    //
    // Elaboration-time folding (SimplifyFunc) disables the resolution entirely (`Opaque`) so its
    // decisions hold for any assignment and designs stay parametric. `AppliedData` resolves in
    // `linear` at the data level instead (see ParamResolve).
    private def strip(v: DFVal): DFVal = v.stripTypePreservingAliases match
      case dp: DFVal.DesignParam
          if mode == ParamResolve.AppliedExpr && !dp.getOwnerDesign.isTop =>
        dp.appliedValOpt.map(strip).getOrElse(dp)
      case stripped => stripped

    // Ops whose operand order is irrelevant when comparing opaque bases.
    // Additive ops never reach the generic Func comparison (they are always
    // linearized) and products get dedicated factor-multiset handling.
    private val commutativeOps = Set(FuncOp.max, FuncOp.min, FuncOp.&, FuncOp.|, FuncOp.^)

    // The non-constant factor multiset of a product-like base: a `*` chain, or a
    // width/length query whose queried width is a pure product of parameters. The constant
    // factor is already carried by the term coefficient (see `linear`), so bases match by
    // their non-constant factors only.
    private def productFactorsOpt(f: DFVal.Func): Option[List[DFVal]] = f.op match
      case FuncOp.`*`                   => Some(flattenProduct(f)._2)
      case FuncOp.width | FuncOp.length => widthQueryFactors(f).filter(_._2.nonEmpty).map(_._2)
      case _                            => None

    // Equivalence of opaque bases: same op/type Funcs with equivalent args
    // (each arg compared through its full linear form, so `clog2(2 * W)`
    // matches `clog2(W + W)`), or `=~` leaves after stripping. Commutative
    // ops compare their args as multisets, so `v1 * v2` matches `v2 * v1`.
    def baseEq(a: DFVal, b: DFVal): Boolean =
      (strip(a), strip(b)) match
        // Product-like bases match by their non-constant factor multisets, in any order
        // and across the two shapes, so `vec.width` matches `W * N`.
        case (af: DFVal.Func, bf: DFVal.Func)
            if af.dfType =~ bf.dfType &&
              productFactorsOpt(af).nonEmpty && productFactorsOpt(bf).nonEmpty =>
          multisetEquiv(productFactorsOpt(af).get, productFactorsOpt(bf).get)
        case (af: DFVal.Func, bf: DFVal.Func) if af.op == bf.op && af.dfType =~ bf.dfType =>
          if (commutativeOps.contains(af.op))
            af.args.length == bf.args.length &&
            multisetEquiv(af.args.map(_.get), bf.args.map(_.get))
          else
            af.args.length == bf.args.length &&
            af.args.lazyZip(bf.args).forall((ar, br) => equivalent(ar.get, br.get))
        case (sa, sb) => sa =~ sb

    // Multiset equality of DFVal lists under `equivalent` (order-insensitive).
    private def multisetEquiv(lhs: List[DFVal], rhs: List[DFVal]): Boolean =
      lhs.length == rhs.length && {
        val remaining = mutable.ListBuffer.from(rhs)
        lhs.forall { l =>
          remaining.indexWhere(equivalent(l, _)) match
            case -1 => false
            case i  => remaining.remove(i); true
        }
      }

    // Splits a product into its overall constant factor and the list of
    // non-constant factors, flattening nested products. A width/length query over a
    // pure-product width expands into the width parameters' factors, so `vec.width * 2`
    // and `W * N * 2` normalize identically. A non-product part
    // whose linear form is a constant folds into the constant factor, and one
    // that is a single scaled term contributes its base with the scale folded
    // in, so `(W + W) * v` and `2 * W * v` normalize identically.
    private def flattenProduct(v: DFVal): (Int, List[DFVal]) = strip(v) match
      case DFVal.Func(op = FuncOp.`*`, args = args) =>
        args.map(_.get).foldLeft((1, List.empty[DFVal])) { case ((c, fs), arg) =>
          val (argC, argFs) = flattenProduct(arg)
          (c * argC, fs ++ argFs)
        }
      case sv @ DFVal.Func(op = FuncOp.width | FuncOp.length) if widthQueryFactors(sv).nonEmpty =>
        widthQueryFactors(sv).get
      case sv =>
        linear(sv) match
          case Linear(Nil, k)          => (k, Nil)
          case Linear(List((k, b)), 0) => (k, List(b))
          case _                       => (1, List(sv))

    // The multiplicative decomposition (constant factor, non-constant factor values) of a
    // width/length type query whose queried width is a pure product of parameters, e.g. a
    // vector of parametric cells: `vec.width` then normalizes exactly like `W * N`. `None`
    // for a width with additive structure (fixed-point, structs) or no decomposition at all.
    private def widthQueryFactors(v: DFVal): Option[(Int, List[DFVal])] =
      def paramRefFactors(ref: IntParamRef): (Int, List[DFVal]) =
        ref.getRef match
          case Some(typeRef) => flattenProduct(typeRef.get)
          case None          => (ref.getIntUNSAFE, Nil)
      def typeFactors(t: DFType): Option[(Int, List[DFVal])] = t match
        case _ if t.getRefs.isEmpty      => t.widthIntOpt.map((_, Nil))
        case DFBits(widthParamRef)       => Some(paramRefFactors(widthParamRef))
        case DFXInt(_, widthParamRef, _) => Some(paramRefFactors(widthParamRef))
        case vec: DFVector               =>
          vec.cellDimParamRefs.foldLeft(typeFactors(vec.cellType)) { (accOpt, dim) =>
            accOpt.map { (c, fs) =>
              val (dimC, dimFs) = paramRefFactors(dim)
              (c * dimC, fs ++ dimFs)
            }
          }
        case opaque: DFOpaque => typeFactors(opaque.actualType)
        case _                => None
      v match
        case DFVal.Func(op = op @ (FuncOp.width | FuncOp.length), args = List(argRef)) =>
          val argType = argRef.get.dfType
          (op, argType) match
            case (FuncOp.length, vec: DFVector) => Some(paramRefFactors(vec.cellDimParamRefs.head))
            case _                              => typeFactors(argType)
        case _ => None
    end widthQueryFactors

    // Merge coefficients of equivalent bases and drop cancelled-out terms.
    private def canonical(terms: List[(Int, DFVal)]): List[(Int, DFVal)] =
      val merged = mutable.ListBuffer.empty[(Int, DFVal)]
      terms.foreach { (c, b) =>
        merged.indexWhere((_, mb) => baseEq(mb, b)) match
          case -1 => merged += ((c, b))
          case i  => merged.update(i, (merged(i)._1 + c, merged(i)._2))
      }
      merged.filter(_._1 != 0).toList

    def add(l: Linear, r: Linear): Linear =
      Linear(canonical(l.terms ++ r.terms), l.offset + r.offset)
    private def negate(l: Linear): Linear =
      Linear(l.terms.map((c, b) => (-c, b)), -l.offset)
    private def scale(l: Linear, k: Int): Linear =
      if (k == 0) Linear(Nil, 0)
      else Linear(l.terms.map((c, b) => (c * k, b)), l.offset * k)
    // The symbolic parts of both linear forms cancel term-by-term.
    private def sameTerms(l: Linear, r: Linear): Boolean =
      canonical(l.terms ++ negate(r).terms).isEmpty

    def sub(l: Linear, r: Linear): Linear = add(l, negate(r))

    def linearOfParamRef(ref: IntParamRef): Linear =
      ref.getRef match
        case Some(typeRef) => linear(typeRef.get)
        case None          => Linear(Nil, ref.getIntUNSAFE)

    /** Total bit width of a type as a linear form (in this calc's own parameter-resolution mode),
      * when expressible. A type carrying width-parameter refs decomposes through its structure so
      * the refs resolve in this calc's mode; the `widthIntOpt` shortcut applies only to ref-free
      * types, since its internal resolution (the default `Always` policy) folds a TOP design's
      * parameters through their default values, which no calc mode does.
      */
    def linearOfTypeWidth(t: DFType): Option[Linear] =
      t match
        case _ if t.getRefs.isEmpty => t.widthIntOpt.map(Linear(Nil, _))
        case DFBits(widthParamRef)  => Some(linearOfParamRef(widthParamRef))
        case dec: DFDecimal         =>
          Some(DataCalc.addConst(linearOfParamRef(dec.magnitudeWidthParamRef), dec.fractionWidth))
        case vec: DFVector =>
          vec.cellDimParamRefs.foldLeft(linearOfTypeWidth(vec.cellType)) { (accOpt, dim) =>
            accOpt.flatMap(DataCalc.mulOpt(_, linearOfParamRef(dim)))
          }
        case opaque: DFOpaque => linearOfTypeWidth(opaque.actualType)
        case _                => t.widthIntOpt.map(Linear(Nil, _))

    def equivalent(a: DFVal, b: DFVal): Boolean =
      constDiff(a, b).contains(0)

    def constDiff(a: DFVal, b: DFVal): Option[Int] =
      val la = linear(a)
      val lb = linear(b)
      Option.when(sameTerms(la, lb))(la.offset - lb.offset)

    /** Whether `a >= b` holds by CONSTRUCTION rather than by arithmetic: a `max` is at least each
      * of its own branches and a `min` at most each of its, whatever those branches are. That
      * decides a comparison the linear calculus cannot touch, two unrelated symbolic branches never
      * cancelling under subtraction, and it is what makes a value resized to a common width and
      * back again recover itself.
      *
      * Only these two orientations. The mirrored ones (`b >= max(b, c)`) genuinely depend on the
      * other branch and stay undecided, which is exactly the assumption a design states.
      */
    def dominatesByBranch(a: DFVal, b: DFVal): Boolean =
      def hasBranch(chain: DFVal, branch: DFVal, op: FuncOp): Boolean =
        strip(chain) match
          case f: DFVal.Func if f.op == op => f.args.exists(r => strip(r.get) =~ strip(branch))
          case _                           => false
      hasBranch(a, b, FuncOp.max) || hasBranch(b, a, FuncOp.min)

    /** Proves `e >= 0` for every valid parameter assignment, where each fact in `facts` is a linear
      * form known to be `>= 1` on the valid domain. Two proof rules: a constant `e` decides
      * directly, and a single-fact proportional bound: if `e == λ*f + c` with rational `λ >= 0`,
      * then `e >= λ*1 + c`, so `λ + c >= 0` proves it. The proof runs in this calc's own
      * linearization mode, so `e` and the facts must be produced by the same calc.
      */
    def proveNonNeg(e: Linear, facts: List[Linear]): Boolean =
      if (e.terms.isEmpty) e.offset >= 0
      else
        facts.exists { f =>
          f.terms.nonEmpty && f.terms.length == e.terms.length && {
            // pair each e-term with its baseEq f-term and derive λ = p/q from the first pair
            val paired = e.terms.map { (ce, be) =>
              f.terms.collectFirst { case (cf, bf) if baseEq(be, bf) => (ce, cf) }
            }
            paired.forall(_.nonEmpty) && {
              val pairs = paired.flatten
              val (p0, q0) = pairs.head
              // normalize the denominator positive; λ >= 0 then requires p >= 0
              val (p, q) = if (q0 < 0) (-p0, -q0) else (p0, q0)
              p >= 0 &&
              pairs.forall((ce, cf) => ce * q == cf * p) &&
              // λ + c >= 0 with c = e.offset - λ*f.offset, scaled by q > 0
              p + q * e.offset - p * f.offset >= 0
            }
          }
        }

    def linear(v: DFVal): Linear = strip(v) match
      case ConstInt(i)                            => Linear(Nil, i)
      case DFVal.Func(op = FuncOp.+, args = args) =>
        args.map(r => linear(r.get)).foldLeft(Linear(Nil, 0))(add)
      case DFVal.Func(op = FuncOp.-, args = List(aRef, bRef)) =>
        add(linear(aRef.get), negate(linear(bRef.get)))
      case DFVal.Func(op = FuncOp.unary_-, args = List(aRef)) =>
        negate(linear(aRef.get))
      case sv @ DFVal.Func(op = FuncOp.`*`) =>
        // The constant factor distributes over a single non-constant part's
        // terms, so `c * v` is equivalent to summing `c` copies of `v`. Two
        // or more non-constant factors stay a single opaque product base
        // whose constant factor is carried by the term coefficient (`baseEq`
        // then matches product bases by their factor multisets).
        val (c, fs) = flattenProduct(sv)
        fs match
          case Nil         => Linear(Nil, c)
          case f :: Nil    => scale(linear(f), c)
          case _ if c == 0 => Linear(Nil, 0)
          case _           => Linear(List((c, sv)), 0)
      // A parameter whose data the design's elaboration READS is fixed at that value for this
      // design (see `DesignParam.isDataImpure`): the body is the one that value produced, and
      // the design states the equality as a static assertion, so every instantiation is held to
      // it. This is the one case where a parameter with no instantiation site still folds, the
      // elaboration root included: the assertion travels with the generated module and is
      // checked wherever it is instantiated from.
      case dp: DFVal.DesignParam if mode == ParamResolve.AppliedExpr && dp.isDataImpure =>
        dp.getConstData[Any](using getSet, ConstData.CachePolicy.GoThroughDesignParams) match
          case ConstData.KnownConst(Some(i: BigInt)) if i.isValidInt => Linear(Nil, i.toInt)
          case _                                                     => Linear(List((1, dp)), 0)
      // AppliedData: fold a design parameter to its applied constant data, resolved only
      // through an instantiation site, so an elaboration root's parameters (which have none)
      // and anything else unresolvable stay opaque bases
      case dp: DFVal.DesignParam if mode == ParamResolve.AppliedData =>
        dp.instAppliedConstDataOpt(using getSet, ConstData.CachePolicy.NoCache) match
          case Some(ConstData.KnownConst(Some(i: BigInt))) if i.isValidInt => Linear(Nil, i.toInt)
          case _ => Linear(List((1, dp)), 0)
      case sv @ DFVal.Func(op = op @ (FuncOp.max | FuncOp.min), args = args) =>
        // max/min reduce to a single linear form when all operands share the
        // same symbolic terms and differ only by their constant offsets:
        //   max(a, a + c) => (a + c) if c > 0, else a
        //   min(a, a + c) => a if c > 0, else (a + c)
        // (in particular max(a, a) == min(a, a) == a), in any operand order.
        val linears = args.map(r => linear(r.get))
        val head = linears.head
        if (linears.tail.forall(sameTerms(_, head)))
          val offsets = linears.map(_.offset)
          Linear(head.terms, if (op == FuncOp.max) offsets.max else offsets.min)
        // symbolic elimination (see `constDiff`): a mixed max/min reduces to its constant
        // operands, dropping the symbolic ones, so e.g. `max(W, 16)` reads as `16`
        else if (elimSymbolicMaxMin && linears.exists(_.terms.isEmpty))
          val offsets = linears.collect { case Linear(Nil, k) => k }
          Linear(Nil, if (op == FuncOp.max) offsets.max else offsets.min)
        else Linear(List((1, sv)), 0)
      // a width/length type query decomposes through the argument type's width parameters,
      // so `Bits(W * 3).width` linearizes as `3 * W` and a literal width as its constant; a
      // pure-product width over several parameters (a vector of parametric cells) keeps an
      // opaque base whose product factors match the equivalent `*` expression (see `baseEq`)
      case sv @ DFVal.Func(op = op @ (FuncOp.width | FuncOp.length), args = List(argRef)) =>
        val argType = argRef.get.dfType
        val additiveOpt = (op, argType) match
          case (FuncOp.length, vec: DFVector) => Some(linearOfParamRef(vec.cellDimParamRefs.head))
          case _                              => linearOfTypeWidth(argType)
        additiveOpt.orElse {
          widthQueryFactors(sv).map { (c, fs) =>
            fs match
              case Nil         => Linear(Nil, c)
              case f :: Nil    => scale(linear(f), c)
              case _ if c == 0 => Linear(Nil, 0)
              case _           => Linear(List((c, sv)), 0)
          }
        }.getOrElse(Linear(List((1, sv)), 0))
      case sv => Linear(List((1, sv)), 0)
    end linear
  end Calc
end IntExprCalc
