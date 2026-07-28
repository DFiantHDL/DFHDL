package dfhdl.compiler.ir
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
    Calc(resolveDesignParams).linear(v)

  /** If `a - b` reduces to a constant (all symbolic terms cancel), returns it. */
  def constDiff(a: DFVal, b: DFVal, resolveDesignParams: Boolean)(using
      MemberGetSet
  ): Option[Int] =
    Calc(resolveDesignParams).constDiff(a, b)

  private object ConstInt:
    def unapply(v: DFVal): Option[Int] = v match
      case c: DFVal.Const =>
        c.data match
          case Some(i: BigInt) => Some(i.toInt)
          case _               => None
      case _ => None

  private final class Calc(resolveDesignParams: Boolean)(using MemberGetSet):
    // Strip type-preserving AsIs wrappers and, when `resolveDesignParams` is
    // enabled, DesignParams whose owner design has a parent (i.e., is not the
    // top design). For non-top designs, the parameter was provided by the
    // instantiating parent, so resolve it via `appliedOrDefaultVal`. Params on
    // a top design have no parent and stay opaque: they are the symbolic free
    // variables exposed to the user at elaboration time. Elaboration-time
    // folding (SimplifyFunc) disables the resolution so its decisions hold for
    // any parameter assignment and designs stay parametric.
    private def strip(v: DFVal): DFVal = v match
      case DFVal.Alias.AsIs(dfType = dt, relValRef = DFRef(relVal)) if dt == relVal.dfType =>
        strip(relVal)
      case dp: DFVal.DesignParam if resolveDesignParams && !dp.getOwnerDesign.isTop =>
        strip(dp.appliedOrDefaultVal)
      case _ => v

    // Ops whose operand order is irrelevant when comparing opaque bases.
    // Additive ops never reach the generic Func comparison (they are always
    // linearized) and products get dedicated factor-multiset handling.
    private val commutativeOps = Set(FuncOp.max, FuncOp.min, FuncOp.&, FuncOp.|, FuncOp.^)

    // Equivalence of opaque bases: same op/type Funcs with equivalent args
    // (each arg compared through its full linear form, so `clog2(2 * W)`
    // matches `clog2(W + W)`), or `=~` leaves after stripping. Commutative
    // ops compare their args as multisets, so `v1 * v2` matches `v2 * v1`.
    private def baseEq(a: DFVal, b: DFVal): Boolean =
      (strip(a), strip(b)) match
        case (af: DFVal.Func, bf: DFVal.Func) if af.op == bf.op && af.dfType =~ bf.dfType =>
          if (af.op == FuncOp.`*`)
            // Product bases: the constant factor is already carried by the
            // term coefficient (see `linear`), so only the non-constant
            // factor multisets must match, in any order.
            multisetEquiv(flattenProduct(af)._2, flattenProduct(bf)._2)
          else if (commutativeOps.contains(af.op))
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
    // non-constant factors, flattening nested products. A non-product part
    // whose linear form is a constant folds into the constant factor, and one
    // that is a single scaled term contributes its base with the scale folded
    // in, so `(W + W) * v` and `2 * W * v` normalize identically.
    private def flattenProduct(v: DFVal): (Int, List[DFVal]) = strip(v) match
      case DFVal.Func(op = FuncOp.`*`, args = args) =>
        args.map(_.get).foldLeft((1, List.empty[DFVal])) { case ((c, fs), arg) =>
          val (argC, argFs) = flattenProduct(arg)
          (c * argC, fs ++ argFs)
        }
      case sv =>
        linear(sv) match
          case Linear(Nil, k)          => (k, Nil)
          case Linear(List((k, b)), 0) => (k, List(b))
          case _                       => (1, List(sv))

    // Merge coefficients of equivalent bases and drop cancelled-out terms.
    private def canonical(terms: List[(Int, DFVal)]): List[(Int, DFVal)] =
      val merged = mutable.ListBuffer.empty[(Int, DFVal)]
      terms.foreach { (c, b) =>
        merged.indexWhere((_, mb) => baseEq(mb, b)) match
          case -1 => merged += ((c, b))
          case i  => merged.update(i, (merged(i)._1 + c, merged(i)._2))
      }
      merged.filter(_._1 != 0).toList

    private def add(l: Linear, r: Linear): Linear =
      Linear(canonical(l.terms ++ r.terms), l.offset + r.offset)
    private def negate(l: Linear): Linear =
      Linear(l.terms.map((c, b) => (-c, b)), -l.offset)
    private def scale(l: Linear, k: Int): Linear =
      if (k == 0) Linear(Nil, 0)
      else Linear(l.terms.map((c, b) => (c * k, b)), l.offset * k)
    // The symbolic parts of both linear forms cancel term-by-term.
    private def sameTerms(l: Linear, r: Linear): Boolean =
      canonical(l.terms ++ negate(r).terms).isEmpty

    def equivalent(a: DFVal, b: DFVal): Boolean =
      constDiff(a, b).contains(0)

    def constDiff(a: DFVal, b: DFVal): Option[Int] =
      val la = linear(a)
      val lb = linear(b)
      Option.when(sameTerms(la, lb))(la.offset - lb.offset)

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
        else Linear(List((1, sv)), 0)
      case sv => Linear(List((1, sv)), 0)
    end linear
  end Calc
end IntExprCalc
