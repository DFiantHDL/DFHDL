package dfhdl.compiler.ir
import scala.collection.mutable
import DFVal.Func.Op as FuncOp

/** Shared symbolic calculus for integer `DFVal` expressions.
  *
  * Every expression is decomposed into a linear combination `sum(coeff_i * base_i) + offset`, where
  * the bases are non-constant terms kept opaque (design params, ports, non-linear functions such as
  * `clog2`). Two expressions are equivalent when their bases cancel term-by-term and their offsets
  * match, so e.g. `2 * W` matches `W + W`, `(W + 5) - W` matches `5`, and `max(W, W + 1)` matches
  * `W + 1`.
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

    // Equivalence of opaque bases: same op/type Funcs with pairwise equivalent
    // args (each arg compared through its full linear form, so `clog2(2 * W)`
    // matches `clog2(W + W)`), or `=~` leaves after stripping.
    private def baseEq(a: DFVal, b: DFVal): Boolean =
      (strip(a), strip(b)) match
        case (af: DFVal.Func, bf: DFVal.Func) =>
          af.op == bf.op && af.dfType =~ bf.dfType &&
          af.args.length == bf.args.length &&
          af.args.lazyZip(bf.args).forall((ar, br) => equivalent(ar.get, br.get))
        case (sa, sb) => sa =~ sb

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
      case sv @ DFVal.Func(op = FuncOp.`*`, args = args) =>
        // A constant factor distributes over the other side's terms, so
        // `c * v` is equivalent to summing `c` copies of `v`. A product of
        // two non-constant parts stays a single opaque base.
        args.map(r => linear(r.get)).foldLeft(Option(Linear(Nil, 1))) {
          case (Some(acc), arg) =>
            if (acc.terms.isEmpty) Some(scale(arg, acc.offset))
            else if (arg.terms.isEmpty) Some(scale(acc, arg.offset))
            else None
          case (None, _) => None
        }.getOrElse(Linear(List((1, sv)), 0))
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
