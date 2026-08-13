package dfhdl.compiler.ir

import scala.collection.immutable

/** A write region on a DFVal. Used by assignment / connection coverage tracking and by
  * [[DFMember.departial]] to describe which bits of the underlying declaration an alias chain
  * touches.
  *
  * Parameter-dependent endpoints are kept as [[Slice.Symbolic]] linear forms (see
  * [[IntExprCalc.DataCalc]]), so provably-disjoint parametric slices are recognized as such;
  * [[Slice.Unknown]] remains the conservative fallback when the bounds cannot be linearized.
  */
enum Slice derives CanEqual:
  /** A concrete bit range in the root value's coordinates. */
  case Concrete(range: Range)

  /** A bit range `[lo, lo + width)` whose endpoints are linear forms over unresolved (top-design)
    * parameters, in the root value's coordinates. Constructed only via [[Slice.symbolic]], so at
    * least one of the two forms is non-constant.
    */
  case Symbolic(lo: IntExprCalc.Linear, width: IntExprCalc.Linear)

  /** The entire value. Used when the value's width itself is symbolic. */
  case Full

  /** A slice whose endpoints are symbolic and could not be resolved. */
  case Unknown
end Slice

object Slice:
  def fromRange(range: Range): Slice = Concrete(range)

  /** Build a slice covering all bits of a value whose width may be symbolic. */
  def fromWidthOpt(widthOpt: Option[Int]): Slice = widthOpt match
    case Some(w) => Concrete(0 until w)
    case None    => Full

  /** Build a symbolic slice, collapsing to [[Concrete]] when both forms are constant. */
  def symbolic(lo: IntExprCalc.Linear, width: IntExprCalc.Linear): Slice =
    if (lo.terms.isEmpty && width.terms.isEmpty)
      Concrete(lo.offset until lo.offset + width.offset)
    else Symbolic(lo, width)

  /** Map an outer selection `outer` (relative to an alias whose selected region starts at bit
    * `loBits` of the relative value and spans `selWidthBits` bits) into the relative value's
    * coordinates.
    */
  def compose(outer: Slice, loBits: IntExprCalc.Linear, selWidthBits: IntExprCalc.Linear)(using
      MemberGetSet
  ): Slice =
    import IntExprCalc.DataCalc.{add, const}
    outer match
      case Concrete(r)     => symbolic(add(loBits, const(r.start)), const(r.length))
      case Symbolic(lo, w) => symbolic(add(lo, loBits), w)
      case Full            => symbolic(loBits, selWidthBits)
      case Unknown         => Unknown

  extension (slice: Slice)
    /** Shift the slice by a (concrete) delta in bit positions. Unknown/Full stay themselves —
      * shifting an unknown slice is still unknown.
      */
    def shift(delta: Int): Slice = slice match
      case Concrete(r)     => Concrete(Range(r.start + delta, r.end + delta))
      case Symbolic(lo, w) => Symbolic(lo.copy(offset = lo.offset + delta), w)
      case other           => other
    def isEmpty: Boolean = slice match
      case Concrete(r)    => r.isEmpty
      case Symbolic(_, w) => w.terms.isEmpty && w.offset <= 0
      case _              => false

    /** Does this slice cover the full width of the underlying value? `Tri.Unknown` when either the
      * slice or the width is symbolic.
      */
    def isFullOf(widthOpt: Option[Int]): Tri = slice match
      case Full        => Tri.Yes
      case Unknown     => Tri.Unknown
      case _: Symbolic => Tri.Unknown
      case Concrete(r) =>
        widthOpt match
          case Some(w) => if (r.start == 0 && r.end == w) Tri.Yes else Tri.No
          case None    => Tri.Unknown
  end extension
end Slice

/** Tri-state result for [[Coverage]] queries. `Unknown` is returned when a symbolic slice was seen
  * and we cannot prove either Yes or No.
  */
enum Tri derives CanEqual:
  case Yes, No, Unknown

object Tri:
  def fromBool(b: Boolean): Tri = if (b) Yes else No

/** Accumulated write coverage over one DFVal.
  *
  *   - `bits` holds the concretely-tracked bit positions that are proven assigned/connected.
  *   - `symbolics` holds the written regions whose endpoints are parameter-dependent, kept as
  *     linear forms so a query can still be decided over them (see [[Coverage.contains]]).
  *   - `unknownTouched` is set when a write with a [[Slice.Unknown]], or a [[Slice.Full]] over an
  *     unknown width, has been observed, meaning we know the value was touched but not precisely
  *     where. A symbolic write past [[Coverage.maxSymbolicRegions]] degrades into it too.
  *   - `fullyCovered` is a latch flag set when we observe a write that covers the entire value,
  *     even if the value's width is symbolic (so we cannot represent it as a concrete BitSet). Once
  *     set, any coverage query returns `Yes` regardless of `bits`.
  */
final case class Coverage(
    bits: immutable.BitSet,
    symbolics: List[Slice.Symbolic],
    unknownTouched: Boolean,
    fullyCovered: Boolean
) derives CanEqual:
  def |(that: Coverage): Coverage =
    Coverage(
      bits | that.bits,
      Nil,
      unknownTouched || that.unknownTouched,
      fullyCovered || that.fullyCovered
    ).withSymbolics(symbolics ++ that.symbolics)

  /** Intersection, used to merge what all branches of a conditional assign. The symbolic half is an
    * UNDER-approximation (only a region both sides carry survives), which can only weaken a
    * containment proof, never strengthen one.
    */
  def &(that: Coverage)(using MemberGetSet): Coverage =
    Coverage(
      bits & that.bits,
      symbolics.filter(a => that.symbolics.exists(Coverage.sameRegion(a, _))),
      unknownTouched && that.unknownTouched,
      fullyCovered && that.fullyCovered
    )

  // Symbolic regions are kept as a bounded list, past which they all degrade into
  // `unknownTouched`: dropping regions can only lose a proof, and it keeps the containment
  // search below bounded.
  private def withSymbolics(all: List[Slice.Symbolic]): Coverage =
    if (all.sizeIs > Coverage.maxSymbolicRegions) copy(symbolics = Nil, unknownTouched = true)
    else copy(symbolics = all)

  def assign(slice: Slice, widthOpt: Option[Int]): Coverage =
    slice match
      case Slice.Concrete(r) =>
        copy(bits = bits ++ immutable.BitSet.fromSpecific(r))
      case Slice.Full =>
        widthOpt match
          case Some(w) => copy(bits = bits ++ immutable.BitSet.fromSpecific(0 until w))
          case None    => copy(fullyCovered = true)
      case s: Slice.Symbolic => withSymbolics(symbolics :+ s)
      // an unknown slice has nothing to track, so it degrades to "touched"
      case Slice.Unknown => copy(unknownTouched = true)

  /** Does this coverage touch any bit of `slice`? */
  def overlaps(slice: Slice, widthOpt: Option[Int]): Tri =
    if (fullyCovered)
      slice match
        case Slice.Concrete(r) if r.isEmpty => Tri.No
        case _                              => Tri.Yes
    else
      val maybe = unknownTouched || symbolics.nonEmpty
      slice match
        case Slice.Concrete(r) =>
          val sliceBits = immutable.BitSet.fromSpecific(r)
          if ((bits & sliceBits).nonEmpty) Tri.Yes
          else if (maybe) Tri.Unknown
          else Tri.No
        case Slice.Full =>
          if (bits.nonEmpty) Tri.Yes
          else if (maybe) Tri.Unknown
          else Tri.No
        case _: Slice.Symbolic | Slice.Unknown =>
          if (bits.nonEmpty || maybe) Tri.Unknown
          else Tri.No
    end if
  end overlaps

  /** Does this coverage fully cover `slice`? `Tri.Yes` only when proven, so a query the symbolic
    * proofs cannot decide answers `Tri.Unknown` rather than `Tri.No`.
    */
  def contains(slice: Slice, widthOpt: Option[Int])(using MemberGetSet): Tri =
    import IntExprCalc.DataCalc.const
    if (fullyCovered) Tri.Yes
    else
      val proven = slice match
        case Slice.Concrete(r) =>
          r.isEmpty || (immutable.BitSet.fromSpecific(r) &~ bits).isEmpty ||
          proveCovered(const(r.start), const(r.length))
        case Slice.Full => coversWholeValue(widthOpt)
        // a slice is by construction within the value's own bounds, so a fully covered value
        // contains every slice of it, whatever its endpoints are
        case s: Slice.Symbolic => coversWholeValue(widthOpt) || proveCovered(s.lo, s.width)
        case Slice.Unknown     => coversWholeValue(widthOpt)
      if (proven) Tri.Yes
      else if (unknownTouched || symbolics.nonEmpty) Tri.Unknown
      else
        slice match
          // concrete coverage decides a concrete query outright
          case Slice.Concrete(_) | Slice.Full => Tri.No
          case _                              => Tri.Unknown
    end if
  end contains

  private def coversWholeValue(widthOpt: Option[Int])(using MemberGetSet): Boolean =
    import IntExprCalc.DataCalc.const
    widthOpt.exists { w =>
      (immutable.BitSet.fromSpecific(0 until w) &~ bits).isEmpty ||
      proveCovered(const(0), const(w))
    }

  /** Proof that every position of `[qLo, qLo + qW)` is written by some accumulated region, run as a
    * sweep: starting at `qLo`, extend the covered prefix by a region that provably starts at or
    * before the cursor and provably ends after it, until the prefix provably reaches the end of the
    * query. Each region's width is a `>= 1` fact for the inequality proofs (see
    * [[IntExprCalc.DataCalc.proveNonNeg]]), a slice of non-positive width never being a valid
    * elaboration.
    */
  private def proveCovered(qLo: IntExprCalc.Linear, qW: IntExprCalc.Linear)(using
      MemberGetSet
  ): Boolean =
    import IntExprCalc.DataCalc.*
    if (symbolics.isEmpty) false // a concrete-only coverage is already decided by the BitSet paths
    else
      val regions: Vector[(IntExprCalc.Linear, IntExprCalc.Linear)] =
        Coverage.bitRuns(bits).map(r => (const(r.start), const(r.length))).toVector ++
          symbolics.view.map(s => (s.lo, s.width))
      if (regions.sizeIs > Coverage.maxSymbolicRegions) false
      else
        val facts = qW :: regions.view.map(_._2).toList
        def nonNeg(e: IntExprCalc.Linear): Boolean = proveNonNeg(e, facts)
        def sweep(cursor: IntExprCalc.Linear, unused: Set[Int]): Boolean =
          // the query ends at or before the covered prefix
          nonNeg(sub(cursor, add(qLo, qW))) ||
            unused.exists { i =>
              val (lo, w) = regions(i)
              // starts at or before the cursor, and ends after it
              nonNeg(sub(cursor, lo)) && nonNeg(addConst(sub(add(lo, w), cursor), -1)) &&
              sweep(add(lo, w), unused - i)
            }
        sweep(qLo, regions.indices.toSet)
    end if
  end proveCovered

  /** Is this coverage full for the given (possibly unknown) width? */
  def isFull(widthOpt: Option[Int])(using MemberGetSet): Tri = contains(Slice.Full, widthOpt)

  def isEmpty: Boolean = bits.isEmpty && symbolics.isEmpty && !unknownTouched && !fullyCovered
  def nonEmpty: Boolean = !isEmpty
end Coverage

object Coverage:
  /** The bound on how many regions a containment proof sweeps over. Above it the coverage degrades
    * to `unknownTouched`, which loses proofs but never invents one.
    */
  private[ir] val maxSymbolicRegions: Int = 8

  val empty: Coverage =
    Coverage(immutable.BitSet.empty, Nil, unknownTouched = false, fullyCovered = false)
  def full(widthOpt: Option[Int]): Coverage =
    empty.assign(Slice.Full, widthOpt)

  /** The two regions are the same region for every parameter assignment. */
  private def sameRegion(a: Slice.Symbolic, b: Slice.Symbolic)(using MemberGetSet): Boolean =
    import IntExprCalc.DataCalc.*
    def same(x: IntExprCalc.Linear, y: IntExprCalc.Linear): Boolean =
      val d = sub(x, y)
      d.terms.isEmpty && d.offset == 0
    same(a.lo, b.lo) && same(a.width, b.width)

  /** The maximal contiguous runs of set bits. */
  private def bitRuns(bits: immutable.BitSet): List[Range] =
    bits.toList.foldLeft(List.empty[Range]) {
      case (run :: rest, bit) if bit == run.end => Range(run.start, run.end + 1) :: rest
      case (acc, bit)                           => Range(bit, bit + 1) :: acc
    }.reverse
end Coverage
