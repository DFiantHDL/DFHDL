package dfhdl.compiler.ir

import scala.collection.immutable

/** A write region on a DFVal. Used by assignment / connection coverage tracking and by
  * [[DFMember.departial]] to describe which bits of the underlying declaration an alias chain
  * touches.
  *
  * The representation is deliberately conservative: when a slice's endpoints depend on a design
  * parameter, we fall back to [[Slice.Unknown]] rather than attempting symbolic interval
  * arithmetic.
  */
enum Slice derives CanEqual:
  /** A concrete bit range in the root value's coordinates. */
  case Concrete(range: Range)

  /** The entire value. Used when the value's width itself is symbolic. */
  case Full

  /** A slice whose endpoints are symbolic and could not be resolved. */
  case Unknown

object Slice:
  def fromRange(range: Range): Slice = Concrete(range)

  /** Build a slice covering all bits of a value whose width may be symbolic. */
  def fromWidthOpt(widthOpt: Option[Int]): Slice = widthOpt match
    case Some(w) => Concrete(0 until w)
    case None    => Full

  extension (slice: Slice)
    /** Shift the slice by a (concrete) delta in bit positions. Unknown/Full stay themselves —
      * shifting an unknown slice is still unknown.
      */
    def shift(delta: Int): Slice = slice match
      case Concrete(r) => Concrete(Range(r.start + delta, r.end + delta))
      case other       => other
    def isEmpty: Boolean = slice match
      case Concrete(r) => r.isEmpty
      case _           => false

    /** Does this slice cover the full width of the underlying value? `Tri.Unknown` when either the
      * slice or the width is symbolic.
      */
    def isFullOf(widthOpt: Option[Int]): Tri = slice match
      case Full        => Tri.Yes
      case Unknown     => Tri.Unknown
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
  *   - `unknownTouched` is set when a write with a [[Slice.Unknown]] or a [[Slice.Full]] over an
  *     unknown width has been observed, meaning we know the value was touched but not precisely
  *     where.
  *   - `fullyCovered` is a latch flag set when we observe a write that covers the entire value,
  *     even if the value's width is symbolic (so we cannot represent it as a concrete BitSet). Once
  *     set, any coverage query returns `Yes` regardless of `bits`.
  */
final case class Coverage(
    bits: immutable.BitSet,
    unknownTouched: Boolean,
    fullyCovered: Boolean
) derives CanEqual:
  def |(that: Coverage): Coverage =
    Coverage(
      bits | that.bits,
      unknownTouched || that.unknownTouched,
      fullyCovered || that.fullyCovered
    )
  def &(that: Coverage): Coverage =
    Coverage(
      bits & that.bits,
      unknownTouched && that.unknownTouched,
      fullyCovered && that.fullyCovered
    )

  def assign(slice: Slice, widthOpt: Option[Int]): Coverage =
    slice match
      case Slice.Concrete(r) =>
        copy(bits = bits ++ immutable.BitSet.fromSpecific(r))
      case Slice.Full =>
        widthOpt match
          case Some(w) => copy(bits = bits ++ immutable.BitSet.fromSpecific(0 until w))
          case None    => copy(fullyCovered = true)
      case Slice.Unknown => copy(unknownTouched = true)

  /** Does this coverage touch any bit of `slice`? */
  def overlaps(slice: Slice, widthOpt: Option[Int]): Tri =
    if (fullyCovered)
      slice match
        case Slice.Concrete(r) if r.isEmpty => Tri.No
        case _                              => Tri.Yes
    else
      slice match
        case Slice.Concrete(r) =>
          val sliceBits = immutable.BitSet.fromSpecific(r)
          if ((bits & sliceBits).nonEmpty) Tri.Yes
          else if (unknownTouched) Tri.Unknown
          else Tri.No
        case Slice.Full =>
          if (bits.nonEmpty) Tri.Yes
          else if (unknownTouched) Tri.Unknown
          else Tri.No
        case Slice.Unknown =>
          if (bits.nonEmpty || unknownTouched) Tri.Unknown
          else Tri.No

  /** Does this coverage fully cover `slice`? */
  def contains(slice: Slice, widthOpt: Option[Int]): Tri =
    if (fullyCovered) Tri.Yes
    else
      slice match
        case Slice.Concrete(r) =>
          val sliceBits = immutable.BitSet.fromSpecific(r)
          if ((sliceBits &~ bits).isEmpty) Tri.Yes
          else if (unknownTouched) Tri.Unknown
          else Tri.No
        case Slice.Full =>
          widthOpt match
            case Some(w) =>
              val fullBits = immutable.BitSet.fromSpecific(0 until w)
              if ((fullBits &~ bits).isEmpty) Tri.Yes
              else if (unknownTouched) Tri.Unknown
              else Tri.No
            case None =>
              if (unknownTouched) Tri.Unknown else Tri.No
        case Slice.Unknown => Tri.Unknown

  /** Is this coverage full for the given (possibly unknown) width? */
  def isFull(widthOpt: Option[Int]): Tri = contains(Slice.Full, widthOpt)

  def isEmpty: Boolean = bits.isEmpty && !unknownTouched && !fullyCovered
  def nonEmpty: Boolean = !isEmpty
end Coverage

object Coverage:
  val empty: Coverage =
    Coverage(immutable.BitSet.empty, unknownTouched = false, fullyCovered = false)
  def full(widthOpt: Option[Int]): Coverage =
    empty.assign(Slice.Full, widthOpt)
end Coverage
