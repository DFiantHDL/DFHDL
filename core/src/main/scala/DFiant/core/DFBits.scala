package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName

opaque type DFBits[W <: Int] <: DFType.Of[ir.DFBits] = DFType.Of[ir.DFBits]
object DFBits:
  def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] =
    ir.DFBits(width).asInstanceOf[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W]): DFBits[W] =
    DFBits[W](Inlined.Int.forced[W](valueOf[W])).asInstanceOf[DFBits[W]]
  extension [W <: Int](dfType: DFBits[W])
    def width: Inlined.Int[W] = Inlined.Int.forced[W](dfType.asIR.width)

  opaque type Token[W <: Int] <: DFToken.Of[DFBits[W], (BitVector, BitVector)] =
    DFToken.Of[DFBits[W], (BitVector, BitVector)]
  //TODO: remove after https://github.com/lampepfl/dotty/issues/12927 is fixed
  extension [W <: Int](token: Token[W])
    def width: Inlined.Int[W] = Inlined.Int.forced[W](token.asIR.width)
    def data: (BitVector, BitVector) =
      token.asIR.data.asInstanceOf[(BitVector, BitVector)]
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR, (valueBits, bubbleBits))
        .asInstanceOf[Token[W]]
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
        valueBits: BitVector,
        bubbleBits: BitVector
    ): Token[W] =
      Token(DFBits(width), (valueBits, bubbleBits))
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
        value: Bubble
    ): Token[W] =
      Token(
        width,
        BitVector.low(width.value),
        BitVector.high(width.value)
      )
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
        value: SameBitsVector
    ): Token[W] =
      val level = value match
        case SameBitsVector.b0s => false
        case SameBitsVector.b1s => true
      Token(
        width,
        BitVector.fill(width.value)(level),
        BitVector.low(width.value)
      )
    extension [W <: Int](token: Token[W])
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    extension [LW <: Int](lhs: DFBits.Token[LW])
      @targetName("concat")
      def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
        val width = lhs.width + rhs.width
        val valueBits = lhs.valueBits ++ rhs.valueBits
        val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
        DFBits.Token(width, valueBits, bubbleBits)

      @targetName("bitwiseAnd")
      def &[RW <: Int](rhs: DFBits.Token[RW])(using
          bb: Bubble.Behaviour
      ): DFBits.Token[LW] =
        assert(lhs.width == rhs.width)
        val width = lhs.width
        bb match
          case Bubble.Behaviour.Stall =>
            DFBits.Token(
              width,
              lhs.valueBits & rhs.valueBits,
              lhs.bubbleBits | rhs.bubbleBits
            )
          case Bubble.Behaviour.DontCare =>
            val valueBits =
              (lhs.valueBits | lhs.bubbleBits) & (rhs.valueBits | rhs.bubbleBits)
            val bubbleBits =
              (lhs.bubbleBits & rhs.bubbleBits) | (lhs.bubbleBits & rhs.valueBits) |
                (rhs.bubbleBits & lhs.valueBits)
            DFBits.Token(width, valueBits, bubbleBits)
