package dfhdl.compiler.ir
import dfhdl.internals.*
import scala.annotation.targetName

extension (tokenComp: DFBits.Token.type)
  def apply(width: Int, valueBits: BitVector, bubbleBits: BitVector): DFBits.Token =
    DFBits.Token(DFBits(width), (valueBits, bubbleBits))
extension (token: DFBits.Token)
  def valueBits: BitVector = token.data._1
  def bubbleBits: BitVector = token.data._2

abstract class DFBitsCompanion extends DFType.Companion[DFBits, (BitVector, BitVector)]:
  object Ops:
    extension (lhs: DFBits.Token)
      def uint: DFDecimal.Token = lhs.as(DFUInt(lhs.width))
      def sint: DFDecimal.Token = lhs.as(DFSInt(lhs.width))
      def repeat(num: Int): DFBits.Token =
        assert(num > 0)
        DFBits.Token(
          lhs.width * num,
          BitVector.concat(List.fill(num)(lhs.data._1)),
          BitVector.concat(List.fill(num)(lhs.data._2))
        )
      def sel[I <: Int](relIdx: Int): DFBoolOrBit.Token =
        assert(relIdx >= 0 && relIdx < lhs.width)
        val value = lhs.valueBits.bit(relIdx.toLong)
        val bubble = lhs.bubbleBits.bit(relIdx.toLong)
        val tokenData = if (bubble) None else Some(value)
        DFBoolOrBit.Token(DFBit, tokenData)
      def msbit: DFBoolOrBit.Token = sel(lhs.width - 1)
      def lsbit: DFBoolOrBit.Token = sel(0)
      def sel(relBitHigh: Int, relBitLow: Int): DFBits.Token =
        assert(relBitHigh >= 0 && relBitHigh < lhs.width)
        assert(relBitLow >= 0 && relBitLow < lhs.width)
        assert(relBitHigh >= relBitLow)
        val valueBits =
          lhs.valueBits.bits(relBitHigh.toLong, relBitLow.toLong)
        val bubbleBits =
          lhs.bubbleBits.bits(relBitHigh.toLong, relBitLow.toLong)
        val width = relBitHigh - relBitLow + 1
        DFBits.Token(width, valueBits, bubbleBits)
      def resize(updatedWidth: Int): DFBits.Token =
        if (updatedWidth == lhs.width) lhs
        else
          assert(updatedWidth > 0)
          val data = lhs.data
          import dfhdl.internals.{resize => resizeBV}
          DFBits.Token(
            updatedWidth,
            lhs.valueBits.resizeBV(updatedWidth),
            lhs.bubbleBits.resizeBV(updatedWidth)
          )
      @targetName("concat")
      def ++(rhs: DFBits.Token): DFBits.Token =
        val width = lhs.width + rhs.width
        val valueBits = lhs.valueBits ++ rhs.valueBits
        val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
        DFBits.Token(width, valueBits, bubbleBits)
    end extension
  end Ops
end DFBitsCompanion
