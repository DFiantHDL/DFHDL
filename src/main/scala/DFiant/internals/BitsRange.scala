package DFiant.internals

case class BitsRange(bitHigh : Int, bitLow : Int) {
  val width : Int = bitHigh-bitLow+1
  require(bitLow <= bitHigh, "Low bit index must be smaller or equal to high bit index")
  require(bitLow >= 0, "Low bit index must not be negative")

  //Relative subrange
  //high = 7, low = 4, relBitHigh = 2, relBitLow = 1 ==> returned range 6:5
  def subRangeRel(relBitsRange : BitsRange) : BitsRange = {
    require(relBitsRange.bitHigh < width && relBitsRange.bitLow >= 0, "Relative bit selection must be within confinement of [width-1::0]")
    BitsRange(bitLow + relBitsRange.bitLow + relBitsRange.width-1, bitLow + relBitsRange.bitLow)
  }
  def max(that : BitsRange) : BitsRange = BitsRange(math.max(this.width, that.width)-1,0)
  def +(that : BitsRange) : BitsRange = BitsRange(this.width + that.width - 1, 0)
  def incBy(that : Int) : BitsRange = BitsRange(this.width + that - 1, 0)
  override def toString: String = if(bitLow==bitHigh) s"$bitHigh" else s"$bitHigh::$bitLow"
}

