/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

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
  def max(that : BitsRange) : BitsRange = BitsRange(math.max(this.width, that.width))
  def +(that : BitsRange) : BitsRange = BitsRange(this.width + that.width)
  def incBy(that : Int) : BitsRange = BitsRange(this.width + that)
  override def toString: String = if(bitLow==bitHigh) s"$bitHigh" else s"$bitHigh::$bitLow"
}

object BitsRange {
  def apply(width : Int) : BitsRange = BitsRange(width-1, 0)
}
