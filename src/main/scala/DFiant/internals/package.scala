package DFiant

import java.lang.Double._
import java.lang.Float._
import singleton.ops._
import singleton.twoface._
import scala.math.{ceil, floor, log}

package object internals {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  // Conversions
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  private def LongtoHexString(l: scala.Long): String = {
    val zeros = "00000000" // 8 zeros
    @inline def padBinary8(i: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length) + s
    }

    val lo = l.toInt
    val hi = (l >>> 32).toInt

    if (hi != 0) Integer.toHexString(hi) + padBinary8(lo)
    else Integer.toHexString(lo)
  }

  //Int conversion is done like so to avoid negative values (MSbit of Int is 1)
  def byteToBigIntBits(value : Byte) : BigInt = BigInt(value.toString)
  def intToBigIntBits(value : Int) : BigInt = BigInt(Integer.toHexString(value),16)
  def longToBigIntBits(value : Long) : BigInt = BigInt(LongtoHexString(value),16)
  def floatToBigIntBits(value : Float) : BigInt = intToBigIntBits(floatToRawIntBits(value))
  def doubleToBigIntBits(value : Double) : BigInt = longToBigIntBits(doubleToRawLongBits(value))
  def bigIntBitsToFloat(value : BigInt) : Float = intBitsToFloat(value.toInt)
  def bigIntBitsToDouble(value : BigInt) : Double = longBitsToDouble(value.toLong)

  object BitsWidthOf {
    private type IsPositive[V] = V > W.`0`.T
    private type IsZero[V] = V == W.`0`.T
    private type One = W.`1`.T
    private type Calc[V, W] =
      ITE[
        IsPositive[V],
        W - NumberOfLeadingZeros[V],
        ITE[
          IsZero[V],
          One,
          W + One - NumberOfLeadingZeros[Negate[V]]
          ]
        ]
    type CalcInt[V] = Calc[V, W.`32`.T]
    type CalcLong[V] = Calc[V, W.`64`.T]
    type Int[V] = TwoFace.Int.Shell1[CalcInt, V, scala.Int]
    type IntAux[V, Ret_Out] = Int[V] {type Out = Ret_Out}
    type Long[V] = TwoFace.Int.Shell1[CalcLong, V, scala.Long]
    type LongAux[V, Ret_Out] = Long[V] {type Out = Ret_Out}
  }

  def bigIntRepWidth(value : BigInt) : Int = {
    if (value > 0) value.bitLength
    else if (value == 0) 1
    else (-value).bitLength + 1
  }
  def bigIntToBinaryString(value : BigInt, width : Int = 0) : String = {
    val _width = if (width <= bigIntRepWidth(value)) bigIntRepWidth(value) else width
    if (value >= 0) {
      val valueBinStr = value.toString(2)
      var space = ""
      for (i <- 0 until _width-valueBinStr.length())
        space += "0"
      space + valueBinStr
    } else {
      var _value = -value
      for (i <- 0 until _width) _value=_value.flipBit(i)
      _value = _value + 1
      _value.toString(2)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////

  //get the maximum BigInt given a bits width
  def bitsWidthToMaxBigIntBits(width : Int) : BigInt = {
    var mask: BigInt = 2
    mask = mask.pow(width) - 1
    mask
  }
  def bitsSel(in0 : BigInt, bitHigh : Int, bitLow : Int) : BigInt =
    (in0 >> bitLow) & bitsWidthToMaxBigIntBits(bitHigh-bitLow+1)


  def int2hex (int : Int, nibblesMax : Int = 0): String = {
    var str = Integer.toHexString(int).toUpperCase
    var space = ""
    for (i <- 0 until nibblesMax - str.length)
      space += "0"
    "0x" + space + str
  }
  def int2hex (intArr : Array[Int]): String = {
    var str = ""
    for (i <- 0 until intArr.length)
      str += int2hex(intArr(i),8) + ","
    str
  }

  object RelWidth {
    type Calc[H, L] = H - L + W.`1`.T
    type TF[H, L] = TwoFace.Int.Shell2[Calc, H, Int, L, Int]
    type TFAux[H, L, RetOut] = TF[H, L]{type Out = RetOut}
  }

  object BitsHiLo extends Checked1Param.Int {
    type Cond[H, L] = H >= L
    type Msg[H, L] = "Low bit index " + ToString[L] + " is bigger than High bit index " + ToString[H]
    type ParamFace = Int
  }

  object BitIndex extends Checked1Param.Int {
    type Cond[I, W] = (I < W) && (I >= 0)
    type Msg[I, W] = "Bit index " + ToString[I] + " is out of range of width " + ToString[W]
    type ParamFace = Int
  }

  object PartWidth extends Checked1Param.Int {
    type Cond[PW, W] = (PW <= W) && (PW > 0)
    type Msg[PW, W] = "Partial width bit selection " + ToString[PW] + " is out of range of width " + ToString[W]
    type ParamFace = Int
  }

  object BitsWidth extends Checked0Param.Int {
    type Cond[W] = (W > 0)
    type Msg[W] = "DFBits width must be positive. Found width = " + ToString[W]
  }

  object Natural extends Checked0Param.Int {
    type Cond[N] = N >= 0
    type Msg[N] = "Number must be natural, but found " + ToString[N]
  }
}
