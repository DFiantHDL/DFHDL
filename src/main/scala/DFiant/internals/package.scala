package DFiant

import java.lang.Double._
import java.lang.Float._
import singleton.ops._
import singleton.twoface._
import scodec.bits._
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

  type ZeroI = 0

  //Int conversion is done like so to avoid negative values (MSbit of Int is 1)
  def byteToBigIntBits(value : Byte) : BigInt = BigInt(value.toString)
  def intToBigIntBits(value : Int) : BigInt = BigInt(Integer.toHexString(value),16)
  def longToBigIntBits(value : Long) : BigInt = BigInt(LongtoHexString(value),16)
  def floatToBigIntBits(value : Float) : BigInt = intToBigIntBits(floatToRawIntBits(value))
  def doubleToBigIntBits(value : Double) : BigInt = longToBigIntBits(doubleToRawLongBits(value))
  def bigIntBitsToFloat(value : BigInt) : Float = intBitsToFloat(value.toInt)
  def bigIntBitsToDouble(value : BigInt) : Double = longBitsToDouble(value.toLong)

  object BitsWidthOf {
    private type Zero[V] = ITE[IsLong[V], 0L, 0]
    private type IsPositive[V] = V > Zero[V]
    private type IsZero[V] = V == Zero[V]
    private type Calc[V, W] =
      ITE[
        IsPositive[V],
        W - NumberOfLeadingZeros[V],
        ITE[
          IsZero[V],
          1,
          W + 1 - NumberOfLeadingZeros[Negate[V]]
          ]
        ]
    type CalcInt[V] = Calc[V, 32]
    type CalcLong[V] = Calc[V, 64]
    type Arg0Int = TwoFace.Int.Shell1[CalcInt, GetArg0, scala.Int]
    type Arg0Long = TwoFace.Int.Shell1[CalcLong, GetArg0, scala.Long]
    type Int[V] = TwoFace.Int.Shell1[CalcInt, V, scala.Int]
    type IntAux[V, Ret_Out] = Int[V] {type Out = Ret_Out}
    type Long[V] = TwoFace.Int.Shell1[CalcLong, V, scala.Long]
    type LongAux[V, Ret_Out] = Long[V] {type Out = Ret_Out}
  }

  implicit class BigIntExtras(value : BigInt) {
    //get the maximum BigInt given a bits width
    private def bitsWidthToMaxBigIntBits(width : Int) : BigInt = {
      var mask: BigInt = 2
      mask = mask.pow(width) - 1
      mask
    }

    def bitsWidth : Int = {
      if (value > 0) value.bitLength
      else if (value == 0) 1
      else (-value).bitLength + 1
    }
    def asUnsigned(ofWidth : Int) : BigInt = {
      if (value >= 0) value
      else {
        BigInt(2).pow(ofWidth) + value
      }
    }
    def asUnsigned : BigInt = asUnsigned(bitsWidth)
    def codeString : String = {
      if (value.isValidInt) s"$value"
      else if (value.isValidLong) s"${value}L"
      else s"""BigInt("$value")"""
    }
    def toBitVector(width : Int) : BitVector = BitVector(value.toByteArray).toLength(width)
  }

  implicit class BitVectorExtras(vec : BitVector) {
    def lzc : Long = {
      val l = for (i <- 0L until vec.length if vec(i)) yield i
      if (l.isEmpty) vec.length else l.head
    }
    def lengthOfValue : Long = if (lzc == vec.length) 1L else vec.length - lzc
    def toLength(newLength : Long) : BitVector = {
      if (newLength > vec.length) vec.padLeft(newLength)
      else if (newLength < vec.length) vec.drop(vec.length - newLength)
      else vec
    }
    def revIdx(bitIdx : Long) : Long = vec.length - 1 - bitIdx //reverse index for BitVector
    def bit(idx : Long) : Boolean = vec(revIdx(idx))
    def bits(hiIdx : Long, loIdx : Long) : BitVector = {
      val riLoIdx = revIdx(hiIdx)
      val riHiLow = revIdx(loIdx)
      vec.slice(riLoIdx, riHiLow + 1)
    }
    def padToMulsOf(bitsNum : Int) : BitVector = {
      val paddedVecLength = ((vec.length + bitsNum - 1) / bitsNum) * bitsNum
      vec.padLeft(paddedVecLength)
    }
    def toShortString : String = {
      val nibble = 4
      val lov = lengthOfValue
      //narrowing the vector by removing all the leftest zeros
      val narrowVec = vec.takeRight(lov)
      //default printing of bitvectors is padding-right in `toHex`.
      //padding left is much more intuitive for us because we consider
      // the leftest presented bit to be to MSbit.
      s"0x${narrowVec.padToMulsOf(nibble).toHex}"
    }
    def toBigInt : BigInt = {
      val len = vec.length
      val ext = vec.padLeft(len + 1)
      BigInt(ext.padToMulsOf(8).toByteArray)
    }
  }

  implicit class IntExtras(value : Int) {
    def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
  }

  implicit class LongExtras(value : Long) {
    def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
  }

  implicit class BooleanExtras(value : Boolean) {
    def toBitVector(width : Int) : BitVector = BitVector.fill(width)(value)
  }

  def bigIntToBinaryString(value : BigInt, width : Int = 0) : String = {
    val _width = if (width <= value.bitsWidth) value.bitsWidth else width
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
    type Calc[H, L] = H - L + 1
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

  object Natural {
    object Int extends Checked0Param.Int {
      type Cond[N] = N >= 0
      type Msg[N] = "Argument must be a natural number, but found " + ToString[N]
    }
    object Long extends Checked0Param.Long {
      type Cond[N] = N >= 0L
      type Msg[N] = "Argument must be a natural number, but found " + ToString[N]
    }
  }

  object Positive extends Checked0Param.Int {
    type Cond[N] = N > 0
    type Msg[N] = "Argument must be a positive number, but found " + ToString[N]
  }

  trait `N >= 0` {
    type MsgCommon[N]
    object Int extends Checked0Param.Int {
      type Cond[N] = N >= 0
      type Msg[N] = MsgCommon[N]
    }
    object Long extends Checked0Param.Long {
      type Cond[N] = N >= 0L
      type Msg[N] = MsgCommon[N]
    }
    object BigInt extends Checked1Param.Boolean {
      type Cond[T, P] = T
      type Msg[T, P] = MsgCommon[P]
      type ParamFace = String
      def unsafeCheck(n : BigInt)(implicit chk : BigInt.CheckedShell[Boolean, String]) : Unit =
        chk.unsafeCheck(n >= 0, n.toString())
    }
  }

  object BinaryInt extends Checked0Param.Int {
    type Cond[N] = (N == 0) || (N == 1)
    type Msg[N] = "Number must be binary (0 or 1). Found = " + ToString[N]
  }

  type Arg0IsNonLit = Require[IsNonLiteral[GetArg0]]
}
