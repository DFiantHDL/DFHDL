/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import java.lang.Double._
import java.lang.Float._

import singleton.ops._
import singleton.twoface._

import scala.math.{ceil, floor, log}
import scodec.bits._
import continuum._

import scala.annotation.tailrec

package object internals {
  trait DevAccess
  implicit object devAccess extends DevAccess

  final implicit class Tagger[T](t : T) {
    def @@[Tag] : T with Tag = t.asInstanceOf[T with Tag]
  }
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
        1
        ]
    type CalcInt[V] = Calc[V, 32]
    type CalcLong[V] = Calc[V, 64]
    type Arg0Int = TwoFace.Int.Shell1[CalcInt, GetArg0, scala.Int]
    type Arg0Long = TwoFace.Int.Shell1[CalcLong, GetArg0, scala.Long]
    type Int[V] = TwoFace.Int.Shell1[CalcInt, V, scala.Int]
    type IntAux[V, Ret_Out] = Int[V] {type Out = Ret_Out}
    type Long[V] = TwoFace.Int.Shell1[CalcLong, V, scala.Long]
    type LongAux[V, Ret_Out] = Long[V] {type Out = Ret_Out}
    object Signed {
      private[BitsWidthOf] type Calc[V, W] =
        ITE[
          IsPositive[V],
          W + 1 - NumberOfLeadingZeros[V],
          ITE[
            IsZero[V],
            2,
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
    object SignedCfg {
      private type Calc[S, V, W] =
        ITE[S, Signed.Calc[V, W], BitsWidthOf.Calc[V, W]]
      type CalcInt[S, V] = Calc[S, V, 32]
      type CalcLong[S, V] = Calc[S, V, 64]
      type Int[S, V] = TwoFace.Int.Shell2[CalcInt, S, scala.Boolean, V, scala.Int]
      type IntAux[S, V, Ret_Out] = Int[S, V] {type Out = Ret_Out}
      type Long[S, V] = TwoFace.Int.Shell2[CalcLong, S, scala.Boolean, V, scala.Long]
      type LongAux[S, V, Ret_Out] = Long[S, V] {type Out = Ret_Out}
    }
  }
  object IsPowerOfTwo {
    type CalcInt[V] = (V BitwiseAnd (V-1)) == 0
    type Int[V] = TwoFace.Int.Shell1[CalcInt, V, scala.Int]
    type IntAux[V, Ret_Out] = Int[V] {type Out = Ret_Out}
  }

  implicit class BigIntExtrasCO(value : BigInt.type) {
    //get the maximum BigInt given a bits width
    def maxUnsignedFromWidth(width : Int) : BigInt = BigInt(2).pow(width) - 1
    def maxSignedFromWidth(width : Int) : BigInt = BigInt(2).pow(width-1) - 1
    def minSignedFromWidth(width : Int) : BigInt = -BigInt(2).pow(width-1)
  }

  implicit class BigIntExtras(value : BigInt) {
    def bitsWidth(signed : Boolean) : Int = {
      if (value > 0)
        if (signed) value.bitLength + 1 else value.bitLength
      else if (value == 0)
        if (signed) 2 else 1
      else (-value).bitLength + 1
    }
    def asUnsigned(ofWidth : Int) : BigInt = {
      if (value >= 0) value
      else {
        BigInt(2).pow(ofWidth) + value
      }
    }
    def asUnsigned : BigInt = asUnsigned(bitsWidth(false))
    def toBitVector(width : Int) : BitVector = {
      val vec = BitVector(value.toByteArray)
      if (value < 0 && vec.length < width) BitVector.fill(width - vec.length)(true) ++ vec
      else vec.resize(width)
    }
  }

  type BitVector = scodec.bits.BitVector
  val BitVector = scodec.bits.BitVector

  implicit class BitVectorExtras(vec : BitVector) {
    def lzc : Long = {
      val l = for (i <- 0L until vec.length if vec(i)) yield i
      if (l.isEmpty) vec.length else l.head
    }
    def lengthOfValue : Long = if (lzc == vec.length) 1L else vec.length - lzc
    def resize(newLength : Int) : BitVector = {
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
    def bitsWL[W](relWidth : Int, loIdx : Int) : BitVector = {
      val hiIdx = relWidth + loIdx - 1
      bits(hiIdx, loIdx)
    }
    def padToMulsOf(bitsNum : Int) : BitVector = {
      val paddedVecLength = ((vec.length + bitsNum - 1) / bitsNum) * bitsNum
      vec.padLeft(paddedVecLength)
    }
    def toHexProper : String = padToMulsOf(4).toHex
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
    def bitsWidth(signed : Boolean) : Int = BigInt(value).bitsWidth(signed)
    def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
  }

  implicit class LongExtras(value : Long) {
    def bitsWidth(signed : Boolean) : Int = BigInt(value).bitsWidth(signed)
    def toBitVector(width : Int) : BitVector = BigInt(value).toBitVector(width)
  }

  implicit class BooleanExtras(value : Boolean) {
    def toBitVector(width : Int) : BitVector = BitVector.fill(width)(value)
  }

  def bigIntToBinaryString(value : BigInt, width : Int = 0) : String = {
    val _width = if (width <= value.bitsWidth(false)) value.bitsWidth(false) else width
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

  implicit class IntervalIntExtras(value : Interval[Int]) {
    def toBigIntInterval : Interval[BigInt] = value.map(b => BigInt(b))
  }
  implicit class IntervalLongExtras(value : Interval[Long]) {
    def toBigIntInterval : Interval[BigInt] = value.map(b => BigInt(b))
  }

  //from Map[K,V] to Map[V,Set[K]], traverse the input only once
  //From: https://stackoverflow.com/a/51356499/3845175
  implicit class MapInverterA[K,V](m :Map[K,V]) {
    def invert :Map[V,Set[K]] =
      m.foldLeft(Map.empty[V, Set[K]]) {
        case (acc,(k, v)) => acc + (v -> (acc.getOrElse(v,Set()) + k))
      }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////


  implicit class ReflectionClassExtras(extended : Any) {
    import java.lang.reflect.Field

    def getNestedDeclaredFieldsOf[T, B](subClass : Class[_], fieldApply : (Field, T) => B) : List[B] = {
      def allFieldsFrom(c : Class[_]) : List[Field] = {
        if (c == null) List()
        else
          c.getDeclaredFields.filter(f => subClass.isAssignableFrom(f.getType)).toList ++ allFieldsFrom(c.getSuperclass)
      }

      allFieldsFrom(extended.getClass).map(f => {
        f.setAccessible(true)
        val t = f.get(extended).asInstanceOf[T]
        fieldApply(f, t)
      })
    }


  }
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
    type Msg[H, L] = "Low index " + ToString[L] + " is bigger than High bit index " + ToString[H]
    type ParamFace = Int
  }

  object BitIndex extends Checked1Param.Int {
    type Cond[I, W] = (I < W) && (I >= 0)
    type Msg[I, W] = "Index " + ToString[I] + " is out of range of width/length " + ToString[W]
    type ParamFace = Int
  }

  object PartWidth extends Checked1Param.Int {
    type Cond[PW, W] = (PW <= W) && (PW > 0)
    type Msg[PW, W] = "Partial width bit selection " + ToString[PW] + " is out of range of width " + ToString[W]
    type ParamFace = Int
  }

  object ExtWidth extends Checked1Param.Int {
    type Cond[EW, W] = EW > W
    type Msg[EW, W] = "Extended width " + ToString[EW] + " is not larger than current width " + ToString[W]
    type ParamFace = Int
  }

  object AsWidth extends Checked1Param.Int {
    type Cond[AW, CW] = AW == CW
    type Msg[AW, CW] = "The aliased type width " + ToString[AW] + " is not equal to the current width " + ToString[CW]
    type ParamFace = Int
  }

  object BitsWidth extends Checked0Param.Int {
    type Cond[W] = (W > 0)
    type Msg[W] = "DFBits/DFUInt width must be positive.\nFound width = " + ToString[W]
  }

  object SIntWidth extends Checked0Param.Int {
    type Cond[W] = (W > 1)
    type Msg[W] = "DFSInt width must be 2 or larger.\nFound width = " + ToString[W]
  }

  object Positive extends Checked0Param.Int {
    type Cond[N] = N > 0
    type Msg[N] = "Argument must be a positive number, but found " + ToString[N]
  }

  object Natural {
    type MsgCommon[N] = "Argument must be a natural number, but found " + ToString[N]
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
    type Msg[N] = "Number must be binary (0 or 1).\nFound = " + ToString[N]
  }

  object `LW >= RW` extends Checked1Param.Int {
    type Cond[LW, RW] = LW >= RW
    type Msg[LW, RW] = "This operation does not permit applying a wider RHS expression.\nFound: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
    type ParamFace = Int
    type CheckedExtendable[LW, LE, RW] = CheckedShell[LW, ITE[LE, 0, RW]]
  }
  object `LW == RW` extends Checked1Param.Int {
    type Cond[LW, RW] = LW == RW
    type Msg[LW, RW] = "This operation does not permit applying different width DF variables.\nFound: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
    type ParamFace = Int
  }

  type Arg0IsNonLit = Require[IsNonLiteral[GetArg0]]

  ///////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit class SetOps[T](set : Set[T]) {
    def intersectsWith(iterable: Iterable[T]) : Boolean =
      iterable.collectFirst {
        case i if set.contains(i) => true
      }.getOrElse(false)
  }

  implicit class IterableStringsOps(list: Iterable[String]) {
    def longestCommmonPrefix : String = list.foldLeft("")((_,_) =>
      (list.min.view lazyZip list.max.view).takeWhile(v => v._1 == v._2).map(_._1).mkString)
  }

  implicit class GroupByOrderedImplicitImpl[T](val seq: Iterable[T]) extends AnyVal {
    def groupByOrdered[P](f: T => P): Seq[(P, Iterable[T])] = {
      @tailrec
      def accumulator(seq: Iterable[T], f: T => P, res: List[(P, Iterable[T])]): Seq[(P, Iterable[T])] = seq.headOption match {
        case None => res.reverse
        case Some(h) =>
          val key = f(h)
          val subseq = seq.takeWhile(f(_) == key)
          accumulator(seq.drop(subseq.size), f, (key -> subseq) :: res)
      }
      accumulator(seq, f, Nil)
    }
  }

  def tmeas[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
    result
  }

  implicit def fromValueOf[T](valueOf: ValueOf[T]) : T = valueOf.value
  class Exact[T](val value : T)
  object Exact {
    import scala.reflect.macros.blackbox
    //For singleton integers we create a special macro that offers some protection from hex literals that
    //overflow into negative values. E.g., 0x80000000
    //This is no way close to a full protection from such incidents, but this is enough for most newbie cases
    //that DFiant code may encounter.
    implicit def fromIntSing[T <: Int with Singleton](value : T) : Exact[ValueOf[T]] = macro fromIntSingMacro[T]
    def fromIntSingMacro[T : c.WeakTypeTag](c: blackbox.Context)(value : c.Tree) : c.Tree = {
      import c.universe._
      val tpe = weakTypeOf[T]
      value match {
        case Literal(Constant(i : Int)) if i < 0 =>
          val content = new String(value.pos.source.content)
          val startStr = content.splitAt(value.pos.start)._2
          if (startStr.startsWith("0x") || startStr.startsWith("0X"))
            c.abort(c.enclosingPosition, "Found a hex integer literal that overflows into a negative value. \nPlease use DFiant's built in string interpolator literals instead.")
        case _ => //do nothing
      }
      q"new DFiant.internals.Exact[ValueOf[$tpe]](new ValueOf($value))"
    }
    implicit def fromAnyValSing[T <: Singleton with AnyVal](value : T) : Exact[ValueOf[T]] = new Exact[ValueOf[T]](new ValueOf(value))
    implicit def fromStringSing[T <: Singleton with String](value : T) : Exact[ValueOf[T]] = new Exact[ValueOf[T]](new ValueOf(value))
    implicit def fromNonSing[T](value : T)(implicit di : DummyImplicit) : Exact[T] = new Exact[T](value)
    implicit def toValue[T](precise: Exact[T]) : T = precise.value
  }

  implicit class WhiteboxMacroExt(c : scala.reflect.macros.whitebox.Context) {
    import c.universe._
    private val defaultAnnotatedSym : Option[TypeSymbol] =
      if (c.enclosingImplicits.isEmpty) None else c.enclosingImplicits.last.pt match {
        case TypeRef(_,sym,_) => Some(sym.asType)
        case x => Some(x.typeSymbol.asType)
      }
    ////////////////////////////////////////////////////////////////////
    // Code thanks to Shapeless
    // https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/lazy.scala
    ////////////////////////////////////////////////////////////////////
    private def setAnnotation(msg: String, annotatedSym : TypeSymbol) : Unit = {
      import c.internal._
      import decorators._
      val tree0 =
        c.typecheck(
          q"""new _root_.scala.annotation.implicitNotFound("dummy")""",
          silent = false
        )

      class SubstMessage extends Transformer {
        val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

        override def transform(tree: Tree): Tree = {
          super.transform {
            tree match {
              case Literal(Constant("dummy")) => Literal(Constant(msg))
              case t => t
            }
          }
        }
      }
      val tree = new SubstMessage().transform(tree0)
      annotatedSym.setAnnotations(Annotation(tree))
      ()
    }
    def setAnnotation(msg : String) : Unit = defaultAnnotatedSym.foreach(sym => setAnnotation(msg, sym))
    def abort(msg : String) : Nothing = {
      defaultAnnotatedSym.foreach(sym => setAnnotation(msg, sym))
      c.abort(c.enclosingPosition, msg)
    }
  }

}
