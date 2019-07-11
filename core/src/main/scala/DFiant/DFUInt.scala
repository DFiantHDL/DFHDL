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

import DFiant.targetlib._
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import shapeless.<:!<

trait DFUInt[W] extends DFUInt.Unbounded {
  type Width = W
}

object DFUInt extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFUInt.type] {
    protected[DFiant] type TUnbounded = Unbounded
    protected[DFiant] type TVal = DFUInt[Width]
    protected[DFiant] type TVar = DFUInt.Var[Width]
    protected[DFiant] type TToken = DFUInt.Token
    protected[DFiant] type TPattern = DFUInt.Pattern
    protected[DFiant] type TPatternAble[+R] = DFUInt.Pattern.Able[R]
    protected[DFiant] type TPatternBuilder[L <: DFAny] = DFUInt.Pattern.Builder[L]
    protected[DFiant] type OpAble[R] = Op.Able[R]
    protected[DFiant] type `Op<>Builder`[R] = `Op<>`.Builder[TVal, R]
    protected[DFiant] type `Op:=Builder`[R] = `Op:=`.Builder[TVal, R]
    protected[DFiant] type `Op==Builder`[R] = `Op==`.Builder[TVal, R]
    protected[DFiant] type `Op!=Builder`[R] = `Op!=`.Builder[TVal, R]
    protected[DFiant] type InitAble[L <: DFAny] = Init.Able[L]
    protected[DFiant] type InitBuilder = Init.Builder[TVal, TToken]
    protected[DFiant] type PortBuilder[Dir <: DFDir] = Port.Builder[TVal, Dir]
    type TExtendable
    import __dev._

    final lazy val maxValue : BigInt = BigInt(2) << (width - 1) - 1
    final def +  [R](right: Op.Able[R])(implicit op: `Op+`.Builder[TVal, TExtendable, R]) = op(left, right)
    final def -  [R](right: Op.Able[R])(implicit op: `Op-`.Builder[TVal, TExtendable, R]) = op(left, right)
    final def *  [R](right: Op.Able[R])(implicit op: `Op*`.Builder[TVal, TExtendable, R]) = op(left, right)
    //  def /  (right : DFUInt)         : DFUInt = ???

    final def <  [R](right: Op.Able[R])(implicit op: `Op<`.Builder[TVal, R]) = op(left, right)
    final def >  [R](right: Op.Able[R])(implicit op: `Op>`.Builder[TVal, R]) = op(left, right)
    final def <= [R](right: Op.Able[R])(implicit op: `Op<=`.Builder[TVal, R]) = op(left, right)
    final def >= [R](right: Op.Able[R])(implicit op: `Op>=`.Builder[TVal, R]) = op(left, right)

    final def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : DFBool = op(left, right)
    final def == [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : DFBool = op(left, right)
    final def == (that : BigInt)(implicit op: `Op==`.Builder[TVal, BigInt]) : DFBool = op(left, that)
    final def != [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) : DFBool = op(left, right)
    final def != [R](that : Long)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) : DFBool = op(left, right)
    final def != (that : BigInt)(implicit op: `Op!=`.Builder[TVal, BigInt]) = op(left, that)

    final def extendBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFUInt[tfs.Out] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits, 0))
      new DFUInt.Alias[tfs.Out](DFAny.Alias.Reference.Concat(List(zeros, this), s".bits.uint")).setAutoConstructCodeString(s"$refCodeString.extendBy($numOfBits)")
    }

    protected[DFiant] def protToWidth[EW](toWidth : TwoFace.Int[EW])(implicit ctx : DFAny.Alias.Context)
    : DFUInt[EW] = new DFUInt.Alias[EW](DFAny.Alias.Reference.Resize(this, toWidth))

    final def toWidth[EW](toWidth : Positive.Checked[EW])(implicit ctx : DFAny.Alias.Context)
    : DFUInt[EW] = protToWidth(toWidth)

    final def pattern[R](right : Pattern.Able[R]*)(implicit bld : Pattern.Builder[TVal]) = bld(left, right)

    final def isZero(implicit ctx : DFAny.Op.Context) = left == 0
    final def isNonZero(implicit ctx : DFAny.Op.Context) = left != 0
    final def extendable(implicit ctx : DFAny.Alias.Context) : DFUInt.Extendable[Width] = new DFUInt.Extendable[Width](left)

    //    def within[Start, End](right : XRange[Start, End])(implicit op : OpWithin.Builder[TVal, XRange[Start, End]]) = op(left, right)
    final protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <~> Dir = new Port(new NewVar[Width](width), dir)
    final protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
      implicit ctx : DFAny.Alias.Context
    ) : TAlias = new Alias(reference)(ctx).asInstanceOf[TAlias]
    __dev.setAutoTypeName(s"DFUInt[$width]")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFUInt[W] with DFAny.Var {
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit ctx : DFAny.NewVar.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : NewVar[W] = new NewVar[W](checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit ctx : DFAny.NewVar.Context
  ) : NewVar[W] = new NewVar[W](checkedWidth.unsafeCheck())

  //Returns new unsigned dataflow variable with a supremum bound of supLimit (max value is supLimit-1)
  //Does not apply auto-rolling at maximum value!!
  def rangeUntil[U](supLimit : Positive.Checked[U])(
    implicit ctx : DFAny.NewVar.Context, widthOf: BitsWidthOf.Int[U - 1]
  ) : NewVar[widthOf.Out] =  new NewVar[widthOf.Out](widthOf(supLimit-1))

  //Returns new unsigned dataflow variable with a maximum bound of maxLimit
  //Does not apply auto-rolling at maximum value!!
  def rangeTo[U](maxLimit : Positive.Checked[U])(
    implicit ctx : DFAny.NewVar.Context, widthOf: BitsWidthOf.Int[U]
  ) : NewVar[widthOf.Out] =  new NewVar[widthOf.Out](widthOf(maxLimit))

  //  def rangeUntil(supLimit : Int)    : Var = rangeUntil(intToBigIntBits(supLimit))
//  def rangeUntil(supLimit : Long)   : Var = rangeUntil(longToBigIntBits(supLimit))
//  def rangeTo(maxLimit : Int)       : Var = rangeTo(intToBigIntBits(maxLimit))
//  def rangeTo(maxLimit : Long)      : Var = rangeTo(longToBigIntBits(maxLimit))
//  def rangeTo(maxLimit : BigInt)    : Var = apply(bigIntRepWidth(maxLimit))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFUInt[W]](width, s"DFUInt($width)") with Var[W]

  protected[DFiant] final class Alias[W](reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFUInt[W]](reference) with Var[W]

  protected[DFiant] final class Extendable[W](extendedVar : DFUInt[W])(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFUInt[W]](DFAny.Alias.Reference.AsIs(extendedVar, ".extendable")) with Var[W] {
    type TExtendable = true
    override def toString : String = s"DFUInt[$width] & Extendable"
  }

  protected[DFiant] final class Const[W](token : DFUInt.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const[DFUInt[W]](token) with DFUInt[W]

  protected[DFiant] final class Port[W, Dir <: DFDir](dfVar : DFUInt[W], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFUInt[W], Dir](dfVar, dir) with DFUInt[W]

//  protected[DFiant] class Func2Comp[Kind, L <: DFAny, R <: DFAny, OW](leftArg : L, rightArg : R)(
//    implicit ctx : DFComponent.Context[Func2Comp[Kind, L, R, OW]], kind : Kind
//  ) extends DiSoComp[Func2Comp[Kind, L, R, OW], L, R](leftArg, kind.toString, rightArg) with DFUInt[OW]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Token private[DFiant] (width : Int, value : BigInt, bubble : Boolean) extends DFAny.Token.Of[BigInt, Pattern] {
    protected[DFiant] type TToken = Token
    lazy val valueBits : BitVector = value.toBitVector(width)
    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
    def toBubbleToken : Token = Token(width, Bubble)
    def mkTokenU(that : Token, result : BigInt, resultWidth : Int) : Token = {
      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
      else Token(resultWidth, result.asUnsigned(resultWidth))
    }

    final def + (that : Token) : Token = mkTokenU(that, this.value + that.value, scala.math.max(this.width, that.width) + 1)
    final def - (that : Token) : Token = mkTokenU(that, this.value - that.value, scala.math.max(this.width, that.width) + 1)
    final def * (that : Token) : Token = mkTokenU(that, this.value * that.value, this.width + that.width)
    final def / (that : Token) : Token = mkTokenU(that, this.value / that.value, this.width)
    final def % (that : Token) : Token = mkTokenU(that, this.value % that.value, that.width)
    final def <  (that : Token) : DFBool.Token = DFBool.Token(this.value < that.value, this.isBubble || that.isBubble)
    final def >  (that : Token) : DFBool.Token = DFBool.Token(this.value > that.value, this.isBubble || that.isBubble)
    final def <= (that : Token) : DFBool.Token = DFBool.Token(this.value <= that.value, this.isBubble || that.isBubble)
    final def >= (that : Token) : DFBool.Token = DFBool.Token(this.value >= that.value, this.isBubble || that.isBubble)
    final def == (that : Token) : DFBool.Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    final def != (that : Token) : DFBool.Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
    def select[ST <: DFAny.Token](list : List[ST]) : ST = {
      if (this.isBubble) list.head.toBubbleToken.asInstanceOf[ST]
      else list(this.value.toInt)
    }
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    val + : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l + r)
    val - : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l - r)
    val * : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l * r)
    val / : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l / r)
    val % : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l % r)
    val < : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l < r)
    val > : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l > r)
    val <= : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l <= r)
    val >= : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l >= r)
    val == : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    val != : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def select[ST <: DFAny.Token](sel : Seq[Token], list : List[Seq[ST]]) : Seq[ST] = TokenSeq(sel, list)((s, l) => s.select(l))

    def apply(width : Int, value : Int) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : Long) : Token = Token(width, BigInt(value))
    def apply(width : Int, value : BigInt) : Token = {
      if (value < 0 ) throw new IllegalArgumentException(s"Unsigned token value must not be negative. Found $value")
      assert(value.bitsWidth <= width, s"\nThe init value $value width must smaller or equal to $width")
      new Token(width, value, false)
    }
    def apply(width : Int, value : Bubble) : Token = new Token(width, 0, true)
    def apply(width : Int, token : Token) : Token = {
      assert(token.width <= width, s"\nThe init value $token width must smaller or equal to $width")
      new Token(width, token.value, token.bubble)
    }
    implicit def bubbleOf[W] : DFUInt[W] => Token = t => Token(t.width, Bubble)
    implicit val fromBits : DFBits.Token => Token = t => t.toUInt
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[LW, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFUInt[LW], Dir] = (right, dir) => new Port[LW, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.Width](DFAny.Alias.Reference.AsIs(left, s".as(DFUInt(${mold.width}))"))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntWithinWidth[LW] = CompileTime[Natural.Int.Cond[GetArg0] && (BitsWidthOf.CalcInt[GetArg0] <= LW)]
      private type LongWithinWidth[LW] = CompileTime[Natural.Long.Cond[GetArg0] && (BitsWidthOf.CalcLong[GetArg0] <= LW)]
      implicit class DFUIntBubble[LW](val right : Bubble) extends Able[DFUInt[LW]]
      implicit class DFUIntToken[LW](val right : DFUInt.Token) extends Able[DFUInt[LW]]
      implicit class DFUIntTokenSeq[LW](val right : Seq[DFUInt.Token]) extends Able[DFUInt[LW]]
      implicit class DFUIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFUInt[LW]]
      implicit class DFUIntBigInt[LW](val right : BigInt) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfInt[LW](val right : Seq[Int]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfLong[LW](val right : Seq[Long]) extends Able[DFUInt[LW]]
      implicit class DFUIntSeqOfBigInt[LW](val right : Seq[BigInt]) extends Able[DFUInt[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFUInt[LW]]]) : Seq[Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFUInt.Token(width, t)
          case (t : DFUInt.Token) => DFUInt.Token(width, t)
          case (t : Int) => DFUInt.Token(width, t)
          case (t : Long) => DFUInt.Token(width, t)
          case (t : BigInt) => DFUInt.Token(width, t)
        })

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFUInt[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(intervalSet : IntervalSet[BigInt]) extends DFAny.Pattern.OfIntervalSet[BigInt, Pattern](intervalSet)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val interval : Interval[BigInt]
    }
    object Able {
      implicit class DFUIntPatternInt[R <: Int](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFULongPatternLong[R <: Long](val right : R)(implicit di : DummyImplicit) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFUIntPatternBigInt[R <: BigInt](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(right)
      }
      implicit class DFUIntPatternRange[R <: Range](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.fromRange(right).toBigIntInterval
      }
      implicit class DFUIntPatternIntervalInt[R <: Interval[Int]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFUIntPatternIntervalLong[R <: Interval[Long]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFUIntPatternIntervalBigInt[R <: Interval[BigInt]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right
      }
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[LW] : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
        def apply[R](left: DFUInt[LW], right: Seq[Able[R]]): Pattern = {
          val reqInterval = IntervalSet(Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(left.width)))
          val patternSet = right.map(e => e.interval).foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
            if (set.intersect(interval).nonEmpty) throw new IllegalArgumentException(s"\nThe interval $interval already intersects with $set")
            if (!reqInterval.contains(interval)) throw new IllegalArgumentException(s"\nThe interval $interval is outside of range allowed by ${left.name}: $reqInterval")
            set + interval
          })

          require(patternSet.intersect(reqInterval).nonEmpty, s"\nPattern must intersect with $reqInterval. Pattern is: $patternSet")
          new Pattern(patternSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def +  [RW](right : DFUInt[RW])(implicit op: `Op+`.Builder[L, Extendable[Int], DFUInt[RW]]) = op(left, right)
      def -  [RW](right : DFUInt[RW])(implicit op: `Op-`.Builder[L, Extendable[Int], DFUInt[RW]]) = op(left, right)
      def <  [RW](right : DFUInt[RW])(implicit op: `Op<`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >  [RW](right : DFUInt[RW])(implicit op: `Op>`.Builder[L, DFUInt[RW]]) = op(left, right)
      def <= [RW](right : DFUInt[RW])(implicit op: `Op<=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def >= [RW](right : DFUInt[RW])(implicit op: `Op>=`.Builder[L, DFUInt[RW]]) = op(left, right)
      def <> [RW](port : DFAny.Connectable[DFUInt[RW]] with DFUInt[RW])(
        implicit op: `Op<>`.Builder[DFUInt[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectWith(op(port, left))
      def toDFUInt(implicit op : Const.PosOnly[Const.PosOnly[_,_],L]) = op(left)
    }
    trait Implicits {
      sealed class DFUIntFromInt[L <: Int](left : L) extends Able[L](left)
      final implicit def DFUIntFromInt[L <: Int](left: L): DFUIntFromInt[L] = new DFUIntFromInt(left)
      sealed class DFUIntFromXInt[L <: XInt](left : L) extends Able[L](left)
      final implicit def DFUIntFromXInt[L <: XInt](left: L): DFUIntFromXInt[L] = new DFUIntFromXInt(left)
      sealed class DFUIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFUIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFUIntFromLong[L] = new DFUIntFromLong(left)
      sealed class DFUIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends Able[L](left)
      final implicit def DFUIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFUIntFromXLong[L] = new DFUIntFromXLong(left)
      sealed class DFUIntFromBigInt[L <: BigInt](left : L) extends Able[L](left)
      final implicit def DFUIntFromBigInt[L <: BigInt](left: L): DFUIntFromBigInt[L] = new DFUIntFromBigInt[L](left)
      final implicit def ofDFUInt[R <: DFUInt.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFUInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait PosNeg[N] {
      type W
      def apply(value : N) : (DFUInt[W], Boolean)
    }
    object PosNeg {
      type Aux[N, W0] = PosNeg[N]{type W = W0}
      import singleton.ops.math.Abs
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Int[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (new Const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Const.Context, w : BitsWidthOf.Long[Abs[N]])
      : Aux[N, w.Out] = new PosNeg[N] {
        type W = w.Out
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = scala.math.abs(value)
          (new Const[W](Token(w(absValue), absValue)), value < 0)
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[N, Int] = new PosNeg[N] {
        type W = Int
        def apply(value : N) : (DFUInt[W], Boolean) = {
          val absValue = value.abs
          (new Const[W](Token(absValue.bitsWidth, absValue)), value < 0)
        }
      }
    }
    trait PosOnly[Sym, N] {
      type W
      def apply(value : N) : DFUInt[W]
    }
    object PosOnly {
      type Aux[Sym, N, W0] = PosOnly[Sym, N]{type W = W0}
      object `N >= 0` extends `N >= 0` {
        type MsgCommon[N] = "Operation or assignment do not permit a negative number. Found literal: " + ToString[N]
      }
      implicit def fromInt[Sym, N <: Int](
        implicit
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Int.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Int[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          new Const[W](Token(w(value), value))
        }
      }
      implicit def fromLong[Sym, N <: Long](
        implicit
        ctx : DFAny.Const.Context,
        checkPos : `N >= 0`.Long.CheckedShellSym[Sym, N],
        w : BitsWidthOf.Long[N]
      ) : Aux[Sym, N, w.Out] = new PosOnly[Sym, N] {
        type W = w.Out
        def apply(value : N) : DFUInt[W] = {
          checkPos.unsafeCheck(value)
          new Const[W](Token(w(value), value))
        }
      }
      implicit def fromBigInt[Sym, N <: BigInt](implicit ctx : DFAny.Const.Context)
      : Aux[Sym, N, Int] = new PosOnly[Sym, N] {
        type W = Int
        def apply(value : N) : DFUInt[W] = {
          `N >= 0`.BigInt.unsafeCheck(value)
          new Const[W](Token(value.bitsWidth, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>`[SkipLengthCheck] extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = SkipLengthCheck || (LW >= RW)
        type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, R, RW](properR : (L, R) => DFUInt[RW]) : Aux[L, R, DFUInt[RW]] =
        new Builder[L, R] {
          type Comp = DFUInt[RW]
          def apply(leftL : L, rightR : R) : Comp =  properR(leftL, rightR)
        }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Alias.Context,
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], DFUInt[RW], DFUInt[LW]] =
        create[DFUInt[LW], DFUInt[RW], LW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right.protToWidth[LW](left.width)
        })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx :  DFAny.Alias.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `LW >= RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFUInt[LW], R, DFUInt[LW]] = create[DFUInt[LW], R, LW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        new Const[LW](Token(left.width, right.constLB.get))
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`[false]
  object `Op<>` extends `Ops:=,<>`[true]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[K <: `Ops+Or-`.Kind](kind : K) {
    //NCW = No-carry width
    //WCW = With-carry width
    final class Component[NCW, WCW](val wc : Func2Comp[_,_,_] with DFUInt[WCW])(implicit ctx : DFAny.Alias.Context) extends
      DFAny.Alias[DFUInt[NCW]](DFAny.Alias.Reference.BitsWL(wc, wc.width-1, 0,if (wc.isFolded) "" else s".bits(${wc.width-2}, 0).uint")) with DFUInt[NCW] with CompAlias {
      lazy val c = new DFBool.Alias(DFAny.Alias.Reference.BitsWL(wc, 1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      lazy val comp = wc
      lazy val bypassAlias = c.isNotDiscovered
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = Max[LW, RW] + 1
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW]
            def apply(properLR : (L, R) => (`Ops+Or-`.Kind, DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import stdlib.DFUIntOps._
                  val (creationKind, left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = wcW(left.width, right.width)
                  val opInst = creationKind match {
                    case `Ops+Or-`.+ => `Func2Comp+`[LW, RW, WCW](left, right)
                    case `Ops+Or-`.- => `Func2Comp-`[LW, RW, WCW](left, right)
                  }
                  opInst.__dev.setAutoName(s"${ctx}WC")
                  // Creating extended component aliasing the op
                  new Component[NCW, WCW](opInst)
                }
              }
          }
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((left, right) => (kind, left, right))

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosNeg.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val (right, negative) = rConst(rightNum)
        val creationKind = if (negative) -kind else kind
        (creationKind, left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (kind, lConst(leftNum), right)
      })
    }
  }
  protected object `Ops+Or-` {
    abstract class Kind(val opString : String) {
      def unary_- : Kind
    }
    case object + extends Kind("+") {def unary_- : Kind = `Ops+Or-`.-}
    case object - extends Kind("-") {def unary_- : Kind = `Ops+Or-`.+}
  }
  object `Op+` extends `Ops+Or-`(`Ops+Or-`.+)
  object `Op-` extends `Ops+Or-`(`Ops+Or-`.-)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // * operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op*` {
    //NCW = No-carry width
    //WCW = With-carry width
    //CW = Carry width
    final class Component[NCW, WCW, CW](val wc : Func2Comp[_,_,_] with DFUInt[WCW], ncW : TwoFace.Int[NCW], cW : TwoFace.Int[CW])(
      implicit ctx : DFAny.Alias.Context
    ) extends DFAny.Alias[DFUInt[NCW]](DFAny.Alias.Reference.BitsWL(wc, ncW, 0, if(wc.isFolded) "" else s".bits(${wc.width-cW-1}, 0).uint")) with DFUInt[NCW] with CompAlias {
      lazy val c = new DFBits.Alias[CW](DFAny.Alias.Reference.BitsWL(wc, cW, wc.width - cW, s".bits(${wc.width-1}, ${wc.width-cW})")).setAutoName(s"${ctx}C")
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
      lazy val comp = wc
      lazy val bypassAlias = c.isNotDiscovered
    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Op `*` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Comp = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[Sym, LW, LE, RW] = CheckedShellSym[Sym, LW, ITE[IsBoolean[LE], 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcWCW[LW, RW] = LW + RW
        type WCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcWCW, LW, Int, RW, Int, ResW]
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
        type CalcCW[LW, RW] = CalcWCW[LW, RW] - CalcNCW[LW, RW]
        type CW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, CW, NCW, WCW](
          implicit
          ctx : DFAny.Op.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          wcW : Inference.WCW[LW, RW, WCW],
          cW : Inference.CW[LW, RW, CW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[Builder[_,_,_], LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Comp = Component[NCW, WCW, CW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Comp = Component[NCW, WCW, CW]
            def apply(properLR : (L, R) => (DFUInt[LW], DFUInt[RW])) : Builder.Aux[L, LE, R, Comp] =
              new Builder[L, LE, R] {
                type Comp = Component[NCW, WCW, CW]
                def apply(leftL : L, rightR : R) : Comp = {
                  import stdlib.DFUIntOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val ncWidth = ncW(left.width, right.width)
                  val cWidth = cW(left.width, right.width)

                  val opInst = `Func2Comp*`[LW, RW, WCW](left, right)
                  opInst.__dev.setAutoName(s"${ctx}WC")

                  // Creating extended component aliasing the op
                  new Component[NCW, WCW, CW](opInst, ncWidth, cWidth)
                }
              }
          }
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, LE, R <: DFUInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_,_], R, RW],
        detailedBuilder: DetailedBuilder[DFUInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFUInt[L, LW, LE, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_,_], L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFUInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool with CanBePiped}

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      object `VecW >= ConstW` extends Checked1Param.Int { //Needs to be mitigated to a warning
        type Cond[VW, CW] = VW >= CW
        type Msg[VW, CW] = "A static boolean result detected, due to an unsigned comparison between a DF variable and a larger number. Found: DFVar-width = "+ ToString[VW] + " and Num-width = " + ToString[CW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFUInt[LW], DFUInt[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import stdlib.DFUIntOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => `Func2Comp==`(left, right)
          case DiSoOp.Kind.!= => `Func2Comp!=`(left, right)
          case DiSoOp.Kind.<  => `Func2Comp<`(left, right)
          case DiSoOp.Kind.>  => `Func2Comp>`(left, right)
          case DiSoOp.Kind.<= => `Func2Comp<=`(left, right)
          case DiSoOp.Kind.>= => `Func2Comp>=`(left, right)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.__dev.setAutoName(s"${ctx}")
        opInst
      }

      implicit def evDFUInt_op_DFUInt[L <: DFUInt[LW], LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFUInt[LW], DFUInt[RW]] = create[DFUInt[LW], LW, DFUInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFUInt_op_Const[L <: DFUInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.PosOnly.Aux[Builder[_,_], R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFUInt[LW], R] = create[DFUInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFUInt[L, LW, R <: DFUInt[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.PosOnly.Aux[Builder[_,_], L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFUInt[RW]] = create[L, LW, DFUInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  object `Op<`  extends OpsCompare(DiSoOp.Kind.< )
  object `Op>`  extends OpsCompare(DiSoOp.Kind.> )
  object `Op<=` extends OpsCompare(DiSoOp.Kind.<=)
  object `Op>=` extends OpsCompare(DiSoOp.Kind.>=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}