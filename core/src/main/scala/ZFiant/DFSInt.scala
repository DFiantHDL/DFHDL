package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.Func2
import ZFiant.compiler.printer.Printer

object DFSInt extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token[W]
    type TPattern = DFSInt.Pattern
    type TPatternAble[+R] = DFSInt.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFSInt.Pattern.Builder[LType]
    type OpAble[R] = DFSInt.Op.Able[R]
    type `Op==Builder`[L, R] = DFSInt.`Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = DFSInt.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = DFSInt.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = DFSInt.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFSInt.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFSInt.Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token = fromToken.toSInt
    override def toString: String = s"DFSInt[$width]"
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      s"$TP DFSInt($LIT$width)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(width) => this.width.getValue == width.getValue
      case _ => false
    }
  }
  trait Extendable

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : SIntWidth.Checked[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedWidth))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token[W](width : TwoFace.Int[W], value : BigInt, bubble : Boolean) extends DFAny.Token.Of[BigInt, W] { //with DFAny.Token.Resizable
    lazy val valueBits : XBitVector[W] = value.toBitVector(width)
    lazy val bubbleMask: XBitVector[W] = bubble.toBitVector(width)
    def mkTokenS[T <: DFAny.Token, OW](that : T, result : BigInt, resultWidth : TwoFace.Int[OW]) : Token[OW] = {
      if (this.isBubble || that.isBubble) Token(resultWidth, Bubble)
      else Token(resultWidth, result)
    }
    def + [RW](that : Token[RW])(
      implicit ncW : `Op+`.Builder.Inference.NCW[W, RW, _]
    ) : Token[ncW.Out] = mkTokenS(that, this.value + that.value, ncW(this.width, that.width))
    def - [RW](that : Token[RW])(
      implicit ncW : `Op-`.Builder.Inference.NCW[W, RW, _]
    ) : Token[ncW.Out] = mkTokenS(that, this.value - that.value, ncW(this.width, that.width))

//    final def * [RW](that : Token[RW]) : Token = mkTokenU(that, this.value * that.value, this.width + that.width)
//    final def / [RW](that : Token[RW]) : Token = mkTokenU(that, this.value / that.value, this.width)
//    final def % [RW](that : Token[RW]) : Token = mkTokenU(that, this.value % that.value, that.width)
    def <  [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value < that.value, this.isBubble || that.isBubble)
    def >  [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value > that.value, this.isBubble || that.isBubble)
    def <= [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value <= that.value, this.isBubble || that.isBubble)
    def >= [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value >= that.value, this.isBubble || that.isBubble)
    def == [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    def != [RW](that : Token[RW]) : DFBool.Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
    def << [RW](that : DFUInt.Token[RW]) : Token[W] = mkTokenS(that, this.value << that.value.toInt, this.width)
    def >> [RW](that : DFUInt.Token[RW]) : Token[W] = mkTokenS(that, this.value >> that.value.toInt, this.width)
    def resize[RW](toWidth : TwoFace.Int[RW]) : Token[RW] = {
      if (toWidth > width) Token(toWidth, value, bubble)
      else if (toWidth < width) bits.resize(toWidth).toSInt
      else this.asInstanceOf[Token[RW]]
    }
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      if (value.isValidInt) s"$LIT$value"
      else if (value.isValidLong) s"$LIT${value}L"
      else s"""$LIT BigInt($STR"$value")"""
    }
  }

  object Token {
    implicit def bubbleOfToken[W] : DFAny.Token.BubbleOfToken[Token[W]] = t => Token[W](t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => Token[W](t.width, Bubble)

    def apply[W](width : TwoFace.Int[W], value : Int) : Token[W] = Token(width, BigInt(value))
    def apply[W](width : TwoFace.Int[W], value : Long) : Token[W] = Token(width, BigInt(value))
    def apply[W](width : TwoFace.Int[W], value : BigInt) : Token[W] = {
      assert(value.bitsWidth <= width, s"\nThe init value $value width must smaller or equal to $width")
      new Token(width, value, false)
    }
    def apply[W](width : TwoFace.Int[W], value : Bubble) : Token[W] = new Token(width, 0, true)
    def apply[W](width : TwoFace.Int[W], token : Token[W]) : Token[W] = {
      assert(token.width <= width, s"\nThe init value $token width must smaller or equal to $width")
      new Token(width, token.value, token.bubble)
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
      implicit class DFSIntPatternInt[R <: Int](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFULongPatternLong[R <: Long](val right : R)(implicit di : DummyImplicit) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFSIntPatternBigInt[R <: BigInt](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(right)
      }
      implicit class DFSIntPatternRange[R <: Range](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.fromRange(right).toBigIntInterval
      }
      implicit class DFSIntPatternIntervalInt[R <: Interval[Int]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalLong[R <: Interval[Long]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right.toBigIntInterval
      }
      implicit class DFSIntPatternIntervalBigInt[R <: Interval[BigInt]](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val reqInterval = IntervalSet(Interval.closed(BigInt.minSignedFromWidth(left.width), BigInt.maxSignedFromWidth(left.width)))
          val patternSet = right.map(e => e.interval).foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
            if (set.intersect(interval).nonEmpty) throw new IllegalArgumentException(s"\nThe interval $interval already intersects with $set")
            if (!reqInterval.contains(interval)) throw new IllegalArgumentException(s"\nThe interval $interval is outside of range allowed: $reqInterval")
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
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntWithinWidth[LW] = CompileTime[BitsWidthOf.Signed.CalcInt[GetArg0] <= LW]
      private type LongWithinWidth[LW] = CompileTime[BitsWidthOf.Signed.CalcLong[GetArg0] <= LW]
      implicit class DFSIntBubble[LW](val right : Bubble) extends Able[DFSInt[LW]]
      implicit class DFSIntToken[LW](val right : DFSInt.Token[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntTokenSeq[LW](val right : Seq[DFSInt.Token[LW]]) extends Able[DFSInt[LW]]
      implicit class DFSIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFSInt[LW]]
      implicit class DFSIntBigInt[LW](val right : BigInt) extends Able[DFSInt[LW]]
      implicit class DFSIntSeqOfInt[LW](val right : Seq[Int]) extends Able[DFSInt[LW]]
      implicit class DFSIntSeqOfLong[LW](val right : Seq[Long]) extends Able[DFSInt[LW]]
      implicit class DFSIntSeqOfBigInt[LW](val right : Seq[BigInt]) extends Able[DFSInt[LW]]

      def toTokenSeq[LW](width : TwoFace.Int[LW], right : Seq[Able[DFSInt[LW]]]) : Seq[Token[LW]] =
        right.toSeqAny.collect {
          case (t : Bubble) => DFSInt.Token(width, t)
          case (t : Token[_]) => t.asInstanceOf[Token[LW]]
          case (t : Int) => DFSInt.Token(width, t)
          case (t : Long) => DFSInt.Token(width, t)
          case (t : BigInt) => DFSInt.Token(width, t)
        }

    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFSInt[LW], Token[LW]] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : DFSInt[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromInt[N <: Int](implicit ctx : DFAny.Context, w : BitsWidthOf.Signed.Int[N])
      : Aux[N, w.Out] = new Builder[N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = {
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromLong[N <: Long](implicit ctx : DFAny.Context, w : BitsWidthOf.Signed.Long[N])
      : Aux[N, w.Out] = new Builder[N] {
        type W = w.Out
        def apply(value : N) : DFSInt[W] = {
          val width = w(value)
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
      implicit def fromBigInt[N <: BigInt](implicit ctx : DFAny.Context)
      : Aux[N, Int] = new Builder[N] {
        type W = Int
        def apply(value : N) : DFSInt[W] = {
          val width = value.bitsWidth
          DFAny.Const[Type[W]](Type(width), Token(width, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
      final def +   [RW](right : DFSInt[RW])(implicit op: `Op+`.Builder[L, false, DFSInt[RW]]) = op(left, right)
      final def -   [RW](right : DFSInt[RW])(implicit op: `Op-`.Builder[L, false, DFSInt[RW]]) = op(left, right)
      final def <   [RW](right : DFSInt[RW])(implicit op: `Op<`.Builder[L, DFSInt[RW]]) = op(left, right)
      final def >   [RW](right : DFSInt[RW])(implicit op: `Op>`.Builder[L, DFSInt[RW]]) = op(left, right)
      final def <=  [RW](right : DFSInt[RW])(implicit op: `Op<=`.Builder[L, DFSInt[RW]]) = op(left, right)
      final def >=  [RW](right : DFSInt[RW])(implicit op: `Op>=`.Builder[L, DFSInt[RW]]) = op(left, right)
      final def === [RW](right : DFSInt[RW])(implicit op: `Op===`.Builder[L, DFSInt[RW]]) = op(left, right)
      final def =!= [RW](right : DFSInt[RW])(implicit op: `Op=!=`.Builder[L, DFSInt[RW]]) = op(left, right)
    }
    trait Implicits {
      sealed class DFSIntFromInt[L <: Int](left : L) extends AbleOps[L](left)
      final implicit def DFSIntFromInt[L <: Int](left: L): DFSIntFromInt[L] = new DFSIntFromInt(left)
      sealed class DFSIntFromXInt[L <: XInt](left : L) extends AbleOps[L](left)
      final implicit def DFSIntFromXInt[L <: XInt](left: L): DFSIntFromXInt[L] = new DFSIntFromXInt(left)
      sealed class DFSIntFromLong[L <: Long](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def DFSIntFromLong[L <: Long](left: L)(implicit di: DummyImplicit): DFSIntFromLong[L] = new DFSIntFromLong(left)
      sealed class DFSIntFromXLong[L <: XLong](left : L)(implicit di : DummyImplicit) extends AbleOps[L](left)
      final implicit def DFSIntFromXLong[L <: XLong](left: L)(implicit di: DummyImplicit): DFSIntFromXLong[L] = new DFSIntFromXLong(left)
      sealed class DFSIntFromBigInt[L <: BigInt](left : L) extends AbleOps[L](left)
      final implicit def DFSIntFromBigInt[L <: BigInt](left: L): DFSIntFromBigInt[L] = new DFSIntFromBigInt[L](left)
      sealed class DFSIntFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) extends AbleOps[DFSInt[W]](left)
      final implicit def DFSIntFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) : DFSIntFromDefaultRet[W] = new DFSIntFromDefaultRet(left)
      final implicit def ofDFSInt[W](left : DFSInt[W]) : Able[DFSInt[W]] = new Able(left)
      implicit class ExtendableDFSIntOps[LW](val left : DFSInt[LW] with Extendable){
        final def +  [R](right : Able[R])(implicit op: `Op+`.Builder[DFSInt[LW], true, R]) = op(left, right)
        final def -  [R](right : Able[R])(implicit op: `Op-`.Builder[DFSInt[LW], true, R]) = op(left, right)
      }
      final implicit class DFSIntOps[LW](val left : DFSInt[LW]){
        def maxValue : BigInt = BigInt(2) << (left.width - 1) - 1
        def sign(implicit ctx : DFAny.Context) : DFBool = left.asInstanceOf[DFSInt[Int]].bit(left.width-1)
        def unary_- (implicit op: `Op-`.Builder[0, false, DFSInt[LW]]) = op(0, left)
        def +   [R](right : Able[R])(implicit op: `Op+`.Builder[DFSInt[LW], false, R]) = op(left, right)
        def -   [R](right : Able[R])(implicit op: `Op-`.Builder[DFSInt[LW], false, R]) = op(left, right)
        def <   [R](right : Able[R])(implicit op: `Op<`.Builder[DFSInt[LW], R]) = op(left, right)
        def >   [R](right : Able[R])(implicit op: `Op>`.Builder[DFSInt[LW], R]) = op(left, right)
        def <=  [R](right : Able[R])(implicit op: `Op<=`.Builder[DFSInt[LW], R]) = op(left, right)
        def >=  [R](right : Able[R])(implicit op: `Op>=`.Builder[DFSInt[LW], R]) = op(left, right)
        def === [R](right : Able[R])(implicit op: `Op===`.Builder[DFSInt[LW], R]) = op(left, right)
        def =!= [R](right : Able[R])(implicit op: `Op=!=`.Builder[DFSInt[LW], R]) = op(left, right)
        def << [R](right: DFUInt.Op.Able[R])(implicit op: `Op<<`.Builder[DFSInt[LW], R]) = op(left, right)
        def >> [R](right: DFUInt.Op.Able[R])(implicit op: `Op>>`.Builder[DFSInt[LW], R]) = op(left, right)
        def resize[RW](toWidth : SIntWidth.Checked[RW])(implicit ctx : DFAny.Context) =
          DFAny.Alias.Resize.sint(left, toWidth)
        def extendable : DFSInt[LW] with Extendable = left.asInstanceOf[DFSInt[LW] with Extendable]
      }
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, R] extends DFAny.Op.Builder[LType, R] {
      type Out = DFAny.Of[LType]
    }

    object Builder {
      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "An assignment operation does not permit a wider RHS expression. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFSInt_op_DFSInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW >= RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFSInt[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFSInt_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW >= RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token[_], Token[_]) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

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

      def create[L, LW, R, RW](properLR : (L, R) => (DFSInt[LW], DFSInt[RW]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(), left, op, right)(func)
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFSInt[LW], DFSInt[RW]] = create[DFSInt[LW], LW, DFSInt[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, LW, RW]
      ) : Builder[DFSInt[LW], R] = create[DFSInt[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFSInt[L, LW, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `VecW >= ConstW`.CheckedShellSym[Warn, RW, LW]
      ) : Builder[L, DFSInt[RW]] = create[L, LW, DFSInt[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(right.width, left.width)
        (left, right)
      })
    }
  }
  object `Op==`  extends OpsCompare(Func2.Op.==)(_ == _) with `Op==`
  object `Op!=`  extends OpsCompare(Func2.Op.!=)(_ != _) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)(_ == _)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)(_ != _)
  object `Op<`   extends OpsCompare(Func2.Op.< )(_ <  _)
  object `Op>`   extends OpsCompare(Func2.Op.> )(_ >  _)
  object `Op<=`  extends OpsCompare(Func2.Op.<=)(_ <= _)
  object `Op>=`  extends OpsCompare(Func2.Op.>=)(_ >= _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[Op <: Func2.Op.Negateable](op : Op) {
    //NCW = No-carry width
    //WCW = With-carry width
//    final class Component[NCW, WCW](_wc : DFFunc2[_,_,_] with DFSInt[WCW])(implicit ctx : DFAny.Context) extends
//      DFAny.Alias[DFSInt[NCW]](DFAny.Alias.Reference.Resize(_wc, _wc.width-1)) with DFSInt[NCW] with CompAlias { //,if (wc.isFolded) "" else s".bits(${wc.width-2}, 0).uint"
//      lazy val wc = {_wc.usedAsWide = true; _wc}
//      lazy val c = new DFBool.Alias(DFAny.Alias.Reference.BitsWL(wc, 1, wc.width-1, s".bit(${wc.width-1})")).setAutoName(s"${ctx}C")
//      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token.toUInt
//      lazy val comp = wc
//      //      lazy val bypassAlias = c.isNotDiscovered
//    }

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[L, LE, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object `LW >= RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW >= RW
        type Msg[LW, RW] = "Operation does not permit a LHS-width("+ ToString[LW] + ") smaller than RHS-width(" + ToString[RW] + ")"
        type ParamFace = Int
        type CheckedExtendable[LW, LE, RW] = CheckedShell[LW, ITE[LE, 0, RW]]
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcNCW[LW, RW] = Max[LW, RW]
        type NCW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcNCW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, LE, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, LE, R, RW, NCW, WCW](
          implicit
          ctx : DFAny.Context,
          ncW : Inference.NCW[LW, RW, NCW],
          checkLWvRW : `LW >= RW`.CheckedExtendable[LW, LE, RW]
        ) : DetailedBuilder[L, LW, LE, R, RW]{type Out = DFSInt[NCW]} =
          new DetailedBuilder[L, LW, LE, R, RW]{
            type Out = DFSInt[NCW]
            def apply(properLR : (L, R) => (DFSInt[LW], DFSInt[RW])) : Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFSInt[NCW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opWidth = ncW(left.width, right.width)
                  val out = Type(opWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = op match {
                    case _ : Func2.Op.+ => _ + _
                    case _ : Func2.Op.- => _ - _
                    case _ => ???
                  }
                  DFAny.Func2(out, left, op, right)(func)
                }
              }
          }
      }

      implicit def evDFSInt_op_DFSInt[L <: DFSInt[LW], LW, LE, R <: DFSInt[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFSInt_op_Const[L <: DFSInt[LW], LW, LE, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFSInt[LW], LW, LE, R, RW]
      ) = detailedBuilder((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFSInt[L, LW, LE, R <: DFSInt[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, LE, DFSInt[RW], RW]
      ) = detailedBuilder((leftNum, right) => {
        (lConst(leftNum), right)
      })
    }
  }
  object `Op+`  extends `Ops+Or-`(Func2.Op.+)
  object `Op-`  extends `Ops+Or-`(Func2.Op.-)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op<<` extends OpsShift[Type](Func2.Op.<<) {
    def tokenFunc[LW, RW](left: DFSInt.Token[LW], right: DFUInt.Token[RW]) : DFSInt.Token[LW] = left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW, RW](left: DFSInt.Token[LW], right: DFUInt.Token[RW]) : DFSInt.Token[LW] = left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
