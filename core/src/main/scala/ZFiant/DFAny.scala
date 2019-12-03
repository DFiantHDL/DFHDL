package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

trait DFType {
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
  protected type OpAble[R] <: DFAny.Op.Able[R]
  type `Op==Builder`[-L, -R] <: DFAny.`Op==Builder`[L, R]
}
object DFType {
  implicit def ev[T <: DFAny](t : T) : t.TType = t.dfType
}

trait DFAny extends DFMember with Product with Serializable {
  type TType <: DFType
  type TVar
  val dfType : TType
  type Width = dfType.Width
  type TToken = dfType.TToken
  final lazy val width = dfType.width
  final protected val left : this.type = this
  protected type AsVal = DFAny.Val[TType]
  protected type AsVar = DFAny.Var[TType]
  protected type AsType[T <: DFType] = DFAny.ValOrVar[T, TVar]
  protected type This = DFAny.Of[TType]

//  protected type `Op<>Builder`[R] <: DFAny.Op.Builder[This, R]
//  protected type `Op:=Builder`[R] <: DFAny.Op.Builder[This, R]

//  @scala.annotation.implicitNotFound("shit")
//  type `Op==Builder`[R] <: dfType.`Op==Builder`[This, R]
//  protected type `Op!=Builder`[R] <: DFAny.`Op!=Builder`[This, R]
  protected type InitAble[L <: DFAny] <: DFAny.Init.Able[L]
  protected type InitBuilder <: DFAny.Init.Builder[This, InitAble, TToken]
}

object DFAny {
  trait Context extends DFMember.Context

  trait Of[Type <: DFType] extends DFAny {
    type TType = Type
    //////////////////////////////////////////////////////////////////////////
    // Bit range selection
    //////////////////////////////////////////////////////////////////////////
    final def bits(implicit ctx : DFAny.Context) : AsType[DFBits[dfType.Width]] =
      DFAny.Alias.BitsWL(this, dfType.width, 0)

    final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
      implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
    ) : AsType[DFBits[relWidth.Out]] =
      DFAny.Alias.BitsWL(this, relWidth(relBitHigh, relBitLow), relBitLow)

    final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
      implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
    ) : AsType[DFBits[relWidth.Out]] = {
      checkHiLow.unsafeCheck(relBitHigh, relBitLow)
      protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
    }

    final protected def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(
      implicit ctx : DFAny.Context
    ) : AsType[DFBits[W]] = DFAny.Alias.BitsWL(this, relWidth, relBitLow)

    final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
      implicit checkRelWidth : PartWidth.CheckedShell[W, Width - L], ctx : DFAny.Context
    ) : AsType[DFBits[W]] = {
      checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
      protBitsWL(relWidth, relBitLow.unsafeCheck(width))
    }
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Prev
    //////////////////////////////////////////////////////////////////////////
    final protected def protPrev(step : Int)(implicit ctx : DFAny.Context)
    : AsVal = DFAny.Alias.Prev(this, step)
    final def prev()(implicit ctx : DFAny.Context) : AsVal = protPrev(1)
    final def prev[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Context) : AsVal = protPrev(step)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Casting/Aliasing
    //////////////////////////////////////////////////////////////////////////
    final def as[AT <: DFType](aliasType : AT)(implicit ctx : DFAny.Context) : AsType[AT] =
      DFAny.Alias.AsIs(aliasType, this)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Equality
    //////////////////////////////////////////////////////////////////////////
    final def == [R](right : R)(implicit op: dfType.`Op==Builder`[This, R]) = op(left, right)
//      final def != [R <: TUnbounded](right : R)(implicit op: `Op!=Builder`[right.TVal]) = op(left, right.tVal)
    //////////////////////////////////////////////////////////////////////////
  }

  trait ValOrVar[Type <: DFType, Var] extends DFAny.Of[Type] {
    type TVar = Var
  }

  type Val[Type <: DFType] = ValOrVar[Type, false]
  type Var[Type <: DFType] = ValOrVar[Type, true]

  trait `Op:=`[To <: DFAny, From] {
    def apply(left : To, right : From) : Unit = {}
  }

  implicit class VarOps[L <: DFAny.Var[_ <: DFType]](left : L) {
    def := [R](right : R)(implicit op : `Op:=`[L, R]) : Unit = op(left, right)
  }

  abstract class Constructor[Type <: DFType, Var] extends ValOrVar[Type, Var] {
    val ctx : DFAny.Context
  }

  final case class Const[Type <: DFType](dfType : Type, token : Type#TToken)(
    implicit val ctx : DFAny.Context
  ) extends Constructor[Type, false]

  sealed trait Initializable[Type <: DFType, Var] extends Constructor[Type, Var] {
    val externalInit : Seq[Type#TToken]
  }

  sealed trait Port[Type <: DFType, Var] extends Initializable[Type, Var] {
    val dir : Port.Dir
  }
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    final case class In[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
      implicit val ctx : DFAny.Context
    ) extends Port[Type, false] {
      val dir : Port.Dir = Dir.IN
    }
    final case class Out[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
      implicit val ctx : DFAny.Context
    ) extends Port[Type, true] {
      val dir : Port.Dir = Dir.OUT
    }
  }

  final case class NewVar[Type <: DFType](dfType : Type, externalInit : Seq[Type#TToken])(
    implicit val ctx : DFAny.Context
  ) extends Initializable[Type, true] {
    def ifdf(cond : ValOrVar[DFBool, _])(block : => ValOrVar[Type, _])(implicit ctx : DFBlock.Context)
    : ConditionalBlock.WithRetVal.IfBlock[Type] = ConditionalBlock.WithRetVal.IfBlock[Type](dfType, cond, () => block)
    override def toString: String = dfType.toString
  }

  trait Alias[Type <: DFType, RefVal <: DFAny, Var] extends Constructor[Type, Var] {
    val refVal : RefVal
  }
  object Alias {
    final case class AsIs[Type <: DFType, RefVal <: DFAny](dfType : Type, refVal : RefVal)(
      implicit val ctx : DFAny.Context
    ) extends Alias[Type, RefVal, RefVal#TVar]
    final case class BitsWL[W, L, RefVal <: DFAny](refVal : RefVal, relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(
      implicit val ctx : DFAny.Context
    ) extends Alias[DFBits[W], RefVal, RefVal#TVar]{
      val dfType : TType = DFBits.dfType(relWidth)
    }
    final case class Prev[RefVal <: DFAny](refVal : RefVal, step : Int)(
      implicit val ctx : DFAny.Context
    ) extends Alias[RefVal#TType, RefVal, false] {
      val dfType : TType = refVal.dfType
    }
  }

  sealed abstract class Func[Type <: DFType] extends Constructor[Type, false]
  final case class Func2[Type <: DFType, L <: DFAny, Op <: DiSoOp, R <: DFAny](dfType: Type, leftArg : L, op : Op, rightArg : R)(
    implicit val ctx : DFAny.Context
  ) extends Func[Type]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token extends Product with Serializable {
    type TValue
    type Width
    //maximum token value width
    val width : TwoFace.Int[Width]
    val value : TValue
    val bubbleMask : XBitVector[Width]
    val valueBits : XBitVector[Width]
    final def isBubble : Boolean = !(bubbleMask === BitVector.low(width.getValue))
    final def bits : DFBits.Token[Width] = DFBits.Token(width, valueBits, bubbleMask)
    final def bit(relBit : Int) : DFBool.Token = {
      val outBitsValue = valueBits.bit(relBit)
      val outBubbleMask = bubbleMask.bit(relBit)
      DFBool.Token(outBitsValue, outBubbleMask)
    }
    final def bitsWL[W](relWidth : TwoFace.Int[W], relBitLow : Int) : DFBits.Token[W] = {
      val outBitsValue = valueBits.bitsWL(relWidth, relBitLow)
      val outBubbleMask = bubbleMask.bitsWL(relWidth, relBitLow)
      DFBits.Token(relWidth, outBitsValue, outBubbleMask)
    }
  }
  object Token {
    trait Of[Value, W] extends Token {
      type TValue = Value
      type Width = W
    }
    trait BubbleOfToken[T <: Token] {
      def apply(t : T) : T
    }
    trait BubbleOfDFType[Type <: DFType] {
      def apply(t : Type) : Type#TToken
    }
    implicit class TokenSeqInit[T <: Token](tokenSeq : Seq[T]) {
      def prevInit(step : Int) : Seq[T] = {
        val length = tokenSeq.length
        //No init at all, so invoking prev does not change anything (bubble tokens will be used)
        if ((length == 0) || (step == 0)) tokenSeq
        //The step is larger or equals to the init sequence, so only the last init token remains
        else if (length <= step) Seq(tokenSeq.last)
        //More tokens are available than the step size, so we drop the first, according to the step count
        else tokenSeq.drop(step)
      }
      def bits : Seq[DFBits.Token[T#Width]] =
        tokenSeq.map(t => t.bits.asInstanceOf[DFBits.Token[T#Width]])
      def bitsWL[W](relWidth : TwoFace.Int[W], relBitLow : Int) : Seq[DFBits.Token[W]] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      //      def patternMatch(pattern : T#TPattern) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
    }
  }
  //  trait Token {self =>
  //    type TValue
  //    protected[DFiant] type TToken <: Token
  //    protected[DFiant] type TPattern <: DFAny.Pattern[TPattern]{type TValue = self.TValue}
  //    val value : TValue
  //    val width : Int
  //    final lazy val widthOfValue : Int = scala.math.max(valueBits.lengthOfValue, bubbleMask.lengthOfValue).toInt
  //    val valueBits : BitVector
  //    val bubbleMask : BitVector
  //    //leading zero counter
  //    final lazy val lzc : Int = scala.math.min(valueBits.lzc, bubbleMask.lzc).toInt
  //    def toBubbleToken : Token
  //
  //    final def patternMatch(that : TPattern) : DFBool.Token = DFBool.Token(that.matches(this.value), this.isBubble)
  //  }
  //
  //  object Token {
  ////    trait Resizable extends Token {
  ////      def resize(toWidth : Int) : TToken
  ////    }
  ////    abstract class Of[V, P <: DFAny.Pattern[P]{type TValue = V}](implicit codeStringOf : CodeStringOf[V]) extends Token {
  ////      type TValue = V
  ////      protected[DFiant] type TPattern = P
  ////      final def codeString : String = if (isBubble) "Φ" else value.codeString
  ////    }
  ////    def patternMatch[T <: Token, P <: Pattern[_]](tokenSeq : Seq[T], pattern : P) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
  //  }
  //
  object TokenSeq {
    def apply[O <: Token, T1 <: Token, T2 <: Token, T3 <: Token](t1 : Seq[T1], t2 : Seq[T2], t3 : Seq[T3])(op : (T1, T2, T3) => O) : Seq[O] =
      if (t1.isEmpty || t2.isEmpty || t3.isEmpty) Seq() else{
        val leftSeq = t1
        val rightSeq = t2
        val leftSeq2 = leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last)
        val rightSeq2 = t3
        leftSeq2.zipAll(rightSeq2, leftSeq2.last, rightSeq2.last).map(t => op(t._1._1, t._1._2, t._2))
      }
    def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
      if (leftSeq.isEmpty || rightSeq.isEmpty) Seq() else
        leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
    def apply[O <: Token, L <: Token, R](leftSeq : Seq[L], rightConst : R)(op : (L, R) => O) : Seq[O] =
      leftSeq.map(t => op(t, rightConst))
    def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
      seq.map(t => op(t))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Pattern[P <: Pattern[P]] {
    type TValue
    def matches(value : TValue) : Boolean
    def overlapsWith(pattern: P) : Boolean
    def codeString : String
    override def toString: String = codeString
  }
  object Pattern {
    abstract class OfIntervalSet[T, P <: OfIntervalSet[T, P]](val patternSet : IntervalSet[T])(implicit codeStringOf: CodeStringOf[Interval[T]]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.containsPoint(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    abstract class OfSet[T, P <: OfSet[T, P]](val patternSet : Set[T])(implicit codeStringOf: CodeStringOf[T]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.contains(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    trait Able[+R] {
      val right : R
    }

    //    trait Builder[L <: DFAny, Able[+R] <: Pattern.Able[R]] {
    //      def apply[R](left : L, right : Seq[Able[R]]) : L#TPattern
    //    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init {
    trait Able[L <: DFAny] {
      val right : Any

      override def toString: String = right.toString
    }
    object Able {
      implicit class AbleSeq[L <: DFAny](s : Seq[Able[L]]) {
        private def flatten(s: Seq[Any]): Seq[Any] = s flatMap {
          case ss: Seq[_] => flatten(ss)
          case e => Seq(e)
        }
        def toSeqAny : Seq[Any] = {
          flatten(s.map(e => e.right))
        }

        override def toString: String = s.toString()
      }
    }
    trait Builder[L <: DFAny, Able[L0 <: DFAny] <: Init.Able[L0], Token <: DFAny.Token] {
      def apply(left : L, right : Seq[Able[L]]) : Seq[Token]
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    trait Able[R]{val value : R}
    object Able {
      implicit def fromAble[R](able : Able[R]) : R = able.value
    }
    trait Builder[-L, -R] {
      type TType <: DFType
      def apply(left : L, rightR : R) : DFAny.Val[TType]
    }
  }
  type `Op==Builder`[-L, -R] = Op.Builder[L, R]{type TType = DFBool}
//  type `Op!=Builder`[L, R, Sym] = Op.Builder[L, R, Sym]{type Comp = DFBool}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Init
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//    trait InitCO {
//      type Able[L <: DFAny] <: DFAny.Init.Able[L]
//      type Builder[L <: DFAny, Token <: DFAny.Token] <: DFAny.Init.Builder[L, Able, Token]
//    }
//    val Init : InitCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Match Pattern
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//    trait PatternCO {
//      type Able[+R] <: DFAny.Pattern.Able[R]
//      type Builder[L <: DFAny] <: DFAny.Pattern.Builder[L, Able]
//    }
//    val Pattern : PatternCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // General Op
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//    trait OpCO {
//      type Able[R] <: DFAny.Op.Able[R]
//      type Implicits
//      val Able : Implicits
//    }
//    val Op : OpCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Common Ops
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
//    trait `Op:=` {
//      type Builder[L, R] <: DFAny.Op.Builder[L, R]
//    }
//    val `Op:=` : `Op:=`
//    trait `Op<>` {
//      type Builder[L, R] <: DFAny.Op.Builder[L, R]
//    }
//    val `Op<>` : `Op<>`
    trait `Op==` {
      type Builder[-L, -R] <: DFAny.`Op==Builder`[L, R]
    }
    val `Op==` : `Op==`
//    trait `Op!=` {
//      type Builder[L, R] <: DFAny.`Op!=Builder`[L, R]
//    }
//    val `Op!=` : `Op!=`
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}



object Test {
  trait BB extends DFBlock {
    val a = DFUInt(8)
    DFUInt(8).ifdf(???) {
      a
    }.elsedf {
      a
    }
    val b = DFBits(8)
    val b2 = DFBits(8)
    b == b2
  }
//  val aa = a.bits.as(DFUInt(8)).bits

//  a := a
//  implicitly[aa.Var =:= true]
}

