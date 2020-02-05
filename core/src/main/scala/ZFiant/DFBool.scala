package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object DFBool extends DFAny.Companion {
  final case class Type() extends DFAny.Type {
    type Width = 1
    type TToken = Token
    type TPattern = DFBool.Pattern
    type TPatternAble[+R] = DFBool.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBool.Pattern.Builder[LType]
    type OpAble[R] = DFBool.Op.Able[R]
    type `Op==Builder`[L, R] = DFBool.`Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = DFBool.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = DFBool.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = DFBool.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFBool.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBool.Init.Builder[L, TToken]
    val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token =
      Token(fromToken.valueBits(0), fromToken.bubbleMask(0))
    override def toString: String = "DFBool"
    def codeString : String = "DFBool()"
  }
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(Type())

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token(value : Boolean, bubble : Boolean) extends DFAny.Token.Of[Boolean, 1] {
    val width: TwoFace.Int[1] = 1
    lazy val valueBits : XBitVector[1] = XBitVector.bit(value)
    lazy val bubbleMask: XBitVector[1] = XBitVector.bit(bubble)
    def && (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value && that.value)
    }
    def || (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value || that.value)
    }
    def ^ (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value ^ that.value)
    }
    def unary_! : Token = {
      if (this.isBubble) Token(Bubble)
      else Token(!this.value)
    }
    def select[ST <: DFAny.Token](thenSel : ST, elseSel : ST)(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST]
    ) : ST = {
      if (this.value) if (this.isBubble) bubbleOf(thenSel) else thenSel
      else if (this.isBubble) bubbleOf(elseSel) else elseSel
    }
    def == (that : Token) : Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    def != (that : Token) : Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)

    def codeString : String = value.codeString
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = _ => Token(Bubble)
    implicit val bubbleOfDFType : DFAny.Token.BubbleOfDFType[DFBool.Type] = _ => Token(Bubble)
    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)

    import DFAny.TokenSeq
    val || : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l || r)
    val && : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l && r)
    val ^  : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    val == : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    val != : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_! (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => !t)
    def select[ST <: DFAny.Token](cond : Seq[Token], thenSel : Seq[ST], elseSel : Seq[ST])(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST]
    ) : Seq[ST] = TokenSeq(cond, thenSel, elseSel)((c, t, e) => c.select(t, e))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[Boolean]) extends DFAny.Pattern.OfSet[Boolean, Pattern](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val bool : Boolean
    }
    object Able {
      implicit class DFBoolPatternBoolean[R <: Boolean](val right : R) extends Able[R] {
        val bool : Boolean = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type] = new Builder[Type] {
        def apply[R](left: Type, right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.bool).foldLeft(Set.empty[Boolean])((set, bool) => {
            if (set.contains(bool)) throw new IllegalArgumentException(s"\nThe boolean $bool already intersects with $set")
            set + bool
          })

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
      private type IntIsBoolean = CompileTime[(GetArg0 == 0) || (GetArg0 == 1)]
      implicit class DFBoolBubble(val right : Bubble) extends Able[DFBool]
      implicit class DFBoolToken(val right : DFBool.Token) extends Able[DFBool]
      implicit class DFBoolTokenSeq(val right : Seq[DFBool.Token]) extends Able[DFBool]
      implicit class DFBoolSeqOfBoolean(val right : Seq[Boolean]) extends Able[DFBool]
      implicit class DFBoolSeqOfInt(val right : Seq[Int]) extends Able[DFBool]
      implicit class DFBoolXInt[T <: XInt](val right : T)(implicit chk : IntIsBoolean) extends Able[DFBool]
      implicit class DFBoolBoolean(val right : Boolean) extends Able[DFBool]

      def toTokenSeq(right : Seq[Able[DFBool]]) : Seq[DFBool.Token] =
        right.toSeqAny.map {
          case t : Bubble => DFBool.Token(t)
          case t : DFBool.Token => t
          case t : Int => DFBool.Token(t)
          case t : Boolean => DFBool.Token(t)
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev : Builder[DFBool, Token] = (left, right) => Able.toTokenSeq(right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type Const = DFAny.Const[Type]
  object Const {
    trait Builder[Sym, C] {
      def apply(value : C) : Const
    }
    object Builder {
      implicit def fromInt[Sym, C <: Int](implicit ctx : DFAny.Context, checkBin : BinaryInt.CheckedShellSym[Sym, C])
      : Builder[Sym, C] = value => {
        checkBin.unsafeCheck(value)
        DFAny.Const[Type](Type(),Token(value))
      }
      implicit def fromBoolean[Sym, C <: Boolean](implicit ctx : DFAny.Context)
      : Builder[Sym, C] = value => DFAny.Const[Type](Type(), Token(value))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able(value) {
      final val left = value
      final def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      final def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      final def ^   (right : DFBool)(implicit op: `Op^`.Builder[L, DFBool]) = op(left, right)
      final def === (right : DFBool)(implicit op: `Op===`.Builder[L, DFBool]) = op(left, right)
      final def =!= (right : DFBool)(implicit op: `Op=!=`.Builder[L, DFBool]) = op(left, right)
    }
    trait Implicits {
      sealed class DFBoolFrom0(left : 0) extends AbleOps[0](left)
      final implicit def DFBoolFrom0(left: 0): DFBoolFrom0 = new DFBoolFrom0(left)
      sealed class DFBoolFrom1(left : 1) extends AbleOps[1](left)
      final implicit def DFBoolFrom1(left: 1): DFBoolFrom1 = new DFBoolFrom1(left)
      sealed class DFBoolFromBoolean[L <: Boolean](left : L) extends AbleOps[L](left)
      final implicit def DFBoolFromBoolean[L <: Boolean](left: L): DFBoolFromBoolean[L] = new DFBoolFromBoolean[L](left)
      sealed class DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) extends AbleOps[DFBool](DFAny.Alias.AsIs(Type(), left))
      final implicit def DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) : DFBoolFromDFBitsW1[LW] = new DFBoolFromDFBitsW1[LW](left)
      sealed class DFBoolFromDefaultRet(left : DFAny.DefaultRet[Type])(implicit ctx : DFAny.Context) extends AbleOps[DFBool](left)
      final implicit def DFBoolFromDefaultRet(left : DFAny.DefaultRet[Type])(implicit ctx : DFAny.Context) : DFBoolFromDefaultRet = new DFBoolFromDefaultRet(left)
      final implicit def ofDFBool(left : DFBool) : Able[DFBool] = new Able(left)
      implicit class DFBoolOps[LW](val left : DFBool) {
        final def rising()(implicit ctx : ContextOf[Rising]) : DFBool = Rising(left).outPort
        final def ||  [R](right : Able[R])(implicit op: `Op||`.Builder[DFBool, R]) = op(left, right)
        final def &&  [R](right : Able[R])(implicit op: `Op&&`.Builder[DFBool, R]) = op(left, right)
        final def ^   [R](right : Able[R])(implicit op: `Op^`.Builder[DFBool, R]) = op(left, right)
        final def === [R](right : Able[R])(implicit op: `Op===`.Builder[DFBool, R]) = op(left, right)
        final def =!= [R](right : Able[R])(implicit op: `Op=!=`.Builder[DFBool, R]) = op(left, right)
        final def unary_!(implicit ctx : DFAny.Context) : DFBool = DFAny.Alias.Invert(left)
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
      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Context
      ) : Builder[Type, DFBool] = (left, right) => right

      implicit def evDFBool_op_Const[L <: DFBool, R](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder[Builder[_,_], R]
      ) : Builder[Type, R] = (left, rightNum) => {
        val right = rConst(rightNum)
        right
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class BoolOps[Op <: DiSoOp](op : Op)(func : (Token, Token) => DFBool.Token) {
    type ErrorSym
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[L, R](properLR : (L, R) => (DFBool, DFBool))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(Type(), left, op, right)(func)
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Context,
      ) : Builder[DFBool, DFBool] = create[DFBool, DFBool]((left, right) => (left, right))

      implicit def evDFBool_op_Const[L <: DFBool, R](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder[ErrorSym, R]
      ) : Builder[DFBool, R] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBool[L, R <: DFBool](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder[ErrorSym, L]
      ) : Builder[L, DFBool] = create[L, DFBool]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })
    }
  }
  object `Op==` extends BoolOps(DiSoOp.==)((l, r) => l == r) with `Op==`{type ErrorSym = CaseClassSkipper[_]}
  object `Op!=` extends BoolOps(DiSoOp.!=)((l, r) => l != r) with `Op!=`{type ErrorSym = CaseClassSkipper[_]}
  object `Op===` extends BoolOps(DiSoOp.==)((l, r) => l == r) {type ErrorSym = Builder[_,_]}
  object `Op=!=` extends BoolOps(DiSoOp.!=)((l, r) => l != r) {type ErrorSym = Builder[_,_]}
  object `Op||` extends BoolOps(DiSoOp.||)((l, r) => l || r){type ErrorSym = Builder[_,_]}
  object `Op&&` extends BoolOps(DiSoOp.&&)((l, r) => l && r){type ErrorSym = Builder[_,_]}
  object `Op^` extends BoolOps(DiSoOp.^)((l, r) => l ^ r){type ErrorSym = Builder[_,_]}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}


