package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import csprinter.CSPrinter
import DFAny.Func2

object DFBool extends DFAny.Companion {
  final case class Type(logical : Boolean) extends DFAny.Type {
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
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      Token(logical = false, fromToken.valueBits(0), fromToken.bubbleMask(0))
    def assignCheck(from : DFAny)(implicit ctx : DFAny.Context) : Unit = from match {
      case DFBool() =>
      case DFBit() =>
    }
    override def toString: String = if (logical) "DFBool" else "DFBit"
    def codeString(implicit printer: CSPrinter) : String =
      if (logical) s"${printer.config.TP}DFBool()" else s"${printer.config.TP}DFBit()"
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply()(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(logical = true))
  def unapply(arg: Any): Boolean = arg match {
    case dfAny : DFAny => dfAny.dfType match {
      case Type(logical) if logical => true
      case _ => false
    }
    case _ => false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Macro error in case the user uses `if` instead of `ifdf`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  import scala.reflect.macros.blackbox
  implicit def toBooleanError(dfbool : DFBool) : Boolean = macro toBooleanErrorMacro
  def toBooleanErrorMacro(c: blackbox.Context)(dfbool : c.Tree) : c.Tree = {
    c.abort(c.enclosingPosition,
      """Type mismatch. It appears you are trying to use a (DFiant) `DFBool` where a (Scala) `Boolean` is expected.
        |Make sure you use `ifdf` and not `if`.""".stripMargin
    )
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token(logical : Boolean, value : Boolean, bubble : Boolean) extends DFAny.Token.Of[Boolean] {
    val width: Int = 1
    lazy val valueBits : BitVector = XBitVector.bit(value)
    lazy val bubbleMask: BitVector = XBitVector.bit(bubble)
    def && (that : Token) : Token = {
      val logicalResult = this.logical || that.logical
      if (this.isBubble || that.isBubble) Token(logicalResult, Bubble)
      else Token.fromValue(logicalResult, this.value && that.value)
    }
    def || (that : Token) : Token = {
      val logicalResult = this.logical || that.logical
      if (this.isBubble || that.isBubble) Token(logicalResult, Bubble)
      else Token.fromValue(logicalResult, this.value || that.value)
    }
    def ^ (that : Token) : Token = {
      val logicalResult = this.logical || that.logical
      if (this.isBubble || that.isBubble) Token(logicalResult, Bubble)
      else Token.fromValue(logicalResult, this.value ^ that.value)
    }
    def unary_! : Token = {
      if (this.isBubble) Token(logical, Bubble)
      else Token.fromValue(logical, !this.value)
    }
    def select[ST <: DFAny.Token](thenSel : ST, elseSel : ST)(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST]
    ) : ST = {
      if (this.value) if (this.isBubble) bubbleOf(thenSel) else thenSel
      else if (this.isBubble) bubbleOf(elseSel) else elseSel
    }
    def == (that : Token) : Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
    def != (that : Token) : Token = DFBool.Token(logical = true, this.value != that.value, this.isBubble || that.isBubble)

    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      val valueStr = if (logical) {
        value.toString
      } else if (value) "1" else "0"
      s"$LIT$valueStr"
    }
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token(t.logical, Bubble)
    implicit val bubbleOfDFType : DFAny.Token.BubbleOfDFType[DFBool.Type] = t => Token(t.logical, Bubble)
    def apply(value : Int) : Token = value match {
      case 0 => Token(logical = false, value = false, bubble = false)
      case 1 => Token(logical = false, value = true, bubble = false)
    }
    def fromValue(logical : Boolean, value : Boolean) : Token = new Token(logical, value, false)
    def apply(logical : Boolean, value : Bubble) : Token = new Token(logical, false, true)

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

      def toTokenSeq(left : DFBool, right : Seq[Able[DFBool]]) : Seq[DFBool.Token] =
        right.toSeqAny.map {
          case t : Bubble => DFBool.Token(left.dfType.logical, t)
          case t : DFBool.Token => t.copy(logical = left.dfType.logical)
          case t : Int => DFBool.Token(t).copy(logical = left.dfType.logical)
          case t : Boolean => DFBool.Token.fromValue(left.dfType.logical, t)
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev : Builder[DFBool, Token] = (left, right) => Able.toTokenSeq(left, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type Const = DFAny.Const.Of[Type]
  object Const {
    trait Builder[C] {
      def apply(value : C) : Const
    }
    object Builder {
      implicit def fromInt[C <: Int](implicit ctx : DFAny.Context, checkBin : BinaryInt.CheckedShell[C])
      : Builder[C] = value => {
        checkBin.unsafeCheck(value)
        DFAny.Const[Type](Type(logical = false),Token(value))
      }
      implicit def fromBoolean[C <: Boolean](implicit ctx : DFAny.Context)
      : Builder[C] = value => DFAny.Const[Type](Type(logical = true), Token.fromValue(logical = true, value))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Arg[Idx] {
    def apply() : DFBool
  }
  object Arg {
    implicit def ev[Idx, B](
      implicit arg : GetArg.Aux[Idx, B], conv : DFBool.`Op:=`.Builder[DFBool.Type, B]
    ) : Arg[Idx] = () => conv(DFBool.Type(logical = true), arg.value.asInstanceOf[B])
  }
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
      sealed class __DFBoolFrom0(left : 0) extends AbleOps[0](left)
      final implicit def __DFBoolFrom0(left: 0): __DFBoolFrom0 = new __DFBoolFrom0(left)
      sealed class __DFBoolFrom1(left : 1) extends AbleOps[1](left)
      final implicit def __DFBoolFrom1(left: 1): __DFBoolFrom1 = new __DFBoolFrom1(left)
      sealed class __DFBoolFromBoolean[L <: Boolean](left : L) extends AbleOps[L](left)
      final implicit def __DFBoolFromBoolean[L <: Boolean](left: L): __DFBoolFromBoolean[L] = new __DFBoolFromBoolean[L](left)
      sealed class __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) extends AbleOps[DFBool](DFAny.Alias.AsIs(Type(logical = false), left))
      final implicit def __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) : __DFBoolFromDFBitsW1[LW] = new __DFBoolFromDFBitsW1[LW](left)
      sealed class __DFBoolFromDefaultRet(left : DFAny.DefaultRet[Type])(implicit ctx : DFAny.Context) extends AbleOps[DFBool](left)
      final implicit def __DFBoolFromDefaultRet(left : DFAny.DefaultRet[Type])(implicit ctx : DFAny.Context) : __DFBoolFromDefaultRet = new __DFBoolFromDefaultRet(left)
      final implicit def __ofDFBool(left : DFBool) : Able[DFBool] = new Able(left)
      implicit class __DFBoolOps(val left : DFBool) {
        final def rising()(implicit ctx : ContextOf[EdgeDetect]) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Rising).outPort
        final def falling()(implicit ctx : ContextOf[EdgeDetect]) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Falling).outPort
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
      implicit def evDFBool_op_DFBool[R <: DFBool](
        implicit
        ctx : DFAny.Context
      ) : Builder[Type, R] = (left, right) => right

      implicit def evDFBool_op_RVal[R <: DFAny.DefaultRet[Type]](
        implicit
        ctx : DFAny.Context
      ) : Builder[Type, R] = (left, right) => right.thisVal

      implicit def evDFBool_op_Const[L <: DFBool, R](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder[R]
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
  protected abstract class BoolOps[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[L, R](properLR : (L, R) => (DFBool, DFBool))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val logicalResult = op match {
          case _ : Func2.Op.== | _ : Func2.Op.!= => true
          case _ =>  left.dfType.logical || right.dfType.logical
        }
        DFAny.Func2(Type(logicalResult), left, op, right)(func)
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Context,
      ) : Builder[DFBool, DFBool] = create[DFBool, DFBool]((left, right) => (left, right))

      implicit def evDFBool_op_Const[L <: DFBool, R](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder[R]
      ) : Builder[DFBool, R] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBool[L, R <: DFBool](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder[L]
      ) : Builder[L, DFBool] = create[L, DFBool]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })
    }
  }
  object `Op==` extends BoolOps(Func2.Op.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends BoolOps(Func2.Op.!=)((l, r) => l != r) with `Op!=`
  object `Op===` extends BoolOps(Func2.Op.==)((l, r) => l == r)
  object `Op=!=` extends BoolOps(Func2.Op.!=)((l, r) => l != r)
  object `Op||` extends BoolOps(Func2.Op.||)((l, r) => l || r)
  object `Op&&` extends BoolOps(Func2.Op.&&)((l, r) => l && r)
  object `Op^` extends BoolOps(Func2.Op.^)((l, r) => l ^ r)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}


