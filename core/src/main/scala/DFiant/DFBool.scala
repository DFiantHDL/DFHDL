package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import compiler.csprinter.CSPrinter
import DFAny.Func2

object DFBool extends DFAny.Companion {
  final case class Type(logical : Boolean) extends DFAny.Type {
    type Width = 1
    type TToken = Token
    type TPattern = DFBool.Pattern
    type TPatternAble[+R] = DFBool.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBool.Pattern.Builder[LType]
    type `Op==Builder`[-L, -R] = DFBool.`Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = DFBool.`Op!=`.Builder[L, R]
    type InitAble[L <: DFAny] = DFBool.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBool.Init.Builder[L, TToken]
    val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      if (fromToken.isBubble) Token.bubble(logical = false)
      else Token(logical = false, fromToken.valueBits(0))
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
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
  def apply()(implicit ctx : DFAny.Context) : DFAny.NewVar[Type] = DFAny.NewVar(Type(logical = true))
  def unapply(arg: DFAny.Member): Boolean = arg.dfType match {
    case Type(logical) if logical => true
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
  final case class Token(logical : Boolean, value : Option[Boolean]) extends DFAny.Token.Of[Type, Boolean] { left =>
    val width = 1
    def && (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      val logical = left.logical || right.logical
      (left.value, right.value, bb) match {
        case (Some(l), Some(r), _) => Token(logical, l && r)
        case (_, _, Bubble.Stall) => Token.bubble(logical)
        case (Some(false), None, Bubble.DontCare) => Token(logical, value = false)
        case (None, Some(false), Bubble.DontCare) => Token(logical, value = false)
        case (Some(true), None, Bubble.DontCare) => Token.bubble(logical)
        case (None, Some(true) | None, Bubble.DontCare) => Token.bubble(logical)
      }
    }
    def || (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      val logical = left.logical || right.logical
      (left.value, right.value, bb) match {
        case (Some(l), Some(r), _) => Token(logical, l || r)
        case (_, _, Bubble.Stall) => Token.bubble(logical)
        case (Some(true), None, Bubble.DontCare) => Token(logical, value = true)
        case (None, Some(true), Bubble.DontCare) => Token(logical, value = true)
        case (Some(false), None, Bubble.DontCare) => Token.bubble(logical)
        case (None, Some(false) | None, Bubble.DontCare) => Token.bubble(logical)
      }
    }
    //dontcare in xor will always produce dontcare, like stall bubbles
    def ^ (right : Token) : Token = {
      val logical = left.logical || right.logical
      (left.value, right.value) match {
        case (Some(l), Some(r)) => Token(logical, l ^ r)
        case _ => Token.bubble(logical)
      }
    }
    def unary_! : Token = left.value match {
      case Some(l) => Token(logical, !l)
      case _ => left
    }
    def select[ST <: DFAny.Token](thenSel : ST, elseSel : ST)(
      implicit bubbleOf : DFAny.Token.BubbleOfToken[ST], bb : Bubble.Behaviour
    ) : ST = {
      //      if (this.value) if (this.isBubble) bubbleOf(thenSel) else thenSel
      //      else if (this.isBubble) bubbleOf(elseSel) else elseSel
      ???
    }
    def valueToBitVector(value : Boolean) : BitVector = value.toBitVector(width)
    def valueCodeString(value : Boolean)(implicit printer: CSPrinter) : String = {
      import printer.config._
      val valueStr = if (logical) {
        value.toString
      } else if (value) "1" else "0"
      s"$LIT$valueStr"
    }
  }

  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token.bubble(t.logical)
    implicit val bubbleOfDFType : DFAny.Token.BubbleOfDFType[DFBool.Type] = t => Token.bubble(t.logical)
    def apply(value : Int) : Token = value match {
      case 0 => Token(logical = false, value = false)
      case 1 => Token(logical = false, value = true)
    }
    def apply(logical : Boolean, value : Boolean) : Token = Token(logical, Some(value))
    def bubble(logical : Boolean) : Token = Token(logical, None)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[Boolean]) extends DFAny.Pattern.OfSet[Type, Boolean, Pattern](set) {
    protected def matchCond(matchVal: DFAny.Of[Type], value : Boolean)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Implicits._
      matchVal === value
    }
  }
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
          case t : Bubble => DFBool.Token.bubble(left.dfType.logical)
          case t : DFBool.Token => t.copy(logical = left.dfType.logical)
          case t : Int => DFBool.Token(t).copy(logical = left.dfType.logical)
          case t : Boolean => DFBool.Token(left.dfType.logical, t)
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
  object Const {
    trait Builder[C] {
      def apply(value : C) : DFAny.Of[Type]
    }
    object Builder {
      implicit def fromValueOf[N](
        implicit const : Builder[N]
      ) : Builder[ValueOf[N]] = (value : ValueOf[N]) => const(value.value)
      implicit def fromInt[C <: Int](implicit ctx : DFAny.Context, checkBin : BinaryInt.CheckedShell[C])
      : Builder[C] = value => {
        checkBin.unsafeCheck(value)
        DFAny.Const[Type](Type(logical = false),Token(value))
      }
      implicit def fromBoolean[C <: Boolean](implicit ctx : DFAny.Context)
      : Builder[C] = value => DFAny.Const[Type](Type(logical = true), Token(logical = true, value))
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
      implicit arg : GetArg.Aux[Idx, B], conv : DFAny.`Op:=,<>`.Builder[DFBool.Type, B]
    ) : Arg[Idx] = () => conv(DFBool.Type(logical = true), arg.value.asInstanceOf[B])
  }
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able(value) {
      final val left = value
      final def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      final def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      final def ^   (right : DFBool)(implicit op: `Op^`.Builder[L, DFBool]) = op(left, right)
      final def === (right : DFBool)(implicit op: `Op===`.Builder[L, DFBool]) = op(left, right)
      final def =!= (right : DFBool)(implicit op: `Op=!=`.Builder[L, DFBool]) = op(left, right)
    }
    trait Implicits extends `Op:=,<>`.Implicits {
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
      final implicit def __ofDFBool(left : DFBool) : Able[DFBool] = new Able(left)
      implicit class __DFBoolOps(val left : DFBool) {
        final def rising()(implicit ctx : ContextOf[EdgeDetect]) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Rising).outPort
        final def falling()(implicit ctx : ContextOf[EdgeDetect]) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Falling).outPort
        final def ||  [R](right : Exact[R])(implicit op: `Op||`.Builder[DFBool, R]) = op(left, right)
        final def &&  [R](right : Exact[R])(implicit op: `Op&&`.Builder[DFBool, R]) = op(left, right)
        final def ^   [R](right : Exact[R])(implicit op: `Op^`.Builder[DFBool, R]) = op(left, right)
        final def === [R](right : Exact[R])(implicit op: `Op===`.Builder[DFBool, R]) = op(left, right)
        final def =!= [R](right : Exact[R])(implicit op: `Op=!=`.Builder[DFBool, R]) = op(left, right)
        final def unary_!(implicit ctx : DFAny.Context) : DFBool = DFAny.Alias.Invert[Type](left)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    import DFAny.`Op:=,<>`.Builder

    trait Implicits {
      final implicit def __DFBool_ac_Const[L <: DFBool, R](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder[R]
      ) : Builder[Type, R] = (left, rightNum) => {
        val right = rConst(rightNum)
        right
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class BoolOps[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

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


