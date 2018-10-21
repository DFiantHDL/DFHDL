package DFiant

import DFiant.BasicLib.DiSoOp
import DFiant.internals._
import singleton.ops._
import singleton.twoface._

trait DFBool extends DFBool.Unbounded

object DFBool extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBool.type] {
    type TVal = DFBool
    type TVar = DFBool.Var
    type TToken = DFBool.Token
    type TPattern = DFBool.Pattern
    type TPatternAble[+R] = DFBool.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFBool.Pattern.Builder[L]
    type Width = 1
    def select[T <: DFAny, L >: T, R >: T](t : Tuple2[L, R]) : T = ???
    def unary_!(implicit ctx : DFAny.Op.Context) : DFBool =
      new DFBool.Alias(List(this), DFAny.Alias.Reference.Invert(".invert"))

    def || [R](right: Op.Able[R])(implicit op: `Op||`.Builder[TVal, R]) : DFBool = op(left, right)
    def && [R](right: Op.Able[R])(implicit op: `Op&&`.Builder[TVal, R]) : DFBool = op(left, right)
    def ^  [R](right: Op.Able[R])(implicit op: `Op^`.Builder[TVal, R]) : DFBool = op(left, right)
    def rising (implicit ctx : DFAny.Op.Context) : DFBool = left && !left.prev(1)
    def falling (implicit ctx : DFAny.Op.Context) : DFBool = !left && left.prev(1)

    def newEmptyDFVar(implicit ctx : DFAny.NewVar.Context) = DFBool()

    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar(), dir)
    override lazy val typeName : String = s"DFBool"
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var extends DFAny.Var with DFBool {
    final def set(implicit ctx : DFAny.Op.Context) : DFBool = this := true
    final def clear(implicit ctx : DFAny.Op.Context) : DFBool = this := false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply()(implicit ctx : DFAny.NewVar.Context) : NewVar = new NewVar()
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar()(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFBool](1, "DFBool()") with Var {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
  }

  protected[DFiant] final class Alias(aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFBool](aliasedVars, reference) with Var

  protected[DFiant] final class Const(token : DFBool.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFBool

  protected[DFiant] final class Port[Dir <: DFDir](dfVar : DFBool, dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFBool, Dir](dfVar, dir) with DFBool

//  protected[DFiant] class Func2Comp[OpKind, L <: DFAny, R <: DFAny](leftArg : L, rightArg : R)
//    (val tokenFunc : (L#TToken, R#TToken) => Token)(
//    implicit ctx : DFComponent.Context[Func2Comp[OpKind, L, R]], opKind : OpKind
//  ) extends DiSoComp[Func2Comp[OpKind, L, R], OpKind, L, R](leftArg, rightArg)(1) with DFBool
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (value : Boolean, val bubble : Boolean) extends DFAny.Token.Of[Boolean, Pattern](1, value) {
    lazy val valueBits : BitVector = BitVector.bit(value)
    lazy val bubbleMask: BitVector = BitVector.bit(bubble)
    def toBubbleToken : Token = Token(Bubble)

    final def && (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value && that.value)
    }
    final def || (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value || that.value)
    }
    final def ^ (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.value ^ that.value)
    }
    final def unary_! : Token = {
      if (this.isBubble) Token(Bubble)
      else Token(!this.value)
    }
    def select[ST <: DFAny.Token](thenSel : ST, elseSel : ST) : ST = {
      if (this.value) if (this.isBubble) thenSel.toBubbleToken.asInstanceOf[ST] else thenSel
      else if (this.isBubble) elseSel.toBubbleToken.asInstanceOf[ST] else elseSel
    }
    final def == (that : Token) : Token = DFBool.Token(this.value == that.value, this.isBubble || that.isBubble)
    final def != (that : Token) : Token = DFBool.Token(this.value != that.value, this.isBubble || that.isBubble)
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    val || : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l || r)
    val && : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l && r)
    val ^  : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    val == : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    val != : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_! (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => !t)
    def select[ST <: DFAny.Token](cond : Seq[Token], thenSel : Seq[ST], elseSel : Seq[ST]) : Seq[ST] = TokenSeq(cond, thenSel, elseSel)((c, t, e) => c.select(t, e))

    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(valueBool : Boolean, bubble : Boolean) : Token = new Token(valueBool, bubble)
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)
    implicit val bubbleOf : DFBool => Token = t => Token(Bubble)
    implicit val fromBits : DFBits.Token => Token = t => Token(t.valueBits(0))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFBool, Dir] = (right, dir) => new Port[Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias(List(left), DFAny.Alias.Reference.AsIs(s".as(DFBool())"))
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
      implicit class DFBoolXInt[T <: XInt](val right : T)(implicit chk : IntIsBoolean) extends Able[DFBool]
      implicit class DFBoolBoolean(val right : Boolean) extends Able[DFBool]

      def toTokenSeq(right : Seq[Able[DFBool]]) : Seq[DFBool.Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFBool.Token(t)
          case (t : DFBool.Token) => t
          case (t : Int) => DFBool.Token(t)
          case (t : Boolean) => DFBool.Token(t)
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev : Builder[DFBool, Token] = (left, right) => Able.toTokenSeq(right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[Boolean]) extends DFAny.Pattern.OfSet[Boolean, Pattern](set)
  object Pattern extends PatternCO {
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends PrevCO {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev(implicit ctx : DFAny.Alias.Context) : Builder[DFBool] = new Builder[DFBool] {
        def apply[P](left : DFBool, right : Natural.Int.Checked[P]) : DFBool =
          new Alias(List(left), DFAny.Alias.Reference.Prev(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      final val left = value
      final def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      final def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      final def ^   (right : DFBool)(implicit op: `Op^`.Builder[L, DFBool]) = op(left, right)
      final def <> [RDIR <: DFDir](port : DFBool <> RDIR)(
        implicit op: `Op<>`.Builder[DFBool, L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFBoolFromXInt[L <: XInt](left : L) extends Able[L](left)
      final implicit def DFBoolFromXInt[L <: XInt](left: L): DFBoolFromXInt[L] = new DFBoolFromXInt[L](left)
      sealed class DFBoolFromBoolean[L <: Boolean](left : L) extends Able[L](left)
      final implicit def DFBoolFromBoolean[L <: Boolean](left: L): DFBoolFromBoolean[L] = new DFBoolFromBoolean[L](left)
      final implicit def ofDFBool[R <: DFBool.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFUInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[Sym, N] {
      def apply(value : N) : DFBool
    }
    object Builder {
      implicit def fromInt[Sym, N <: Int](implicit ctx : DFAny.Const.Context, checkBin : BinaryInt.CheckedShellSym[Sym, N])
      : Builder[Sym, N] = value => {
        checkBin.unsafeCheck(value)
        new Const(Token(value))
      }
      implicit def fromBoolean[Sym, N <: Boolean](implicit ctx : DFAny.Const.Context)
      : Builder[Sym, N] = value => new Const(Token(value))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      def create[L, R](properR : (L, R) => DFBool)
      : Aux[L, R, DFBool] = new Builder[L, R] {
        type Comp = DFBool
        def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](implicit ctx : DFAny.Op.Context)
      : Aux[DFBool, DFBool, DFBool] = create[DFBool, DFBool]((left, right) => right)

      implicit def evDFBool_op_Const[L <: DFBool, R](implicit ctx : DFAny.Op.Context, rConst : Const.Builder[Builder[_,_], R])
      : Aux[DFBool, R, DFBool] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        right
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class BoolOps(kind : DiSoOp.Kind) {
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      def create[L, R](properLR : (L, R) => (DFBool, DFBool))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import FunctionalLib.DFBoolOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = kind match {
          case DiSoOp.Kind.|| => `Func2Comp||`(left, right)
          case DiSoOp.Kind.&& => `Func2Comp&&`(left, right)
          case DiSoOp.Kind.^  => `Func2Comp^`(left, right)
          case DiSoOp.Kind.== => `Func2Comp==`(left, right)
          case DiSoOp.Kind.!= => `Func2Comp!=`(left, right)
          case _ => throw new IllegalArgumentException("Unexpected boolean operation")
        }
        opInst.setAutoName(s"${ctx}")
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Op.Context,
      ) : Builder[DFBool, DFBool] = create[DFBool, DFBool]((left, right) => (left, right))

      implicit def evDFBool_op_Const[L <: DFBool, R](implicit ctx : DFAny.Op.Context, rConst : Const.Builder[Builder[_,_], R])
      : Builder[DFBool, R] = create[DFBool, R]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBool[L, R <: DFBool](implicit ctx : DFAny.Op.Context, lConst : Const.Builder[Builder[_,_], L])
      : Builder[L, DFBool] = create[L, DFBool]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })
    }
  }
  object `Op==` extends BoolOps(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends BoolOps(DiSoOp.Kind.!=) with `Op!=`
  object `Op||` extends BoolOps(DiSoOp.Kind.||)
  object `Op&&` extends BoolOps(DiSoOp.Kind.&&)
  object `Op^` extends BoolOps(DiSoOp.Kind.^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
