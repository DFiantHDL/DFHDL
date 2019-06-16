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
    protected[DFiant] type TUnbounded = Unbounded
    protected[DFiant] type TVal = DFBool
    protected[DFiant] type TVar = DFBool.Var
    protected[DFiant] type TToken = DFBool.Token
    protected[DFiant] type TPattern = DFBool.Pattern
    protected[DFiant] type TPatternAble[+R] = DFBool.Pattern.Able[R]
    protected[DFiant] type TPatternBuilder[L <: DFAny] = DFBool.Pattern.Builder[L]
    type Width = 1
    protected[DFiant] type OpAble[R] = Op.Able[R]
    protected[DFiant] type `Op<>Builder`[R] = `Op<>`.Builder[TVal, R]
    protected[DFiant] type `Op:=Builder`[R] = `Op:=`.Builder[TVal, R]
    protected[DFiant] type `Op==Builder`[R] = `Op==`.Builder[TVal, R]
    protected[DFiant] type `Op!=Builder`[R] = `Op!=`.Builder[TVal, R]
    protected[DFiant] type InitAble[L <: DFAny] = Init.Able[L]
    protected[DFiant] type InitBuilder = Init.Builder[TVal, TToken]
    protected[DFiant] type PortBuilder[Dir <: DFDir] = Port.Builder[TVal, Dir]
    final def unary_!(implicit ctx : DFAny.Op.Context) : DFBool =
      new DFBool.Alias(DFAny.Alias.Reference.Invert(this, ".invert"))

    final def == (right : Boolean)(implicit op: `Op==`.Builder[TVal, Boolean]) : DFBool = op(left, right)
    final def == [R](that : Int)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : DFBool = op(left, right)
    final def || [R](right: Op.Able[R])(implicit op: `Op||`.Builder[TVal, R]) = op(left, right)
    final def && [R](right: Op.Able[R])(implicit op: `Op&&`.Builder[TVal, R]) = op(left, right)
    final def ^  [R](right: Op.Able[R])(implicit op: `Op^`.Builder[TVal, R]) = op(left, right)
    final def rising (implicit ctx : DFAny.Op.Context) : DFBool = left && !left.prev(1)
    final def falling (implicit ctx : DFAny.Op.Context) : DFBool = !left && left.prev(1)

    final def newEmptyDFVar(implicit ctx : DFAny.NewVar.Context) = DFBool()

    final protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <~> Dir = new Port(new NewVar(), dir)
    final protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
      implicit ctx : DFAny.Alias.Context
    ) : TAlias = new Alias(reference)(ctx).asInstanceOf[TAlias]
    __dev.setAutoTypeName(s"DFBool")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] sealed trait VHDL
  protected[DFiant] object VHDL {
    case object std_logic extends VHDL
    case object boolean extends VHDL
    case object bit extends VHDL
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var extends DFAny.Var with DFBool {
    final def set(implicit ctx : DFAny.Op.Context) : Unit = this := true
    final def clear(implicit ctx : DFAny.Op.Context) : Unit = this := false
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
  ) extends DFAny.NewVar[DFBool](1, "DFBool()") with Var

  protected[DFiant] final class Alias(reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFBool](reference) with Var

  protected[DFiant] final class Const(token : DFBool.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const[DFBool](token) with DFBool

  protected[DFiant] final class Port[Dir <: DFDir](dfVar : DFBool, dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFBool, Dir](dfVar, dir) with DFBool {
    def asVHDLBit : this.type = this
    def asVHDLBoolean : this.type = this
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Token private[DFiant] (value : Boolean, bubble : Boolean) extends DFAny.Token.Of[Boolean, Pattern] {
    protected[DFiant] type TToken = Token
    val width = 1
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
    implicit val fromBits : DFBits.Token => Token = t => Token(t.valueBits(0), t.bubbleMask(0))
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
      new Alias(DFAny.Alias.Reference.AsIs(left, s".as(DFBool())"))
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
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[LW] : Builder[DFBool] = new Builder[DFBool] {
        def apply[R](left: DFBool, right: Seq[Able[R]]): Pattern = {
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
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      final val left = value
      final def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      final def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      final def ^   (right : DFBool)(implicit op: `Op^`.Builder[L, DFBool]) = op(left, right)
      final def <> (port : DFAny.Connectable[DFBool] with DFBool)(
        implicit op: `Op<>`.Builder[DFBool, L], ctx : DFAny.Connector.Context
      ) = port.connectWith(op(port, left))
    }
    trait Implicits {
      sealed class DFBoolFrom0(left : 0) extends Able[0](left)
      final implicit def DFBoolFrom0(left: 0): DFBoolFrom0 = new DFBoolFrom0(left)
      sealed class DFBoolFrom1(left : 1) extends Able[1](left)
      final implicit def DFBoolFrom1(left: 1): DFBoolFrom1 = new DFBoolFrom1(left)
      sealed class DFBoolFromBoolean[L <: Boolean](left : L) extends Able[L](left)
      final implicit def DFBoolFromBoolean[L <: Boolean](left: L): DFBoolFromBoolean[L] = new DFBoolFromBoolean[L](left)
      sealed class DFBoolFromDFBitsW1[LW](left : DFBits[LW])(implicit ctx : DFAny.Alias.Context, r : Require[LW == 1]) extends Able[DFBool](new Alias(DFAny.Alias.Reference.AsIs(left, "")))
      final implicit def DFBoolFromDFBitsW1[LW](left : DFBits[LW])(implicit ctx : DFAny.Alias.Context, r : Require[LW == 1]) : DFBoolFromDFBitsW1[LW] = new DFBoolFromDFBitsW1[LW](left)
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
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool with CanBePiped}

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
        opInst.__dev.setAutoName(s"${ctx}")
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
