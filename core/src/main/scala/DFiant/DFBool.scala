package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import compiler.csprinter.CSPrinter
import DFAny.{Func2, `Op==,!=`}

/**
  * A dataflow logical boolean companion object
  */
object DFBool extends DFAny.Companion {
  final case class Type(logical : Boolean) extends DFAny.Type {
    type Width = 1
    type TToken = Token
    type TPattern = DFBool.Pattern
    type TPatternAble[+R] = DFBool.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBool.Pattern.Builder[LType]
    val width : TwoFace.Int[Width] = TwoFace.Int.create[1](1)
    def getBubbleToken: TToken = Token.bubble(logical)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      if (fromToken.isBubble) Token.bubble(logical = false)
      else Token(logical = false, fromToken.valueBits(0))
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = trydf {
      from match {
        case DFBool() =>
        case DFBit() =>
      }
    }
    override def toString: String = if (logical) "DFBool" else "DFBit"
    def codeString(implicit printer: CSPrinter) : String =
      if (logical) s"${printer.config.TP}DFBool" else s"${printer.config.TP}DFBit"
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Construct a new dataflow logical boolean
    * @param ctx An implicit dataflow design context
    */
  def unapply(arg: DFAny.Member): Boolean = arg.dfType match {
    case Type(logical) if logical => true
    case _ => false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Frontend
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Frontend {
    trait Inherited extends Op.Frontend.Inherited with Token.Frontend.Inherited
    trait Imported extends Op.Frontend.Imported with Token.Frontend.Imported
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
  type TokenB = DFAny.TokenT[Token, Type]
  final case class Token(logical : Boolean, value : Option[Boolean]) extends DFAny.Token.Of[Type, Boolean] { left =>
    val width = 1
    val dfType : DFAny.Type = Type(logical)
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
      implicit bb : Bubble.Behaviour
    ) : ST = {
      assert(thenSel.dfType == elseSel.dfType)
      value match {
        case Some(true) => thenSel
        case Some(false) => elseSel
        case None => thenSel.dfType.getBubbleToken.asInstanceOf[ST]
      }
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
    def apply(value : Int) : Token = value match {
      case 0 => Token(logical = false, value = false)
      case 1 => Token(logical = false, value = true)
    }
    def apply(logical : Boolean, value : Boolean) : Token = Token(logical, Some(value))
    def bubble(logical : Boolean) : Token = Token(logical, None)

    type Summon[V] = DFAny.Token.Exact.Summon.SAM[Type, V, TokenB]
    sealed trait Frontend {
      protected implicit def __DFBoolTokenInt[V <: Int](
        implicit
        checkBin : BinaryInt.CheckedShell[V]
      ) : Summon[V] = (from, value) => {
        checkBin.unsafeCheck(value)
        Token(from.logical, value == 1).typeTag[Type]
      }
      protected implicit def __DFBoolTokenBoolean[V <: Boolean]
      : Summon[V] = (from, value) => {
        Token(from.logical, value).typeTag[Type]
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFBoolTokenBoolean[V <: Boolean] : Summon[V] = super.__DFBoolTokenBoolean
        final override protected implicit def __DFBoolTokenInt[V <: Int](implicit checkBin : internals.BinaryInt.CheckedShell[V]) : Summon[V] = super.__DFBoolTokenInt
      }
      trait Imported extends Frontend {
        final override implicit def __DFBoolTokenBoolean[V <: Boolean] : Summon[V] = super.__DFBoolTokenBoolean
        final override implicit def __DFBoolTokenInt[V <: Int](implicit checkBin : internals.BinaryInt.CheckedShell[V]) : Summon[V] = super.__DFBoolTokenInt
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[Boolean]) extends DFAny.Pattern.OfSet[Type, Boolean, Pattern](set) {
    protected def matchCond(matchVal: DFAny.Of[Type], value : Boolean)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Frontend._
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
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Arg[R] {
    def apply(arg : R) : DFBool
  }
  object Arg {
    implicit def ev[R](
      implicit conv : DFAny.`Op:=,<>`.Builder[DFBool.Type, R]
    ) : Arg[R] = arg => conv(DFBool.Type(logical = true), arg)
  }
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able(value) {
      final val left = value
      /**
        * @return the dataflow logical Or result.
        */
      final def ||  (right : DFBool)(implicit op: `Op||`.Builder[L, DFBool]) = op(left, right)
      /**
        * @return the dataflow logical And result.
        */
      final def &&  (right : DFBool)(implicit op: `Op&&`.Builder[L, DFBool]) = op(left, right)
      /**
        * @return the dataflow logical Xor result.
        */
      final def ^   (right : DFBool)(implicit op: `Op^`.Builder[L, DFBool]) = op(left, right)
      /**
        * @return the dataflow comparison equality result.
        */
      final def === (right : DFBool)(implicit op: DFAny.`Op==`.Builder[L, DFBool]) = op(left, right)
      /**
        * @return the dataflow comparison inequality result.
        */
      final def =!= (right : DFBool)(implicit op: DFAny.`Op!=`.Builder[L, DFBool]) = op(left, right)
    }
    sealed trait Frontend {
      sealed class __DFBoolFrom10[L <: XInt](left : L) extends AbleOps[ValueOf[L]](new ValueOf(left))
      protected implicit def __DFBoolFrom10[L <: XInt](left: L): __DFBoolFrom10[L] = new __DFBoolFrom10[L](left)
      sealed class __DFBoolFromBoolean[L <: Boolean](left : L) extends AbleOps[L](left)
      protected implicit def __DFBoolFromBoolean[L <: Boolean](left: L): __DFBoolFromBoolean[L] = new __DFBoolFromBoolean[L](left)
      sealed class __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) extends AbleOps[DFBool](DFAny.Alias.AsIs(Type(logical = false), left))
      protected implicit def __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(
        implicit ctx : DFAny.Context, r : Require[LW == 1]
      ) : __DFBoolFromDFBitsW1[LW] = new __DFBoolFromDFBitsW1[LW](left)
      protected implicit def __ofDFBool(left : DFBool) : Able[DFBool] = new Able(left)
      protected implicit def __DFBool_eq_Capable : DFAny.`Op==,!=`.Capable[Type, Type] = (_, _) => {}
      protected implicit def __DFBool_eq_ConstCapable : DFAny.`Op==,!=`.ConstCapable[Type, Type] = (_, _) => {}
      protected implicit class __DFBoolOps(val left : DFBool) {
        /**
          * @return a dataflow rising edge boolean
          */
        final def rising()(
          implicit ctx : ContextOf[EdgeDetect]
        ) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Rising).outPort
        /**
          * @return a dataflow falling edge boolean
          */
        final def falling()(
          implicit ctx : ContextOf[EdgeDetect]
        ) : DFBool = EdgeDetect(left, EdgeDetect.Edge.Falling).outPort
        /**
          * @return the dataflow logical Or result.
          */
        final def ||  [R](right : Exact[R])(implicit op: `Op||`.Builder[DFBool, R]) = op(left, right)
        /**
          * @return the dataflow logical And result.
          */
        final def &&  [R](right : Exact[R])(implicit op: `Op&&`.Builder[DFBool, R]) = op(left, right)
        /**
          * @return the dataflow logical Xor result.
          */
        final def ^   [R](right : Exact[R])(implicit op: `Op^`.Builder[DFBool, R]) = op(left, right)
        /**
          * @return the dataflow comparison equality result.
          */
        final def === [R](right : Exact[R])(implicit op: DFAny.`Op==`.Builder[DFBool, R]) = op(left, right)
        /**
          * @return the dataflow comparison inequality result.
          */
        final def =!= [R](right : Exact[R])(implicit op: DFAny.`Op!=`.Builder[DFBool, R]) = op(left, right)
        /**
          * @return the dataflow Bit Inversion result.
          */
        final def unary_!(implicit ctx : DFAny.Context) : DFBool =
          DFAny.Func1(left.dfType, left, DFAny.Func1.Op.unary_!)(!_)
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFBoolFrom10[L <: XInt](left : L) : __DFBoolFrom10[L] = super.__DFBoolFrom10(left)
        final override protected implicit def __DFBoolFromBoolean[L <: Boolean](left : L) : __DFBoolFromBoolean[L] = super.__DFBoolFromBoolean(left)
        final override protected implicit def __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(implicit ctx : DFAny.Context, r : Require[LW == 1]) : __DFBoolFromDFBitsW1[LW] = super.__DFBoolFromDFBitsW1(left)
        final override protected implicit def __ofDFBool(left : DFBool) : Able[DFBool] = super.__ofDFBool(left)
        final override protected implicit def __DFBoolOps(left : DFBool) : __DFBoolOps = super.__DFBoolOps(left)
        final override protected implicit def __DFBool_eq_Capable : `Op==,!=`.Capable[Type, Type] = super.__DFBool_eq_Capable
        final override protected implicit def __DFBool_eq_ConstCapable : `Op==,!=`.ConstCapable[Type, Type] = super.__DFBool_eq_ConstCapable
      }
      trait Imported extends Frontend {
        final override implicit def __DFBoolFrom10[L <: XInt](left : L) : __DFBoolFrom10[L] = super.__DFBoolFrom10(left)
        final override implicit def __DFBoolFromBoolean[L <: Boolean](left : L) : __DFBoolFromBoolean[L] = super.__DFBoolFromBoolean(left)
        final override implicit def __DFBoolFromDFBitsW1[LW](left : DFBits[LW])(implicit ctx : DFAny.Context, r : Require[LW == 1]) : __DFBoolFromDFBitsW1[LW] = super.__DFBoolFromDFBitsW1(left)
        final override implicit def __ofDFBool(left : DFBool) : Able[DFBool] = super.__ofDFBool(left)
        final override implicit def __DFBoolOps(left : DFBool) : __DFBoolOps = super.__DFBoolOps(left)
        final override implicit def __DFBool_eq_Capable : `Op==,!=`.Capable[Type, Type] = super.__DFBool_eq_Capable
        final override implicit def __DFBool_eq_ConstCapable : `Op==,!=`.ConstCapable[Type, Type] = super.__DFBool_eq_ConstCapable
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logical/Binary operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class BoolOps[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Operation is not supported between type ${L} and type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[L, R](properLR : (L, R) => (DFBool, DFBool))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => trydf {
        val (left, right) = properLR(leftL, rightR)
        val logicalResult = left.dfType.logical || right.dfType.logical
        DFAny.Func2(Type(logicalResult), left, op, right)(func)
      }

      implicit def evDFBool_op_DFBool[L <: DFBool, R <: DFBool](
        implicit
        ctx : DFAny.Context,
      ) : Builder[DFBool, DFBool] = create[DFBool, DFBool]((left, right) => (left, right))

      implicit def evDFBool_op_Const[R](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.ToFit[Type, R]
      ) : Builder[DFBool, R] = create[DFBool, R]((left, rightValue) => {
        val right = rConst(left.dfType, rightValue)
        (left, right)
      })

      implicit def evConst_op_DFBool[L](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.ToFit[Type, L]
      ) : Builder[L, DFBool] = create[L, DFBool]((leftValue, right) => {
        val left = lConst(right.dfType, leftValue)
        (left, right)
      })
    }
  }
  object `Op||` extends BoolOps(Func2.Op.||)((l, r) => l || r)
  object `Op&&` extends BoolOps(Func2.Op.&&)((l, r) => l && r)
  object `Op^` extends BoolOps(Func2.Op.^)((l, r) => l ^ r)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}


