package DFiant.core

import DFiant.core
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import scodec.bits._


trait DFBool extends DFBool.Unbounded

object DFBool extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBool.type] {
    type TVal = DFBool
    type TVar = DFBool.Var
    type Width = 1
    def unary_!(implicit dsn : DFDesign)               : DFBool = DFBool.op("!", DFBool.Token.unary_!(getInit), this)
    //  def == (that : Boolean)   : DFBool = __==(this, AlmanacEntryConst(if (that) 1 else 0))
    //  def != (that : Boolean)   : DFBool = __!=(this, AlmanacEntryConst(if (that) 1 else 0))
    def || (that : DFBool) : DFBool = ??? //AlmanacEntryOpOr(this, that)
    def && (that : DFBool) : DFBool = ??? //AlmanacEntryOpAnd(this, that)
    //  def ^^ (that : DFBool) : DFBool = AlmanacEntryOpXor(this, that)
    //  def ## (that : DFBits.Unsafe)    : DFBits.Unsafe = this.bits() ## that
    //  def ## (that : DFBool)    : DFBits.Unsafe = this.bits() ## that.bits()
    def rising(implicit dsn : DFDesign)                : DFBool = left && !left.prev(1)
    def falling(implicit dsn : DFDesign)               : DFBool = !left && left.prev(1)

    def newEmptyDFVar(implicit dsn : DFDesign) = DFBool.newVar()

    override def toString : String = s"DFBool"

    //  protected[DFiant] def __!= (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0!=arg1
    //  protected[DFiant] def __== (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0==arg1
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var extends DFAny.Var with DFBool {
//    final def := (that : ZeroOrOne1) : TVar = assign(that.getAlmanacEntry)
//    final def set() : Unit = this := true
//    final def clear() : Unit = this := false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply()(implicit dsn : DFDesign) : Var = newVar()
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar()(implicit dsn : DFDesign) : Var =
    new DFAny.NewVar(1, Seq(DFBool.Token(false))) with Var {
      def codeString(idRef : String) : String = s"val $idRef = DFBool()"
    }

  protected[DFiant] def alias(aliasedVar : DFAny, relBit : Int, deltaStep : Int = 0, updatedInit : Seq[DFBool.Token] = Seq())(implicit dsn : DFDesign) : Var =
    new core.DFAny.Alias(aliasedVar, 1, relBit, deltaStep, updatedInit) with Var {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = DFBool.Token(token.valueBits(0))
      def codeString(idRef : String) : String = {
        val bitCodeString = s".bit($relBit)"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def const(token : DFBool.Token)(implicit dsn : DFDesign) : DFBool =
    new DFAny.Const(token) with DFBool

  protected[DFiant] def op(opString : String, opInit : Seq[DFBool.Token], args : DFAny*)(implicit dsn : DFDesign) : DFBool =
    new DFAny.Op(1, opString, opInit, args) with DFBool {

    }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val valueBool : Boolean, val bubble : Boolean) extends DFAny.Token {
    val width : Int = 1
    lazy val valueBits : BitVector = BitVector.bit(valueBool)
    lazy val bubbleMask: BitVector = BitVector.bit(bubble)

    final def && (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.valueBool && that.valueBool)
    }
    final def || (that : Token) : Token = {
      if (this.isBubble || that.isBubble) Token(Bubble)
      else Token(this.valueBool || that.valueBool)
    }
    final def unary_! : Token = {
      if (this.isBubble) Token(Bubble)
      else Token(!this.valueBool)
    }
    override def valueString : String = valueBool.toString()
  }

  object Token {
    import DFAny.TokenSeq
    def || (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l || r)
    def && (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l && r)
    def unary_! (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => !t)

    def apply(value : Int) : Token = value match {
      case 0 => Token(false)
      case 1 => Token(true)
    }
    def apply(valueBool : Boolean, bubble : Boolean) : Token = new Token(valueBool, bubble)
    def apply(value : Boolean) : Token = new Token(value, false)
    def apply(value : Bubble) : Token = new Token(false, true)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
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
    trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
    object Builder {
      implicit def fromDFBool(implicit dsn : DFDesign) : Builder[DFBool] = new Builder[DFBool] {
        def apply(left : DFBool, right : Seq[Able[DFBool]]) : DFBool =
          DFBool.alias(left, 0, 0, Able.toTokenSeq(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends Prev {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev(implicit dsn : DFDesign) : Builder[DFBool] = new Builder[DFBool] {
        def apply[P](left : DFBool, right : Natural.Int.Checked[P]) : DFBool =
          DFBool.alias(left, 0, -right, left.getInit)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Assign extends Assign {

  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // For If Clause
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit class ElseIfClauseBuilder(cond : DFBool)(implicit dsn : DFDesign){
    def apply(block : => Unit) : ElseIfClause = new ElseIfClause(cond, block)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
