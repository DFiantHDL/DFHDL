package DFiant.core

import DFiant.core
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.tokens._


trait DFBool extends DFAny.Val[DFBool.Width, DFBool.type, DFBool, DFBool.Var] {
  def unary_!(implicit dsn : DFDesign)               : DFBool = DFBool.op("!", TokenBool.unary_!(getInit), this)
//  def == (that : Boolean)   : DFBool = __==(this, AlmanacEntryConst(if (that) 1 else 0))
//  def != (that : Boolean)   : DFBool = __!=(this, AlmanacEntryConst(if (that) 1 else 0))
  def || (that : DFBool) : DFBool = ??? //AlmanacEntryOpOr(this, that)
  def && (that : DFBool) : DFBool = ??? //AlmanacEntryOpAnd(this, that)
//  def ^^ (that : DFBool) : DFBool = AlmanacEntryOpXor(this, that)
//  def ## (that : DFBits.Unsafe)    : DFBits.Unsafe = this.bits() ## that
//  def ## (that : DFBool)    : DFBits.Unsafe = this.bits() ## that.bits()
  def rising(implicit dsn : DFDesign)                : DFBool = this && !this.prev(1)
  def falling(implicit dsn : DFDesign)               : DFBool = !this && this.prev(1)

  def newEmptyDFVar(implicit dsn : DFDesign) = DFBool.newVar()

  override def toString : String = s"DFBool"

  //  protected[DFiant] def __!= (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0!=arg1
//  protected[DFiant] def __== (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0==arg1
}


object DFBool extends DFAny.Companion {
  type TToken = TokenBool

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait InitAble[L <: DFAny] extends DFAny.Init.Able[L]
  object InitAble {
    private type IntIsBoolean = CompileTime[(GetArg0 == 0) || (GetArg0 == 1)]
    implicit class DFBoolBubble(val right : Bubble) extends InitAble[DFBool]
    implicit class DFBoolToken(val right : TokenBool) extends InitAble[DFBool]
    implicit class DFBoolTokenSeq(val right : Seq[TokenBool]) extends InitAble[DFBool]
    implicit class DFBoolXInt[T <: XInt](val right : T)(implicit chk : IntIsBoolean) extends InitAble[DFBool]
    implicit class DFBoolBoolean(val right : Boolean) extends InitAble[DFBool]

    def toTokenBoolSeq(right : Seq[InitAble[DFBool]]) : Seq[TokenBool] =
      right.toSeqAny.map(e => e match {
        case (t : Bubble) => TokenBool(t)
        case (t : TokenBool) => t
        case (t : Int) => TokenBool(t)
        case (t : Boolean) => TokenBool(t)
      })
  }
  trait InitBuilder[L <: DFAny] extends DFAny.Init.Builder[L, InitAble]
  object InitBuilder {
    implicit def fromDFBool(implicit dsn : DFDesign) : InitBuilder[DFBool] = new InitBuilder[DFBool] {
      def apply(left : DFBool, right : Seq[InitAble[DFBool]]) : DFBool =
        DFBool.alias(left, 0, 0, InitAble.toTokenBoolSeq(right))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  type Width = 1
  ///////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////
  trait Var extends DFAny.Var[DFBool.Width, DFBool.type, DFBool, DFBool.Var] with DFBool {
//    final def := (that : ZeroOrOne1) : TVar = assign(that.getAlmanacEntry)
//    final def set() : Unit = this := true
//    final def clear() : Unit = this := false
  }
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply()(implicit dsn : DFDesign) : Var = newVar()
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar()(implicit dsn : DFDesign) : Var =
    new DFAny.NewVar(1, Seq(TokenBool(false))) with Var {
      def codeString(idRef : String) : String = s"val $idRef = DFBool()"
    }

  protected[DFiant] def alias(aliasedVar : DFAny, relBit : Int, deltaStep : Int = 0, updatedInit : Seq[TokenBool] = Seq())(implicit dsn : DFDesign) : Var =
    new core.DFAny.Alias(aliasedVar, 1, relBit, deltaStep, updatedInit) with Var {
      protected def protTokenBitsToTToken(token : TokenBits) : TToken = TokenBool(token.valueBits(0))
      def codeString(idRef : String) : String = {
        val bitCodeString = s".bit($relBit)"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def const(token : TokenBool)(implicit dsn : DFDesign) : DFBool =
    new DFAny.Const(token) with DFBool

  protected[DFiant] def op(opString : String, opInit : Seq[TokenBool], args : DFAny*)(implicit dsn : DFDesign) : DFBool =
    new DFAny.Op(1, opString, opInit, args) with DFBool {

    }
  ///////////////////////////////////////////////////////////////////////////////////////////
}
