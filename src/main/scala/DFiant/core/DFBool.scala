package DFiant.core

import DFiant.core
import DFiant.internals._
import singleton.twoface._
import DFiant.tokens._

sealed trait DFBool extends DFAny.Val[DFBool.Width, TokenBool, DFBool, DFBool.Var] {
//  implicit def bool2Entry(dfBool: DFBool) : AlmanacEntry = dfBool.getCurrentEntry
//  implicit def entry2DFBits(entry: AlmanacEntry) : DFBits.Unsafe = DFBits.Unsafe.op(entry)
//  implicit def entry2DFBool(entry: AlmanacEntry) : DFBool = DFBool.op(entry)

//  def == (that : Boolean)   : DFBool = __==(this, AlmanacEntryConst(if (that) 1 else 0))
//  def == (that : ZeroOrOne1)         : DFBool = __==(this, that)
//  def == (that : 0)         : DFBool = __==(this, that)
//  def == (that : 1)(implicit dummy : DummyImplicit)         : DFBool = __==(this, that)
//  def != (that : Boolean)   : DFBool = __!=(this, AlmanacEntryConst(if (that) 1 else 0))
//  def != (that : ZeroOrOne) : DFBool = __!=(this, that)
  def || (that : DFBool) : DFBool = ??? //AlmanacEntryOpOr(this, that)
  def && (that : DFBool) : DFBool = ??? //AlmanacEntryOpAnd(this, that)
//  def ^^ (that : DFBool) : DFBool = AlmanacEntryOpXor(this, that)
  def unary_!               : DFBool = ??? //AlmanacEntryOpInv(this)
//  def ## (that : DFBits.Unsafe)    : DFBits.Unsafe = this.bits() ## that
//  def ## (that : DFBool)    : DFBits.Unsafe = this.bits() ## that.bits()
//  def rising                : DFBool = this && !this.prev(1)
//  def falling               : DFBool = !this && this.prev(1)

  def dfTypeName : String = "DFBool"
  def newEmptyDFVar = DFBool.create()

//  protected[DFiant] def __!= (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0!=arg1
//  protected[DFiant] def __== (arg0 : DFBool, arg1 : DFBool) : DFBool = arg0==arg1
}

//import singleton.ops._
//case class Foo[W](){
////  def limitOp (that : Int)(implicit unsafe: Unsafe) : Unit = {}
//  def limitOp [H <: XInt, L <: XInt](bitHi : H, bitLo : L)(implicit
//  hiCheck : RequireMsg[(H < W) && (H >= 0), "High bit index is out of range"],
//  loCheck : RequireMsg[(L < W) && (L >= 0), "Low bit index is out of range"],
//  hiloCheck : RequireMsg[L <= H, "High bit index must not be smaller than Low bit index"],
//  ) : Unit = {}
//}

object DFBool {
  type Width = 1
  trait Var extends DFAny.Var[DFBool.Width, TokenBool, DFBool, DFBool.Var] with DFBool {
//    final def := (that : ZeroOrOne1) : TVar = assign(that.getAlmanacEntry)
//    final def set() : Unit = this := true
//    final def clear() : Unit = this := false
  }
  protected[DFiant] def create() : Var =
    new DFAny.NewVar(1) with Var

  implicit def apply() : Var = create()

  protected[DFiant] def alias[I](aliasedVar : DFAny, relBit : TwoFace.Int[I]) : Var =
    new core.DFAny.Alias(aliasedVar, 1, relBit) with Var {}
}
