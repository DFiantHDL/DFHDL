package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.core

trait DFBits[W] extends DFAny.ValW[W, DFBits[W], DFBits.Var[W]] {
//  implicit def bits2Entry(dfBits: DFBits[W]) : AlmanacEntry = dfBits.getCurrentEntry
//  implicit def entry2DFBits(entry: AlmanacEntry) : DFBits[W] = DFBits.Unsafe.op(entry)
//  implicit def entry2DFBool(entry: AlmanacEntry) : DFBool = DFBool.op(entry)

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[I](relBit : CheckedBitIndex[I, W]) : TBool = protBit(relBit.unsafeCheck(width))
  final def apply[I](implicit relBit : CheckedBitIndex[I, W], di : DummyImplicit, di2 : DummyImplicit) : TBool =
    protBit(relBit.unsafeCheck(width))

  final def msbit : TBool = protBit(width-1)
  final def lsbit : TBool = protBit(0)
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[H, L](relBitHigh : CheckedBitIndex[H, W], relBitLow : CheckedBitIndex[L, W])(
    implicit checkHiLow : CheckedBitsRange.Shell[H, L], relWidth : RelWidth.TF[H, L]
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def apply[H, L](implicit relBitHigh : CheckedBitIndex[H, W], relBitLow : CheckedBitIndex[L, W],
    checkHiLow : CheckedBitsRange[H, L], relWidth : RelWidth.TF[H, L], di : DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

  final protected def protMSBits[PW](partWidth : TwoFace.Int[PW]) : TBits[PW] =
    DFBits.alias(this, width-1, width-partWidth).asInstanceOf[TBits[PW]]
  final def msbits[PW](partWidth : CheckedPartWidth[PW, W]) = protMSBits(partWidth.unsafeCheck(width))
  final def msbits[PW](implicit partWidth : CheckedPartWidth[PW, W], di : DummyImplicit) =
    protMSBits(partWidth.unsafeCheck(width))

  final protected def protLSBits[PW](partWidth : TwoFace.Int[PW]) : TBits[PW] =
    DFBits.alias(this, partWidth-1, 0).asInstanceOf[TBits[PW]]
  final def lsbits[PW](partWidth : CheckedPartWidth[PW, W]) = protLSBits(partWidth.unsafeCheck(width))
  final def lsbits[PW](implicit partWidth : CheckedPartWidth[PW, W], di : DummyImplicit) =
    protLSBits(partWidth.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

//  def extBy(numOfBits : Int)    : TBits[WUnsafe] = (DFBits.Unsafe(width + numOfBits) := this).asInstanceOf[TBits[WUnsafe]]

//  def ^ (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpXor(this, that)
//  def | (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpOr(this, that)
//  def & (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpAnd(this, that)
  def + [R](that : `Op+`.Able[TVal, R])(implicit op : `Op+`.Builder[TVal, R]) = op(this, that)
  //AlmanacEntryOpAdd(this, that)
//  def - (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpSub(this, that)
//  def unary_~                   : DFBits.Unsafe = ??? //AlmanacEntryOpInv(this)
//  def unary_-                   : DFBits.Unsafe = ??? //AlmanacEntryOpNeg(this)
//  def >> (that : DFBits.Unsafe)        : DFBits.Unsafe = ???
//  def << (that : DFBits.Unsafe)        : DFBits.Unsafe = ???
//  def << (that : Int)           : DFBits.Unsafe = ??? //AlmanacEntryOpLsh(this, AlmanacEntryConst(that))
//  def >> (that : Int)           : DFBits.Unsafe = ??? //AlmanacEntryOpRsh(this, AlmanacEntryConst(that))
//  def ## (that : DFBits.Unsafe)        : DFBits.Unsafe = ??? //AlmanacEntryOpCat(this, that)
  //      def ## (that : DFBool)        : DFBits.Unsafe = AlmanacEntryOpCat(this, that.bits())
  def == (that : Int)           : DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def == (that : Long)          : DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def == (that : BigInt)        : DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def != (that : Int)           : DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def != (that : Long)          : DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def != (that : BigInt)        : DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def isZero                    : DFBool = this == 0
  def isNonZero                 : DFBool = this != 0
  def isAllOnes                 : DFBool = this == bitsWidthToMaxBigIntBits(width)
  def isNotAllOnes              : DFBool = this != bitsWidthToMaxBigIntBits(width)
//  def < (that : DFBits.Unsafe)         : DFBool = ??? //AlmanacEntryOpLsTn(this, that)
//  def >= (that : DFBits.Unsafe)        : DFBool = ??? //!(this < that)
//  def > (that : DFBits.Unsafe)         : DFBool = ??? //that < this
//  def <= (that : DFBits.Unsafe)        : DFBool = ??? //that >= this

  def dfTypeName : String = "DFBits"
  def newEmptyDFVar = DFBits.create(width)

//  protected[DFiant] def __!= (arg0 : DFBits.Unsafe, arg1 : DFBits.Unsafe) : DFBool = arg0!=arg1
//  protected[DFiant] def __== (arg0 : DFBits.Unsafe, arg1 : DFBits.Unsafe) : DFBool = arg0==arg1
}

object DFBits {
  ///////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFBits[W] with DFAny.VarW[W, DFBits[W], DFBits.Var[W]] {
    //    def setBits(range : BitsRange)                       : TVar = assignBits(range, bitsWidthToMaxBigIntBits(range.width))
    //    def clearBits(range : BitsRange)                     : TVar = assignBits(range,0)
    //    def assignBits(range : BitsRange, value : DFBits.Unsafe) : TVar = {this.protBitsUnsafe(range) := value; this}
  }
  protected[DFiant] def create[W](_width : TwoFace.Int[W]) : Var[W] = new Var[W] {
    val width : TwoFace.Int[W] = _width
  }
  implicit def apply[W](implicit checkedWidth : CheckedWidth[W], di: DummyImplicit) : Var[W] = create(checkedWidth)
  def apply[W](checkedWidth : CheckedWidth[W]) : Var[W] = create(checkedWidth.unsafeCheck())
  ///////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] def alias[H, L]
  (aliasedVar : DFAny, relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])
  (implicit relWidth : RelWidth.TF[H, L]) : Var[relWidth.Out] =
    new core.DFAny.Alias(aliasedVar, relBitHigh, relBitLow) with Var[relWidth.Out] {
      val width : TwoFace.Int[relWidth.Out] = relWidth(relBitHigh, relBitLow)
    }

  def constInt[C](constVal : TwoFace.Int[C])(implicit bitsWidthOf: BitsWidthOf.Int[C]) : DFBits[bitsWidthOf.Out] =
    new DFAny.Const.Int[C](constVal) with DFBits[bitsWidthOf.Out] {
      val width : TwoFace.Int[bitsWidthOf.Out] = bitsWidthOf(constVal)
    }
//  implicit def constInt0[C <: XInt](constVal : C)(implicit bitsWidthOf: BitsWidthOf.Int[C]) : DFBits[bitsWidthOf.Out] =
//    new DFAny.Const.Int[C](constVal) with DFBits[bitsWidthOf.Out] {
//      val width : TwoFace.Int[bitsWidthOf.Out] = bitsWidthOf(constVal)
//    }

  def op[W](width : TwoFace.Int[W], almanacEntryOp: AlmanacEntryOp) : DFBits[W] =
    new DFAny.Op[W](width, almanacEntryOp) with DFBits[W] {}
}