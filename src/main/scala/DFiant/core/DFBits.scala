package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib.{`Op+`, _}
import DFiant.internals.RelWidth.TF
import DFiant.{core, internals}
import DFiant.tokens._

trait DFBits[W] extends DFAny.Val[W, TokenBits, DFBits[W], DFBits.Var[W]] {
  //  implicit def bits2Entry(dfBits: DFBits[W]) : AlmanacEntry = dfBits.getCurrentEntry
  //  implicit def entry2DFBits(entry: AlmanacEntry) : DFBits[W] = DFBits.Unsafe.op(entry)
  //  implicit def entry2DFBool(entry: AlmanacEntry) : DFBool = DFBool.op(entry)

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[I](relBit: internals.BitIndex.Checked[I, W]): TBool = protBit(relBit.unsafeCheck(width))

  final def apply[I](implicit relBit: internals.BitIndex.Checked[I, W], di: DummyImplicit, di2: DummyImplicit): TBool =
    protBit(relBit.unsafeCheck(width))

  final def msbit: TBool = protBit(width - 1)

  final def lsbit: TBool = protBit(0)

  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[H, L](relBitHigh: internals.BitIndex.Checked[H, W], relBitLow: internals.BitIndex.Checked[L, W])(
    implicit checkHiLow: internals.BitsHiLo.CheckedShell[H, L], relWidth: TF[H, L]
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def apply[H, L](implicit relBitHigh: internals.BitIndex.Checked[H, W], relBitLow: internals.BitIndex.Checked[L, W],
                        checkHiLow: internals.BitsHiLo.Checked[H, L], relWidth: TF[H, L], di: DummyImplicit
                       ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

  final protected def protMSBits[PW](partWidth: TwoFace.Int[PW]): TBits[PW] =
    DFBits.alias(this, partWidth, width - partWidth).asInstanceOf[TBits[PW]]

  final def msbits[PW](partWidth: internals.PartWidth.Checked[PW, W]) = protMSBits(partWidth.unsafeCheck(width))

  final def msbits[PW](implicit partWidth: internals.PartWidth.Checked[PW, W], di: DummyImplicit) =
    protMSBits(partWidth.unsafeCheck(width))

  final protected def protLSBits[PW](partWidth: TwoFace.Int[PW]): TBits[PW] =
    DFBits.alias(this, partWidth, 0).asInstanceOf[TBits[PW]]

  final def lsbits[PW](partWidth: internals.PartWidth.Checked[PW, W]) = protLSBits(partWidth.unsafeCheck(width))

  final def lsbits[PW](implicit partWidth: internals.PartWidth.Checked[PW, W], di: DummyImplicit) =
    protLSBits(partWidth.unsafeCheck(width))

  //////////////////////////////////////////////////////////////////////////

  //  def extBy(numOfBits : Int)    : TBits[WUnsafe] = (DFBits.Unsafe(width + numOfBits) := this).asInstanceOf[TBits[WUnsafe]]

  //  def ^ (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpXor(this, that)
  //  def | (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpOr(this, that)
  //  def & (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpAnd(this, that)
  def +[R](that: `Op+`.Able[DFBits[W], R])(implicit op: `Op+`.Builder[DFBits[W], R]) = op(this, that)

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
  def ==(that: WUnsafe): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def ==(that: Long): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def ==(that: BigInt): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def !=(that: WUnsafe): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def !=(that: Long): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def !=(that: BigInt): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def isZero: DFBool = this == 0

  def isNonZero: DFBool = this != 0

  def isAllOnes: DFBool = this == bitsWidthToMaxBigIntBits(width)

  def isNotAllOnes: DFBool = this != bitsWidthToMaxBigIntBits(width)

  //  def < (that : DFBits.Unsafe)         : DFBool = ??? //AlmanacEntryOpLsTn(this, that)
  //  def >= (that : DFBits.Unsafe)        : DFBool = ??? //!(this < that)
  //  def > (that : DFBits.Unsafe)         : DFBool = ??? //that < this
  //  def <= (that : DFBits.Unsafe)        : DFBool = ??? //that >= this

  def dfTypeName: String = "DFBits"

  def newEmptyDFVar = DFBits.create(width)

  //  protected[DFiant] def __!= (arg0 : DFBits.Unsafe, arg1 : DFBits.Unsafe) : DFBool = arg0!=arg1
  //  protected[DFiant] def __== (arg0 : DFBits.Unsafe, arg1 : DFBits.Unsafe) : DFBool = arg0==arg1
}

object DFBits {
  ///////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFBits[W] with DFAny.Var[W, TokenBits, DFBits[W], DFBits.Var[W]] {
    //    def setBits(range : BitsRange)                       : TVar = assignBits(range, bitsWidthToMaxBigIntBits(range.width))
    //    def clearBits(range : BitsRange)                     : TVar = assignBits(range,0)
    //    def assignBits(range : BitsRange, value : DFBits.Unsafe) : TVar = {this.protBitsUnsafe(range) := value; this}
  }
  protected[DFiant] def create[W](width : TwoFace.Int[W]) : Var[W] =
    new DFAny.NewVar(width, Seq(TokenBits.fromNum(width, 0))) with Var[W]

  implicit def apply[W](implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit) : Var[W] = create(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = create(checkedWidth.unsafeCheck())
  ///////////////////////////////////////////////////////////////////////////////////////////

  protected[DFiant] def alias[W, L]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L]) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow) with Var[W]

  def constInt[C](constVal : TwoFace.Int[C])(implicit bitsWidthOf: BitsWidthOf.Int[C]) : DFBits[bitsWidthOf.Out] =
    new DFAny.Const(TokenBits.fromNum(bitsWidthOf(constVal), constVal)) with DFBits[bitsWidthOf.Out]

  def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[TokenBits], args : DFAny*) : DFBits[W] =
    new DFAny.Op(width, opString, opInit, args) with DFBits[W]
}