package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.tokens._

trait DFBits[W] extends DFAny.Val[W, TokenBits, DFBits[W], DFBits.Var[W]] {
  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[I](relBit: BitIndex.Checked[I, W]): TBool = protBit(relBit.unsafeCheck(width))

  final def apply[I](implicit relBit: BitIndex.Checked[I, W], di: DummyImplicit, di2: DummyImplicit): TBool =
    protBit(relBit.unsafeCheck(width))

  final def msbit: TBool = protBit(width - 1)

  final def lsbit: TBool = protBit(0)
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def apply[H, L](relBitHigh: BitIndex.Checked[H, W], relBitLow: BitIndex.Checked[L, W])(
    implicit checkHiLow: BitsHiLo.CheckedShell[H, L], relWidth: RelWidth.TF[H, L]
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def apply[H, L](implicit relBitHigh: BitIndex.Checked[H, W], relBitLow: BitIndex.Checked[L, W],
    checkHiLow: BitsHiLo.Checked[H, L], relWidth: RelWidth.TF[H, L], di: DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

  final protected def protMSBits[PW](partWidth: TwoFace.Int[PW]): TBits[PW] =
    DFBits.alias(this, partWidth, width - partWidth).asInstanceOf[TBits[PW]]

  final def msbits[PW](partWidth: PartWidth.Checked[PW, W]) = protMSBits(partWidth.unsafeCheck(width))

  final def msbits[PW](implicit partWidth: PartWidth.Checked[PW, W], di: DummyImplicit) =
    protMSBits(partWidth.unsafeCheck(width))

  final protected def protLSBits[PW](partWidth: TwoFace.Int[PW]): TBits[PW] =
    DFBits.alias(this, partWidth, 0).asInstanceOf[TBits[PW]]

  final def lsbits[PW](partWidth: PartWidth.Checked[PW, W]) = protLSBits(partWidth.unsafeCheck(width))

  final def lsbits[PW](implicit partWidth: PartWidth.Checked[PW, W], di: DummyImplicit) =
    protLSBits(partWidth.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  def extBy[N](numOfBits : Natural.Int.Checked[N])(
    implicit tfs : TwoFace.Int.Shell2[+, W, Int, N, Int]
  ) : DFBits.Var[tfs.Out] = DFBits.newVar(tfs(width, numOfBits)).init(getInit).assign(this)

  //  def ^ (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpXor(this, that)
  //  def | (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpOr(this, that)
  //  def & (that : DFBits.Unsafe)         : DFBits.Unsafe = ??? //AlmanacEntryOpAnd(this, that)

  //  def unary_~                   : DFBits.Unsafe = ??? //AlmanacEntryOpInv(this)
  //  def >> (that : DFBits.Unsafe)        : DFBits.Unsafe = ???
  //  def << (that : DFBits.Unsafe)        : DFBits.Unsafe = ???
  //  def << (that : Int)           : DFBits.Unsafe = ??? //AlmanacEntryOpLsh(this, AlmanacEntryConst(that))
  //  def >> (that : Int)           : DFBits.Unsafe = ??? //AlmanacEntryOpRsh(this, AlmanacEntryConst(that))
  //  def ## (that : DFBits.Unsafe)        : DFBits.Unsafe = ??? //AlmanacEntryOpCat(this, that)
  //      def ## (that : DFBool)        : DFBits.Unsafe = AlmanacEntryOpCat(this, that.bits())
  def ==(that: Int): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def ==(that: Long): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def ==(that: BigInt): DFBool = ??? //__==(this, AlmanacEntryConst(that))
  def !=(that: Int): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def !=(that: Long): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def !=(that: BigInt): DFBool = ??? //__!=(this, AlmanacEntryConst(that))
  def isZero: DFBool = this == 0
  def isNonZero: DFBool = this != 0

  def isAllOnes: DFBool = ??? //this == bitsWidthToMaxBigIntBits(width)
  def isNotAllOnes: DFBool = ??? //this != bitsWidthToMaxBigIntBits(width)

  def newEmptyDFVar = DFBits.newVar(width)

  ///////////////////////////DFUInt.op[W](width, "toDFUInt", TokenBits.toUInt(getInit))
  def toDFUInt : DFUInt[W] = DFUInt.newVar[W](width).init(TokenBits.toUInt(getInit)).assign(this)

  override def toString : String = s"DFBits[$width]"

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
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit) : Var[W] = newVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = newVar(checkedWidth.unsafeCheck())
  def zeros[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
  def ones[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
  ///////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar[W](width : TwoFace.Int[W]) : Var[W] =
    new DFAny.NewVar(width, Seq(TokenBits(width, 0))) with Var[W] {
      def codeString(idRef : String) : String = s"val $idRef = DFBits($width)"
    }

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[TokenBits] = Seq()) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W] {
      protected def protTokenBitsToTToken(token : TokenBits) : TToken = token
      def codeString(idRef : String) : String = {
        val bitsCodeString = if (relWidth == aliasedVar.width) "" else s".bitsWL($relWidth, $relBitLow)"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitsCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def const[W](token : TokenBits) : DFBits[W] =
    new DFAny.Const(token) with DFBits[W]

  protected[DFiant] def op[W](width : TwoFace.Int[W], opString : String, opInit : Seq[TokenBits], args : DFAny*) : DFBits[W] =
    new DFAny.Op(width, opString, opInit, args) with DFBits[W]
  ///////////////////////////////////////////////////////////////////////////////////////////
}
