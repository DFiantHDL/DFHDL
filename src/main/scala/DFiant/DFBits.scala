package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import scodec.bits._


trait DFBits[W] extends DFBits.Unbounded {
  type Width = W
}


object DFBits extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBits.type] {
    type LW = Width
    type TVal = DFBits[LW]
    type TVar = DFBits.Var[LW]
    //////////////////////////////////////////////////////////////////////////
    // Single bit (Bool) selection
    //////////////////////////////////////////////////////////////////////////
    final def apply[I](relBit: BitIndex.Checked[I, Width]) : TBool = protBit(relBit.unsafeCheck(width))

    final def apply[I](implicit relBit: BitIndex.Checked[I, Width], di: DummyImplicit, di2: DummyImplicit): TBool =
      protBit(relBit.unsafeCheck(width))

    final def msbit: TBool = protBit(width - 1)

    final def lsbit: TBool = protBit(0)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Bit range selection
    //////////////////////////////////////////////////////////////////////////
    final def apply[H, L](relBitHigh: BitIndex.Checked[H, Width], relBitLow: BitIndex.Checked[L, Width])(
      implicit checkHiLow: BitsHiLo.CheckedShell[H, L], relWidth: RelWidth.TF[H, L]
    ) = {
      checkHiLow.unsafeCheck(relBitHigh, relBitLow)
      protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
    }

    final def apply[H, L](implicit relBitHigh: BitIndex.Checked[H, Width], relBitLow: BitIndex.Checked[L, Width],
      checkHiLow: BitsHiLo.Checked[H, L], relWidth: RelWidth.TF[H, L], di: DummyImplicit
    ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

    final protected def protMSBits[PW](partWidth: TwoFace.Int[PW]): TBits[PW] =
      DFBits.alias(this, partWidth, width - partWidth).asInstanceOf[TBits[PW]]

    final def msbits[PW](partWidth: PartWidth.Checked[PW, Width]) = protMSBits(partWidth.unsafeCheck(width))

    final def msbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], di: DummyImplicit) =
      protMSBits(partWidth.unsafeCheck(width))

    final protected def protLSBits[PW](partWidth: TwoFace.Int[PW]) : TBits[PW] =
      DFBits.alias(this, partWidth, 0).asInstanceOf[TBits[PW]]

    final def lsbits[PW](partWidth: PartWidth.Checked[PW, Width]) = protLSBits(partWidth.unsafeCheck(width))

    final def lsbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], di: DummyImplicit) =
      protLSBits(partWidth.unsafeCheck(width))
    //////////////////////////////////////////////////////////////////////////

    def extBy[N](numOfBits : Natural.Int.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int]
    ) : DFBits.Var[tfs.Out] = DFBits.newVar(tfs(width, numOfBits), getInit).assign(this)

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

    def newEmptyDFVar = DFBits.newVar(width, Seq(DFBits.Token(width, 0)))

    ///////////////////////////DFUInt.op[W](width, "toDFUInt", DFBits.Token.toUInt(getInit))
    def toDFUInt : DFUInt[Width] = DFUInt.newVar[Width](width, DFBits.Token.toUInt(getInit)).assign(this)

    override def toString : String = s"DFBits[$width]"
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFBits[W] with DFAny.Var {
    //    def setBits(range : BitsRange)                       : TVar = assignBits(range, bitsWidthToMaxBigIntBits(range.width))
    //    def clearBits(range : BitsRange)                     : TVar = assignBits(range,0)
    //    def assignBits(range : BitsRange, value : DFBits.Unsafe) : TVar = {this.protBitsUnsafe(range) := value; this}
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit dsn : DFDesign, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : Var[W] = newVar(checkedWidth, Seq(DFBits.Token(checkedWidth, 0)))
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit dsn : DFDesign
  ) : Var[W] = newVar(checkedWidth.unsafeCheck(), Seq(DFBits.Token(checkedWidth, 0)))
  def zeros[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
  def ones[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar[W](width : TwoFace.Int[W], init : Seq[Token] = Seq())(implicit dsn : DFDesign) : Var[W] =
    new DFAny.NewVar(width, init) with Var[W] {
      def codeString(idRef : String) : String = s"val $idRef = DFBits($width)"
    }

  protected[DFiant] def alias[W]
  (aliasedVar : DFAny, relWidth : TwoFace.Int[W], relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[DFBits.Token] = Seq())(implicit dsn : DFDesign) : Var[W] =
    new DFAny.Alias(aliasedVar, relWidth, relBitLow, deltaStep, updatedInit) with Var[W] {
      protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = token
      def codeString(idRef : String) : String = {
        val bitsCodeString = if (relWidth == aliasedVar.width) "" else s".bitsWL($relWidth, $relBitLow)"
        val prevCodeString = if (deltaStep < 0) s".prev(${-deltaStep})" else ""
        val initCodeString = if (updatedInit.isEmpty) "" else s".init(${updatedInit.codeString})"
        s"$idRef$bitsCodeString$initCodeString$prevCodeString"
      }
    }

  protected[DFiant] def const[W](token : DFBits.Token)(implicit dsn : DFDesign) : DFBits[W] =
    new DFAny.Const(token) with DFBits[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (val width : Int, val valueBits : BitVector, val bubbleMask : BitVector) extends DFAny.Token {
    final def | (that : Token) : Token = {
      val outWidth = scala.math.max(this.width, that.width)
      val outBitsValue = this.valueBits | that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def & (that : Token) : Token = {
      val outWidth = scala.math.max(this.width, that.width)
      val outBitsValue = this.valueBits & that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def ^ (that : Token) : Token = {
      val outWidth = scala.math.max(this.width, that.width)
      val outBitsValue = this.valueBits ^ that.valueBits
      val outBubbleMask = this.bubbleMask | that.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def ## (that : Token) : Token = {
      val outWidth = this.width + that.width
      val outBitsValue = this.valueBits ++ that.valueBits
      val outBubbleMask = this.bubbleMask ++ that.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def unary_~ : Token = {
      val outWidth = this.width
      val outBitsValue = ~this.valueBits
      val outBubbleMask = this.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    def toUInt : DFUInt.Token = {
      val outWidth = this.width
      val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width)
      val outBubble = isBubble
      new DFUInt.Token(outWidth, outValueUInt, outBubble)
    }
  }

  object Token {
    import DFAny.TokenSeq
    def | (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l | r)
    def & (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l & r)
    def ^ (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l ^ r)
    def ## (left : Seq[Token], right : Seq[Token]) : Seq[Token] = TokenSeq(left, right)((l, r) => l ## r)
    def unary_~ (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => ~t)
    def toUInt(left : Seq[Token]) : Seq[DFUInt.Token] = TokenSeq(left)(t => t.toUInt)

    def apply(width : Int, value : Int) : Token = Token(width, BitVector.fromInt(value, width))
    def apply(width : Int, value : Long) : Token = Token(width, BitVector.fromLong(value, width))
    def apply(width : Int, value : BitVector) : Token = {
      //TODO: Boundary checks
      new Token(width, value.toLength(width), BitVector.low(width))
    }
    def apply(width : Int, value : Bubble) : Token = new Token(width, BitVector.low(width), BitVector.high(width))
    def apply(width : Int, value : Token) : Token = {
      //TODO: Boundary checks
      value.bits(width-1, 0)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      private type IntWithinWidth[LW] = CompileTime[Natural.Int.Cond[GetArg0] && (BitsWidthOf.CalcInt[GetArg0] <= LW)]
      private type LongWithinWidth[LW] = CompileTime[Natural.Long.Cond[GetArg0] && (BitsWidthOf.CalcLong[GetArg0] <= LW)]
      implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
      implicit class DFBitsToken[LW](val right : DFBits.Token) extends Able[DFBits[LW]]
      implicit class DFBitsTokenSeq[LW](val right : Seq[DFBits.Token]) extends Able[DFBits[LW]]
      implicit class DFBitsInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFBits[LW]]
      implicit class DFBitsLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFBits[LW]]
      implicit class DFBitsBitVector[LW](val right : BitVector) extends Able[DFBits[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFBits[LW]]]) : Seq[DFBits.Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => DFBits.Token(width, t)
          case (t : DFBits.Token) => DFBits.Token(width, t)
          case (t : Int) => DFBits.Token(width, t)
          case (t : Long) => DFBits.Token(width, t)
          case (t : BitVector) => DFBits.Token(width, t)
        })
    }
    trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
    object Builder {
      implicit def ev[LW](implicit dsn : DFDesign) : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
        def apply(left : DFBits[LW], right : Seq[Able[DFBits[LW]]]) : DFBits[LW] =
          DFBits.alias(left, left.width, 0, 0, Able.toTokenSeq(left.width, right))
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
      implicit def ev[LW](implicit dsn : DFDesign) : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
        def apply[P](left : DFBits[LW], right : Natural.Int.Checked[P]) : DFBits[LW] =
          DFBits.alias(left, left.width, 0, -right, left.getInit)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  object Port extends Port {

  }

  object Op extends Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    object Able extends super.Implicits
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=` extends `Op:=` {

  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  object `Op==` extends `Op==` {

  }

  object `Op!=` extends `Op!=` {

  }
}
