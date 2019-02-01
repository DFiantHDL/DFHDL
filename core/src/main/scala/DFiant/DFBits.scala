package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._

object Zeros extends DFBits.SameBitsVector(false)
object Ones extends DFBits.SameBitsVector(true)

trait DFBits[W] extends DFBits.Unbounded {
  type Width = W
}


object DFBits extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBits.type] {
    type TUnbounded = Unbounded
    type TVal = DFBits[Width]
    type TVar = DFBits.Var[Width]
    type TToken = DFBits.Token
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFBits.Pattern.Builder[L]
    type OpAble[R] = Op.Able[R]
    type `Op<>Builder`[R] = `Op<>`.Builder[TVal, R]
    type `Op:=Builder`[R] = `Op:=`.Builder[TVal, R]
    type `Op==Builder`[R] = `Op==`.Builder[TVal, R]
    type `Op!=Builder`[R] = `Op!=`.Builder[TVal, R]
    type InitAble[L <: DFAny] = Init.Able[L]
    type InitBuilder = Init.Builder[TVal, TToken]
    type PortBuilder[Dir <: DFDir] = Port.Builder[TVal, Dir]
    //////////////////////////////////////////////////////////////////////////
    // Single bit (Bool) selection
    //////////////////////////////////////////////////////////////////////////
    final def apply[I](relBit: BitIndex.Checked[I, Width])(implicit ctx : DFAny.Alias.Context) : TBool = protBit(relBit.unsafeCheck(width))

    final def apply[I](implicit relBit: BitIndex.Checked[I, Width], ctx : DFAny.Alias.Context, di: DummyImplicit, di2: DummyImplicit): TBool =
      protBit(relBit.unsafeCheck(width))

    final def msbit(implicit ctx : DFAny.Alias.Context): TBool = protBit(width - 1)

    final def lsbit(implicit ctx : DFAny.Alias.Context): TBool = protBit(0)
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Bit range selection
    //////////////////////////////////////////////////////////////////////////
    final def apply[H, L](relBitHigh: BitIndex.Checked[H, Width], relBitLow: BitIndex.Checked[L, Width])(
      implicit checkHiLow: BitsHiLo.CheckedShell[H, L], relWidth: RelWidth.TF[H, L], ctx : DFAny.Alias.Context
    ) = {
      checkHiLow.unsafeCheck(relBitHigh, relBitLow)
      protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
    }

    final def apply[H, L](implicit relBitHigh: BitIndex.Checked[H, Width], relBitLow: BitIndex.Checked[L, Width],
      checkHiLow: BitsHiLo.Checked[H, L], relWidth: RelWidth.TF[H, L], ctx : DFAny.Alias.Context, di: DummyImplicit
    ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

    final protected[DFiant] def protMSBits[PW](partWidth: TwoFace.Int[PW])(implicit ctx : DFAny.Alias.Context): TBits[PW] =
      new DFBits.Alias[PW](DFAny.Alias.Reference.BitsWL(this, partWidth, width - partWidth, s".msbits($partWidth)")).asInstanceOf[TBits[PW]]

    final def msbits[PW](partWidth: PartWidth.Checked[PW, Width])(implicit ctx : DFAny.Alias.Context) = protMSBits(partWidth.unsafeCheck(width))

//    final def msbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], ctx : DFAny.Alias.Context, di: DummyImplicit) =
//      protMSBits(partWidth.unsafeCheck(width))

    final protected[DFiant] def protLSBits[PW](partWidth: TwoFace.Int[PW])(implicit ctx : DFAny.Alias.Context) : TBits[PW] =
      new DFBits.Alias[PW](DFAny.Alias.Reference.BitsWL(this, partWidth, 0, s".lsbits($partWidth)")).asInstanceOf[TBits[PW]]

    final def lsbits[PW](partWidth: PartWidth.Checked[PW, Width])(implicit ctx : DFAny.Alias.Context) = protLSBits(partWidth.unsafeCheck(width))

//    final def lsbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], ctx : DFAny.Alias.Context, di: DummyImplicit) =
//      protLSBits(partWidth.unsafeCheck(width))
    //////////////////////////////////////////////////////////////////////////

    final def extendLeftBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFBits[tfs.Out] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits, 0))
      new DFBits.Alias[tfs.Out](DFAny.Alias.Reference.Concat(List(zeros, this), s".bits")).setAutoConstructCodeString(s"$refCodeString.extendLeftBy($numOfBits)")
    }

    final def extendLeftTo[EW](numOfBits : ExtWidth.Checked[EW, Width])(implicit ctx : DFAny.Alias.Context)
    : DFBits[EW] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits - width, 0))
      new DFBits.Alias[EW](DFAny.Alias.Reference.Concat(List(zeros, this), s".bits")).setAutoConstructCodeString(s"$refCodeString.extendLeftTo($numOfBits)")
    }

    final def extendRightBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFBits[tfs.Out] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits, 0))
      new DFBits.Alias[tfs.Out](DFAny.Alias.Reference.Concat(List(this, zeros), s".bits")).setAutoConstructCodeString(s"$refCodeString.extendRightBy($numOfBits)")
    }

    final def extendRightTo[EW](numOfBits : ExtWidth.Checked[EW, Width])(implicit ctx : DFAny.Alias.Context)
    : DFBits[EW] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits - width, 0))
      new DFBits.Alias[EW](DFAny.Alias.Reference.Concat(List(this, zeros), s".bits")).setAutoConstructCodeString(s"$refCodeString.extendRightTo($numOfBits)")
    }

    protected object SameWidth extends Checked1Param.Int {
      type Cond[MW, W] = MW == W
      type Msg[MW, W] = "The mold constructor width " + ToString[MW] + " is different than the aliased variable width " + ToString[W]
      type ParamFace = Int
    }

    def as[T <: DFAny.NewVar[_]](mold : T)(
      implicit sameWidth : SameWidth.CheckedShell[mold.Width, Width], ctx : DFAny.Alias.Context
    ) : mold.TVal = {
      sameWidth.unsafeCheck(mold.width, width)
      mold.alias(DFAny.Alias.Reference.AsIs(this, ""))
    }

    final def uint(implicit ctx : DFAny.Alias.Context) : TUInt[Width] =
      new DFUInt.Alias[Width](DFAny.Alias.Reference.AsIs(this, ".uint")).asInstanceOf[TUInt[Width]]

    final def sint(implicit widthCheck : SIntWidth.CheckedShell[Width], ctx : DFAny.Alias.Context) : TSInt[Width] = {
      widthCheck.unsafeCheck(width)
      new DFSInt.Alias[Width](DFAny.Alias.Reference.AsIs(this, s".sint")).asInstanceOf[TSInt[Width]]
    }

    final def |  [R](right: Op.Able[R])(implicit op: `Op|`.Builder[TVal, R]) = op(left, right)
    final def &  [R](right: Op.Able[R])(implicit op: `Op&`.Builder[TVal, R]) = op(left, right)
    final def ^  [R](right: Op.Able[R])(implicit op: `Op^`.Builder[TVal, R]) = op(left, right)
    final def ## [R](right: Op.Able[R])(implicit op: `Op##`.Builder[TVal, R]) = op(left, right)
    final def == (right : SameBitsVector)(implicit op: `Op==`.Builder[TVal, SameBitsVector]) : DFBool = op(left, right)
    final def != (right : SameBitsVector)(implicit op: `Op!=`.Builder[TVal, SameBitsVector]) : DFBool = op(left, right)
    final def == [R](that : BitVector)(implicit right : GetArg.Aux[ZeroI, R], op: `Op==`.Builder[TVal, R]) : DFBool = op(left, right)
    final def != [R](that : BitVector)(implicit right : GetArg.Aux[ZeroI, R], op: `Op!=`.Builder[TVal, R]) : DFBool = op(left, right)
    final private[DFiant] def << (shift: Int)(implicit ctx : DFAny.Alias.Context) : DFBits[Width] = {
      if (shift >= width) new DFBits.Const[Width](DFBits.Token(width, 0))
      else {
        val remainingBits = this.protLSBits(width - shift)
        val zeros = new DFBits.Const[Int](DFBits.Token(shift, 0))
        new DFBits.Alias[Width](DFAny.Alias.Reference.Concat(List(remainingBits, zeros), ".bits")).setAutoConstructCodeString(s"$refCodeString << $shift")
      }
    }
    final private[DFiant] def >> (shift: Int)(implicit ctx : DFAny.Alias.Context) : DFBits[Width] = {
      if (shift >= width) new DFBits.Const[Width](DFBits.Token(width, 0))
      else {
        val remainingBits = this.protMSBits(width - shift)
        val zeros = new DFBits.Const[Int](DFBits.Token(shift, 0))
        new DFBits.Alias[Width](DFAny.Alias.Reference.Concat(List(zeros, remainingBits), ".bits")).setAutoConstructCodeString(s"$refCodeString >> $shift")
      }
    }

    final def << [R](right: OpsShift.Able[R])(implicit op: `Op<<`.Builder[TVal, R]) = op(left, right)
    final def >> [R](right: OpsShift.Able[R])(implicit op: `Op>>`.Builder[TVal, R]) = op(left, right)

    final def unary_~(implicit ctx : DFAny.Alias.Context) : DFBits[Width] =
      new DFBits.Alias[Width](DFAny.Alias.Reference.Invert(this, ".invert")) //TODO: change refCodeString to accept prefix
//    def isZero: DFBool = this == 0
//    def isNonZero: DFBool = this != 0

//    def isAllOnes: DFBool = ??? //this == bitsWidthToMaxBigIntBits(width)
//    def isNotAllOnes: DFBool = ??? //this != bitsWidthToMaxBigIntBits(width)

    def newEmptyDFVar(implicit ctx : DFAny.NewVar.Context) = new DFBits.NewVar[Width](width)
    final protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
      implicit ctx : DFAny.Alias.Context
    ) : TAlias = new Alias(reference)(ctx).asInstanceOf[TAlias]

    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar[Width](width), dir)
    __dslMemberFields.setAutoTypeName(s"DFBits[$width]")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[W] extends DFBits[W] with DFAny.Var {
    final override def as[T <: DFAny.NewVar[_]](mold : T)(
      implicit sameWidth : SameWidth.CheckedShell[mold.Width, Width], ctx : DFAny.Alias.Context
    ) : mold.TVar = {
      sameWidth.unsafeCheck(mold.width, width)
      mold.alias(DFAny.Alias.Reference.AsIs(this, ""))
    }
    //    def setBits(range : BitsRange)                       : TVar = assignBits(range, bitsWidthToMaxBigIntBits(range.width))
    //    def clearBits(range : BitsRange)                     : TVar = assignBits(range,0)
    //    def assignBits(range : BitsRange, value : DFBits.Unsafe) : TVar = {this.protBitsUnsafe(range) := value; this}
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[W] {
      def apply(bits : DFBits[W], sbv : SameBitsVector) : DFBits[W]
    }
    object Builder {
      implicit def ev[W](implicit ctx : DFAny.Const.Context)
      : Builder[W] = (bits, sbv) => new Const[W](Token(BitVector.fill(bits.width.toLong)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def apply[W](
    implicit ctx : DFAny.NewVar.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : NewVar[W] = new NewVar(checkedWidth)
  def apply[W](checkedWidth : BitsWidth.Checked[W])(
    implicit ctx : DFAny.NewVar.Context
  ) : NewVar[W] = new NewVar(checkedWidth.unsafeCheck())
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFBits[W]](width, s"DFBits($width)") with Var[W]

  protected[DFiant] final class Alias[W](reference: DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFBits[W]](reference) with Var[W] {
    //TODO:
    //Every bit in DFBits can have a different path, at least until a top-level output is reached,
    //so no need to balance too early, thus potentially saving resources.
//    override protected def aliasPipeBalance(pipe : Pipe) : Pipe = pipe
  }

  protected[DFiant] final class Const[W](token : DFBits.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const[DFBits[W]](token) with DFBits[W]

  protected[DFiant] final class Port[W, Dir <: DFDir](dfVar : DFBits[W], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFBits[W], Dir](dfVar, dir) with DFBits[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Token private[DFiant] (width : Int, valueBits : BitVector, bubbleMask : BitVector) extends DFAny.Token.Of[BitVector, Pattern] {
    type TToken = Token
    final val value = valueBits
    def toBubbleToken : Token = Token(width, Bubble)
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
    final def << (that : DFUInt.Token) : Token = {
      val shift = that.value.toInt
      val outWidth = this.width
      val outBitsValue = this.valueBits << shift
      val outBubbleMask = this.bubbleMask << shift
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def >> (that : DFUInt.Token) : Token = {
      val shift = that.value.toInt
      val outWidth = this.width
      val outBitsValue = this.valueBits >>> shift
      val outBubbleMask = this.bubbleMask >>> shift
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def unary_~ : Token = {
      val outWidth = this.width
      val outBitsValue = ~this.valueBits
      val outBubbleMask = this.bubbleMask
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def reverse : Token = {
      val outWidth = this.width
      val outBitsValue = this.valueBits.reverseBitOrder
      val outBubbleMask = this.bubbleMask.reverseBitOrder
      new Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def == (that : Token) : DFBool.Token = DFBool.Token(this.valueBits == that.valueBits, this.isBubble || that.isBubble)
    final def != (that : Token) : DFBool.Token = DFBool.Token(this.valueBits != that.valueBits, this.isBubble || that.isBubble)
    def toUInt : DFUInt.Token = {
      val outWidth = this.width
      val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width)
      val outBubble = isBubble
      new DFUInt.Token(outWidth, outValueUInt, outBubble)
    }
    def toSInt : DFSInt.Token = {
      val outWidth = this.width
      val outValueSInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
      val outBubble = isBubble
      new DFSInt.Token(outWidth, outValueSInt, outBubble)
    }
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    val | : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l | r)
    val & : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l & r)
    val ^ : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l ^ r)
    val concat : (Seq[Token], Seq[Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l ## r)
    val << : (Seq[Token], Seq[DFUInt.Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l << r)
    val >> : (Seq[Token], Seq[DFUInt.Token]) => Seq[Token] = (left, right) => TokenSeq(left, right)((l, r) => l >> r)
    val == : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l == r)
    val != : (Seq[Token], Seq[Token]) => Seq[DFBool.Token] = (left, right) => TokenSeq(left, right)((l, r) => l != r)
    def unary_~ (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => ~t)
    def reverse (left : Seq[Token]) : Seq[Token] = TokenSeq(left)(t => t.reverse)
    def toUInt(left : Seq[Token]) : Seq[DFUInt.Token] = TokenSeq(left)(t => t.toUInt)

    def apply(width : Int, value : Int) : Token = Token(width, BigInt(value).toBitVector(width))
    def apply(width : Int, value : BitVector) : Token = {
      assert(value.length == width, s"\nThe init vector $value must have a width of $width")
      new Token(width, value.toLength(width), BitVector.low(width))
    }
    def apply(value : BitVector) : Token = Token(value.length.toInt, value)
    def apply(width : Int, value : Bubble) : Token = new Token(width, BitVector.low(width), BitVector.high(width))
    def apply(width : Int, value : Token) : Token = {
      assert(value.width == width, s"\nThe init vector $value must have a width of $width")
      value.bits(width-1, 0)
    }
    implicit def bubbleOf[W] : DFBits[W] => Token = t => Token(t.width, Bubble)
    implicit val fromBits : DFBits.Token => Token = t => t
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[LW, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFBits[LW], Dir] = (right, dir) => new Port[LW, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : mold.TVar =
      new Alias[mold.Width](DFAny.Alias.Reference.AsIs(left, s".as(DFBits(${mold.width}))"))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
      implicit class DFBitsSameBitsVector[LW](val right : SameBitsVector) extends Able[DFBits[LW]]
      implicit class DFBitsToken[LW](val right : Token) extends Able[DFBits[LW]]
      implicit class DFBitsTokenSeq[LW](val right : Seq[Token]) extends Able[DFBits[LW]]
      implicit class DFBitsBitVector[LW](val right : BitVector) extends Able[DFBits[LW]]
      implicit class DFBitsSeqOfBitVector[LW](val right : Seq[BitVector]) extends Able[DFBits[LW]]
      implicit class DFBitsXBitVector[LW](val right : XBitVector[LW]) extends Able[DFBits[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFBits[LW]]]) : Seq[Token] =
        right.toSeqAny.collect{
          case t : Bubble => Token(width, t)
          case t : Token => Token(width, t)
          case t : BitVector => Token(width, t)
          case t : SameBitsVector => Token(width, BitVector.fill(width)(t.value))
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[BitVector]) extends DFAny.Pattern.OfSet[BitVector, Pattern](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val bitVector : BitVector
    }
    object Able {
      implicit class DFUIntPatternBitVector[R <: BitVector](val right : R) extends Able[R] {
        val bitVector : BitVector = right
      }
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
        def apply[R](left: DFBits[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.bitVector).foldLeft(Set.empty[BitVector])((set, bitVector) => {
            if (set.contains(bitVector)) throw new IllegalArgumentException(s"\nThe bitvector $bitVector already intersects with $set")
            if (bitVector.length > left.width) throw new IllegalArgumentException(s"\nThe bitvector $bitVector is wider than ${left.name}")
            set + bitVector
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
      final def |  [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      final def &  [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ^  [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ## [RW](right : DFBits[RW])(implicit op: `Op##`.Builder[L, DFBits[RW]]) = op(left, right)
      final def <> [RW, RDIR <: DFDir](port : DFBits[RW] <> RDIR)(
        implicit op: `Op<>`.Builder[DFBits[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFBitsFromBitVector(left : BitVector) extends Able[BitVector](left)
      final implicit def DFBitsFromBitVector(left: BitVector): DFBitsFromBitVector = new DFBitsFromBitVector(left)
      sealed class DFBitsFromXBitVector[W](left : XBitVector[W]) extends Able[XBitVector[W]](left)
      final implicit def DFBitsFromXBitVector[W](left: XBitVector[W]): DFBitsFromXBitVector[W] = new DFBitsFromXBitVector[W](left)
      sealed class DFBitsFromZeros(left : SameBitsVector) extends Able[SameBitsVector](left)
      final implicit def DFBitsFromZeros(left : SameBitsVector) : DFBitsFromZeros = new DFBitsFromZeros(left)
      sealed class DFBitsFromDFBool(left : DFBool)(implicit ctx : DFAny.Alias.Context) extends Able[DFBits[1]](new Alias[1](DFAny.Alias.Reference.AsIs(left, "")))
      final implicit def DFBitsFromDFBool(left: DFBool)(implicit ctx : DFAny.Alias.Context): DFBitsFromDFBool = new DFBitsFromDFBool(left)
      final implicit def ofDFBits[R <: DFBits.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Implicit Evidence of DFUInt
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : DFBits[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromBitVector(implicit ctx : DFAny.Const.Context)
      : Aux[BitVector, Int] = new Builder[BitVector] {
        type W = Int
        def apply(value : BitVector) : DFBits[W] = {
          new Const[W](Token(value.length.toInt, value))
        }
      }
      implicit def fromXBitVector[W0](implicit ctx : DFAny.Const.Context)
      : Aux[XBitVector[W0], W0] = new Builder[XBitVector[W0]] {
        type W = W0
        def apply(value : XBitVector[W0]) : DFBits[W] = {
          new Const[W](Token(value.length.toInt, value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>`[Ctx] extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "An assignment/connection operation does not permit different widths. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, R, RW](properR : (L, R) => DFBits[RW]) : Aux[L, R, DFBits[RW]] =
        new Builder[L, R] {
          type Comp = DFBits[RW]
          def apply(leftL : L, rightR : R) : Comp =  properR(leftL, rightR)
        }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : Ctx,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFBits[LW], DFBits[RW], DFBits[RW]] =
        create[DFBits[LW], DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFBits_op_SBV[LW](
        implicit
        rSBV : SameBitsVector.Builder[LW]
      ) : Aux[DFBits[LW], SameBitsVector, DFBits[LW]] =
        create[DFBits[LW], SameBitsVector, LW]((left, right) => {
          rSBV(left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : Ctx,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFBits[LW], R, DFBits[RW]] = create[DFBits[LW], R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right
      })
    }
  }
  object `Op:=` extends `Ops:=,<>`[DFAny.Op.Context]
  object `Op<>` extends `Ops:=,<>`[DFAny.Connector.Context]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsLogic(opKind : DiSoOp.Kind) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Logic operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = Max[LW, RW]
        type OW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, OW](
          implicit
          ctx : DFAny.Op.Context,
          oW : Inference.OW[LW, RW, OW],
          checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
        ) : DetailedBuilder[L, LW, R, RW]{type Comp = DFBits[OW] with CanBePiped} =
          new DetailedBuilder[L, LW, R, RW]{
            type Comp = DFBits[OW] with CanBePiped
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Comp] =
              new Builder[L, R] {
                type Comp = DFBits[OW] with CanBePiped
                def apply(leftL : L, rightR : R) : Comp = {
                  import FunctionalLib.DFBitsOps._
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  val opInst = opKind match {
                    case DiSoOp.Kind.| => `Func2Comp|`[LW, RW, OW](left, right)
                    case DiSoOp.Kind.& => `Func2Comp&`[LW, RW, OW](left, right)
                    case DiSoOp.Kind.^ => `Func2Comp^`[LW, RW, OW](left, right)
                    case _ => throw new IllegalArgumentException("Unexpected logic operation")
                  }
                  opInst.setAutoName(s"${ctx}")
                  opInst
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsgSym[false, "An unconstrained-width literal cannot be used in a logic operation", Builder[_,_]]

      implicit def evDFBits_op_SBV[LW](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SameBitsVector, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW](implicit error : UnconstrainedLiteralError)
      : Aux[SameBitsVector, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(DiSoOp.Kind.|)
  object `Op&` extends OpsLogic(DiSoOp.Kind.&)
  object `Op^` extends OpsLogic(DiSoOp.Kind.^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsShift(opKind : DiSoOp.Kind) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Shift Ops with the type ${R}")
    trait Builder[L <: DFAny, R] extends DFAny.Op.Builder[L, R] {
      type Comp <: DFBits.Unbounded
    }
    object Builder {
      object SmallShift extends Checked1Param.Int {
        type Cond[LW, RW] = BitsWidthOf.CalcInt[LW] >= RW
        type Msg[LW, RW] = "The shift vector is too large. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFBits_op_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : SmallShift.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFBits[LW], DFUInt[RW]] {type Comp = DFBits[LW] with CanBePiped} =
        new Builder[DFBits[LW], DFUInt[RW]]{
          type Comp = DFBits[LW] with CanBePiped
          def apply(left : DFBits[LW], right : DFUInt[RW]) : DFBits[LW] with CanBePiped = {
            import FunctionalLib.DFBitsOps._
            // Completing runtime checks
            checkLWvRW.unsafeCheck(left.width, right.width)
            // Constructing op
            val opInst = opKind match {
              case DiSoOp.Kind.<< => `Func2Comp<<`(left, right)
              case DiSoOp.Kind.>> => `Func2Comp>>`(left, right)
              case _ => throw new IllegalArgumentException("Unexpected logic operation")
            }
            opInst.setAutoName(s"${ctx}")
            opInst
          }
        }
      implicit def evDFBits_op_XInt[LW, R <: Int](
        implicit
        ctx : DFAny.Alias.Context,
        check : Natural.Int.CheckedShellSym[Builder[_,_], R]
      ) : Builder[DFBits[LW], R]{type Comp = DFBits[LW]} = new Builder[DFBits[LW], R]{
        type Comp = DFBits[LW]
        def apply(left : DFBits[LW], right : R) : DFBits[LW] = {
          check.unsafeCheck(right)
          opKind match {
            case DiSoOp.Kind.<< => left << right
            case DiSoOp.Kind.>> => left >> right
            case _ => throw new IllegalArgumentException("Unexpected logic operation")
          }
        }
      }
    }
  }
  object OpsShift {
    class Able[R](val value : R) extends DFAny.Op.Able[R]
    object Able {
      implicit class FromXInt[R <: XInt](right : R) extends Able[R](right)
      implicit class FromInt[R <: Int](right : R) extends Able[R](right)
      implicit class FromDFUInt[RW](right : DFUInt[RW]) extends Able[DFUInt[RW]](right)
    }
  }
  object `Op<<` extends OpsShift(DiSoOp.Kind.<<)
  object `Op>>` extends OpsShift(DiSoOp.Kind.>>)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Concatenation operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op##` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Comp = Comp0
      }

      object Inference {
        type CalcW[LW, RW] = LW + RW
        type OW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Comp
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Comp]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, OW](
          implicit
          ctx : DFAny.Alias.Context,
          oW : Inference.OW[LW, RW, OW],
        ) : DetailedBuilder[L, LW, R, RW]{type Comp = DFBits[OW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Comp = DFBits[OW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Comp] =
              new Builder[L, R] {
                type Comp = DFBits[OW]
                def apply(leftL : L, rightR : R) : Comp = {
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val oWidth = oW(left.width, right.width)
                  val out = new DFBits.Alias[OW](DFAny.Alias.Reference.Concat(List(left, right), ".bits"))
                  out
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Alias.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Alias.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsgSym[false, "An unconstrained-width literal cannot be used in a concatenation operation", Builder[_,_]]

      implicit def evDFBits_op_SBV[LW](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SameBitsVector, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW](implicit error : UnconstrainedLiteralError)
      : Aux[SameBitsVector, DFBits[RW], DFBits[RW]] = ???
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////




  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBool.Token]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool with CanBePiped}

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFBits[LW], DFBits[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import FunctionalLib.DFBitsOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => new `Func2Comp==`(left, right)
          case DiSoOp.Kind.!= => new `Func2Comp!=`(left, right)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.setAutoName(s"$ctx")
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] = create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })

      implicit def evDFBits_op_SBV[LW](
        implicit
        ctx : DFAny.Op.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[DFBits[LW], SameBitsVector] = create[DFBits[LW], LW, SameBitsVector, LW]((left, rightSBV) => {
        val right = rSBV(left, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW](
        implicit
        ctx : DFAny.Op.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SameBitsVector, DFBits[RW]] = create[SameBitsVector, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFBits.Token.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFBits.Token.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
