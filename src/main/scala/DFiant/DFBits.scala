package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._


trait DFBits[W] extends DFBits.Unbounded {
  type Width = W
}


object DFBits extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFBits.type] {
    type TVal = DFBits[Width]
    type TVar = DFBits.Var[Width]
    type TToken = DFBits.Token
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFBits.Pattern.Builder[L]
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
      new DFBits.Alias[PW](List(this), DFAny.Alias.Reference.BitsWL(partWidth, width - partWidth, s".msbits($partWidth)")).asInstanceOf[TBits[PW]]

    final def msbits[PW](partWidth: PartWidth.Checked[PW, Width])(implicit ctx : DFAny.Alias.Context) = protMSBits(partWidth.unsafeCheck(width))

    final def msbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], ctx : DFAny.Alias.Context, di: DummyImplicit) =
      protMSBits(partWidth.unsafeCheck(width))

    final protected[DFiant] def protLSBits[PW](partWidth: TwoFace.Int[PW])(implicit ctx : DFAny.Alias.Context) : TBits[PW] =
      new DFBits.Alias[PW](List(this), DFAny.Alias.Reference.BitsWL(partWidth, 0, s".lsbits($partWidth)")).asInstanceOf[TBits[PW]]

    final def lsbits[PW](partWidth: PartWidth.Checked[PW, Width])(implicit ctx : DFAny.Alias.Context) = protLSBits(partWidth.unsafeCheck(width))

    final def lsbits[PW](implicit partWidth: PartWidth.Checked[PW, Width], ctx : DFAny.Alias.Context, di: DummyImplicit) =
      protLSBits(partWidth.unsafeCheck(width))
    //////////////////////////////////////////////////////////////////////////

    final def extendLeftBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFBits[tfs.Out] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits, 0))
      new DFBits.Alias[tfs.Out](List(zeros, this), DFAny.Alias.Reference.AsIs(s".bits")).setAutoConstructCodeString(s"$refCodeString.extendLeftBy($numOfBits)")
    }

    final def extendLeftTo[EW](numOfBits : ExtWidth.Checked[EW, Width])(implicit ctx : DFAny.Alias.Context)
    : DFBits[EW] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits - width, 0))
      new DFBits.Alias[EW](List(zeros, this), DFAny.Alias.Reference.AsIs(s".bits")).setAutoConstructCodeString(s"$refCodeString.extendLeftTo($numOfBits)")
    }

    final def extendRightBy[N](numOfBits : Positive.Checked[N])(
      implicit
      tfs : TwoFace.Int.Shell2[+, Width, Int, N, Int], ctx : DFAny.Alias.Context
    ) : DFBits[tfs.Out] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits, 0))
      new DFBits.Alias[tfs.Out](List(this, zeros), DFAny.Alias.Reference.AsIs(s".bits")).setAutoConstructCodeString(s"$refCodeString.extendRightBy($numOfBits)")
    }

    final def extendRightTo[EW](numOfBits : ExtWidth.Checked[EW, Width])(implicit ctx : DFAny.Alias.Context)
    : DFBits[EW] = {
      val zeros = new DFBits.Const[Width](DFBits.Token(numOfBits - width, 0))
      new DFBits.Alias[EW](List(this, zeros), DFAny.Alias.Reference.AsIs(s".bits")).setAutoConstructCodeString(s"$refCodeString.extendRightTo($numOfBits)")
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
      mold.protComp.Alias(this, mold.asInstanceOf[mold.protComp.Unbounded]).asInstanceOf[mold.TVal]
    }

    final def uint(implicit ctx : DFAny.Alias.Context) : TUInt[Width] =
      new DFUInt.Alias[Width](List(this), DFAny.Alias.Reference.AsIs(".uint")).asInstanceOf[TUInt[Width]]

    final def sint(implicit widthCheck : SIntWidth.CheckedShell[Width], ctx : DFAny.Alias.Context) : TSInt[Width] = {
      widthCheck.unsafeCheck(width)
      new DFSInt.Alias[Width](List(this), DFAny.Alias.Reference.AsIs(s".sint")).asInstanceOf[TSInt[Width]]
    }

    final def |  [R](right: Op.Able[R])(implicit op: `Op|`.Builder[TVal, R]) = op(left, right)
    final def &  [R](right: Op.Able[R])(implicit op: `Op&`.Builder[TVal, R]) = op(left, right)
    final def ^  [R](right: Op.Able[R])(implicit op: `Op^`.Builder[TVal, R]) = op(left, right)
    final def ## [R](right: Op.Able[R])(implicit op: `Op##`.Builder[TVal, R]) = op(left, right)
    final private[DFiant] def << (shift: Int)(implicit ctx : DFAny.Alias.Context) : DFBits[Width] = {
      if (shift >= width) new DFBits.Const[Width](DFBits.Token(width, 0))
      else {
        val remainingBits = this.protLSBits(width - shift)
        val zeros = new DFBits.Const[Int](DFBits.Token(shift, 0))
        new DFBits.Alias[Width](List(remainingBits, zeros), DFAny.Alias.Reference.AsIs(".bits")).setAutoConstructCodeString(s"$refCodeString << $shift")
      }
    }
    final private[DFiant] def >> (shift: Int)(implicit ctx : DFAny.Alias.Context) : DFBits[Width] = {
      if (shift >= width) new DFBits.Const[Width](DFBits.Token(width, 0))
      else {
        val remainingBits = this.protMSBits(width - shift)
        val zeros = new DFBits.Const[Int](DFBits.Token(shift, 0))
        new DFBits.Alias[Width](List(zeros, remainingBits), DFAny.Alias.Reference.AsIs(".bits")).setAutoConstructCodeString(s"$refCodeString >> $shift")
      }
    }

    final def << [R](right: OpsShift.Able[R])(implicit op: `Op<<`.Builder[TVal, R]) = op(left, right)
    final def >> [R](right: OpsShift.Able[R])(implicit op: `Op>>`.Builder[TVal, R]) = op(left, right)

    final def unary_~(implicit ctx : DFAny.Alias.Context) : DFBits[Width] =
      new DFBits.Alias[Width](List(this), DFAny.Alias.Reference.Invert(".invert")) //TODO: change refCodeString to accept prefix
//    def isZero: DFBool = this == 0
//    def isNonZero: DFBool = this != 0

//    def isAllOnes: DFBool = ??? //this == bitsWidthToMaxBigIntBits(width)
//    def isNotAllOnes: DFBool = ??? //this != bitsWidthToMaxBigIntBits(width)

    def newEmptyDFVar(implicit ctx : DFAny.NewVar.Context) = new DFBits.NewVar[Width](width)

    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar[Width](width), dir)
    override lazy val typeName : String = s"DFBits[$width]"
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
      mold.protComp.Alias(this, mold.asInstanceOf[mold.protComp.Unbounded]).asInstanceOf[mold.TVar]
    }
    //    def setBits(range : BitsRange)                       : TVar = assignBits(range, bitsWidthToMaxBigIntBits(range.width))
    //    def clearBits(range : BitsRange)                     : TVar = assignBits(range,0)
    //    def assignBits(range : BitsRange, value : DFBits.Unsafe) : TVar = {this.protBitsUnsafe(range) := value; this}
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
//  def zeros[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
//  def ones[W](checkedWidth : BitsWidth.Checked[W]) : Var[W] = ???
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[W](width : TwoFace.Int[W])(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFBits[W]](width, s"DFBits($width)") with Var[W] {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
  }

  protected[DFiant] final class Alias[W](aliasedVars : List[DFAny], reference: DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFBits[W]](aliasedVars, reference) with Var[W]

  protected[DFiant] final class Const[W](token : DFBits.Token)(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFBits[W]

  protected[DFiant] final class Port[W, Dir <: DFDir](dfVar : DFBits[W], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFBits[W], Dir](dfVar, dir) with DFBits[W]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token private[DFiant] (width : Int, val valueBits : BitVector, val bubbleMask : BitVector) extends DFAny.Token.Of[BitVector, Pattern](width, valueBits) {
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

    def apply(width : Int, value : Int) : Token = Token(width, BitVector.fromInt(value, width))
    def apply(width : Int, value : BitVector) : Token = {
      //TODO: Boundary checks
      new Token(width, value.toLength(width), BitVector.low(width))
    }
    def apply(value : BitVector) : Token = Token(value.length.toInt, value)
    def apply(width : Int, value : Bubble) : Token = new Token(width, BitVector.low(width), BitVector.high(width))
    def apply(width : Int, value : Token) : Token = {
      //TODO: Boundary checks
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
      new Alias[mold.Width](List(left), DFAny.Alias.Reference.AsIs(s".as(DFBits(${mold.width}))"))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
      implicit class DFBitsToken[LW](val right : Token) extends Able[DFBits[LW]]
      implicit class DFBitsTokenSeq[LW](val right : Seq[Token]) extends Able[DFBits[LW]]
      implicit class DFBitsBitVector[LW](val right : BitVector) extends Able[DFBits[LW]]
      implicit class DFBitsXBitVector[LW](val right : XBitVector[LW]) extends Able[DFBits[LW]]

      def toTokenSeq[LW](width : Int, right : Seq[Able[DFBits[LW]]]) : Seq[Token] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token(width, t)
          case (t : Token) => Token(width, t)
          case (t : BitVector) => Token(width, t)
        })
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
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends PrevCO {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[LW](implicit ctx : DFAny.Alias.Context) : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
        def apply[P](left : DFBits[LW], right : Natural.Int.Checked[P]) : DFBits[LW] =
          new Alias(List(left), DFAny.Alias.Reference.Prev(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def |  [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      def &  [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      def ^  [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      def ## [RW](right : DFBits[RW])(implicit op: `Op##`.Builder[L, DFBits[RW]]) = op(left, right)
      def <> [RW, RDIR <: DFDir](port : DFBits[RW] <> RDIR)(
        implicit op: `Op<>`.Builder[DFBits[RW], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFBitsFromBitVector(left : BitVector) extends Able[BitVector](left)
      final implicit def DFBitsFromBitVector(left: BitVector): DFBitsFromBitVector = new DFBitsFromBitVector(left)
      sealed class DFBitsFromXBitVector[W](left : XBitVector[W]) extends Able[XBitVector[W]](left)
      final implicit def DFBitsFromXBitVector[W](left: XBitVector[W]): DFBitsFromXBitVector[W] = new DFBitsFromXBitVector[W](left)
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

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        ctx : Ctx,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Aux[DFBits[LW], DFBits[RW], DFBits[RW]] =
        create[DFBits[LW], DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          right
        })

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
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
        ) : DetailedBuilder[L, LW, R, RW]{type Comp = DFBits[OW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Comp = DFBits[OW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Comp] =
              new Builder[L, R] {
                type Comp = DFBits[OW]
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
      type Comp = L
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
      ) : Builder[DFBits[LW], DFUInt[RW]] = new Builder[DFBits[LW], DFUInt[RW]]{
        def apply(left : DFBits[LW], right : DFUInt[RW]) : DFBits[LW] = {
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
      ) : Builder[DFBits[LW], R] = new Builder[DFBits[LW], R]{
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
                  val out = new DFBits.Alias[OW](List(left, right), DFAny.Alias.Reference.AsIs(".bits"))
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
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////




  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(opKind : DiSoOp.Kind)(opFunc : (Seq[DFBits.Token], Seq[DFBits.Token]) => Seq[DFBool.Token]) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFBits[LW], DFBits[RW]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import ctx.basicLib.DFBitsOps._
        val (left, right) = properLR(leftL, rightR)
        val opInst = opKind match {
          case DiSoOp.Kind.== => new DFiant.BasicLib.DFBitsOps.`Comp==`(left.width, right.width)
          case DiSoOp.Kind.!= => new DFiant.BasicLib.DFBitsOps.`Comp!=`(left.width, right.width)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }
        opInst.setAutoName(s"${ctx}Comp")
        opInst.inLeft <> left
        opInst.inRight <> right
        opInst.outResult
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] = create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Op.Context,
        rConst : Const.Builder.Aux[R, RW],
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Op.Context,
        lConst : Const.Builder.Aux[L, LW],
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(DiSoOp.Kind.==)(DFBits.Token.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=)(DFBits.Token.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
