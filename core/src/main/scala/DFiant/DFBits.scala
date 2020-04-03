package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.Func2
import DFiant.compiler.printer.Printer

object b0s extends DFBits.SameBitsVector(false)
object b1s extends DFBits.SameBitsVector(true)

object DFBits extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBits.Pattern.Builder[LType]
    type OpAble[R] = DFBits.Op.Able[R]
    type `Op==Builder`[L, R] = DFBits.`Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = DFBits.`Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = DFBits.`Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = DFBits.`Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = DFBits.Init.Able[L]
    type InitBuilder[L <: DFAny] = DFBits.Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = fromToken
    override def toString: String = s"DFBits[$width]"
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      s"$TP DFBits($LIT$width)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(width) => this.width.getValue == width.getValue
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : BitsWidth.Checked[W])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedWidth.unsafeCheck()))
  def apply[W](
    implicit ctx : DFAny.Context, checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) = DFAny.NewVar(Type(checkedWidth))

  def unapply(arg: DFAny): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token(value : BitVector, bubbleMask : BitVector) extends DFAny.Token.Of[BitVector] {
    assert(value.length == bubbleMask.length)
    val width : Int = value.length.toInt
    lazy val valueBits : BitVector = value
    def | (that : Token) : Token = {
      assert(that.width == width)
      Token(this.valueBits | that.valueBits, this.bubbleMask | that.bubbleMask)
    }
    def & (that : Token) : Token = {
      assert(that.width == width)
      Token(this.valueBits & that.valueBits, this.bubbleMask | that.bubbleMask)
    }
    def ^ (that : Token) : Token = {
      assert(that.width == width)
      Token(this.valueBits ^ that.valueBits, this.bubbleMask | that.bubbleMask)
    }
    def ++ (that : Token) : Token = {
      Token(this.valueBits ++ that.valueBits, this.bubbleMask ++ that.bubbleMask)
    }
    def << (that : DFUInt.Token) : Token = {
      val shift = that.value.toInt
      new Token(this.valueBits << shift, this.bubbleMask << shift)
    }
    def >> (that : DFUInt.Token) : Token = {
      val shift = that.value.toInt
      new Token(this.valueBits >>> shift, this.bubbleMask >>> shift)
    }
    def unary_~ : Token = Token(~this.valueBits, this.bubbleMask)
    def reverse : Token = Token(valueBits.reverseBitOrder, bubbleMask.reverseBitOrder)
    def resize(toWidth : Int) : Token = {
      if (toWidth < width) bitsWL(toWidth, 0)
      else if (toWidth > width) (Token(toWidth - width, 0) ++ this)
      else this
    }
    def == (that : Token) : DFBool.Token = DFBool.Token(logical = true, this.valueBits == that.valueBits, this.isBubble || that.isBubble)
    def != (that : Token) : DFBool.Token = DFBool.Token(logical = true, this.valueBits != that.valueBits, this.isBubble || that.isBubble)

    def toUInt : DFUInt.Token = {
      val outWidth = this.width
      val outValueUInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width)
      val outBubble = isBubble
      DFUInt.Token(outWidth, outValueUInt, outBubble)
    }
    def toSInt : DFSInt.Token = {
      val outWidth = this.width
      val outValueSInt = BigInt(this.valueBits.padToMulsOf(8).toByteArray)
      val outBubble = isBubble
      new DFSInt.Token(outWidth, outValueSInt, outBubble)
    }
    def codeString(implicit printConfig : Printer.Config) : String = {
      import printConfig._
      import io.AnsiColor.BOLD
      if (value.length % 4 == 0) s"""$BOLD h$STR"${value.toHex}""""
      else s"""$BOLD b$STR"${value.toBin}""""
    }
//    override def equals(obj: Any): Boolean = obj match {
//      case Token(width, value, bubbleMask) => this.width.getValue == width.getValue && this.value == value && this.bubbleMask == bubbleMask
//      case _ => false
//    }
  }
  object Token {
    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token(t.width, Bubble)
    implicit def bubbleOfDFType[W] : DFAny.Token.BubbleOfDFType[Type[W]] = t => Token(t.width, Bubble)
    def apply(width : Int, value : Int) : Token = Token(BigInt(value).toBitVector(width))
    def apply(value : BitVector) : Token = Token(value, BitVector.low(value.length))
    def apply(width : Int, value : Bubble) : Token = Token(BitVector.low(width), BitVector.high(width))
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
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.bitVector).foldLeft(Set.empty[BitVector])((set, bitVector) => {
            if (set.contains(bitVector)) throw new IllegalArgumentException(s"\nThe bitvector $bitVector already intersects with $set")
            if (bitVector.length > left.width) throw new IllegalArgumentException(s"\nThe bitvector $bitVector is wider than ${left.width}")
            set + bitVector
          })

          new Pattern(patternSet)
        }
      }
    }
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
        right.toSeqAny.collect {
          case t : Bubble => Token(width, t)
          case t : Token => assert(t.width == width); t
          case t : BitVector => Token(t)
          case t : SameBitsVector => Token(XBitVector.fill(width)(t.value))
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[LW] : Builder[DFBits[LW], Token] = (left, right) => Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type Const[W] = DFAny.Const.Of[Type[W]]
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : Const[W]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromBitVector(implicit ctx : DFAny.Context)
      : Aux[BitVector, Int] = new Builder[BitVector] {
        type W = Int
        def apply(value : BitVector) : Const[W] = {
          val width = TwoFace.Int(value.length.toInt)
          DFAny.Const[Type[Int]](Type(width), Token(value))
        }
      }
      implicit def fromXBitVector[W0](implicit ctx : DFAny.Context)
      : Aux[XBitVector[W0], W0] = new Builder[XBitVector[W0]] {
        type W = W0
        def apply(value : XBitVector[W0]) : Const[W] = {
          val width = TwoFace.Int.create[W0](value.length.toInt)
          DFAny.Const[Type[W0]](Type(width), Token(value))
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      final val left = value
      final def |   [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      final def &   [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ^   [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      final def === [RW](right : DFBits[RW])(implicit op: `Op===`.Builder[L, DFBits[RW]]) = op(left, right)
      final def =!= [RW](right : DFBits[RW])(implicit op: `Op=!=`.Builder[L, DFBits[RW]]) = op(left, right)
      final def ++  [RW](right : DFBits[RW])(implicit op: `Op++`.Builder[L, DFBits[RW]]) = op(left, right)
    }
    trait Implicits {
      final implicit def DFBitsWiden[FW, TW](c : DFBits[FW])(implicit eq : OpContainer.Eq[FW, TW, Int]) : DFBits[TW] = c.asInstanceOf[DFBits[TW]]
      sealed class DFBitsFromBitVector(left : BitVector) extends AbleOps[BitVector](left)
      final implicit def DFBitsFromBitVector(left: BitVector): DFBitsFromBitVector = new DFBitsFromBitVector(left)
      sealed class DFBitsFromXBitVector[W](left : XBitVector[W]) extends AbleOps[XBitVector[W]](left)
      final implicit def DFBitsFromXBitVector[W](left: XBitVector[W]): DFBitsFromXBitVector[W] = new DFBitsFromXBitVector[W](left)
      sealed class DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) extends AbleOps[SBV](left)
      final implicit def DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : DFBitsFromZeros[SBV] = new DFBitsFromZeros(left)
//      sealed class DFBitsFromDFBool(left : DFBool)(implicit ctx : DFAny.Context) extends AbleOps[DFBits[1]](DFAny.Alias.AsIs(Type(1), left))
//      final implicit def DFBitsFromDFBool(left: DFBool)(implicit ctx : DFAny.Context): DFBitsFromDFBool = new DFBitsFromDFBool(left)
      sealed class DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) extends AbleOps[DFBits[W]](left)
      final implicit def DFBitsFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) : DFBitsFromDefaultRet[W] = new DFBitsFromDefaultRet(left)
      final implicit def ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = new Able(left)
      final implicit class DFBitsOps[LW](val left : DFBits[LW]){
        def |   [R](right : Able[R])(implicit op: `Op|`.Builder[DFBits[LW], R]) = op(left, right)
        def &   [R](right : Able[R])(implicit op: `Op&`.Builder[DFBits[LW], R]) = op(left, right)
        def ^   [R](right : Able[R])(implicit op: `Op^`.Builder[DFBits[LW], R]) = op(left, right)
        def === [R](right : Able[R])(implicit op: `Op===`.Builder[DFBits[LW], R]) = op(left, right)
        def =!= [R](right : Able[R])(implicit op: `Op=!=`.Builder[DFBits[LW], R]) = op(left, right)
        def ++  [R](right : Able[R])(implicit op: `Op++`.Builder[DFBits[LW], R]) = op(left, right)
        def unary_~(implicit ctx : DFAny.Context) : DFBits[LW] = DFAny.Alias.Invert(left)
        def << [R](right: DFUInt.Op.Able[R])(implicit op: `Op<<`.Builder[DFBits[LW], R]) = op(left, right)
        def >> [R](right: DFUInt.Op.Able[R])(implicit op: `Op>>`.Builder[DFBits[LW], R]) = op(left, right)
        def resize[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] =
          DFAny.Alias.Resize.bits(left, toWidth)
        def resizeRight[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] = {
          val ret = if (left.width < toWidth) {
            val zeroWidth = toWidth - left.width
            val zeros = DFAny.Const.forced(Type(zeroWidth), Token(zeroWidth, 0))
            `Op++`.forced(left, zeros)
          }
          else if (left.width > toWidth) DFAny.Alias.BitsWL(left, toWidth, left.width - toWidth)
          else left
          ret.asInstanceOf[DFBits[RW]]
        }
      }
      final implicit class DFBitsAliases[LW, Mod <: DFAny.Modifier](val left : DFAny.Value[Type[LW], Mod]) {
        def uint(implicit ctx : DFAny.Context) : DFAny.Value[DFUInt.Type[LW], Mod] =
          left.as(DFUInt.Type(left.width)).overrideCodeString(rs => s"$rs.uint")
        def sint(implicit ctx : DFAny.Context) : DFAny.Value[DFSInt.Type[LW], Mod] =
          left.as(DFSInt.Type(left.width)).overrideCodeString(rs => s"$rs.sint")
        def apply[H, L](relBitHigh : BitIndex.Checked[H, left.Width], relBitLow : BitIndex.Checked[L, left.Width])(
          implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
        ) : DFAny.Value[DFBits.Type[relWidth.Out], Mod] =
          left.bits(relBitHigh, relBitLow).overrideCodeString(rs => s"$rs($relBitHigh, $relBitLow)")
        def apply[I](relBit: BitIndex.Checked[I, left.Width])(
          implicit ctx : DFAny.Context
        ) : DFAny.Value[DFBool.Type, Mod] =
          left.bit(relBit).overrideCodeString(rs => s"$rs($relBit)")
        def msbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit(left, left.width-1).overrideCodeString(rs => s"$rs.msbit")
        def lsbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit(left, 0).overrideCodeString(rs => s"$rs.lsbit")
      }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Tuple-handling Implicits
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      sealed abstract class VarProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.asInstanceOf[List[DFAny]].map(f => f.width.getValue).sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFAny.VarOf[Type[w.Out]] = ???
//          new DFBits.Alias[w.Out](DFAny.Alias.Reference.Concat(e.productIterator.toList.asInstanceOf[List[DFAny]], ".bits"))
      }

      sealed abstract class ValProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.collect{
          case dfAny : DFAny => dfAny.width.getValue
          case bv : BitVector => bv.length.toInt
        }.sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFBits[w.Out] = {
          val list : List[DFBits[Int]] = e.productIterator.toList.collect{
            case dfAny : DFAny.Value[_,_] => dfAny.bits.asInstanceOf[DFBits[Int]]
            case bv : BitVector => DFAny.Const.forced(Type(bv.length.toInt), DFBits.Token(bv))
          }
          list.reduce((l, r) => `Op++`.forced(l, r)).asInstanceOf[DFBits[w.Out]]
        }
      }

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 1
      /////////////////////////////////////////////////////////////////////////////////////
      implicit class VarTuple1[T1 <: DFAny.Type](
        val e : Tuple1[DFAny.VarOf[T1]]
      ) extends VarProductExtender(e) {
        type WSum = e._1.Width
      }

      implicit class ValTuple1[T1 <: HasWidth](
        val e : Tuple1[T1]
      ) extends ValProductExtender(e){
        type WSum = e._1.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 2
      /////////////////////////////////////////////////////////////////////////////////////
      implicit class VarTuple2[T1 <: DFAny.Type, T2 <: DFAny.Type](
        val e : Tuple2[DFAny.VarOf[T1], DFAny.VarOf[T2]]
      ) extends VarProductExtender(e) {
        type WSum = e._1.Width + e._2.Width
      }

      implicit class ValTuple2[T1 <: HasWidth, T2 <: HasWidth](
        val e : Tuple2[T1, T2]
      ) extends ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 3
      /////////////////////////////////////////////////////////////////////////////////////
      implicit class VarTuple3[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type](
        val e : Tuple3[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3]]
      ) extends VarProductExtender(e) {
        type WSum = e._1.Width + e._2.Width + e._3.Width
      }

      implicit class ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](
        val e : Tuple3[T1, T2, T3]
      ) extends ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 4
      /////////////////////////////////////////////////////////////////////////////////////
      implicit class VarTuple4[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type](
        val e : Tuple4[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4]]
      ) extends VarProductExtender(e) {
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
      }

      implicit class ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](
        val e : Tuple4[T1, T2, T3, T4]
      ) extends ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 5
      /////////////////////////////////////////////////////////////////////////////////////
      implicit class VarTuple5[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type, T5 <: DFAny.Type](
        val e : Tuple5[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4], DFAny.VarOf[T5]]
      ) extends VarProductExtender(e) {
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
      }

      implicit class ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](
        val e : Tuple5[T1, T2, T3, T4, T5]
      ) extends ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[W] {
      def apply(bits : Type[W], sbv : SameBitsVector) : Const[W]
    }
    object Builder {
      implicit def ev[W](implicit ctx : DFAny.Context)
      : Builder[W] = (bits, sbv) => DFAny.Const[Type[W]](Type(bits.width), Token(XBitVector.fill(bits.width)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable of type ${LType} does not support assignment/connect operation with the type ${R}")
    trait Builder[LType <: DFAny.Type, R] extends DFAny.Op.Builder[LType, R] {
      type Out = DFAny.Of[LType]
    }

    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "An assignment/connection operation does not permit different widths. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[Type[LW], DFBits[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[Type[LW], SBV] = (left, right) => {
        rSBV(left, right)
      }

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShellSym[Builder[_,_], LW, RW]
      ) : Builder[Type[LW], R] = (left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        right.asInstanceOf[DFAny.Of[Type[LW]]]
      }
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}
    object Builder {
      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Comparison operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      def create[L, LW, R, RW](properLR : (L, R) => (DFBits[LW], DFBits[RW]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFBits_op_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], DFBits[RW]] =
        create[DFBits[LW], LW, DFBits[RW], RW]((left, right) => {
          checkLWvRW.unsafeCheck(left.width, right.width)
          (left, right)
        })

      implicit def evDFBits_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], R] = create[DFBits[LW], LW, R, RW]((left, rightNum) => {
        val right = rConst(rightNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evConst_op_DFBits[L, LW, RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : Builder[L, DFBits[RW]] = create[L, LW, DFBits[RW], RW]((leftNum, right) => {
        val left = lConst(leftNum)
        checkLWvRW.unsafeCheck(left.width, right.width)
        (left, right)
      })

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        rSBV : SameBitsVector.Builder[LW]
      ) : Builder[DFBits[LW], SBV] = create[DFBits[LW], LW, SBV, LW]((left, rightSBV) => {
        val right = rSBV(left.dfType, rightSBV)
        (left, right)
      })

      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](
        implicit
        ctx : DFAny.Context,
        lSBV : SameBitsVector.Builder[RW]
      ) : Builder[SBV, DFBits[RW]] = create[SBV, RW, DFBits[RW], RW]((leftSBV, right) => {
        val left = lSBV(right.dfType, leftSBV)
        (left, right)
      })
    }
  }
  object `Op==` extends OpsCompare(Func2.Op.==)((l, r) => l == r) with `Op==`
  object `Op!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)((l, r) => l == r)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsLogic[Op <: Func2.Op](op : Op)(tokenOp : (Token, Token) => Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object `LW == RW` extends Checked1Param.Int {
        type Cond[LW, RW] = LW == RW
        type Msg[LW, RW] = "Logic operations do not permit different width DF variables. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW](
          implicit
          ctx : DFAny.Context,
          checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[LW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[LW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[LW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  DFAny.Func2[Type[LW], DFBits[LW], Op, DFBits[RW]](Type[LW](left.width), left, op, right)(tokenOp)
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
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsg[false, "An unconstrained-width literal cannot be used in a logic operation"]

      implicit def evDFBits_op_SBV[LW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[DFBits[LW], SBV, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW, SBV <: SameBitsVector](implicit error : UnconstrainedLiteralError)
      : Aux[SBV, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(Func2.Op.|)(_ | _)
  object `Op&` extends OpsLogic(Func2.Op.&)(_ & _)
  object `Op^` extends OpsLogic(Func2.Op.^)(_ ^ _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op<<` extends OpsShift[Type](Func2.Op.<<) {
    def tokenFunc[LW](left: DFBits.Token, right: DFUInt.Token) : DFBits.Token = left << right
  }
  object `Op>>` extends OpsShift[Type](Func2.Op.>>) {
    def tokenFunc[LW](left: DFBits.Token, right: DFUInt.Token) : DFBits.Token = left >> right
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Concatenation operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op++` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]

    def forced[LW, RW](left : DFBits[LW], right : DFBits[RW])(implicit ctx : DFAny.Context) : DFBits[Int] =
      DFAny.Func2(Type(left.width.getValue + right.width.getValue), left, DFAny.Func2.Op.++, right)(_ ++ _).asInstanceOf[DFBits[Int]]
    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object Inference {
        type CalcW[LW, RW] = LW + RW
        type OW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, OW](
          implicit
          ctx : DFAny.Context,
          oW : Inference.OW[LW, RW, OW],
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[OW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[OW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[OW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val oWidth = oW(left.width, right.width)
                  val out = DFAny.Func2(Type(oWidth), left, DFAny.Func2.Op.++, right)(_ ++ _)
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
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
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
}