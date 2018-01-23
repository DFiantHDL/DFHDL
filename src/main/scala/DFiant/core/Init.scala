package DFiant.core

import DFiant.tokens._
import DFiant.internals._
import scodec.bits._
import singleton.ops._
import singleton.twoface._


object Init {
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicit configuration of when operation is possible
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Able[L <: DFAny] {
    val right : Any

    override def toString: String = right.toString
  }

  object Able {
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Common
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    implicit class AbleSeq[L <: DFAny](s : Seq[Able[L]]) {
      private def flatten(s: Seq[Any]): Seq[Any] = s flatMap {
        case ss: Seq[_] => flatten(ss)
        case e => Seq(e)
      }
      def toSeqAny : Seq[Any] = {
        flatten(s.map(e => e.right))
      }

      override def toString: String = s.toString()
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBits
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    private type IntWithinWidth[LW] = CompileTime[Natural.Int.Cond[GetArg0] && (BitsWidthOf.CalcInt[GetArg0] <= LW)]
    private type LongWithinWidth[LW] = CompileTime[Natural.Long.Cond[GetArg0] && (BitsWidthOf.CalcLong[GetArg0] <= LW)]
    implicit class DFBitsBubble[LW](val right : Bubble) extends Able[DFBits[LW]]
    implicit class DFBitsToken[LW](val right : TokenBits) extends Able[DFBits[LW]]
    implicit class DFBitsTokenSeq[LW](val right : Seq[TokenBits]) extends Able[DFBits[LW]]
    implicit class DFBitsInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFBits[LW]]
    implicit class DFBitsLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFBits[LW]]
    implicit class DFBitsBitVector[LW](val right : BitVector) extends Able[DFBits[LW]]

    def toTokenBitsSeq[LW](width : Int, right : Seq[Able[DFBits[LW]]]) : Seq[TokenBits] =
      right.toSeqAny.map(e => e match {
        case (t : Bubble) => TokenBits(width, t)
        case (t : TokenBits) => TokenBits(width, t)
        case (t : Int) => TokenBits(width, t)
        case (t : Long) => TokenBits(width, t)
        case (t : BitVector) => TokenBits(width, t)
      })
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBool
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    private type IntIsBoolean = CompileTime[(GetArg0 == 0) || (GetArg0 == 1)]
    implicit class DFBoolBubble(val right : Bubble) extends Able[DFBool]
    implicit class DFBoolToken(val right : TokenBool) extends Able[DFBool]
    implicit class DFBoolTokenSeq(val right : Seq[TokenBool]) extends Able[DFBool]
    implicit class DFBoolXInt[T <: XInt](val right : T)(implicit chk : IntIsBoolean) extends Able[DFBool]
    implicit class DFBoolBoolean(val right : Boolean) extends Able[DFBool]

    def toTokenBoolSeq(right : Seq[Able[DFBool]]) : Seq[TokenBool] =
      right.toSeqAny.map(e => e match {
        case (t : Bubble) => TokenBool(t)
        case (t : TokenBool) => t
        case (t : Int) => TokenBool(t)
        case (t : Boolean) => TokenBool(t)
      })
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    implicit class DFUIntBubble[LW](val right : Bubble) extends Able[DFUInt[LW]]
    implicit class DFUIntToken[LW](val right : TokenUInt) extends Able[DFUInt[LW]]
    implicit class DFUIntTokenSeq[LW](val right : Seq[TokenUInt]) extends Able[DFUInt[LW]]
    implicit class DFUIntInt[LW](val right : Int)(implicit chk: IntWithinWidth[LW]) extends Able[DFUInt[LW]]
    implicit class DFUIntLong[LW](val right : Long)(implicit chk: LongWithinWidth[LW]) extends Able[DFUInt[LW]]
    implicit class DFUIntBigInt[LW](val right : BigInt) extends Able[DFUInt[LW]]

    def toTokenUIntSeq[LW](width : Int, right : Seq[Able[DFUInt[LW]]]) : Seq[TokenUInt] =
      right.toSeqAny.map(e => e match {
        case (t : Bubble) => TokenUInt(width, t)
        case (t : TokenUInt) => TokenUInt(width, t)
        case (t : Int) => TokenUInt(width, t)
        case (t : Long) => TokenUInt(width, t)
        case (t : BigInt) => TokenUInt(width, t)
      })
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////


  trait Builder[L <: DFAny] {
    def apply(left : L, right : Seq[Able[L]]) : L
  }
  object Builder {
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBits
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    implicit def fromDFBits[LW] : Builder[DFBits[LW]] = new Builder[DFBits[LW]] {
      def apply(left : DFBits[LW], right : Seq[Able[DFBits[LW]]]) : DFBits[LW] =
        DFBits.alias(left, left.width, 0, 0, Able.toTokenBitsSeq(left.width, right))
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFBool
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    implicit def fromDFBool : Builder[DFBool] = new Builder[DFBool] {
      def apply(left : DFBool, right : Seq[Able[DFBool]]) : DFBool =
        DFBool.alias(left, 0, 0, Able.toTokenBoolSeq(right))
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    // DFUInt
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    implicit def fromDFUInt[LW] : Builder[DFUInt[LW]] = new Builder[DFUInt[LW]] {
      def apply(left : DFUInt[LW], right : Seq[Able[DFUInt[LW]]]) : DFUInt[LW] =
        DFUInt.alias(left, left.width, 0, 0, Able.toTokenUIntSeq(left.width, right))
    }
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  }
}
