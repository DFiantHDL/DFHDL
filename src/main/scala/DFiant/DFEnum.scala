package DFiant

import scodec.bits._
import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object Enum {
  protected abstract class General[E <: General.Entry] {
    implicit val cnt = new General.Counter {}
    type Entry <: E
    type CalcWidth
    trait DFEnum extends DFEnum.Unbounded {
      import DFEnum._
      def == [E0 <: Entry](that : E0)(implicit op: `Op==`.Builder[TVal, E]) = op(left, that)
      def != [E0 <: Entry](that : E0)(implicit op: `Op!=`.Builder[TVal, E]) = op(left, that)
    }
    object DFEnum extends DFAny.Companion {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Unbounded Val
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      trait Unbounded extends DFAny.Unbounded[DFEnum.type] {
        type Width = CalcWidth
        type TVal = DFEnum
        type TVar = DFEnum.Var

      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Var
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      trait Var extends DFEnum with DFAny.Var {
        def := (that : Entry) : Unit = {}
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Public Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      def apply()(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Var = newVar()
      def apply(that : Init.Able[DFEnum]*)(implicit dsn : DFDesign, op : Init.Builder[DFEnum], w : SafeInt[CalcWidth]): Var = newVar()
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Protected Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      protected[DFiant] def newVar(init : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Var =
        new DFAny.NewVar(w, init) with Var {
          def codeString(idRef : String) : String = s"DFEnum???"
        }


      protected[DFiant] def alias
      (aliasedVar : DFAny, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Var =
        new DFAny.Alias(aliasedVar, w, relBitLow, deltaStep, updatedInit) with Var {
          protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = ??? //token
          def codeString(idRef : String) : String = "AliasOfDFEnum???"
        }

      protected[DFiant] def const[W](token : Token)(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : DFEnum =
        new DFAny.Const(token) with DFEnum {
        }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Token
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      class Token private[DFiant] (val valueEnum : Entry, bubble : Boolean)(implicit w : SafeInt[CalcWidth]) extends DFAny.Token {
        val width : Int = w
        val valueBits: BitVector = valueEnum.value
        val bubbleMask: BitVector = BitVector.fill(width)(bubble)
      }
      object Token {
        import DFAny.TokenSeq
        def apply(value : Bubble)(implicit w : SafeInt[CalcWidth]) : Token = ???
        def apply(value : General.Entry)(implicit w : SafeInt[CalcWidth]) : Token = ???
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Init
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object Init extends Init {
        trait Able[L <: DFAny] extends DFAny.Init.Able[L]
        object Able {
          implicit class DFEnumBubble(val right : Bubble) extends Able[DFEnum]
          implicit class DFEnumEntry(val right : Entry) extends Able[DFEnum]
          implicit class DFEnumToken(val right : Token) extends Able[DFEnum]
          implicit class DFEnumTokenSeq(val right : Seq[Token]) extends Able[DFEnum]

          def toTokenSeq[LW](width : Int, right : Seq[Able[DFEnum]])(implicit w : SafeInt[CalcWidth]) : Seq[Token] =
            right.toSeqAny.map(e => e match {
              case (t : Bubble) => Token(t)
              case (t : General.Entry) => Token(t)
              case (t : Token) => t
            })
        }
        trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
        object Builder {
          implicit def ev[LW](implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Builder[DFEnum] = (left, right) =>
            alias(left, 0, 0, Able.toTokenSeq(left.width, right))
        }
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Prev
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object Prev extends Prev {
        trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
        object Builder {
          implicit def ev(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Builder[DFEnum] = new Builder[DFEnum] {
            def apply[P](left : DFEnum, right : Natural.Int.Checked[P]) : DFEnum =
              alias(left, 0, -right, left.getInit)
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
  }
  protected object General {
    trait Entry {
      val value : BitVector
    }
    trait Counter {
      var value : Int = 0
      def inc : Unit = {value = value + 1}
    }
  }

  trait Auto extends General[Auto.Entry] {
    type CalcWidth = BitsWidthOf.CalcInt[EnumCount[Entry]-1]
  }
  object Auto {
    abstract class Entry(implicit cnt : General.Counter) extends General.Entry {
      cnt.inc
    }
  }
  trait Manual[Width] extends General[Manual.Entry] {
    type CalcWidth = Width
  }
  object Manual {
    abstract class Entry(val value : BitVector) extends General.Entry
  }
}
