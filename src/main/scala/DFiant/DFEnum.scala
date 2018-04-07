package DFiant

import scodec.bits._
import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object Enum {
  protected trait General[E <: General.Entry] {
    type Entry <: E
    type CalcWidth
    trait DFEnum extends DFEnum.Unbounded {
      def == (that : Entry) : DFBool = ???
    }
    object DFEnum extends DFAny.Companion {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Unbounded Val
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      trait Unbounded extends DFAny.Unbounded[DFEnum.type] {
        type TVal = DFEnum
        type TVar = DFEnum.Var
        trait TToken extends DFEnum.Token

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
      def apply()(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) = newVar()
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Protected Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      protected[DFiant] def newVar(init : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Var{type Width = w.Out} =
        new DFAny.NewVar(w, init) with Var {
          type Width = w.Out
          def codeString(idRef : String) : String = s"DFEnum???"
        }


      protected[DFiant] def alias
      (aliasedVar : DFAny, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Var{type Width = w.Out} =
        new DFAny.Alias(aliasedVar, w, relBitLow, deltaStep, updatedInit) with Var {
          type Width = w.Out
          protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = ??? //token
          def codeString(idRef : String) : String = "AliasOfDFEnum???"
        }

      protected[DFiant] def const[W](token : Token)(implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : DFEnum{type Width = w.Out} =
        new DFAny.Const(token) with DFEnum {
          type Width = w.Out
        }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Token
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      class Token private[DFiant] (val valueEnum : Entry) extends DFAny.Token {
        val width: Int = ???
        val bubbleMask: BitVector = ???
        val valueBits: BitVector = ???
      }
      object Token {
        import DFAny.TokenSeq
        def apply(bubble : Bubble) : Token = ???
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Init
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object Init extends Init {
        trait Able[L <: DFAny] extends DFAny.Init.Able[L]
        object Able {
          implicit class DFEnumBubble(val right : Bubble) extends Able[DFEnum]
          implicit class DFEnumToken(val right : Token) extends Able[DFEnum]
          implicit class DFEnumTokenSeq(val right : Seq[Token]) extends Able[DFEnum]

          def toTokenSeq[LW](width : Int, right : Seq[Able[DFEnum]]) : Seq[Token] =
            right.toSeqAny.map(e => e match {
              case (t : Bubble) => Token(t)
              case (t : Token) => t
            })
        }
        trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
        object Builder {
          implicit def ev[LW](implicit dsn : DFDesign, w : SafeInt[CalcWidth]) : Builder[DFEnum] = new Builder[DFEnum] {
            def apply(left : DFEnum, right : Seq[Able[DFEnum]]) : DFEnum =
              alias(left, 0, 0, Able.toTokenSeq(left.width, right))
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

    }
  }

  trait Auto extends General[Auto.Entry] {
    type CalcWidth = EnumCount[Entry]
  }
  object Auto {
    trait Entry extends General.Entry
  }
  trait Manual[Width] extends General[Manual.Entry] {
    type CalcWidth = Width
  }
  object Manual {
    abstract class Entry(value : BitVector) extends General.Entry
  }
}
