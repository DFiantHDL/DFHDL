package DFiant

import scodec.bits._
import singleton.ops._
import singleton.twoface._
import DFiant.internals._

object Enum {
  protected abstract class General {
    type Entry <: General.Entry
    type EntryWidth
    trait DFEnum extends DFEnum.Unbounded
    object DFEnum extends DFAny.Companion {
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Unbounded Val
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      trait Unbounded extends DFAny.Unbounded[DFEnum.type] {
        type Width = EntryWidth
        type TVal = DFEnum
        type TVar = DFEnum.Var
        def == [E0 <: Entry](that : E0)(implicit op: `Op==`.Builder[TVal, E0]) = op(left, that)
        def != [E0 <: Entry](that : E0)(implicit op: `Op!=`.Builder[TVal, E0]) = op(left, that)
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
      def apply()(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : Var = newVar()
      def apply(that : Init.Able[DFEnum]*)(implicit dsn : DFDesign, op : Init.Builder[DFEnum], w : SafeInt[EntryWidth]): Var = newVar()
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Protected Constructors
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      protected[DFiant] def newVar(init : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : Var =
        new DFAny.NewVar(w, init) with Var {
          def codeString(idRef : String) : String = s"DFEnum???"
        }

      protected[DFiant] def alias
      (aliasedVar : DFAny, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : Var =
        new DFAny.Alias(aliasedVar, w, relBitLow, deltaStep, updatedInit) with Var {
          protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = ??? //token
          def codeString(idRef : String) : String = "AliasOfDFEnum???"
        }

      protected[DFiant] def const[W](token : Token)(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : DFEnum =
        new DFAny.Const(token) with DFEnum {
        }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Token
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      class Token private[DFiant] (val valueEnum : Option[Entry])(implicit w : SafeInt[EntryWidth]) extends DFAny.Token {
        val width : Int = w
        val (valueBits, bubbleMask) : (BitVector, BitVector) = valueEnum match {
          case Some(e) => (e.value.toBitVector(width), false.toBitVector(width))
          case None => (0.toBitVector(width), true.toBitVector(width))
        }
      }
      object Token {
        import DFAny.TokenSeq
        def apply(value : Bubble)(implicit w : SafeInt[EntryWidth]) : Token = new Token(None)
        def apply(value : General.Entry)(implicit w : SafeInt[EntryWidth]) : Token = new Token(Some(value.asInstanceOf[Entry]))
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

          def toTokenSeq[LW](width : Int, right : Seq[Able[DFEnum]])(implicit w : SafeInt[EntryWidth]) : Seq[Token] =
            right.toSeqAny.map(e => e match {
              case (t : Bubble) => Token(t)
              case (t : General.Entry) => Token(t)
              case (t : Token) => t
            })
        }
        trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
        object Builder {
          implicit def ev[LW](implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : Builder[DFEnum] = (left, right) =>
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
          implicit def ev(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : Builder[DFEnum] = new Builder[DFEnum] {
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
      val value : BigInt
    }
    trait Counter {
      var value : BigInt = 0
      def inc : Unit = {value = value + 1}
    }
  }

  trait Encoding {
    type EntryWidth[Entry]
    val func : Int => BigInt
  }
  object Encoding {
    object Default extends Encoding {
      type EntryWidth[Entry] = BitsWidthOf.CalcInt[EnumCount[Entry]-1]
      val func : Int => BigInt = t => BigInt(t)
    }
    case class StartAt[V <: Int with Singleton](value : V) extends Encoding {
      type EntryWidth[Entry] = BitsWidthOf.CalcInt[EnumCount[Entry]-1 + V]
      val func : Int => BigInt = t => BigInt(t + value)
    }
    object OneHot extends Encoding {
      type EntryWidth[Entry] = EnumCount[Entry]
      val func : Int => BigInt = t => BigInt(1) << t
    }
  }
  abstract class Auto[E <: Encoding](val encoding : E) extends General {
    type CheckEntry[Entry] = RequireMsgSym[EnumCount[Entry] != 0, "No enumeration entries found or the Entry is not a sealed trait", SafeInt[_]]
    type EntryWidth = CheckEntry[Entry] ==> encoding.EntryWidth[Entry]
    implicit val cnt = new General.Counter {}
  }
  object Auto {
    abstract class Entry(implicit cnt : General.Counter) extends General.Entry {
      val value : BigInt = cnt.value
      cnt.inc
    }
  }
//  trait Manual[Width] extends General {
//    class Entry[T](t : T)(implicit check : Manual.Check[Width, T]) extends General.Entry {
//      val value : BigInt = check.value
//    }
//    type EntryWidth = Width
//  }
//  object Manual {
//    trait Check[Width, T] {
//      val value : BigInt
//    }
//    object Check {
//      implicit def ev[Width, T](implicit v : ValueOf[T]) : Check[Width, T] = new Check[Width, T] {
//        val value : BigInt = 0
//      }
//    }
//  }
}
