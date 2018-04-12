package DFiant

import scodec.bits._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.internals._

object Enum {
  import DFPort._
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
        def == [E <: Entry](right : E)(implicit op: `Op==`.Builder[TVal, E]) = op(left, right)
        def != [E <: Entry](right : E)(implicit op: `Op!=`.Builder[TVal, E]) = op(left, right)
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Var
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      trait Var extends DFEnum with DFAny.Var {}
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

      protected[DFiant] def const(token : Token)(implicit dsn : DFDesign, w : SafeInt[EntryWidth]) : DFEnum =
        new DFAny.Const(token) with DFEnum {
        }

      protected[DFiant] def port[DIR <: DFDir](dfVar : Connection[DFEnum])(implicit dsn : DFDesign, dir : DIR) : DFEnum <> DIR =
        new DFAny.Port[DFEnum, DIR](dfVar) with DFEnum
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


      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Port
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object Port extends Port {
        trait Builder[L <: DFAny, R, DIR <: DFDir] extends DFAny.Port.Builder[L, R, DIR]
        object Builder {
          implicit def conn[C <: Connection[DFEnum], DIR <: DFDir](implicit dsn : DFDesign, dir : DIR)
          : Builder[DFEnum, C, DIR] = right => port[DIR](right)
          implicit def fromEntry[E <: Entry](
            implicit
            dsn : DFDesign,
            w : SafeInt[EntryWidth]
          ) : Builder[DFEnum, E, IN] = rightEntry => {
            port[IN](FullyConnected(const(Token(rightEntry))))
          }
          implicit def fromDFEnum[DIR <: DFDir](
            implicit
            dsn : DFDesign,
            dir : DIR,
            w : SafeInt[EntryWidth]
          ) : Builder[DFEnum, DFEnum, DIR] = rightR => {
            val right = newVar()
            right.assign(rightR)
            port[DIR](FullyConnected(right))
          }
        }
      }
      implicit def inPortFromDFEnum[L <: DFEnum.Unbounded](right : DFEnum)(
        implicit port : Port.Builder[L, DFEnum, IN]
      ) : L <> IN = port(right)
      implicit def outPortFromDFEnum[L <: DFEnum.Unbounded](right : DFEnum.Var)(
        implicit port : Port.Builder[L, DFEnum, OUT]
      ) : L <> OUT = port(right)
      implicit def inPortFromEntry[L <: DFEnum.Unbounded, E <: Entry](right : E)(
        implicit port : Port.Builder[L, E, IN]
      ) : L <> IN = port(right)
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Op
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object Op extends Op {
        class Able[L](val value : L) extends DFAny.Op.Able[L] {}
        trait Implicits extends super.Implicits {
          implicit class FromEntry[L <: Entry](left : L) extends Able[L](left)
        }
        object Able extends Implicits
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Assign
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      object `Op:=` extends `Op:=` {
        @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment operation with the type ${R}")
        trait Builder[L, R] extends DFAny.Op.Builder[L, R]

        object Builder {
          type Aux[L, R, Comp0] = Builder[L, R] {
            type Comp = Comp0
          }

          def create[L, R](properLR : (L, R) => (DFEnum, DFEnum)) : Aux[L, R, DFEnum.Var] =
            new Builder[L, R] {
              type Comp = DFEnum.Var
              def apply(leftL : L, rightR : R) : Comp = {
                val (left, right) = properLR(leftL, rightR)
                left.assign(right)
              }
            }

          implicit def evDFEnum_op_DFEnum[L <: DFEnum, R <: DFEnum](
            implicit
            dsn : DFDesign,
            w : SafeInt[EntryWidth]
          ) : Aux[DFEnum, DFEnum, DFEnum.Var] = create[DFEnum, DFEnum]((left, right) => (left, right))

          implicit def evDFEnum_op_Entry[L <: DFEnum, R <: Entry](
            implicit
            dsn : DFDesign,
            w : SafeInt[EntryWidth]
          ) : Aux[DFEnum, R, DFEnum.Var] = create[DFEnum, R]((left, rightEntry) => (left, const(Token(rightEntry))))
        }
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      
//      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//      // Comparison operations
//      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//      protected abstract class OpsCompare[DiSoOpKind <: DiSoOp.Kind](opFunc : (Seq[DFEnum.Token], Seq[DFEnum.Token]) => Seq[DFBool.Token]) {
//        type CompareOp = basiclib.DiSoOp[DiSoOpKind, DFEnum, DFEnum, DFBool]
//        def compareOp(inLeft : DFEnum <> IN, inRight : DFEnum <> IN, outResult : DFBool <> OUT)(
//          implicit dsn : DFDesign
//        ) : CompareOp
//
//        @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
//        trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}
//
//        object Builder {
//          def create[L, R](properLR : (L, R) => (DFEnum, DFEnum))(implicit dsn : DFDesign, w : SafeInt[EntryWidth])
//          : Builder[L, R] = (leftL, rightR) => {
//            val (left, right) = properLR(leftL, rightR)
//            val result = DFBool.newVar(opFunc(left.getInit, right.getInit))
//
//            compareOp (
//              inLeft = FullyConnected(left),
//              inRight = FullyConnected(right),
//              outResult = FullyConnected(result)
//            )
//            result
//          }
//
//          implicit def evDFEnum_op_DFEnum[L <: DFEnum, R <: DFEnum](implicit dsn : DFDesign, w : SafeInt[EntryWidth])
//          : Builder[DFEnum, DFEnum] = create[DFEnum, DFEnum]((left, right) => (left, right))
//
//          implicit def evDFEnum_op_Entry[L <: DFEnum, R <: Entry](implicit dsn : DFDesign, w : SafeInt[EntryWidth])
//          : Builder[DFEnum, R] = create[DFEnum, R]((left, rightEntry) => (left, const(Token(rightEntry))))
//
//          implicit def evEntry_op_DFEnum[L <: Entry, R <: DFEnum](implicit dsn : DFDesign, w : SafeInt[EntryWidth])
//          : Builder[L, DFEnum] = create[L, DFEnum]((leftEntry, right) => (const(Token(leftEntry)), right))
//        }
//      }
//
//      object `Op==` extends OpsCompare[DiSoOp.Kind.==](DFEnum.Token.==) with `Op==` {
//        def compareOp(inLeft0 : DFEnum <> IN, inRight0 : DFEnum <> IN, outResult0 : DFBool <> OUT)(
//          implicit dsn : DFDesign
//        ) : CompareOp = {
//          import dsn.basicLib._
//          new `U==U`{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
//        }
//      }
//      object `Op!=` extends OpsCompare[DiSoOp.Kind.!=](DFEnum.Token.!=) with `Op!=` {
//        def compareOp[LW, RW](inLeft0 : DFEnum <> IN, inRight0 : DFEnum <> IN, outResult0 : DFBool <> OUT)(
//          implicit dsn : DFDesign
//        ) : CompareOp[LW, RW] = {
//          import dsn.basicLib._
//          new `U!=U`[LW, RW]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
//        }
//      }
//      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
    implicit val cnt = new Auto.Counter(encoding.func) {}
  }
  object Auto {
    abstract class Counter(func : Int => BigInt) {
      def getValue : BigInt = func(cnt)
      private var cnt : Int = 0
      def inc : Unit = {cnt = cnt + 1}
    }
    abstract class Entry(implicit cnt : Counter) extends General.Entry {
      val value : BigInt = cnt.getValue
      cnt.inc
    }
  }
  abstract class Manual[Width](implicit width : SafeInt[Width]) extends General {
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    trait Entry extends General.Entry
    object Entry {
      def apply[T <: Int with Singleton](t : T)(implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]]) : Entry = new Entry {
        val value : BigInt = t
      }
      def apply[T <: Long with Singleton](t : T)(implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]]) : Entry = new Entry {
        val value : BigInt = t
      }
      def apply(t : BigInt) : Entry = new Entry {
        val value : BigInt = {
          require(t.bitsWidth <= width, s"Entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
          t
        }
      }
      def apply(t : BitVector) : Entry = new Entry {
        val value : BigInt = {
          require(t.length == width.toLong, s"Entry value width (${t.length}) is different than the enumeration width ($width)")
          t.toBigInt
        }
      }
    }
    type EntryWidth = Width
  }
}
