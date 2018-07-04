package DFiant

import scodec.bits._
import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.internals._


trait DFEnum[E <: Enum] extends DFEnum.Unbounded {
  type TEnum = E
}
object DFEnum extends DFAny.Companion {
  import DFPort._
  private type WidthOf[E <: Enum] = SafeInt[E#EntryWidth]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFEnum.type] {
    type TEnum <: Enum
    type TEntry = TEnum#Entry
    type Width = TEnum#EntryWidth
    type TVal = DFEnum[TEnum]
    type TVar = DFEnum.Var[TEnum]
    type TToken = DFEnum.Token[TEnum]
    def == [E <: TEntry](right : E)(implicit op: `Op==`.Builder[TVal, E]) = op(left, right)
    def != [E <: TEntry](right : E)(implicit op: `Op!=`.Builder[TVal, E]) = op(left, right)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[E <: Enum] extends DFEnum[E] with DFAny.Var {}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[E <: Enum](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt) : Var[E] = newVar[E]()
//  def apply(that : Init.Able[DFEnum]*)(implicit dsn : DFDesign, op : Init.Builder[DFEnum], w : WidthOf[E]): Var = newVar()
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] def newVar[E <: Enum](init : Seq[Token[E]] = Seq())(
    implicit dsn : DFDesign, w : WidthOf[E], n : NameIt
  ) : Var[E] = new DFAny.NewVar(w, init) with Var[E] {
    def codeString(idRef : String) : String = s"DFEnum???"
  }

  protected[DFiant] def alias[E <: Enum]
  (aliasedVar : DFAny, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token[E]] = Seq())(
    implicit dsn : DFDesign, w : WidthOf[E], n : NameIt
  ) : Var[E] = new DFAny.Alias(aliasedVar, w, relBitLow, deltaStep, updatedInit) with Var[E] {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken = ??? //token
    def codeString(idRef : String) : String = "AliasOfDFEnum???"
  }

  protected[DFiant] def const[E <: Enum](token : Token[E])(
    implicit dsn : DFDesign, w : WidthOf[E], n : NameIt
  ) : DFEnum[E] = new DFAny.Const(token) with DFEnum[E] {  }

  protected[DFiant] def port[E <: Enum, DIR <: DFDir](dfVar : Connection[DFEnum[E]])(
    implicit dsn : DFDesign, dir : DIR, n : NameIt
  ) : DFEnum[E] <> DIR = new DFAny.Port[DFEnum[E], DIR](dfVar) with DFEnum[E]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token[E <: Enum] private[DFiant] (val valueEnum : Option[E#Entry])(implicit w : WidthOf[E]) extends DFAny.Token {
    val width : Int = w
    val (valueBits, bubbleMask) : (BitVector, BitVector) = valueEnum match {
      case Some(e) => (e.value.toBitVector(width), false.toBitVector(width))
      case None => (0.toBitVector(width), true.toBitVector(width))
    }

    final def == (that : Token[E]) : DFBool.Token = (this.valueEnum, that.valueEnum) match {
      case (Some(left), Some(right)) => DFBool.Token(left == right)
      case _ => DFBool.Token(Bubble)
    }
    final def != (that : Token[E]) : DFBool.Token = !(this == that)

  }
  object Token {
    import DFAny.TokenSeq
    def == [E <: Enum](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != [E <: Enum](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply[E <: Enum](value : Bubble)(implicit w : WidthOf[E]) : Token[E] = new Token[E](None)
    def apply[E <: Enum](value : E#Entry)(implicit w : WidthOf[E]) : Token[E] = new Token[E](Some(value))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFEnumBubble[E <: Enum](val right : Bubble) extends Able[DFEnum[E]]
      implicit class DFEnumEntry[E <: Enum](val right : E#Entry) extends Able[DFEnum[E]]
      implicit class DFEnumToken[E <: Enum](val right : Token[E]) extends Able[DFEnum[E]]
      implicit class DFEnumTokenSeq[E <: Enum](val right : Seq[Token[E]]) extends Able[DFEnum[E]]

      def toTokenSeq[E <: Enum](width : Int, right : Seq[Able[DFEnum[E]]])(implicit w : WidthOf[E]) : Seq[Token[E]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[E](t)
          case (t : Enum.Entry) => Token[E](t.asInstanceOf[E#Entry])
          case (t : Token[_]) => t.asInstanceOf[Token[E]]
        })
    }
    trait Builder[L <: DFAny] extends DFAny.Init.Builder[L, Able]
    object Builder {
      implicit def ev[E <: Enum](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt) : Builder[DFEnum[E]] = (left, right) =>
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
      implicit def ev[E <: Enum](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt) : Builder[DFEnum[E]] = new Builder[DFEnum[E]] {
        def apply[P](left : DFEnum[E], right : Natural.Int.Checked[P]) : DFEnum[E] =
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
      implicit def conn[E <: Enum, C <: Connection[DFEnum[E]], DIR <: DFDir](implicit dsn : DFDesign, dir : DIR, n : NameIt)
      : Builder[DFEnum[E], C, DIR] = right => port[E, DIR](right)
      implicit def fromEntry[E <: Enum, Entry <: E#Entry](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt)
      : Builder[DFEnum[E], Entry, IN] = rightEntry => port[E, IN](FullyConnected(const(Token[E](rightEntry))))
      implicit def fromDFEnum[E <: Enum, DIR <: DFDir](implicit dsn : DFDesign, dir : DIR, w : WidthOf[E], n : NameIt)
      : Builder[DFEnum[E], DFEnum[E], DIR] = rightR => {
        val right = newVar()
        right.assign(rightR)
        port[E, DIR](FullyConnected(right))
      }
    }
  }
  implicit def inPortFromDFEnum[E <: Enum](right : DFEnum[E])(
    implicit port : Port.Builder[DFEnum[E], DFEnum[E], IN]
  ) : DFEnum[E] <> IN = port(right)
  implicit def outPortFromDFEnum[E <: Enum](right : DFEnum.Var[E])(
    implicit port : Port.Builder[DFEnum[E], DFEnum[E], OUT]
  ) : DFEnum[E] <> OUT = port(right)
  implicit def inPortFromEntry[E <: Enum, Entry <: E#Entry](right : Entry)(
    implicit port : Port.Builder[DFEnum[E], Entry, IN]
  ) : DFEnum[E] <> IN = port(right)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {}
    trait Implicits extends super.Implicits {
      implicit class FromEntry[L <: Enum.Entry](left : L) extends Able[L](left)
      implicit def ofDFEnum[R <: DFEnum.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
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

      def create[E <: Enum, L, R](properR : (L, R) => DFEnum[E]) : Aux[L, R, DFEnum[E]] =
        new Builder[L, R] {
          type Comp = DFEnum[E]
          def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
        }

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit dsn : DFDesign, w : WidthOf[E])
      : Aux[DFEnum[E], DFEnum[E], DFEnum[E]] =
        create[E, DFEnum[E], DFEnum[E]]((left, right) => right)

      implicit def evDFEnum_op_Entry[E <: Enum, Entry <: E#Entry](implicit dsn : DFDesign, w : WidthOf[E])
      : Aux[DFEnum[E], Entry, DFEnum[E]] =
        create[E, DFEnum[E], Entry]((left, rightEntry) => const(Token[E](rightEntry)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[DiSoOpKind <: DiSoOp.Kind] {
    def opFunc[E <: Enum] : (Seq[DFEnum.Token[E]], Seq[DFEnum.Token[E]]) => Seq[DFBool.Token]
    type CompareOp[E <: Enum] = basiclib.DiSoOp[DiSoOpKind, DFEnum[E], DFEnum[E], DFBool]
    def compareOp[E <: Enum](inLeft : DFEnum[E] <> IN, inRight : DFEnum[E] <> IN, outResult : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[E]

    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      def create[E <: Enum, L, R](properLR : (L, R) => (DFEnum[E], DFEnum[E]))(implicit dsn : DFDesign, w : WidthOf[E], n : NameIt)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val result = DFBool.newVar(opFunc(left.getInit, right.getInit)).setAutoName(n.value)

        compareOp[E] (
          inLeft = FullyConnected(left),
          inRight = FullyConnected(right),
          outResult = FullyConnected(result)
        )
        result
      }

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt)
      : Builder[DFEnum[E], DFEnum[E]] = create[E, DFEnum[E], DFEnum[E]]((left, right) => (left, right))

      implicit def evDFEnum_op_Entry[E <: Enum, R <: E#Entry](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt)
      : Builder[DFEnum[E], R] = create[E, DFEnum[E], R]((left, rightEntry) => (left, const(Token[E](rightEntry))))

      implicit def evEntry_op_DFEnum[E <: Enum, L <: E#Entry](implicit dsn : DFDesign, w : WidthOf[E], n : NameIt)
      : Builder[L, DFEnum[E]] = create[E, L, DFEnum[E]]((leftEntry, right) => (const(Token[E](leftEntry)), right))
    }
  }

  object `Op==` extends OpsCompare[DiSoOp.Kind.==] with `Op==` {
    def opFunc[E <: Enum] = Token.==[E]
    def compareOp[E <: Enum](inLeft0 : DFEnum[E] <> IN, inRight0 : DFEnum[E] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[E] = {
      import dsn.basicLib._
      new `E==E`[E]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  object `Op!=` extends OpsCompare[DiSoOp.Kind.!=] with `Op!=` {
    def opFunc[E <: Enum] = Token.!=[E]
    def compareOp[E <: Enum](inLeft0 : DFEnum[E] <> IN, inRight0 : DFEnum[E] <> IN, outResult0 : DFBool <> OUT)(
      implicit dsn : DFDesign
    ) : CompareOp[E] = {
      import dsn.basicLib._
      new `E!=E`[E]{val inLeft = inLeft0; val inRight = inRight0; val outResult = outResult0}
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class Enum {
  type Entry <: Enum.Entry
  type EntryWidth
}

object Enum {
  sealed trait Entry {
    val value : BigInt
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
    object Grey extends Encoding {
      type EntryWidth[Entry] = BitsWidthOf.CalcInt[EnumCount[Entry]-1]
      val func : Int => BigInt = t => BigInt(t ^ (t >>> 1))
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
  
  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default) extends Enum {
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
    abstract class Entry(implicit cnt : Counter) extends Enum.Entry {
      val value : BigInt = cnt.getValue
      cnt.inc
    }
  }
  abstract class Manual[Width](implicit width : SafeInt[Width]) extends Enum {
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    abstract class Entry private extends Enum.Entry
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
  }
}
