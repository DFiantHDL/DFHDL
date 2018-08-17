package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.basiclib._
import DFiant.internals._

import scala.collection.mutable.HashMap


trait DFEnum[E <: Enum] extends DFEnum.Unbounded {
  type TEnum = E
}
object DFEnum extends DFAny.Companion {
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
    val enum : TEnum
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
  def apply[E <: Enum](implicit ctx : DFAny.NewVar.Context, w : WidthOf[E], e : E) : NewVar[E] = new NewVar[E](e)
  def apply[E <: Enum](e : E)(implicit ctx : DFAny.NewVar.Context, w : WidthOf[E]) : NewVar[E] = new NewVar[E](e)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final class NewVar[E <: Enum](val enum : E)(
    implicit ctx : DFAny.NewVar.Context, w : WidthOf[E]
  ) extends DFAny.NewVar(w, s"DFEnum(${enum.name})") with Var[E]  {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
  }

  final class Alias[E <: Enum](val enum : E, aliasedVars : List[DFAny], reference : AliasReference)(
    implicit ctx : DFAny.Alias.Context, w : WidthOf[E]
  ) extends DFAny.Alias(aliasedVars, reference) with Var[E] {
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken =
      Token[E](enum.entries(token.valueBits.toBigInt).asInstanceOf[E#Entry])
  }

  protected[DFiant] def const[E <: Enum](enum_ : E, token : Token[E])(
    implicit ctx : DFAny.Const.Context, w : WidthOf[E]
  ) : DFEnum[E] = new DFAny.Const(token) with DFEnum[E] {val enum = enum_  }

  protected[DFiant] def port[E <: Enum, Dir <: DFDir](dfVar : DFEnum[E], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) : DFEnum[E] <> Dir = new DFAny.Port[DFEnum[E], Dir](dfVar, dir) with DFEnum[E] {val enum = dfVar.enum}
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

    override def codeString: String = if (isBubble) "Φ" else valueEnum.get.fullName
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
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends Port {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[E <: Enum, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFEnum[E], Dir] = (right, dir) => port[E, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.TEnum](mold.enum, List(left), AliasReference.AsIs(s".as(DFEnum(${mold.enum}))"))(ctx, ???)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends Init {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFEnumBubble[E <: Enum](val right : Bubble) extends Able[DFEnum[E]]
      implicit class DFEnumToken[E <: Enum](val right : Token[E]) extends Able[DFEnum[E]]
      implicit class DFEnumTokenSeq[E <: Enum](val right : Seq[Token[E]]) extends Able[DFEnum[E]]
      implicit class DFEnumEntry[E <: Enum, R <: E#Entry](val right : R) extends Able[DFEnum[E]]

      def toTokenSeq[E <: Enum](width : Int, right : Seq[Able[DFEnum[E]]])(implicit w : WidthOf[E]) : Seq[Token[E]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[E](t)
          case (t : Enum.Entry) => Token[E](t.asInstanceOf[E#Entry])
          case (t : Token[_]) => t.asInstanceOf[Token[E]]
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[E <: Enum](implicit w : WidthOf[E]) : Builder[DFEnum[E], Token[E]] = (left, right) =>
        Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends Prev {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[E <: Enum](implicit ctx : DFAny.Alias.Context, w : WidthOf[E]) : Builder[DFEnum[E]] = new Builder[DFEnum[E]] {
        def apply[P](left : DFEnum[E], right : Natural.Int.Checked[P]) : DFEnum[E] =
          new Alias(left.enum, List(left), AliasReference.Prev(right))
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L] {
      val left = value
      def <> [E <: Enum, RDIR <: DFDir](port : DFEnum[E] <> RDIR)(
        implicit op: `Op<>`.Builder[DFEnum[E], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFEnumFromEntry[L <: Enum.Entry](left : L) extends Able[L](left)
      final implicit def DFEnumFromEntry[L <: Enum.Entry](left: L): DFEnumFromEntry[L] = new DFEnumFromEntry(left)
      final implicit def ofDFEnum[R <: DFEnum.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
    }
    object Able extends Implicits
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait `Ops:=,<>` extends `Op:=` with `Op<>` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support assignment/connect operation with the type ${R}")
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

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Aux[DFEnum[E], DFEnum[E], DFEnum[E]] =
        create[E, DFEnum[E], DFEnum[E]]((left, right) => right)

      implicit def evDFEnum_op_Entry[E <: Enum, Entry <: E#Entry](implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Aux[DFEnum[E], Entry, DFEnum[E]] =
        create[E, DFEnum[E], Entry]((left, rightEntry) => const(left.enum, Token[E](rightEntry)))
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare(kind : DiSoOp.Kind) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool}

    object Builder {
      def create[E <: Enum, L, R](properLR : (L, R) => (DFEnum[E], DFEnum[E]))(implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val leftBits = left.bits()
        val rightBits = right.bits()

        val result : DFBool = kind match {
          case DiSoOp.Kind.== => leftBits == rightBits
          case DiSoOp.Kind.!= => leftBits != rightBits
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }

        result.setAutoName(ctx.n.value)
      }

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Builder[DFEnum[E], DFEnum[E]] = create[E, DFEnum[E], DFEnum[E]]((left, right) => (left, right))

      implicit def evDFEnum_op_Entry[E <: Enum, R <: E#Entry](implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Builder[DFEnum[E], R] = create[E, DFEnum[E], R]((left, rightEntry) => (left, const(left.enum, Token[E](rightEntry))))

      implicit def evEntry_op_DFEnum[E <: Enum, L <: E#Entry](implicit ctx : DFAny.Op.Context, w : WidthOf[E])
      : Builder[L, DFEnum[E]] = create[E, L, DFEnum[E]]((leftEntry, right) => (const(right.enum, Token[E](leftEntry)), right))
    }
  }

  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class Enum(implicit n : NameIt) {
  private[DFiant] val entries : HashMap[BigInt, Enum.Entry] = HashMap.empty[BigInt, Enum.Entry]
  final protected implicit val EnumOwnerToBe : Enum = this
  type Entry <: Enum.Entry
  type EntryWidth
  val name : String = n.value
  override def toString: String = name
}

object Enum {
  sealed trait Entry {
    val value : BigInt
    val enumOwner : Enum
    val name : String
    lazy val fullName = s"${enumOwner.name}.$name"
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
  
  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(implicit n : NameIt) extends Enum {
    type CheckEntry[Entry] = RequireMsgSym[EnumCount[Entry] != 0, "No enumeration entries found or the Entry is not a sealed trait", SafeInt[_]]
    type EntryWidth = CheckEntry[Entry] ==> encoding.EntryWidth[Entry]
    final protected implicit val cnt = new Auto.Counter(encoding.func) {}
  }
  object Auto {
    abstract class Counter(func : Int => BigInt) {
      def getValue : BigInt = func(cnt)
      private var cnt : Int = 0
      def inc : Unit = {cnt = cnt + 1}
    }
    abstract class Entry(implicit cnt : Counter, val enumOwner : Enum, n : NameIt) extends Enum.Entry {
      val value : BigInt = cnt.getValue
      cnt.inc
      enumOwner.entries.update(value, this)
      val name : String = n.value
    }
  }
  abstract class Manual[Width](implicit width : SafeInt[Width]) extends Enum {
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    class Entry private (val value : BigInt, val enumOwner : Enum, val name : String) extends Enum.Entry {
      enumOwner.entries.update(value, this)
    }
    object Entry {
      def apply[T <: Int with Singleton](t : T)(
        implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : Enum, n : NameIt
      ) : Entry = new Entry(t, enumOwner, n.value)

      def apply[T <: Long with Singleton](t : T)(
        implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]], enumOwner : Enum, n : NameIt
      ) : Entry = new Entry(t, enumOwner, n.value)

      def apply(t : BigInt)(
        implicit enumOwner : Enum, n : NameIt
      ) : Entry = {
        require(t.bitsWidth <= width, s"Entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
        new Entry(t, enumOwner, n.value)
      }

      private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
      def apply[W](t : XBitVector[W])(
        implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : Enum, n : NameIt
      ) : Entry = new Entry(t.toBigInt, enumOwner, n.value)

      def apply(t : BitVector)(
        implicit enumOwner : Enum, n : NameIt
      ) : Entry = {
        require(t.length == width.toLong, s"Entry value width (${t.length}) is different than the enumeration width ($width)")
        new Entry(t.toBigInt, enumOwner, n.value)
      }
    }
  }
}
