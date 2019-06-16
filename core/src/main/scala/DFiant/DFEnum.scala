/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the Lesser GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     Lesser GNU General Public License for more details.
 *
 *     You should have received a copy of the Lesser GNU General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._
import DFiant.internals._

import scala.collection.mutable.LinkedHashMap


trait DFEnum[E <: Enum] extends DFEnum.Unbounded {
  type TEnum = E
}
object DFEnum extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFEnum.type] {
    protected[DFiant] type TUnbounded = Unbounded
    type TEnum <: Enum
    type TEntry = TEnum#Entry
    type Width = TEnum#EntryWidth
    protected[DFiant] type TVal = DFEnum[TEnum]
    protected[DFiant] type TVar = DFEnum.Var[TEnum]
    protected[DFiant] type TToken = DFEnum.Token[TEnum]
    protected[DFiant] type TPattern = DFEnum.Pattern[TEnum]
    protected[DFiant] type TPatternAble[+R] = DFEnum.Pattern.Able[R]
    protected[DFiant] type TPatternBuilder[L <: DFAny] = DFEnum.Pattern.Builder[L]
    protected[DFiant] type OpAble[R] = Op.Able[R]
    protected[DFiant] type `Op<>Builder`[R] = `Op<>`.Builder[TVal, R]
    protected[DFiant] type `Op:=Builder`[R] = `Op:=`.Builder[TVal, R]
    protected[DFiant] type `Op==Builder`[R] = `Op==`.Builder[TVal, R]
    protected[DFiant] type `Op!=Builder`[R] = `Op!=`.Builder[TVal, R]
    protected[DFiant] type InitAble[L <: DFAny] = Init.Able[L]
    protected[DFiant] type InitBuilder = Init.Builder[TVal, TToken]
    protected[DFiant] type PortBuilder[Dir <: DFDir] = Port.Builder[TVal, Dir]
    implicit val enum : TEnum
    final def == [E <: TEntry](right : E)(implicit op: `Op==`.Builder[TVal, E]) = op(left, right)
    final def != [E <: TEntry](right : E)(implicit op: `Op!=`.Builder[TVal, E]) = op(left, right)
    final protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <~> Dir = new Port(new NewVar[TEnum], dir)
    final protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
      implicit ctx : DFAny.Alias.Context
    ) : TAlias = new Alias(reference)(ctx, enum).asInstanceOf[TAlias]
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[E <: Enum] extends DFEnum[E] with DFAny.Var {
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[E <: Enum](implicit ctx : DFAny.NewVar.Context, e : E) : NewVar[E] = new NewVar[E]()
  def apply[E <: Enum](e : E)(implicit ctx : DFAny.NewVar.Context) : NewVar[E] = new NewVar[E]()(ctx, e)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[E <: Enum]()(
    implicit ctx : DFAny.NewVar.Context, val enum : E
  ) extends DFAny.NewVar[DFEnum[E]](enum.width, s"DFEnum(${enum.name})") with Var[E]

  protected[DFiant] final class Alias[E <: Enum](reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context, val enum : E
  ) extends DFAny.Alias[DFEnum[E]](reference) with Var[E]

  protected[DFiant] final class Const[E <: Enum](token : Token[E])(
    implicit ctx : DFAny.Const.Context, val enum : E
  ) extends DFAny.Const[DFEnum[E]](token) with DFEnum[E]

  protected[DFiant] final class Port[E <: Enum, Dir <: DFDir](val dfVar : DFEnum[E], dir : Dir)(
    implicit ctx : DFAny.Port.Context, val enum : E
  ) extends DFAny.Port[DFEnum[E], Dir](dfVar, dir) with DFEnum[E]
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Token[E <: Enum] private[DFiant](width : Int, value : E#Entry) extends DFAny.Token.Of[E#Entry, Pattern[E]] {
    protected[DFiant] type TToken = Token[E]
    val (valueBits, bubbleMask) : (BitVector, BitVector) =
      if (value != null) (value.value.toBitVector(width), false.toBitVector(width))
      else (0.toBitVector(width), true.toBitVector(width))

    def toBubbleToken : Token[E] = Token(width, Bubble)

    final def == (that : Token[E]) : DFBool.Token =
      if (this.value != null && that.value != null) DFBool.Token(this.value == that.value)
      else DFBool.Token(Bubble)

    final def != (that : Token[E]) : DFBool.Token = !(this == that)
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    def == [E <: Enum](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != [E <: Enum](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply[E <: Enum](width : Int, value : Bubble) : Token[E] = new Token[E](width, null.asInstanceOf[E#Entry])
    def apply[E <: Enum](width : Int, value : E#Entry) : Token[E] = new Token[E](width, value)
    implicit def bubbleOf[E <: Enum] : DFEnum[E] => Token[E] = t => Token(t.width, Bubble)
    implicit def fromBits[E <: Enum](implicit e : E) : DFBits.Token => Token[E] =
      t => Token[E](e.width, e.entries(t.valueBits.toBigInt).asInstanceOf[E#Entry])
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[E <: Enum, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFEnum[E], Dir] = (right, dir) => new Port[E, Dir](right, dir)(ctx, right.enum)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.TEnum](DFAny.Alias.Reference.AsIs(left, s".as(DFEnum(${mold.enum}))"))(ctx, mold.enum)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFEnumBubble[E <: Enum](val right : Bubble) extends Able[DFEnum[E]]
      implicit class DFEnumToken[E <: Enum](val right : Token[E]) extends Able[DFEnum[E]]
      implicit class DFEnumTokenSeq[E <: Enum](val right : Seq[Token[E]]) extends Able[DFEnum[E]]
      implicit class DFEnumEntry[E <: Enum, R <: E#Entry](val right : R) extends Able[DFEnum[E]]

      def toTokenSeq[E <: Enum](width : Int, right : Seq[Able[DFEnum[E]]]) : Seq[Token[E]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[E](width, t)
          case (t : Enum.Entry) => Token[E](width, t.asInstanceOf[E#Entry])
          case (t : Token[_]) => t.asInstanceOf[Token[E]]
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[E <: Enum] : Builder[DFEnum[E], Token[E]] = (left, right) =>
        Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern[E <: Enum](set : Set[E#Entry]) extends DFAny.Pattern.OfSet[E#Entry, Pattern[E]](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFEnumPattern[E <: Enum](val right : E#Entry) extends Able[E#Entry]
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[E <: Enum] : Builder[DFEnum[E]] = new Builder[DFEnum[E]] {
        def apply[R](left: DFEnum[E], right: Seq[Able[R]]): Pattern[E] = {

          new Pattern[E](right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
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
      val left = value
      def <> [E <: Enum](port : DFAny.Connectable[DFEnum[E]] with DFEnum[E])(
        implicit op: `Op<>`.Builder[DFEnum[E], L], ctx : DFAny.Connector.Context
      ) = port.connectWith(op(port, left))
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

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit ctx : DFAny.Op.Context)
      : Aux[DFEnum[E], DFEnum[E], DFEnum[E]] =
        create[E, DFEnum[E], DFEnum[E]]((left, right) => right)

      implicit def evDFEnum_op_Entry[E <: Enum, Entry <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Aux[DFEnum[E], Entry, DFEnum[E]] =
        create[E, DFEnum[E], Entry]((left, rightEntry) => new Const(Token[E](left.width, rightEntry))(ctx, left.enum))
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[K <: DiSoOp.Kind](kind : K) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool with CanBePiped}

    object Builder {
      def create[E <: Enum, L, R](properLR : (L, R) => (DFEnum[E], DFEnum[E]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        import FunctionalLib.DFEnumOps._
        val (left, right) = properLR(leftL, rightR)

        val result : DFBool with CanBePiped = kind match {
          case k : DiSoOp.Kind.== => new `Func2Comp==`[E](left, right)
          case k : DiSoOp.Kind.!= => new `Func2Comp!=`[E](left, right)
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }

        result.setAutoName(ctx.getName)
      }

      implicit def evDFEnum_op_DFEnum[E <: Enum](implicit ctx : DFAny.Op.Context)
      : Builder[DFEnum[E], DFEnum[E]] = create[E, DFEnum[E], DFEnum[E]]((left, right) => (left, right))

      implicit def evDFEnum_op_Entry[E <: Enum, R <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Builder[DFEnum[E], R] = create[E, DFEnum[E], R]((left, rightEntry) => (left, new Const(Token[E](left.width, rightEntry))(ctx, left.enum)))

      implicit def evEntry_op_DFEnum[E <: Enum, L <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Builder[L, DFEnum[E]] = create[E, L, DFEnum[E]]((leftEntry, right) => (new Const(Token[E](right.width, leftEntry))(ctx, right.enum), right))
    }
  }

  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class Enum(implicit n : NameIt) extends HasCodeString {
  private[DFiant] val entries : LinkedHashMap[BigInt, Enum.Entry] = LinkedHashMap.empty[BigInt, Enum.Entry]
  private[DFiant] def update(entry : Enum.Entry) : Unit = {
    entries.get(entry.value) match {
      case Some(existingEntry) => throw new IllegalArgumentException(s"\nDuplicate enum entry values. Attempted to create new entry `$entry` with the value ${entry.value}. The value is already taken by entry `$existingEntry`.")
      case None => entries.update(entry.value, entry)
    }
  }
  final protected implicit val EnumOwnerToBe : Enum = this
  type Entry <: Enum.Entry
  type EntryWidth
  val width : TwoFace.Int[EntryWidth]
  final val name : String = n.value
  override def toString: String = name
}

object Enum {
  sealed trait Entry extends HasCodeString {
    val value : BigInt
    val enumOwner : Enum
    val name : String
    final lazy val fullName = s"${enumOwner.name}.$name"
    def codeString: String = fullName
    final override def toString: String = name
  }

  trait Encoding extends HasCodeString {
    def calcWidth(entryCount : Int) : Int
    val func : Int => BigInt
    def codeString : String
  }
  object Encoding {
    object Default extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth
      val func : Int => BigInt = t => BigInt(t)
      def codeString : String = "Enum.Encoding.Default"
    }
    object Grey extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth
      val func : Int => BigInt = t => BigInt(t ^ (t >>> 1))
      def codeString : String = "Enum.Encoding.Grey"
    }
    case class StartAt[V <: Int with Singleton](value : V) extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1 + value).bitsWidth
      val func : Int => BigInt = t => BigInt(t + value)
      def codeString : String = s"Enum.Encoding.StartAt($value)"
    }
    object OneHot extends Encoding {
      def calcWidth(entryCount : Int) : Int = entryCount
      val func : Int => BigInt = t => BigInt(1) << t
      def codeString : String = "Enum.Encoding.OneHot"
    }
  }

  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(implicit n : NameIt) extends Enum {
    type EntryWidth = Int
    final lazy val width : TwoFace.Int[EntryWidth] = encoding.calcWidth(entries.size)
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry()  //${e._1.codeString}").mkString
    private def encodingCodeString : String = if (encoding == Encoding.Default) "" else s"(${encoding.codeString})"
    final def codeString : String = s"\nobject $name extends Enum.Auto$encodingCodeString {$entriesCodeString\n}"

    class Entry private[DFiant] (implicit val enumOwner : Enum, n : NameIt) extends Enum.Entry {
      val value : BigInt = encoding.func(entries.size)
      val name : String = n.value
      enumOwner.update(this)
    }
    def Entry()(implicit n : NameIt) : Entry = new Entry
  }

  abstract class Manual[Width <: Int with Singleton](val width : TwoFace.Int[Width])(implicit n : NameIt) extends Enum {
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    private var latestEntryValue : Option[BigInt] = None
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry(${e._1.toBitVector(width).codeString})").mkString
    final def codeString : String = s"\nobject $name extends Enum.Manual($width) {$entriesCodeString\n}"

    class Entry private[DFiant] (val value : BigInt, val enumOwner : Enum, val name : String) extends Enum.Entry {
      enumOwner.update(this)
      latestEntryValue = Some(value)
    }

    def Entry[T <: Int with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : Enum, n : NameIt
    ) : Entry = new Entry(t, enumOwner, n.value)

    def Entry[T <: Long with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]], enumOwner : Enum, n : NameIt
    ) : Entry = new Entry(t, enumOwner, n.value)

    def Entry(t : BigInt)(
      implicit enumOwner : Enum, n : NameIt
    ) : Entry = {
      require(t.bitsWidth <= width, s"`${n.value}` entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
      new Entry(t, enumOwner, n.value)
    }

    private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
    def Entry[W](t : XBitVector[W])(
      implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : Enum, n : NameIt
    ) : Entry = new Entry(t.toBigInt, enumOwner, n.value)

    def Entry(t : BitVector)(
      implicit enumOwner : Enum, n : NameIt
    ) : Entry = {
      require(t.length.toInt == width.getValue, s"`${n.value}` entry value width (${t.length}) is different than the enumeration width ($width)")
      new Entry(t.toBigInt, enumOwner, n.value)
    }

    def EntryDelta(t : BigInt = BigInt(1))(implicit n : NameIt) : Entry = Entry(latestEntryValue match {
      case Some(value) => value + 1
      case None => BigInt(0)
    })
    def EntryIncLastBy(t : BitVector) : Entry = EntryDelta(t.toBigInt)
  }
}
