/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package ZFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

import scala.collection.mutable
import DFAny.Func2
import DFAny.Func2
import ZFiant.compiler.printer.Printer

object DFEnum extends DFAny.Companion {
  final case class Type[E <: EnumType](enumType : E) extends DFAny.Type {
    type Width = enumType.EntryWidth
    val width : TwoFace.Int[Width] = enumType.width
    type TToken = Token[E]
    type TPattern = Pattern[E]
    type TPatternAble[+R] = Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = Pattern.Builder[LType]
    type OpAble[R] = Op.Able[R]
    type `Op==Builder`[L, R] = `Op==`.Builder[L, R]
    type `Op!=Builder`[L, R] = `Op!=`.Builder[L, R]
    type `Op<>Builder`[LType <: DFAny.Type, R] = `Op<>`.Builder[LType, R]
    type `Op:=Builder`[LType <: DFAny.Type, R] = `Op:=`.Builder[LType, R]
    type InitAble[L <: DFAny] = Init.Able[L]
    type InitBuilder[L <: DFAny] = Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token =
      Token[E](enumType, enumType.entries(fromToken.valueBits.toBigInt).asInstanceOf[E#Entry])
    override def toString: String = s"DFEnum[$enumType]"
    def codeString(implicit printConfig : Printer.Config) : String = s"${printConfig.TP}DFEnum($enumType)"
    override def equals(obj: Any): Boolean = obj match {
      case Type(enumType) => this.enumType == enumType
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  def apply[E <: Enum](implicit ctx : DFAny.Context, e : E) : NewVar[E] = new NewVar[E]()
  def apply[E <: EnumType](enumType : E)(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(enumType))
  def unapply(arg: Any): Option[EnumType] = arg match {
    case dfAny : DFAny => dfAny.dfType match {
      case Type(enumType) => Some(enumType)
      case _ => None
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token[E <: EnumType](enumType : E, value : Option[E#Entry]) extends DFAny.Token.Of[Option[E#Entry], E#EntryWidth] {
    val width : TwoFace.Int[Width] = enumType.width.asInstanceOf[TwoFace.Int[E#EntryWidth]]
    val (valueBits, bubbleMask) : (XBitVector[Width], XBitVector[Width]) =
      if (value.isDefined) (value.get.value.toBitVector(width), false.toBitVector(width))
      else (0.toBitVector(width), true.toBitVector(width))

    def == [RE <: EnumType](that : Token[RE]) : DFBool.Token =
      DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
    def != [RE <: EnumType](that : Token[RE]) : DFBool.Token =
      DFBool.Token(logical = true, this.value != that.value, this.isBubble || that.isBubble)
    def codeString(implicit printConfig : Printer.Config) : String = value.get.codeString
    override def equals(obj: Any): Boolean = obj match {
      case Token(enumType, value) => this.enumType == enumType && this.value == value
      case _ => false
    }
  }

  object Token {
    implicit def bubbleOfToken[E <: EnumType] : DFAny.Token.BubbleOfToken[Token[E]] = t => Token.bubble[E](t.enumType)
    implicit def bubbleOfDFType[E <: EnumType] : DFAny.Token.BubbleOfDFType[Type[E]] = t => Token.bubble[E](t.enumType)
    import DFAny.TokenSeq
    def bubble[E <: EnumType](enumType : E) : Token[E] = Token[E](enumType, None)
    def apply[E <: EnumType](enumType : E, entry : E#Entry) : Token[E] = Token[E](enumType, Some(entry))
    def == [E <: EnumType](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)(_ == _)
    def != [E <: EnumType](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)(_ == _)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern[E <: EnumType](set : Set[E#Entry]) extends DFAny.Pattern.OfSet[E#Entry, Pattern[E]](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFEnumPattern[E <: EnumType](val right : E#Entry) extends Able[E#Entry]
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[E <: EnumType] : Builder[Type[E]] = new Builder[Type[E]] {
        def apply[R](left: Type[E], right: Seq[Able[R]]): Pattern[E] = {

          new Pattern[E](right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
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
      implicit class DFEnumBubble[E <: EnumType](val right : Bubble) extends Able[DFEnum[E]]
      implicit class DFEnumToken[E <: EnumType](val right : Token[E]) extends Able[DFEnum[E]]
      implicit class DFEnumTokenSeq[E <: EnumType](val right : Seq[Token[E]]) extends Able[DFEnum[E]]
      implicit class DFEnumEntry[E <: EnumType, R <: E#Entry](val right : R) extends Able[DFEnum[E]]

      def toTokenSeq[E <: EnumType](enumType : E, right : Seq[Able[DFEnum[E]]]) : Seq[Token[E]] =
        right.toSeqAny.map {
          case (t : Bubble) => Token.bubble[E](enumType)
          case (t : EnumType.Entry) => Token[E](enumType, t.asInstanceOf[E#Entry])
          case (t : Token[_]) => t.asInstanceOf[Token[E]]
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[E <: EnumType] : Builder[DFEnum[E], Token[E]] = (left, right) =>
        Able.toTokenSeq(left.dfType.enumType, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op extends OpCO {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
    }
    trait Implicits {
      sealed class DFEnumFromEntry[L <: EnumType.Entry](left : L) extends AbleOps[L](left)
      final implicit def DFEnumFromEntry[L <: EnumType.Entry](left: L): DFEnumFromEntry[L] = new DFEnumFromEntry(left)
      sealed class DFEnumFromDefaultRet[E <: EnumType](left : DFAny.DefaultRet[Type[E]])(implicit ctx : DFAny.Context) extends AbleOps[DFEnum[E]](left)
      final implicit def DFEnumFromDefaultRet[E <: EnumType](left : DFAny.DefaultRet[Type[E]])(implicit ctx : DFAny.Context) : DFEnumFromDefaultRet[E] = new DFEnumFromDefaultRet(left)
      final implicit def ofDFEnum[E <: EnumType](left : DFEnum[E]) : Able[DFEnum[E]] = new Able(left)
    }
    object Able extends Implicits
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
      implicit def evDFEnum_op_DFEnum[E <: EnumType](implicit ctx : DFAny.Context)
      : Builder[Type[E], DFEnum[E]] = (left, right) => right

      implicit def evDFEnum_op_Entry[E <: EnumType, Entry <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[Type[E], Entry] =
        (left, rightEntry) => DFAny.Const[Type[E]](Type(left.enumType), Token[E](left.enumType, rightEntry))
    }
  }
  object `Op:=` extends `Ops:=,<>`
  object `Op<>` extends `Ops:=,<>`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[E <: EnumType, L, R](properLR : (L, R) => (DFEnum[E], DFEnum[E]))(implicit ctx : DFAny.Context)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val func : (left.TToken, right.TToken) => DFBool.Token = op match {
          case _ : Func2.Op.== => _ == _
          case _ : Func2.Op.!= => _ != _
        }
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFEnum_op_DFEnum[E <: EnumType](implicit ctx : DFAny.Context)
      : Builder[DFEnum[E], DFEnum[E]] = create[E, DFEnum[E], DFEnum[E]]((left, right) => (left, right))

      implicit def evDFEnum_op_Entry[E <: EnumType, R <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[DFEnum[E], R] = create[E, DFEnum[E], R]((left, rightEntry) =>
        (left, DFAny.Const[Type[E]](Type(left.dfType.enumType), Token[E](left.dfType.enumType, rightEntry))))

      implicit def evEntry_op_DFEnum[E <: EnumType, L <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[L, DFEnum[E]] = create[E, L, DFEnum[E]]((leftEntry, right) =>
        (DFAny.Const[Type[E]](Type(right.dfType.enumType), Token[E](right.dfType.enumType, leftEntry)), right))
    }
  }
  object `Op==` extends OpsCompare(Func2.Op.==) with `Op==`
  object `Op!=` extends OpsCompare(Func2.Op.!=) with `Op!=`
  object `Op===` extends OpsCompare(Func2.Op.==)
  object `Op=!=` extends OpsCompare(Func2.Op.!=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class EnumType(implicit meta : Meta) extends HasCodeString {
  private[ZFiant] val entries : mutable.Map[BigInt, EnumType.Entry] = mutable.Map.empty[BigInt, EnumType.Entry]
  private[ZFiant] def update(entry : EnumType.Entry) : Unit = {
    entries.get(entry.value) match {
      case Some(existingEntry) => throw new IllegalArgumentException(s"\nDuplicate enum entry values. Attempted to create new entry `$entry` with the value ${entry.value}. The value is already taken by entry `$existingEntry`.")
      case None => entries.update(entry.value, entry)
    }
  }
  final protected implicit val EnumTypeToBe : EnumType = this
  type Entry <: EnumType.Entry
  type EntryWidth
  val width : TwoFace.Int[EntryWidth]
  final val name : String = meta.name
  override def toString: String = name
}

object EnumType {
  sealed trait Entry extends HasCodeString {
    val value : BigInt
    val enumType : EnumType
    val name : String
    final lazy val fullName = s"${enumType.name}.$name"
    def codeString: String = fullName
    final override def toString: String = name
  }
  object Entry {
    implicit def csoEnum[E <: Entry] : CodeStringOf[E] = t => t.codeString
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

  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(implicit meta : Meta) extends EnumType {
    type EntryWidth = Int
    final lazy val width : TwoFace.Int[EntryWidth] = encoding.calcWidth(entries.size)
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry()  //${e._1.codeString}").mkString
    private def encodingCodeString : String = if (encoding == Encoding.Default) "" else s"(${encoding.codeString})"
    final def codeString : String = s"\nobject $name extends Enum.Auto$encodingCodeString {$entriesCodeString\n}"

    class Entry private[ZFiant] (implicit val enumType : EnumType, meta : Meta) extends EnumType.Entry {
      val value : BigInt = encoding.func(entries.size)
      val name : String = meta.name
      enumType.update(this)
    }
    def Entry()(implicit meta : Meta) : Entry = new Entry
  }

  abstract class Manual[Width <: Int with Singleton](val width : TwoFace.Int[Width])(implicit meta : Meta) extends EnumType {
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    private var latestEntryValue : Option[BigInt] = None
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry(${e._1.toBitVector(width).codeString})").mkString
    final def codeString : String = s"\nobject $name extends Enum.Manual($width) {$entriesCodeString\n}"

    class Entry private[ZFiant] (val value : BigInt, val enumType : EnumType, val name : String) extends EnumType.Entry {
      enumType.update(this)
      latestEntryValue = Some(value)
    }

    def Entry[T <: Int with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t, enumOwner, meta.name)

    def Entry[T <: Long with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t, enumOwner, meta.name)

    def Entry(t : BigInt)(
      implicit enumOwner : EnumType, meta : Meta
    ) : Entry = {
      require(t.bitsWidth <= width, s"`${meta.name}` entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
      new Entry(t, enumOwner, meta.name)
    }

    private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
    def Entry[W](t : XBitVector[W])(
      implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t.toBigInt, enumOwner, meta.name)

    def Entry(t : BitVector)(
      implicit enumOwner : EnumType, meta : Meta
    ) : Entry = {
      require(t.length.toInt == width.getValue, s"`${meta.name}` entry value width (${t.length}) is different than the enumeration width ($width)")
      new Entry(t.toBigInt, enumOwner, meta.name)
    }

    def EntryDelta(t : BigInt = BigInt(1))(implicit meta : Meta) : Entry = Entry(latestEntryValue match {
      case Some(value) => value + 1
      case None => BigInt(0)
    })
    def EntryIncLastBy(t : BitVector) : Entry = EntryDelta(t.toBigInt)
  }
}
