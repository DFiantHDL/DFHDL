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

package DFiant

import DFiant.DFAny.`Op==,!=`
import singleton.ops._
import singleton.twoface._
import DFiant.internals._

import scala.collection.{immutable, mutable}
import compiler.csprinter._
import compiler.printer.formatter._

/**
  * A dataflow enumeration companion object
  */
object DFEnum extends DFAny.Companion {
  final case class Type[E <: Entries](entries : E) extends DFAny.Type {
    type Width = entries.EntryWidth
    val width : TwoFace.Int[Width] = entries.width
    type TToken = Token
    type TPattern = Pattern
    type TPatternAble[+R] = Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = Pattern.Builder[LType]
    def getBubbleToken: TToken = Token.bubble(entries)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      Token(entries, entries.all(fromToken.valueBits.toBigInt))
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case DFEnum(entries) if (this.entries == entries) =>
    }
    override def toString: String = s"DFEnum[$entries]"
    def codeString(implicit printer: CSPrinter) : String = entries.refCodeString
    override def equals(obj: Any): Boolean = obj match {
      case Type(entries) => this.entries == entries
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Construct a new dataflow enumeration with a given enumerate type
    * @param entries An enumeration type
    * @param ctx An implicit dataflow design context
    *
    * @example
    * {{{
    *   val enum = DFEnum[MyEnum.type]
    * }}}
    */
  def apply[E <: Entries](implicit v : ValueOf[E], di : DummyImplicit) : Type[E] = Type(valueOf[E])
  /**
    * Construct a new dataflow enumeration with a given enumerate type
    * @param entries An enumeration type
    * @param ctx An implicit dataflow design context
    *
    * @example
    * {{{
    *   val enum = DFEnum(MyEnum)
    * }}}
    */
  def apply[E <: Entries](entries : E) : Type[E] = Type(entries)
  def unapply(arg: DFAny.Member): Option[Entries] = arg.dfType match {
    case Type(entries) => Some(entries)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Frontend
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Frontend {
    trait Inherited extends Op.Frontend.Inherited with Token.Frontend.Inherited
    trait Imported extends Op.Frontend.Imported with Token.Frontend.Imported
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenE[E <: Entries] = DFAny.TokenT[Token, Type[E]]
  final case class Token(entries : Entries, value : Option[Entries.Entry]) extends DFAny.Token.Of[Type[Entries], Entries.Entry] {
    val dfType : DFAny.Type = Type(entries)
    val width : Int = entries.width.getValue
    def valueToBitVector(value : Entries.Entry) : BitVector = value.value.toBitVector(width)
    def valueCodeString(value : Entries.Entry)(implicit printer: CSPrinter) : String = value.codeString
  }
  object Token {
    def bubble(entries : Entries) : Token = Token(entries, None)
    def apply(entries : Entries, entry : Entries.Entry) : Token = Token(entries, Some(entry))

    type Summon[E <: Entries, V] = DFAny.Token.Exact.Summon.SAM[Type[E], V, TokenE[E]]
    sealed trait Frontend {
      protected implicit def __DFEnumTokenEntry[E <: Entries, V <: E#Entry]
      : Summon[E, V] = (from, value) => Token(from.entries, value).typeTag[Type[E]]
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFEnumTokenEntry[E <: Entries, V <: E#Entry] : Summon[E, V] = super.__DFEnumTokenEntry
      }
      trait Imported extends Frontend {
        final override implicit def __DFEnumTokenEntry[E <: Entries, V <: E#Entry] : Summon[E, V] = super.__DFEnumTokenEntry
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[Entries.Entry]) extends DFAny.Pattern.OfSet[Type[Entries], Entries.Entry, Pattern](set) {
    protected def matchCond(matchVal: DFAny.Of[Type[Entries]], value : Entries.Entry)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Frontend._
      matchVal === value.asInstanceOf[Entries#Entry]
    }
  }
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFEnumPattern[E <: Entries](val right : E#Entry) extends Able[E#Entry]
    }
    trait Builder[LType <: DFAny.Type]
        extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[E <: Entries] : Builder[Type[E]] = new Builder[Type[E]] {
        def apply[R](left: Type[E], right: Seq[Able[R]]): Pattern = {
          new Pattern(right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
        }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    class Able[L](val value: L) extends DFAny.Op.Able[L]
    class AbleOps[L](value: L) extends Able[L](value) {
      val left = value
      final def === [E <: Entries](right : DFEnum[E])(implicit op: DFAny.`Op==`.Builder[L, DFEnum[E]]) = op(left, right)
      final def =!= [E <: Entries](right : DFEnum[E])(implicit op: DFAny.`Op!=`.Builder[L, DFEnum[E]]) = op(left, right)
    }
    sealed trait Frontend {
      sealed class __DFEnumFromEntry[L <: Entries.Entry](left : L) extends AbleOps[L](left)
      protected implicit def __DFEnumFromEntry[L <: Entries.Entry](left: L): __DFEnumFromEntry[L] = new __DFEnumFromEntry(left)
      protected implicit def __ofDFEnum[E <: Entries](left : DFEnum[E]) : Able[DFEnum[E]] = new Able(left)
      protected implicit class __DFEnumOps[E <: Entries](val left : DFEnum[E]){
        def === [R](right : Exact[R])(implicit op: DFAny.`Op==`.Builder[DFEnum[E], R]) = op(left, right)
        def =!= [R](right : Exact[R])(implicit op: DFAny.`Op!=`.Builder[DFEnum[E], R]) = op(left, right)
      }
      protected implicit def __DFEnum_eq_Capable[E <: Entries]
      : DFAny.`Op==,!=`.Capable[Type[E], Type[E]] =
        (left, right) => assert(left.entries == right.entries)

      protected implicit def __DFEnum_eq_ConstCapable[E <: Entries]
      : DFAny.`Op==,!=`.ConstCapable[Type[E], Type[E]] =
        (left, right) => assert(left.entries == right.entries)
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFEnumFromEntry[L <: Entries.Entry](left : L) : __DFEnumFromEntry[L] = super.__DFEnumFromEntry(left)
        final override protected implicit def __ofDFEnum[E <: Entries](left : DFEnum[E]) : Able[DFEnum[E]] = super.__ofDFEnum(left)
        final override protected implicit def __DFEnumOps[E <: Entries](left : DFEnum[E]) : __DFEnumOps[E] = super.__DFEnumOps(left)
        final override protected implicit def __DFEnum_eq_Capable[E <: Entries] : `Op==,!=`.Capable[Type[E], Type[E]] = super.__DFEnum_eq_Capable
        final override protected implicit def __DFEnum_eq_ConstCapable[E <: Entries] : `Op==,!=`.ConstCapable[Type[E], Type[E]] = super.__DFEnum_eq_ConstCapable
      }
      trait Imported extends Frontend {
        final override implicit def __DFEnumFromEntry[L <: Entries.Entry](left : L) : __DFEnumFromEntry[L] = super.__DFEnumFromEntry(left)
        final override implicit def __ofDFEnum[E <: Entries](left : DFEnum[E]) : Able[DFEnum[E]] = super.__ofDFEnum(left)
        final override implicit def __DFEnumOps[E <: Entries](left : DFEnum[E]) : __DFEnumOps[E] = super.__DFEnumOps(left)
        final override implicit def __DFEnum_eq_Capable[E <: Entries] : `Op==,!=`.Capable[Type[E], Type[E]] = super.__DFEnum_eq_Capable
        final override implicit def __DFEnum_eq_ConstCapable[E <: Entries] : `Op==,!=`.ConstCapable[Type[E], Type[E]] = super.__DFEnum_eq_ConstCapable
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /**
    * A dataflow's enumeration underlying type that holds all the supported literals
    *
    * @example
    * {{{
    *   object States extends DFEnum.Auto {
    *     val IDLE, READY, RUN = Entry()
    *   }
    *   val state = DFEnum(States)
    * }}}
    *
    * @param meta Provides an implicit meta context. Do not manually apply.
    */
  sealed abstract class Entries(implicit meta : Meta) {
    private[DFiant] var all : immutable.ListMap[BigInt, Entries.Entry] = immutable.ListMap.empty[BigInt, Entries.Entry]
    private[DFiant] def update(entry : Entries.Entry) : Unit = {
      all.get(entry.value) match {
        case Some(existingEntry) => throw new IllegalArgumentException(s"\nDuplicate enum entry values. Attempted to create new entry `$entry` with the value ${entry.value}. The value is already taken by entry `$existingEntry`.")
        case None => all = all + (entry.value -> entry)
      }
    }
    final protected implicit val enumEntriesToBe : Entries = this
    type Entry <: Entries.Entry
    type EntryWidth
    val width : TwoFace.Int[EntryWidth]
    final def name(implicit getSet: MemberGetSet) : String = getSet.getGlobalTag[DFMember.NameTag](this) match {
      case Some(DFMember.NameTag(taggedName)) => taggedName
      case _ => meta.name
    }
    def codeString(implicit printer: CSPrinter) : String
    def refCodeString(implicit printer: CSPrinter) : String = name
    override def toString: String = meta.name
  }

  object Entries {
    implicit def dfTypeTC[E <: Entries] : E => DFEnum.Type[E] = DFEnum(_)

    sealed trait Entry {
      val value : BigInt
      val entries : Entries
      private[DFiant] val meta : Meta
      val name : String = meta.name

      final def getFullName(implicit getSet : MemberGetSet) = s"${entries.name}.$name"

      def codeString(implicit printer : CSPrinter) : String = getFullName

      final override def toString : String = name
    }

    object Entry {
      implicit def csoEnum[E <: Entry] : CodeStringOf[E] = new CodeStringOf[E] {
        override def apply(t : E)(implicit printer : CSPrinter) : String = t.codeString
      }
    }
  }
  /**
    * An encoding methods for automatic enumeration
    */
  trait Encoding extends HasCodeString {
    def calcWidth(entryCount: Int): Int
    val func: Int => BigInt
    def codeString(implicit printer: CSPrinter): String
  }
  object Encoding {

    /**
      * The default encoding is incremental from 0
      */
    object Default extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth(false)
      val func : Int => BigInt = t => BigInt(t)
      def codeString(implicit printer: CSPrinter) : String = "DFEnum.Encoding.Default"
    }

    /**
      * Grey encoding
      */
    object Grey extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth(false)
      val func : Int => BigInt = t => BigInt(t ^ (t >>> 1))
      def codeString(implicit printer: CSPrinter) : String = "DFEnum.Encoding.Grey"
    }

    /**
      * Incremental encoding that starts at a given value
      * @param value A singleton value to start at
      */
    case class StartAt[V <: Int with Singleton](value : V) extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1 + value).bitsWidth(false)
      val func : Int => BigInt = t => BigInt(t + value)
      def codeString(implicit printer: CSPrinter) : String = s"DFEnum.Encoding.StartAt($value)"
    }

    /**
      * One-Hot encoding
      */
    object OneHot extends Encoding {
      def calcWidth(entryCount : Int) : Int = entryCount
      val func : Int => BigInt = t => BigInt(1) << t
      def codeString(implicit printer: CSPrinter) : String = "DFEnum.Encoding.OneHot"
    }
  }

  /**
    * Automatic Enumeration
    *
    * By extending this class, you choose automatic enumeration according to the `encoding` argument
    *
    * @param encoding The automatic encoding method selected.
    * @param meta Provides an implicit meta context. Do not manually apply.
    */
  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(
    implicit meta : Meta
  ) extends Entries { self =>
    type EntryWidth = Int
    final lazy val width : TwoFace.Int[EntryWidth] = encoding.calcWidth(all.size)
    private def entriesCodeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      f"$SC val ${all.map(e => e._2.name).mkString(",")} = $DF Entry()"
    }
    private def encodingCodeString(implicit printer: CSPrinter): String =
      if (encoding == Encoding.Default) "" else s"(${encoding.codeString})"
    final def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"\n$SC object $name $SC extends $DF DFEnum.$DF Auto$encodingCodeString {\n${entriesCodeString.delim()}\n}"
    }

    class Entry private[DFiant] (implicit val entries : Entries, private[DFiant] val meta : Meta) extends Entries.Entry {
      val value : BigInt = encoding.func(all.size)
      entries.update(this)
    }

    /**
      * @param meta Provides an implicit meta context. Do not manually apply.
      * @return A new enumeration entry literal
      */
    def Entry()(implicit meta: Meta): Entry = new Entry
  }

  /**
    * A manually encoded enumeration.
    *
    * @param width The explicit width of the encoding.
    * @param meta Provides an implicit meta context. Do not manually apply.
    */
  abstract class Manual[Width <: Int with Singleton](val width : TwoFace.Int[Width])(
    implicit meta : Meta
  ) extends Entries { self =>
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[
      EW
    ] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    private var latestEntryValue: Option[BigInt] = None
    private def entriesCodeString(implicit printer: CSPrinter): String = {
      import printer.config._
      val cs = implicitly[CodeStringOf[BitVector]]
      all.map(e => f"\n  $SC final $SC val ${e._2.name} ${ALGN(0)}= $DF Entry(${cs(e._1.toBitVector(width))})").mkString
    }
    final def codeString(implicit printer: CSPrinter): String = {
      import printer.config._
      s"\n$SC object $name $SC extends $DF DFEnum.$DF Manual($width) {$entriesCodeString\n}"
    }

    class Entry private[DFiant] (val value : BigInt, val entries : Entries)(implicit private[DFiant] val meta : Meta) extends Entries.Entry {
      entries.update(this)
      latestEntryValue = Some(value)
    }

    def Entry[T <: Int with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : Entries, meta : Meta
    ) : Entry = new Entry(t, enumOwner)

    def Entry(t : BigInt)(
      implicit enumOwner : Entries, meta : Meta
    ) : Entry = {
      val tBitsWidth = t.bitsWidth(false)
      require(
        tBitsWidth <= width,
        s"`${meta.name}` entry value width ($tBitsWidth) is bigger than the enumeration width ($width)"
      )
      new Entry(t, enumOwner)
    }

    private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
    def Entry[W](t : DFBits.TokenW[W])(
      implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : Entries, meta : Meta
    ) : Entry = new Entry(t.valueBits.toBigInt, enumOwner)

    def Entry(t : DFBits.Token)(
      implicit enumOwner : Entries, meta : Meta
    ) : Entry = {
      require(t.width == width.getValue, s"`${meta.name}` entry value width (${t.width}) is different than the enumeration width ($width)")
      new Entry(t.valueBits.toBigInt, enumOwner)
    }

    def EntryDelta(t: BigInt = BigInt(1))(implicit meta: Meta): Entry =
      Entry(latestEntryValue match {
        case Some(value) => value + 1
        case None        => BigInt(0)
      })
    def EntryIncLastBy(t: BitVector): Entry = EntryDelta(t.toBigInt)
  }
}


