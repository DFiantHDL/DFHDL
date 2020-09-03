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

import singleton.ops._
import singleton.twoface._
import DFiant.internals._

import scala.collection.{immutable, mutable}
import compiler.csprinter._
import compiler.printer.formatter._

object DFEnum extends DFAny.Companion {
  final case class Type[E <: EnumType](enumType : E) extends DFAny.Type {
    type Width = enumType.EntryWidth
    val width : TwoFace.Int[Width] = enumType.width
    type TToken = Token
    type TPattern = Pattern
    type TPatternAble[+R] = Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = Pattern.Builder[LType]
    type `Op==Builder`[-L, -R] = `Op==`.Builder[L, R]
    type `Op!=Builder`[-L, -R] = `Op!=`.Builder[L, R]
    type InitAble[L <: DFAny] = Init.Able[L]
    type InitBuilder[L <: DFAny] = Init.Builder[L, TToken]
    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      Token(enumType, enumType.entries(fromToken.valueBits.toBigInt))
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case r @ DFEnum(enumType) if (this.enumType == enumType) =>
    }
    override def toString: String = s"DFEnum[$enumType]"
    def codeString(implicit printer: CSPrinter) : String = s"${printer.config.TP}DFEnum(${enumType.refCodeString})"
    override def equals(obj: Any): Boolean = obj match {
      case Type(enumType) => this.enumType == enumType
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[E <: EnumType](implicit ctx : DFAny.Context, v : ValueOf[E]) : DFAny.NewVar[Type[E]] =
    DFAny.NewVar(Type(valueOf[E]))
  def apply[E <: EnumType](enumType : E)(implicit ctx : DFAny.Context) : DFAny.NewVar[Type[E]] =
    DFAny.NewVar(Type(enumType))
  def unapply(arg: DFAny.Member): Option[EnumType] = arg.dfType match {
    case Type(enumType) => Some(enumType)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  final case class Token(enumType : EnumType, value : Option[EnumType.Entry]) extends DFAny.Token.Of[Type[EnumType], EnumType.Entry] {
    val width : Int = enumType.width.getValue
    def valueToBitVector(value : EnumType.Entry) : BitVector = value.value.toBitVector(width)
    def valueCodeString(value : EnumType.Entry)(implicit printer: CSPrinter) : String = value.codeString
  }
  object Token {
    implicit def bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token.bubble(t.enumType)
    implicit def bubbleOfDFType[E <: EnumType] : DFAny.Token.BubbleOfDFType[Type[E]] = t => Token.bubble(t.enumType)
    def bubble(enumType : EnumType) : Token = Token(enumType, None)
    def apply(enumType : EnumType, entry : EnumType.Entry) : Token = Token(enumType, Some(entry))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(set : Set[EnumType.Entry]) extends DFAny.Pattern.OfSet[Type[EnumType], EnumType.Entry, Pattern](set) {
    protected def matchCond(matchVal: DFAny.Of[Type[EnumType]], value : EnumType.Entry)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Implicits._
      matchVal === value.asInstanceOf[EnumType#Entry]
    }
  }
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFEnumPattern[E <: EnumType](val right : E#Entry) extends Able[E#Entry]
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[E <: EnumType] : Builder[Type[E]] = new Builder[Type[E]] {
        def apply[R](left: Type[E], right: Seq[Able[R]]): Pattern = {
          new Pattern(right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
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
      implicit class DFEnumToken[E <: EnumType](val right : Token) extends Able[DFEnum[E]]
      implicit class DFEnumTokenSeq[E <: EnumType](val right : Seq[Token]) extends Able[DFEnum[E]]
      implicit class DFEnumEntry[E <: EnumType, R <: E#Entry](val right : R) extends Able[DFEnum[E]]

      def toTokenSeq[E <: EnumType](enumType : EnumType, right : Seq[Able[DFEnum[E]]]) : Seq[Token] =
        right.toSeqAny.map {
          case (t : Bubble) => Token.bubble(enumType)
          case (t : EnumType.Entry) => assert(t.enumType == enumType); Token(enumType, t)
          case (t : Token) => assert(t.enumType == enumType); t
        }
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[E <: EnumType] : Builder[DFEnum[E], Token] = (left, right) =>
        Able.toTokenSeq(left.dfType.enumType, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
      final def === [E <: EnumType](right : DFEnum[E])(implicit op: `Op===`.Builder[L, DFEnum[E]]) = op(left, right)
      final def =!= [E <: EnumType](right : DFEnum[E])(implicit op: `Op=!=`.Builder[L, DFEnum[E]]) = op(left, right)
    }
    trait Implicits extends `Op:=,<>`.Implicits  {
      sealed class __DFEnumFromEntry[L <: EnumType.Entry](left : L) extends AbleOps[L](left)
      final implicit def __DFEnumFromEntry[L <: EnumType.Entry](left: L): __DFEnumFromEntry[L] = new __DFEnumFromEntry(left)
      sealed class __DFEnumFromDefaultRet[E <: EnumType](left : DFAny.DefaultRet[Type[E]])(implicit ctx : DFAny.Context) extends AbleOps[DFEnum[E]](left)
      final implicit def __DFEnumFromDefaultRet[E <: EnumType](left : DFAny.DefaultRet[Type[E]])(implicit ctx : DFAny.Context) : __DFEnumFromDefaultRet[E] = new __DFEnumFromDefaultRet(left)
      final implicit def __ofDFEnum[E <: EnumType](left : DFEnum[E]) : Able[DFEnum[E]] = new Able(left)
      final implicit class __DFEnumOps[E <: EnumType](val left : DFEnum[E]){
        def === [R](right : Exact[R])(implicit op: `Op===`.Builder[DFEnum[E], R]) = op(left, right)
        def =!= [R](right : Exact[R])(implicit op: `Op=!=`.Builder[DFEnum[E], R]) = op(left, right)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    import DFAny.`Op:=,<>`.Builder
    trait Implicits {
      final implicit def __DFEnum_ac_DFEnum[E <: EnumType](implicit ctx : DFAny.Context)
      : Builder[Type[E], DFEnum[E]] = (left, right) => right

      final implicit def __DFEnum_ac_Entry[E <: EnumType, Entry <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[Type[E], Entry] =
        (left, rightEntry) => DFAny.Const[Type[E]](Type(left.enumType), Token(left.enumType, rightEntry))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: DFAny.Func2.Op](op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[E <: EnumType, L, R](properLR : (L, R) => (DFEnum[E], DFEnum[E]))(implicit ctx : DFAny.Context)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val func : (left.TToken, right.TToken) => DFBool.Token = op match {
          case _ : DFAny.Func2.Op.== => _ == _
          case _ : DFAny.Func2.Op.!= => _ != _
        }
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFEnum_op_DFEnum[E <: EnumType](implicit ctx : DFAny.Context)
      : Builder[DFEnum[E], DFEnum[E]] = create[E, DFEnum[E], DFEnum[E]]((left, right) => (left, right))

      implicit def evDFEnum_op_Entry[E <: EnumType, R <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[DFEnum[E], R] = create[E, DFEnum[E], R]((left, rightEntry) =>
        (left, DFAny.Const[Type[E]](Type(left.dfType.enumType), Token(left.dfType.enumType, rightEntry))))

      implicit def evEntry_op_DFEnum[E <: EnumType, L <: E#Entry](implicit ctx : DFAny.Context)
      : Builder[L, DFEnum[E]] = create[E, L, DFEnum[E]]((leftEntry, right) =>
        (DFAny.Const[Type[E]](Type(right.dfType.enumType), Token(right.dfType.enumType, leftEntry)), right))
    }
  }
  object `Op==` extends OpsCompare(DFAny.Func2.Op.==) with `Op==`
  object `Op!=` extends OpsCompare(DFAny.Func2.Op.!=) with `Op!=`
  object `Op===` extends OpsCompare(DFAny.Func2.Op.==)
  object `Op=!=` extends OpsCompare(DFAny.Func2.Op.!=)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class EnumType(implicit meta : Meta) {
  private[DFiant] var entries : immutable.ListMap[BigInt, EnumType.Entry] = immutable.ListMap.empty[BigInt, EnumType.Entry]
  private[DFiant] def update(entry : EnumType.Entry) : Unit = {
    entries.get(entry.value) match {
      case Some(existingEntry) => throw new IllegalArgumentException(s"\nDuplicate enum entry values. Attempted to create new entry `$entry` with the value ${entry.value}. The value is already taken by entry `$existingEntry`.")
      case None => entries = entries + (entry.value -> entry)
    }
  }
  final protected implicit val EnumTypeToBe : EnumType = this
  type Entry <: EnumType.Entry
  type EntryWidth
  val width : TwoFace.Int[EntryWidth]
  final def name(implicit getSet: MemberGetSet) : String = getSet.getGlobalTag[EnumType.NameTag](this) match {
    case Some(EnumType.NameTag(taggedName)) => taggedName
    case _ => meta.name
  }
  def codeString(implicit printer: CSPrinter) : String
  def refCodeString(implicit printer: CSPrinter) : String = name
  override def toString: String = meta.name
}

object EnumType {
  final case class NameTag(name : String) extends DFMember.CustomTagOf[DFMember]
  class Context(val container : Option[DFOwner.Container], val meta : Meta)
  trait LowPriority {
    implicit def topCtx(implicit lp : shapeless.LowPriority, meta : Meta) : Context = new Context(None, meta)
  }
  object Context extends LowPriority {
    implicit def fromCtx(implicit ctx : DFMember.Context, meta : Meta) : Context = new Context(Some(ctx.container), meta)
  }

  sealed trait Entry {
    val value : BigInt
    val enumType : EnumType
    private[DFiant] val meta : Meta
    val name : String = meta.name
    final def getFullName(implicit getSet: MemberGetSet) = s"${enumType.name}.$name"
    def codeString(implicit printer: CSPrinter) : String = getFullName
    final override def toString: String = name
  }
  object Entry {
    implicit def csoEnum[E <: Entry] : CodeStringOf[E] = new CodeStringOf[E] {
      override def apply(t : E)(implicit printer: CSPrinter) : String = t.codeString
    }
  }

  trait Encoding extends HasCodeString {
    def calcWidth(entryCount : Int) : Int
    val func : Int => BigInt
    def codeString(implicit printer: CSPrinter) : String
  }
  object Encoding {
    object Default extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth
      val func : Int => BigInt = t => BigInt(t)
      def codeString(implicit printer: CSPrinter) : String = "Enum.Encoding.Default"
    }
    object Grey extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth
      val func : Int => BigInt = t => BigInt(t ^ (t >>> 1))
      def codeString(implicit printer: CSPrinter) : String = "Enum.Encoding.Grey"
    }
    case class StartAt[V <: Int with Singleton](value : V) extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1 + value).bitsWidth
      val func : Int => BigInt = t => BigInt(t + value)
      def codeString(implicit printer: CSPrinter) : String = s"Enum.Encoding.StartAt($value)"
    }
    object OneHot extends Encoding {
      def calcWidth(entryCount : Int) : Int = entryCount
      val func : Int => BigInt = t => BigInt(1) << t
      def codeString(implicit printer: CSPrinter) : String = "Enum.Encoding.OneHot"
    }
  }

  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(
    implicit meta : Meta
  ) extends EnumType { self =>
    type EntryWidth = Int
    final lazy val width : TwoFace.Int[EntryWidth] = encoding.calcWidth(entries.size)
    private def entriesCodeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      f"$SC val ${entries.map(e => e._2.name).mkString(",")} = $DF Entry()"
    }
    private def encodingCodeString(implicit printer: CSPrinter) : String =
      if (encoding == Encoding.Default) "" else s"(${encoding.codeString})"
    final def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"\n$SC object $name $SC extends $DF EnumType.$DF Auto$encodingCodeString {\n${entriesCodeString.delim()}\n}"
    }

    class Entry private[DFiant] (implicit val enumType : EnumType, private[DFiant] val meta : Meta) extends EnumType.Entry {
      val value : BigInt = encoding.func(entries.size)
      enumType.update(this)
    }
    def Entry()(implicit meta : Meta) : Entry = new Entry
  }

  abstract class Manual[Width <: Int with Singleton](val width : TwoFace.Int[Width])(
    implicit meta : Meta
  ) extends EnumType { self =>
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    private var latestEntryValue : Option[BigInt] = None
    private def entriesCodeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      val cs = implicitly[CodeStringOf[BitVector]]
      entries.map(e => f"\n  $SC final $SC val ${e._2.name} ${ALGN(0)}= $DF Entry(${cs(e._1.toBitVector(width))})").mkString
    }
    final def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"\n$SC object $name $SC extends $DF EnumType.$DF Manual($width) {$entriesCodeString\n}"
    }

    class Entry private[DFiant] (val value : BigInt, val enumType : EnumType)(implicit private[DFiant] val meta : Meta) extends EnumType.Entry {
      enumType.update(this)
      latestEntryValue = Some(value)
    }

    def Entry[T <: Int with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t, enumOwner)

    def Entry[T <: Long with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t, enumOwner)

    def Entry(t : BigInt)(
      implicit enumOwner : EnumType, meta : Meta
    ) : Entry = {
      require(t.bitsWidth <= width, s"`${meta.name}` entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
      new Entry(t, enumOwner)
    }

    private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
    def Entry[W](t : DFBits.TokenW[W])(
      implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : EnumType, meta : Meta
    ) : Entry = new Entry(t.valueBits.toBigInt, enumOwner)

    def Entry(t : DFBits.Token)(
      implicit enumOwner : EnumType, meta : Meta
    ) : Entry = {
      require(t.width == width.getValue, s"`${meta.name}` entry value width (${t.width}) is different than the enumeration width ($width)")
      new Entry(t.valueBits.toBigInt, enumOwner)
    }

    def EntryDelta(t : BigInt = BigInt(1))(implicit meta : Meta) : Entry = Entry(latestEntryValue match {
      case Some(value) => value + 1
      case None => BigInt(0)
    })
    def EntryIncLastBy(t : BitVector) : Entry = EntryDelta(t.toBigInt)
  }
}
