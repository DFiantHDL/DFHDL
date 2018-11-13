package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._
import DFiant.internals._

import scala.collection.mutable.LinkedHashMap


trait DFStruct[E <: DFFields] extends DFStruct.Unbounded {
  type TDFFields = E
}
object DFStruct extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFStruct.type] {
    type TDFFields <: DFFields
    type TEntry = TDFFields#Entry
    type Width = TDFFields#EntryWidth
    type TVal = DFStruct[TDFFields]
    type TVar = DFStruct.Var[TDFFields]
    type TToken = DFStruct.Token[TDFFields]
    type TPattern = DFStruct.Pattern[TDFFields]
    type TPatternAble[+R] = DFStruct.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFStruct.Pattern.Builder[L]
    implicit val enum : TDFFields
    def == [E <: TEntry](right : E)(implicit op: `Op==`.Builder[TVal, E]) = op(left, right)
    def != [E <: TEntry](right : E)(implicit op: `Op!=`.Builder[TVal, E]) = op(left, right)
    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar[TDFFields](enum), dir)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[E <: DFFields] extends DFStruct[E] with DFAny.Var {
    final def := [R](right: Op.Able[R])(
      implicit dir : MustBeOut, op: `Op:=`.Builder[TVal, R], ctx : DFAny.Op.Context
    ) = assign(op(left, right))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[E <: DFFields](implicit ctx : DFAny.NewVar.Context, e : E) : NewVar[E] = new NewVar[E](e)
  def apply[E <: DFFields](e : E)(implicit ctx : DFAny.NewVar.Context) : NewVar[E] = new NewVar[E](e)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[E <: DFFields](val enum : E)(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFStruct[E]](enum.width, s"DFStruct(${enum.name})") with Var[E]  {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object selectdf extends ConditionalBlock.SelectWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
  }

  protected[DFiant] final class Alias[E <: DFFields](aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
    implicit val enum : E, ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFStruct[E]](aliasedVars, reference) with Var[E]

  protected[DFiant] final class Const[E <: DFFields](enum_ : E, token : Token[E])(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFStruct[E] {val enum = enum_  }

  protected[DFiant] final class Port[E <: DFFields, Dir <: DFDir](val dfVar : DFStruct[E], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFStruct[E], Dir](dfVar, dir) with DFStruct[E] {val enum = dfVar.enum}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token[E <: DFFields] private[DFiant](width : Int, value : E#Entry) extends DFAny.Token.Of[E#Entry, Pattern[E]](width, value) {
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
    def == [E <: DFFields](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != [E <: DFFields](left : Seq[Token[E]], right : Seq[Token[E]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply[E <: DFFields](width : Int, value : Bubble) : Token[E] = new Token[E](width, null.asInstanceOf[E#Entry])
    def apply[E <: DFFields](width : Int, value : E#Entry) : Token[E] = new Token[E](width, value)
    implicit def bubbleOf[E <: DFFields] : DFStruct[E] => Token[E] = t => Token(t.width, Bubble)
    implicit def fromBits[E <: DFFields](implicit e : E) : DFBits.Token => Token[E] =
      t => Token[E](e.width, e.entries(t.valueBits.toBigInt).asInstanceOf[E#Entry])
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[E <: DFFields, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFStruct[E], Dir] = (right, dir) => new Port[E, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.TDFFields](List(left), DFAny.Alias.Reference.AsIs(s".as(DFStruct(${mold.enum}))"))(mold.enum, ctx)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFStructBubble[E <: DFFields](val right : Bubble) extends Able[DFStruct[E]]
      implicit class DFStructToken[E <: DFFields](val right : Token[E]) extends Able[DFStruct[E]]
      implicit class DFStructTokenSeq[E <: DFFields](val right : Seq[Token[E]]) extends Able[DFStruct[E]]
      implicit class DFStructEntry[E <: DFFields, R <: E#Entry](val right : R) extends Able[DFStruct[E]]

      def toTokenSeq[E <: DFFields](width : Int, right : Seq[Able[DFStruct[E]]]) : Seq[Token[E]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[E](width, t)
          case (t : DFFields.Entry) => Token[E](width, t.asInstanceOf[E#Entry])
          case (t : Token[_]) => t.asInstanceOf[Token[E]]
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[E <: DFFields] : Builder[DFStruct[E], Token[E]] = (left, right) =>
        Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern[E <: DFFields](set : Set[E#Entry]) extends DFAny.Pattern.OfSet[E#Entry, Pattern[E]](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFStructPattern[E <: DFFields](val right : E#Entry) extends Able[E#Entry]
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[E <: DFFields] : Builder[DFStruct[E]] = new Builder[DFStruct[E]] {
        def apply[R](left: DFStruct[E], right: Seq[Able[R]]): Pattern[E] = {

          new Pattern[E](right.map(e => e.right.asInstanceOf[E#Entry]).toSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev extends PrevCO {
    trait Builder[L <: DFAny] extends DFAny.Prev.Builder[L]
    object Builder {
      implicit def ev[E <: DFFields](implicit ctx : DFAny.Alias.Context) : Builder[DFStruct[E]] = new Builder[DFStruct[E]] {
        def apply[P](left : DFStruct[E], right : Natural.Int.Checked[P]) : DFStruct[E] =
          new Alias(List(left), DFAny.Alias.Reference.Prev(right))(left.enum, ctx)
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
      def <> [E <: DFFields, RDIR <: DFDir](port : DFStruct[E] <> RDIR)(
        implicit op: `Op<>`.Builder[DFStruct[E], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFStructFromEntry[L <: DFFields.Entry](left : L) extends Able[L](left)
      final implicit def DFStructFromEntry[L <: DFFields.Entry](left: L): DFStructFromEntry[L] = new DFStructFromEntry(left)
      final implicit def ofDFStruct[R <: DFStruct.Unbounded](value : R) : Able[value.TVal] = new Able[value.TVal](value.left)
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

      def create[E <: DFFields, L, R](properR : (L, R) => DFStruct[E]) : Aux[L, R, DFStruct[E]] =
        new Builder[L, R] {
          type Comp = DFStruct[E]
          def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
        }

      implicit def evDFStruct_op_DFStruct[E <: DFFields](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[E], DFStruct[E], DFStruct[E]] =
        create[E, DFStruct[E], DFStruct[E]]((left, right) => right)

      implicit def evDFStruct_op_Entry[E <: DFFields, Entry <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[E], Entry, DFStruct[E]] =
        create[E, DFStruct[E], Entry]((left, rightEntry) => new Const(left.enum, Token[E](left.width, rightEntry)))
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
      def create[E <: DFFields, L, R](properLR : (L, R) => (DFStruct[E], DFStruct[E]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val leftBits = left.bits
        val rightBits = right.bits

        val result : DFBool = kind match {
          case DiSoOp.Kind.== => leftBits == rightBits
          case DiSoOp.Kind.!= => leftBits != rightBits
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }

        result.setAutoName(ctx.getName)
      }

      implicit def evDFStruct_op_DFStruct[E <: DFFields](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[E], DFStruct[E]] = create[E, DFStruct[E], DFStruct[E]]((left, right) => (left, right))

      implicit def evDFStruct_op_Entry[E <: DFFields, R <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[E], R] = create[E, DFStruct[E], R]((left, rightEntry) => (left, new Const(left.enum, Token[E](left.width, rightEntry))))

      implicit def evEntry_op_DFStruct[E <: DFFields, L <: E#Entry](implicit ctx : DFAny.Op.Context)
      : Builder[L, DFStruct[E]] = create[E, L, DFStruct[E]]((leftEntry, right) => (new Const(right.enum, Token[E](right.width, leftEntry)), right))
    }
  }

  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



sealed abstract class DFFields(implicit n : NameIt) extends HasCodeString {
  private[DFiant] val entries : LinkedHashMap[BigInt, DFFields.Entry] = LinkedHashMap.empty[BigInt, DFFields.Entry]
  private[DFiant] def update(entry : DFFields.Entry) : Unit = {
    entries.get(entry.value) match {
      case Some(existingEntry) => throw new IllegalArgumentException(s"\nDuplicate enum entry values. Attempted to create new entry `$entry` with the value ${entry.value}. The value is already taken by entry `$existingEntry`.")
      case None => entries.update(entry.value, entry)
    }
  }
  final protected implicit val DFFieldsOwnerToBe : DFFields = this
  type Entry <: DFFields.Entry
  type EntryWidth
  val width : TwoFace.Int[EntryWidth]
  final val name : String = n.value
  override def toString: String = name
}

object DFFields {
  sealed trait Entry extends HasCodeString {
    val value : BigInt
    val enumOwner : DFFields
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
      def codeString : String = "DFFields.Encoding.Default"
    }
    object Grey extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1).bitsWidth
      val func : Int => BigInt = t => BigInt(t ^ (t >>> 1))
      def codeString : String = "DFFields.Encoding.Grey"
    }
    case class StartAt[V <: Int with Singleton](value : V) extends Encoding {
      def calcWidth(entryCount : Int) : Int = BigInt(entryCount-1 + value).bitsWidth
      val func : Int => BigInt = t => BigInt(t + value)
      def codeString : String = s"DFFields.Encoding.StartAt($value)"
    }
    object OneHot extends Encoding {
      def calcWidth(entryCount : Int) : Int = entryCount
      val func : Int => BigInt = t => BigInt(1) << t
      def codeString : String = "DFFields.Encoding.OneHot"
    }
  }

  abstract class Auto[E <: Encoding](val encoding : E = Encoding.Default)(implicit n : NameIt) extends DFFields {
    type EntryWidth = Int
    final lazy val width : TwoFace.Int[EntryWidth] = encoding.calcWidth(entries.size)
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry()  //${e._1.codeString}").mkString
    private def encodingCodeString : String = if (encoding == Encoding.Default) "" else s"(${encoding.codeString})"
    final def codeString : String = s"\nobject $name extends DFFields.Auto$encodingCodeString {$entriesCodeString\n}"

    class Entry private[DFiant] (implicit val enumOwner : DFFields, n : NameIt) extends DFFields.Entry {
      val value : BigInt = encoding.func(entries.size)
      val name : String = n.value
      enumOwner.update(this)
    }
    def Entry()(implicit n : NameIt) : Entry = new Entry
  }

  abstract class Manual[Width <: Int with Singleton](val width : TwoFace.Int[Width])(implicit n : NameIt) extends DFFields {
    type EntryWidth = Width
    private type Msg[EW] = "Entry value width (" + ToString[EW] + ") is bigger than the enumeration width (" + ToString[Width] + ")"
    private var latestEntryValue : Option[BigInt] = None
    private def entriesCodeString : String = entries.map(e => f"\n  val ${e._2.name}%-15s = Entry(${e._1.toBitVector(width).codeString})").mkString
    final def codeString : String = s"\nobject $name extends DFFields.Manual($width) {$entriesCodeString\n}"

    class Entry private[DFiant] (val value : BigInt, val enumOwner : DFFields, val name : String) extends DFFields.Entry {
      enumOwner.update(this)
      latestEntryValue = Some(value)
    }

    def Entry[T <: Int with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcInt[T] <= Width, Msg[BitsWidthOf.CalcInt[T]]], enumOwner : DFFields, n : NameIt
    ) : Entry = new Entry(t, enumOwner, n.value)

    def Entry[T <: Long with Singleton](t : T)(
      implicit check : RequireMsg[BitsWidthOf.CalcLong[T] <= Width, Msg[BitsWidthOf.CalcLong[T]]], enumOwner : DFFields, n : NameIt
    ) : Entry = new Entry(t, enumOwner, n.value)

    def Entry(t : BigInt)(
      implicit enumOwner : DFFields, n : NameIt
    ) : Entry = {
      require(t.bitsWidth <= width, s"`${n.value}` entry value width (${t.bitsWidth}) is bigger than the enumeration width ($width)")
      new Entry(t, enumOwner, n.value)
    }

    private type Msg2[EW] = "Entry value width (" + ToString[EW] + ") is different than the enumeration width (" + ToString[Width] + ")"
    def Entry[W](t : XBitVector[W])(
      implicit check : RequireMsg[W == Width, Msg2[W]], enumOwner : DFFields, n : NameIt
    ) : Entry = new Entry(t.toBigInt, enumOwner, n.value)

    def Entry(t : BitVector)(
      implicit enumOwner : DFFields, n : NameIt
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
