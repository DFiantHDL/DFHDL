package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._
import DFiant.internals._

trait DFStruct[SF <: DFStruct.Fields] extends DFStruct.Unbounded {
  type TSFields = SF
}
object DFStruct extends DFAny.Companion {

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Fields of the struct
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Fields extends HasWidth with HasCodeString {
    type TFields <: DFFields
    type TMemberHList// <: shapeless.HList
    val width : TwoFace.Int[Width]
    val members : TMemberHList
    val fields : TFields
    lazy val name : String = fields.name
    override def toString: String = name
    def codeString: String = fields.codeString
  }
  object Fields {
    type Aux[Width0, Fields0, MemberHList] = Fields {
      type Width = Width0
      type TFields = Fields0
      type TMemberHList = MemberHList
    }
    implicit def fromFields[F <: DFFields](implicit something : DummyImplicit) //TODO: use shapeless here
    : F => Aux[Int, F, List[DFAny]] = dfFields => new Fields {
      type Width = Int
      type TFields = F
      type TMemberHList = List[DFAny]
      val fields : TFields = dfFields
      val width: TwoFace.Int[Width] = ??? //TwoFace.Int.create[Width](dfFields.members.map(m => m.width.getValue).sum)
      val members: TMemberHList = ??? //dfFields.members
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFStruct.type] {
    type TSFields <: Fields
    type TFields = TSFields#TFields
    type Width = TSFields#Width
    type TVal = DFStruct[TSFields]
    type TVar = DFStruct.Var[TSFields]
    type TToken = DFStruct.Token[TSFields]
    type TPattern = DFStruct.Pattern[TSFields]
    type TPatternAble[+R] = DFStruct.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFStruct.Pattern.Builder[L]
    type OpAble[R] = Op.Able[R]
    type `Op<>Builder`[R] = `Op<>`.Builder[TVal, R]
    type `Op:=Builder`[R] = `Op:=`.Builder[TVal, R]
    implicit val structFields : TSFields
    final lazy val fields : TFields = structFields.fields
    def == [SF <: Product](right : SF)(implicit op: `Op==`.Builder[TVal, SF]) = op(left, right)
    def != [SF <: Product](right : SF)(implicit op: `Op!=`.Builder[TVal, SF]) = op(left, right)
    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar[TSFields](structFields), dir)
    final protected[DFiant] def alias(aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
      implicit ctx : DFAny.Alias.Context
    ) : TAlias = new Alias(aliasedVars, reference)(ctx, structFields).asInstanceOf[TAlias]
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[SF <: Fields] extends DFStruct[SF] with DFAny.Var {
    final def := [R](right: Op.Able[R])(
      implicit dir : MustBeOut, op: `Op:=`.Builder[TVal, R], ctx : DFAny.Op.Context
    ) = assign(op(left, right))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[F <: DFFields, W, M](fields : F)(implicit ctx : DFAny.NewVar.Context, structFields : F => Fields.Aux[W, F, M])
  : NewVar[Fields.Aux[W, F, M]] = new NewVar[Fields.Aux[W, F, M]](structFields(fields))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[SF <: Fields](val structFields : SF)(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFStruct[SF]](structFields.width, s"DFStruct(${structFields.name})") with Var[SF]  {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object selectdf extends ConditionalBlock.SelectWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
  }

  protected[DFiant] final class Alias[SF <: Fields](aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context, val structFields : SF
  ) extends DFAny.Alias[DFStruct[SF]](aliasedVars, reference) with Var[SF]

  protected[DFiant] final class Const[SF <: Fields](structFields_ : SF, token : Token[SF])(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFStruct[SF] {val structFields = structFields_}

  protected[DFiant] final class Port[SF <: Fields, Dir <: DFDir](val dfVar : DFStruct[SF], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFStruct[SF], Dir](dfVar, dir) with DFStruct[SF] {val structFields = dfVar.structFields}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  implicit val codeStringOfListOfToken : CodeStringOf[List[DFAny.Token]] = list => list.mkString("(",", ",")")
  implicit val codeStringOfProduct : CodeStringOf[Product] = p => p.productIterator.mkString("(",", ",")")
  class Token[SF <: Fields] private[DFiant](width : Int, value : Product) extends DFAny.Token.Of[Product, Pattern[SF]](width, value) {
    type TToken = Token[SF]
    val (valueBits, bubbleMask) : (BitVector, BitVector) = ???
//    {
//      if (value.isEmpty) (0.toBitVector(width), true.toBitVector(width))
//      else (value.map(t => t.valueBits).reduce((l, r) => l ++ r), value.map(t => t.bubbleMask).reduce((l, r) => l ++ r))
//    }
    def toBubbleToken : Token[SF] = Token(width, Bubble)

    final def == (that : Token[SF]) : DFBool.Token =
      if (this.value != null && that.value != null) DFBool.Token(this.value == that.value)
      else DFBool.Token(Bubble)

    final def != (that : Token[SF]) : DFBool.Token = !(this == that)
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    def == [SF <: Fields](left : Seq[Token[SF]], right : Seq[Token[SF]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != [SF <: Fields](left : Seq[Token[SF]], right : Seq[Token[SF]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply[SF <: Fields](width : Int, value : Bubble) : Token[SF] = new Token[SF](width, List())
    def apply[SF <: Fields](width : Int, value : Product) : Token[SF] = {
      //TODO: Need to convert to TokenList
//      val tokenList : List[DFAny.Token] = ???
      new Token[SF](width, value)
    }
    implicit def bubbleOf[SF <: Fields] : DFStruct[SF] => Token[SF] = t => Token(t.width, Bubble)
    implicit def fromBits[SF <: Fields](implicit e : SF) : DFBits.Token => Token[SF] = ???
//      t => Token[SF](e.width, e.entries(t.valueBits.toBigInt).asInstanceOf[Product])
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[SF <: Fields, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFStruct[SF], Dir] = (right, dir) => new Port[SF, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.TSFields](List(left), DFAny.Alias.Reference.AsIs(s".as(DFStruct(${mold.structFields}))"))(ctx, mold.structFields)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFStructBubble[SF <: Fields](val right : Bubble) extends Able[DFStruct[SF]]
      implicit class DFStructToken[SF <: Fields](val right : Token[SF]) extends Able[DFStruct[SF]]
      implicit class DFStructTokenSeq[SF <: Fields](val right : Seq[Token[SF]]) extends Able[DFStruct[SF]]
      implicit class DFStructProduct[SF <: Fields, R <: Product](val right : R) extends Able[DFStruct[SF]]

      def toTokenSeq[SF <: Fields](width : Int, right : Seq[Able[DFStruct[SF]]]) : Seq[Token[SF]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[SF](width, t)
          case (t : Product) => Token[SF](width, t)
          case (t : Token[_]) => t.asInstanceOf[Token[SF]]
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[SF <: Fields] : Builder[DFStruct[SF], Token[SF]] = (left, right) =>
        Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern[SF <: Fields](set : Set[Product]) extends DFAny.Pattern.OfSet[Product, Pattern[SF]](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFStructPattern[SF <: Fields](val right : Product) extends Able[Product]
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[SF <: Fields] : Builder[DFStruct[SF]] = new Builder[DFStruct[SF]] {
        def apply[R](left: DFStruct[SF], right: Seq[Able[R]]): Pattern[SF] = {

          new Pattern[SF](right.map(e => e.right.asInstanceOf[Product]).toSet)
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
      def <> [SF <: Fields, RDIR <: DFDir](port : DFStruct[SF] <> RDIR)(
        implicit op: `Op<>`.Builder[DFStruct[SF], L], ctx : DFAny.Connector.Context
      ) = port.connectVal2Port(op(port, left))
    }
    trait Implicits {
      sealed class DFStructFromProduct[L <: Product](left : L) extends Able[L](left)
      final implicit def DFStructFromProduct[L <: Product](left: L): DFStructFromProduct[L] = new DFStructFromProduct(left)
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

      def create[SF <: Fields, L, R](properR : (L, R) => DFStruct[SF]) : Aux[L, R, DFStruct[SF]] =
        new Builder[L, R] {
          type Comp = DFStruct[SF]
          def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
        }

      implicit def evDFStruct_op_DFStruct[SF <: Fields](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[SF], DFStruct[SF], DFStruct[SF]] =
        create[SF, DFStruct[SF], DFStruct[SF]]((left, right) => right)

      implicit def evDFStruct_op_Product[SF <: Fields, P <: Product](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[SF], P, DFStruct[SF]] =
        create[SF, DFStruct[SF], P]((left, rightProduct) => new Const(left.structFields, Token[SF](left.width, rightProduct)))
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
    trait Builder[L, R] extends DFAny.Op.Builder[L, R]{type Comp = DFBool with CanBePiped}

    object Builder {
      def create[SF <: Fields, L, R](properLR : (L, R) => (DFStruct[SF], DFStruct[SF]))(implicit ctx : DFAny.Op.Context)
      : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        val leftBits = left.bits
        val rightBits = right.bits

        val result : DFBool with CanBePiped = kind match {
          case DiSoOp.Kind.== => leftBits == rightBits
          case DiSoOp.Kind.!= => leftBits != rightBits
          case _ => throw new IllegalArgumentException("Unexpected compare operation")
        }

        result.setAutoName(ctx.getName)
      }

      implicit def evDFStruct_op_DFStruct[SF <: Fields](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[SF], DFStruct[SF]] = create[SF, DFStruct[SF], DFStruct[SF]]((left, right) => (left, right))

      implicit def evDFStruct_op_Product[SF <: Fields, R <: Product](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[SF], R] = create[SF, DFStruct[SF], R]((left, rightProduct) => (left, new Const(left.structFields, Token[SF](left.width, rightProduct))))

      implicit def evProduct_op_DFStruct[SF <: Fields, L <: Product](implicit ctx : DFAny.Op.Context)
      : Builder[L, DFStruct[SF]] = create[SF, L, DFStruct[SF]]((leftProduct, right) => (new Const(right.structFields, Token[SF](right.width, leftProduct)), right))
    }
  }

  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
