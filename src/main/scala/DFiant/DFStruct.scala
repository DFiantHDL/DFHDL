package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.BasicLib._
import DFiant.internals._

trait DFStruct[F <: DFFields] extends DFStruct.Unbounded {
  type TFields = F
}
object DFStruct extends DFAny.Companion {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Unbounded Val
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded extends DFAny.Unbounded[DFStruct.type] {
    type TFields <: DFFields
    type Width = Int
    type TVal = DFStruct[TFields]
    type TVar = DFStruct.Var[TFields]
    type TToken = DFStruct.Token[TFields]
    type TPattern = DFStruct.Pattern[TFields]
    type TPatternAble[+R] = DFStruct.Pattern.Able[R]
    type TPatternBuilder[L <: DFAny] = DFStruct.Pattern.Builder[L]
    implicit val fields : TFields
    def == [F <: Product](right : F)(implicit op: `Op==`.Builder[TVal, F]) = op(left, right)
    def != [F <: Product](right : F)(implicit op: `Op!=`.Builder[TVal, F]) = op(left, right)
    protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context)
    : TVal <> Dir = new Port(new NewVar[TFields](fields), dir)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Var
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Var[F <: DFFields] extends DFStruct[F] with DFAny.Var {
    final def := [R](right: Op.Able[R])(
      implicit dir : MustBeOut, op: `Op:=`.Builder[TVal, R], ctx : DFAny.Op.Context
    ) = assign(op(left, right))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[F <: DFFields](implicit ctx : DFAny.NewVar.Context, e : F) : NewVar[F] = new NewVar[F](e)
  def apply[F <: DFFields](e : F)(implicit ctx : DFAny.NewVar.Context) : NewVar[F] = new NewVar[F](e)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Protected Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] final class NewVar[F <: DFFields](val fields : F)(
    implicit ctx : DFAny.NewVar.Context
  ) extends DFAny.NewVar[DFStruct[F]](fields.members.map(f => f.width.getValue).sum, s"DFStruct(${fields.name})") with Var[F]  {
    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : Port.Builder[TVal, Dir]) : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
    final object selectdf extends ConditionalBlock.SelectWithRetVal[TVal, Op.Able, `Op:=`.Builder](this)
  }

  protected[DFiant] final class Alias[F <: DFFields](aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
    implicit val fields : F, ctx : DFAny.Alias.Context
  ) extends DFAny.Alias[DFStruct[F]](aliasedVars, reference) with Var[F]

  protected[DFiant] final class Const[F <: DFFields](fields_ : F, token : Token[F])(
    implicit ctx : DFAny.Const.Context
  ) extends DFAny.Const(token) with DFStruct[F] {val fields = fields_  }

  protected[DFiant] final class Port[F <: DFFields, Dir <: DFDir](val dfVar : DFStruct[F], dir : Dir)(
    implicit ctx : DFAny.Port.Context
  ) extends DFAny.Port[DFStruct[F], Dir](dfVar, dir) with DFStruct[F] {val fields = dfVar.fields}
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Token[F <: DFFields] private[DFiant](width : Int, value : List[DFAny.Token]) extends DFAny.Token.Of[List[DFAny.Token], Pattern[F]](width, value) {
    val (valueBits, bubbleMask) : (BitVector, BitVector) = ???
//      if (value != null) (value.value.toBitVector(width), false.toBitVector(width))
//      else (0.toBitVector(width), true.toBitVector(width))
//
    def toBubbleToken : Token[F] = Token(width, Bubble)

    final def == (that : Token[F]) : DFBool.Token =
      if (this.value != null && that.value != null) DFBool.Token(this.value == that.value)
      else DFBool.Token(Bubble)

    final def != (that : Token[F]) : DFBool.Token = !(this == that)
  }

  object Token extends TokenCO {
    import DFAny.TokenSeq
    def == [F <: DFFields](left : Seq[Token[F]], right : Seq[Token[F]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l == r)
    def != [F <: DFFields](left : Seq[Token[F]], right : Seq[Token[F]]) : Seq[DFBool.Token] = TokenSeq(left, right)((l, r) => l != r)

    def apply[F <: DFFields](width : Int, value : Bubble) : Token[F] = new Token[F](width, List())
    def apply[F <: DFFields](width : Int, value : List[DFAny]) : Token[F] = new Token[F](width, value)
    implicit def bubbleOf[F <: DFFields] : DFStruct[F] => Token[F] = t => Token(t.width, Bubble)
    implicit def fromBits[F <: DFFields](implicit e : F) : DFBits.Token => Token[F] = ???
//      t => Token[F](e.width, e.entries(t.valueBits.toBigInt).asInstanceOf[Product])
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Port extends PortCO {
    trait Builder[L <: DFAny, Dir <: DFDir] extends DFAny.Port.Builder[L, Dir]
    object Builder {
      implicit def conn[F <: DFFields, Dir <: DFDir](implicit ctx : DFAny.Port.Context)
      : Builder[DFStruct[F], Dir] = (right, dir) => new Port[F, Dir](right, dir)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Alias
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Alias extends AliasCO {
    def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny =
      new Alias[mold.TFields](List(left), DFAny.Alias.Reference.AsIs(s".as(DFStruct(${mold.fields}))"))(mold.fields, ctx)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init extends InitCO {
    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
    object Able {
      implicit class DFStructBubble[F <: DFFields](val right : Bubble) extends Able[DFStruct[F]]
      implicit class DFStructToken[F <: DFFields](val right : Token[F]) extends Able[DFStruct[F]]
      implicit class DFStructTokenSeq[F <: DFFields](val right : Seq[Token[F]]) extends Able[DFStruct[F]]
      implicit class DFStructProduct[F <: DFFields, R <: Product](val right : R) extends Able[DFStruct[F]]

      def toTokenSeq[F <: DFFields](width : Int, right : Seq[Able[DFStruct[F]]]) : Seq[Token[F]] =
        right.toSeqAny.map(e => e match {
          case (t : Bubble) => Token[F](width, t)
          case (t : Product) => Token[F](width, t.productIterator.toList.asInstanceOf[List[DFAny]])
          case (t : Token[_]) => t.asInstanceOf[Token[F]]
        })
    }
    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
    object Builder {
      implicit def ev[F <: DFFields] : Builder[DFStruct[F], Token[F]] = (left, right) =>
        Able.toTokenSeq(left.width, right)
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern[F <: DFFields](set : Set[Product]) extends DFAny.Pattern.OfSet[Product, Pattern[F]](set)
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R]
    object Able {
      implicit class DFStructPattern[F <: DFFields](val right : Product) extends Able[Product]
    }
    trait Builder[L <: DFAny] extends DFAny.Pattern.Builder[L, Able]
    object Builder {
      implicit def ev[F <: DFFields] : Builder[DFStruct[F]] = new Builder[DFStruct[F]] {
        def apply[R](left: DFStruct[F], right: Seq[Able[R]]): Pattern[F] = {

          new Pattern[F](right.map(e => e.right.asInstanceOf[Product]).toSet)
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
      implicit def ev[F <: DFFields](implicit ctx : DFAny.Alias.Context) : Builder[DFStruct[F]] = new Builder[DFStruct[F]] {
        def apply[P](left : DFStruct[F], right : Natural.Int.Checked[P]) : DFStruct[F] =
          new Alias(List(left), DFAny.Alias.Reference.Prev(right))(left.fields, ctx)
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
      def <> [F <: DFFields, RDIR <: DFDir](port : DFStruct[F] <> RDIR)(
        implicit op: `Op<>`.Builder[DFStruct[F], L], ctx : DFAny.Connector.Context
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

      def create[F <: DFFields, L, R](properR : (L, R) => DFStruct[F]) : Aux[L, R, DFStruct[F]] =
        new Builder[L, R] {
          type Comp = DFStruct[F]
          def apply(leftL : L, rightR : R) : Comp = properR(leftL, rightR)
        }

      implicit def evDFStruct_op_DFStruct[F <: DFFields](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[F], DFStruct[F], DFStruct[F]] =
        create[F, DFStruct[F], DFStruct[F]]((left, right) => right)

      implicit def evDFStruct_op_Product[F <: DFFields, P <: Product](implicit ctx : DFAny.Op.Context)
      : Aux[DFStruct[F], P, DFStruct[F]] =
        create[F, DFStruct[F], P]((left, rightProduct) => new Const(left.fields, Token[F](left.width, rightProduct)))
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
      def create[F <: DFFields, L, R](properLR : (L, R) => (DFStruct[F], DFStruct[F]))(implicit ctx : DFAny.Op.Context)
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

      implicit def evDFStruct_op_DFStruct[F <: DFFields](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[F], DFStruct[F]] = create[F, DFStruct[F], DFStruct[F]]((left, right) => (left, right))

      implicit def evDFStruct_op_Product[F <: DFFields, R <: Product](implicit ctx : DFAny.Op.Context)
      : Builder[DFStruct[F], R] = create[F, DFStruct[F], R]((left, rightProduct) => (left, new Const(left.fields, Token[F](left.width, rightProduct))))

      implicit def evProduct_op_DFStruct[F <: DFFields, L <: Product](implicit ctx : DFAny.Op.Context)
      : Builder[L, DFStruct[F]] = create[F, L, DFStruct[F]]((leftProduct, right) => (new Const(right.fields, Token[F](right.width, leftProduct)), right))
    }
  }

  object `Op==` extends OpsCompare(DiSoOp.Kind.==) with `Op==`
  object `Op!=` extends OpsCompare(DiSoOp.Kind.!=) with `Op!=`
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



abstract class DFFields(implicit n : NameIt) extends HasCodeString {
  final protected implicit val emptyDesign : DFDesign = new DFDesign() {}
  final lazy val members : List[DFAny] = emptyDesign.memberList.collect{case x : DFAny => x}
  final val name : String = n.value
  override def toString: String = name
  private val fieldsCodeString : String = emptyDesign.bodyCodeString
  final def codeString : String = s"\nobject $name extends DFFields {$fieldsCodeString\n}"
}

