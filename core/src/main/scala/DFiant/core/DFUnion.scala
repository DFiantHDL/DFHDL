package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import collection.immutable.ListSet
import DFiant.compiler.printing.DefaultPrinter

type DFUnion[U <: DFTypeAny] = DFType[ir.DFUnion, Args1[U]]
object DFUnion:
  def apply[U <: DFTypeAny](fieldSet: ListSet[ir.DFType]): DFUnion[U] =
    ir.DFUnion(fieldSet).asFE[DFUnion[U]]

  private def collisionError(collisions: List[String]): String =
    s"Dataflow union types must be exclusive.\nThe following types are repeated: ${collisions.mkString(", ")}"
  private def widthError(lhsWidth: Int, rhsWidth: Int): String =
    s"All union types must have the same width.\nFound LHS-width $lhsWidth and RHS-width $rhsWidth"
  trait Able[T]:
    type U <: DFTypeAny
    def apply(t: T): DFUnion[U]
  object Able:
    given fromDFType[T <: DFTypeAny]: Able[T] with
      type U = T
      def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(t.asIR))
    transparent inline given fromFields[T](using tc: DFType.TC[T]): Able[T] =
      new Able[T]:
        type U = tc.Type
        def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(tc(t).asIR))
    given fromUnion[U0 <: DFTypeAny]: Able[DFUnion[U0]] with
      type U = U0
      def apply(t: DFUnion[U0]): DFUnion[U] = t
  object Ops:
    extension [L](lhs: L)(using l: Able[L])
      def |[R](rhs: R)(using r: Able[R])(using
          VerifyUnion[l.U, r.U]
      ): DFUnion[l.U | r.U] =
        val lhsUnion = l(lhs).asIR
        val rhsUnion = r(rhs).asIR
        val collisions = lhsUnion.fieldSet & rhsUnion.fieldSet
        if (collisions.nonEmpty)
          throw new IllegalArgumentException(
            collisionError(
              collisions.toList.map(DefaultPrinter.csDFType)
            )
          )
        assert(
          lhsUnion.width == rhsUnion.width,
          widthError(lhsUnion.width, rhsUnion.width)
        )
        DFUnion[l.U | r.U](lhsUnion.fieldSet ++ rhsUnion.fieldSet)
  end Ops
  trait VerifyUnion[Current <: DFTypeAny, Added <: DFTypeAny]
  object VerifyUnion:
    inline given [Current <: DFTypeAny, Added <: DFTypeAny]
        : VerifyUnion[Current, Added] =
      ${ verifyMacro[Current, Added] }
    def verifyMacro[Current <: DFTypeAny, Added <: DFTypeAny](using
        Quotes,
        Type[Current],
        Type[Added]
    ): Expr[VerifyUnion[Current, Added]] =
      import quotes.reflect.*
      //flattens all the OrType into a list
      def flattenOr(tpe: TypeRepr): List[TypeRepr] =
        tpe.dealias match
          case OrType(left, right) => flattenOr(left) ++ flattenOr(right)
          case t                   => List(t)
      val currentTpes = flattenOr(TypeRepr.of[Current])
      val addedTpes = flattenOr(TypeRepr.of[Added])
      //checking for collisions
      val collisions =
        currentTpes.filter(c => addedTpes.exists(a => a <:< c | c <:< a))
      if (collisions.nonEmpty)
        report.error(collisionError(collisions.map(_.toString)))
      import Width.*
      //checking widths match
      val currentWidth = currentTpes.head.calcWidth
      val addedWidth = addedTpes.head.calcWidth
      (currentWidth, addedWidth) match
        case (ConstantType(IntConstant(c)), ConstantType(IntConstant(a)))
            if a != c =>
          report.error(widthError(c, a))
        //either unknown at compile-time or are matching
        case _ =>

      '{ new VerifyUnion[Current, Added] {} }
    end verifyMacro
  end VerifyUnion
end DFUnion
