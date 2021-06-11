package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import collection.immutable.ListSet

opaque type DFUnion[U <: DFType] <: ir.DFUnion = ir.DFUnion
object DFUnion:
  def apply[U <: DFType](fieldSet: ListSet[DFType]): DFUnion[U] =
    ir.DFUnion(fieldSet)
  trait Able[T]:
    type U <: DFType
    def apply(t: T): DFUnion[U]
  object Able:
    given fromDFType[T <: DFType]: Able[T] with
      type U = T
      def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(t))
    given fromFields[T](using tc: DFType.TC[T]): Able[T] with
      type U = tc.Type
      def apply(t: T): DFUnion[U] = DFUnion[U](ListSet(tc(t)))
    given fromUnion[U0 <: DFType]: Able[DFUnion[U0]] with
      type U = U0
      def apply(t: DFUnion[U0]): DFUnion[U] = t
  object Ops:
    extension [L](lhs: L)(using l: Able[L])
      def |[R](rhs: R)(using r: Able[R])(using
          VerifyUnion[l.U, r.U]
      ): DFUnion[l.U | r.U] =
        DFUnion[l.U | r.U](l(lhs).fieldSet ++ r(rhs).fieldSet)

trait VerifyUnion[Current <: DFType, Added <: DFType]
object VerifyUnion:
  inline given [Current <: DFType, Added <: DFType]
      : VerifyUnion[Current, Added] =
    ${ verifyMacro[Current, Added] }
  def verifyMacro[Current <: DFType, Added <: DFType](using
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
    val collision =
      currentTpes.filter(c => addedTpes.exists(a => a <:< c | c <:< a))
    if (collision.nonEmpty)
      report.error(
        s"Dataflow union types must be exclusive.\nThe following types are repeated: ${collision.map(_.show).mkString(", ")}"
      )

    println(currentTpes)
    println(addedTpes)
    '{ new VerifyUnion[Current, Added] {} }
