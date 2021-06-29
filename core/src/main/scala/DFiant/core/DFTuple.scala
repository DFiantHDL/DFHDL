package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*

opaque type DFTuple[T] <: DFType.Of[ir.DFTuple] = DFType.Of[ir.DFTuple]
extension [T](dfType: DFTuple[T])
  def fieldList: List[DFType] = dfType.asIR.fieldList.asInstanceOf[List[DFType]]
object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val fieldList: List[ir.DFType] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        //TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(x => DFType(x).asIR)
    ir.DFTuple(fieldList).asInstanceOf[DFTuple[T]]

  opaque type Token[T] <: DFToken.Of[DFTuple[T], List[DFToken]] =
    DFToken.Of[DFTuple[T], List[DFToken]]
  extension [T](token: Token[T])
    def data: List[DFToken] =
      token.asIR.data.asInstanceOf[List[DFToken]]
  object Token:
    protected[core] def apply[T](
        dfType: DFTuple[T],
        data: List[DFToken]
    ): Token[T] =
      ir.DFToken(dfType.asIR, data).asInstanceOf[Token[T]]

    trait Creator[T, V <: NonEmptyTuple]:
      def apply(fieldList: List[DFType], tokenTupleValue: V): List[DFToken]
    object Creator:
      inline given [T, V <: NonEmptyTuple]: Creator[T, V] = ${
        createMacro[T, V]
      }

      def createMacro[T, V <: NonEmptyTuple](using
          Quotes,
          Type[T],
          Type[V]
      ): Expr[Creator[T, V]] =
        import quotes.reflect.*
        val tTpe = TypeRepr.of[T]
        val vTpe = TypeRepr.of[V]
        println(tTpe.show)
        println(vTpe.show)
        '{
          new Creator[T, V]:
            def apply(
                fieldList: List[DFType],
                tokenTupleValue: V
            ): List[DFToken] =
              ???
        }
