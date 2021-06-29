package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFTuple[T] <: DFType.Of[ir.DFTuple] = DFType.Of[ir.DFTuple]
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
