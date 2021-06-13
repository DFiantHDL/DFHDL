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
