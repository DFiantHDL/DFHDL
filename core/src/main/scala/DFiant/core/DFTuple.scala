package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFTuple[T] <: ir.DFTuple = ir.DFTuple
object DFTuple:
  def apply[T <: AnyRef](t: T): DFTuple[T] =
    val fieldList: List[DFType] =
      t.asInstanceOf[NonEmptyTuple]
        .toList
        //TODO: Hack due to https://github.com/lampepfl/dotty/issues/12721
        .asInstanceOf[List[AnyRef]]
        .map(DFType.apply)
    ir.DFTuple(fieldList)
