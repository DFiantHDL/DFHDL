package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

opaque type DFVector[T <: DFType, D <: NonEmptyTuple] <: ir.DFVector =
  ir.DFVector

object DFVector:
  def apply[T <: DFType, D <: NonEmptyTuple](
      cellType: T,
      cellDims: D
  ): DFVector[T, D] =
    ir.DFVector(cellType, cellDims.toList.asInstanceOf[List[Int]])
