package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

export DFOwner.asFE
export DFNet.asFE
extension (dfVal: ir.DFVal)
  def asFE[T <: DFType, M <: ir.DFVal.Modifier]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]

export DFType.asFE
