package DFiant.core
import DFiant.compiler.ir

export DFOwner.asFE
export DFNet.asFE
extension (dfVal: ir.DFVal)
  def asFE[T <: DFType, M <: DFVal.Modifier]: DFVal[T, M] =
    dfVal.asInstanceOf[DFVal[T, M]]

export DFType.asFE
