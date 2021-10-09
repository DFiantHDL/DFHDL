package DFiant.core
import DFiant.internals.*
import DFiant.compiler.ir

export DFOwner.asFE
export DFNet.asFE
export DFType.asFE

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived
