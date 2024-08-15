package dfhdl.core
import dfhdl.internals.*
import dfhdl.compiler.ir

export DFOwner.asFE
export DFNet.asFE
export DFType.asFE
export RTDomainCfg.asFE
export Time.asFE
export Freq.asFE
export Rate.asFE

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived
