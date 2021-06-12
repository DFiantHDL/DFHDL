package DFiant

export core.DFType.Ops.*
export core.DFUnion.Ops.*
export core.DFOpaque.Ops.*
export core.DFVector.Ops.*
export core.SameBitsVector.*
export internals.CommonOps.*

type Context = core.Context
type DFType = core.DFType
val DFBool = core.DFBool
type DFBool = core.DFBool
val DFBit = core.DFBit
type DFBit = core.DFBit
type DFBits[W <: Int] = core.DFBits[W]
val DFBits = core.DFBits
type DFFields = core.DFFields
type DFEncoding = core.DFEncoding
val DFEncoding = core.DFEncoding
