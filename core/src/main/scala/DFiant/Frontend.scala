package DFiant

export core.DFType.Ops.*
export core.DFUnion.Ops.*
export core.DFOpaque.Ops.*
export core.DFVector.Ops.*
export core.SameBitsVector.*
export internals.CommonOps.*

type DFC = core.DFC
export core.dfc

type DFType = core.DFType
val DFBool = core.DFBool
type DFBool = core.DFBool
val DFBit = core.DFBit
type DFBit = core.DFBit
type DFBits[W <: Int] = core.DFBits[W]
type DFUInt[W <: Int] = core.DFUInt[W]
val DFUInt = core.DFUInt
type DFSInt[W <: Int] = core.DFSInt[W]
val DFSInt = core.DFSInt
val DFBits = core.DFBits
type DFFields = core.DFFields
type DFEncoding = core.DFEncoding
val DFEncoding = core.DFEncoding
