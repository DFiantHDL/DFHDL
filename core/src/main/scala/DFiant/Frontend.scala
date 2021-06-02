package DFiant

type Context = core.Context
type DFType = core.DFType
val DFBool = core.DFType.DFBool
type DFBool = DFBool.type
val DFBit = core.DFType.DFBit
type DFBit = DFBit.type
type DFBits[W <: Int] = core.DFType.DFBits[W]
val DFBits = core.DFType.DFBits
type DFStruct = core.DFType.DFStruct
type DFOpaque[T <: DFType] = core.DFType.DFOpaque[T]
val DFOpaque = core.DFType.DFOpaque
