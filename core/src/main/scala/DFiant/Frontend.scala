package DFiant

export core.DFType.Ops.*
export core.DFType.DFUnion.Ops.*

type Context = core.Context
type DFType = core.DFType
val DFBool = core.DFType.DFBool
type DFBool = DFBool.type
val DFBit = core.DFType.DFBit
type DFBit = DFBit.type
type DFBits[W <: Int] = core.DFType.DFBits[W]
val DFBits = core.DFType.DFBits
type DFFields = core.DFType.DFFields
type DFOpaque[T <: DFType] = core.DFType.DFOpaque[T]
val DFOpaque = core.DFType.DFOpaque

extension (value: BigInt)
  def bitsWidth(signed: Boolean): Int =
    if (value > 0)
      if (signed) value.bitLength + 1 else value.bitLength
    else if (value == 0)
      if (signed) 2 else 1
    else (-value).bitLength + 1

extension (value: Int)
  def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)
