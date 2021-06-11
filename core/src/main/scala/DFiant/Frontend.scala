package DFiant

export core.DFType.Ops.*
export core.SameBitsVector.*

type Context = core.Context
type DFType = core.DFType
val DFBool = core.DFBool
type DFBool = core.DFBool
val DFBit = core.DFBit
type DFBit = core.DFBit
type DFBits[W <: Int] = core.DFBits[W]
val DFBits = core.DFBits
type DFFields = core.DFFields
//type DFOpaque[T <: DFType] = core.DFOpaque[T]
//val DFOpaque = core.DFOpaque
type DFEncoding = core.DFEncoding
val DFEncoding = core.DFEncoding

extension (value: BigInt)
  def bitsWidth(signed: Boolean): Int =
    if (value > 0)
      if (signed) value.bitLength + 1 else value.bitLength
    else if (value == 0)
      if (signed) 2 else 1
    else (-value).bitLength + 1

extension (value: Int)
  def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)
