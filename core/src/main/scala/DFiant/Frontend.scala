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
type DFEncoding = core.DFType.DFEncoding
val DFEncoding = core.DFType.DFEncoding

extension (value: BigInt)
  def bitsWidth(signed: Boolean): Int =
    if (value > 0)
      if (signed) value.bitLength + 1 else value.bitLength
    else if (value == 0)
      if (signed) 2 else 1
    else (-value).bitLength + 1

extension (value: Int)
  def bitsWidth(signed: Boolean): Int = BigInt(value).bitsWidth(signed)

//given fromDFBits[W <: Int]: core.HasWidth[DFBits[W]] with
//  type Width = W
//  def apply(t: DFBits[W]): Inlined.Int[Width] =
//    Inlined.Int.forced[Width](t.__width)
//given fromDFBitsToken[W <: Int]: core.HasWidth[DFBits.Token[W]] with
//  type Width = W
//  def apply(t: DFBits.Token[W]): Inlined.Int[Width] =
//    Inlined.Int.forced[Width](t.dfType.__width)
//given fromDFType[T <: DFType](using
//    w: core.DFType.Width[T]
//): core.HasWidth[T] with
//  type Width = w.Out
//  def apply(t: T): Inlined.Int[Width] = Inlined.Int.forced[Width](t.__width)
//given fromDFToken[T <: DFType](using
//    w: core.DFType.Width[T]
//): core.HasWidth[core.DFToken[T]] with
//  type Width = w.Out
//  def apply(t: core.DFToken[T]): Inlined.Int[Width] =
//    Inlined.Int.forced[Width](t.dfType.__width)
//// given fromObject[T <: AnyRef](using
////     tc: core.DFType.TC[T],
////     w: core.DFType.Width[T]
//// ): core.HasWidth[T] with
////   type Width = w.Out
////   def apply(t: T): Inlined.Int[Width] =
////     Inlined.Int.forced[Width](tc(t).__width)
