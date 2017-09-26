package DFiant.core

object Msg {
  type BitErr = "Bit index is out of range"
  val BitErr : BitErr = valueOf[BitErr]
  type HiBitErr = "High bit index is out of range"
  val HiBitErr : HiBitErr = valueOf[HiBitErr]
  type LoBitErr = "Low bit index is out of range"
  val LoBitErr : LoBitErr = valueOf[LoBitErr]
  type LoHiBitErr = "Low bit index must be smaller or equal to high bit index"
  val LoHiBitErr : LoHiBitErr = valueOf[LoHiBitErr]

}
