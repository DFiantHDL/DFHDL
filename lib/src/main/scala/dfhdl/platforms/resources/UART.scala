package dfhdl.platforms.resources

class UART extends ResourceGroup:
  val TX_FROM_DEVICE = Sig()
  val RX_TO_DEVICE = Sig()
  val RTS_FROM_DEVICE = Sig()
  val CTS_TO_DEVICE = Sig()
end UART
