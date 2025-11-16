package dfhdl.platforms.resources

class UART extends ResourceGroup:
  val TX_FROM_DEVICE = Sig.OUT()
  val RX_TO_DEVICE = Sig.IN()
  val RTS_FROM_DEVICE = Sig.OUT()
  val CTS_TO_DEVICE = Sig.IN()
end UART
