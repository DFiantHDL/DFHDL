package dfhdl.platforms.resources

class VGA() extends ResourceGroup:
  val R0 = Sig()
  val R1 = Sig()
  val R2 = Sig()
  val R3 = Sig()
  val G0 = Sig()
  val G1 = Sig()
  val G2 = Sig()
  val G3 = Sig()
  val B0 = Sig()
  val B1 = Sig()
  val B2 = Sig()
  val B3 = Sig()
  val HS = Sig()
  val VS = Sig()
  val R = IOBus(R0, R1, R2, R3)
  val G = IOBus(G0, G1, G2, G3)
  val B = IOBus(B0, B1, B2, B3)
end VGA
