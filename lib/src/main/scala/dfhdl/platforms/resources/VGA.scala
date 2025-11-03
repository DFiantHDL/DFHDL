package dfhdl.platforms.resources

class VGA() extends ResourceGroup:
  val R = IOBus.fill(4)(Sig.OUT())
  val G = IOBus.fill(4)(Sig.OUT())
  val B = IOBus.fill(4)(Sig.OUT())
  val HS = Sig.OUT()
  val VS = Sig.OUT()
end VGA
