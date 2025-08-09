package dfhdl.platforms.resources

class VGA() extends ResourceGroup:
  val R = IOBus.fill(4)(Sig())
  val G = IOBus.fill(4)(Sig())
  val B = IOBus.fill(4)(Sig())
  val HS = Sig()
  val VS = Sig()
end VGA
