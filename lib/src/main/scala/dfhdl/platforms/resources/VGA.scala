package dfhdl.platforms.resources

/** VGA output.
  *
  * @param bitsPerColor
  *   The number of bits for each of the RGB color channels.
  */
class VGA[BR <: Int & Singleton](val bitsPerColor: BR = 4) extends ResourceGroup:
  val R = IOBus.fill(bitsPerColor)(Sig.OUT())
  val G = IOBus.fill(bitsPerColor)(Sig.OUT())
  val B = IOBus.fill(bitsPerColor)(Sig.OUT())
  val HS = Sig.OUT()
  val VS = Sig.OUT()
end VGA
