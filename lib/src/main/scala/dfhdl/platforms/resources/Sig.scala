package dfhdl.platforms.resources
import dfhdl.hw.constraints.io

abstract class Sig extends IO

object Sig:
  @io(dir = io.Dir.IN)
  class IN extends Sig
  @io(dir = io.Dir.OUT)
  class OUT extends Sig
  @io(dir = io.Dir.INOUT)
  class INOUT extends Sig
