package dfhdl.platforms.resources
import dfhdl.hw.constraints.io

abstract class Sig extends IO

object Sig:
  @io(dir = _.in)
  class IN extends Sig
  @io(dir = _.out)
  class OUT extends Sig
  @io(dir = _.inout)
  class INOUT extends Sig
