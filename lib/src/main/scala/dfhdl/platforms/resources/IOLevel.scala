package dfhdl.platforms.resources

enum IOLevel:
  case LVCMOS18, LVCMOS25, LVCMOS33

trait HasIOLevel extends IO:
  val level: IOLevel
