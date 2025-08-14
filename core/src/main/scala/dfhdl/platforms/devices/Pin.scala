package dfhdl.platforms.devices
import dfhdl.compiler.ir.constraints.IO

abstract class Pin(using bankOwner: Bank) extends dfhdl.platforms.resources.IO:
  val name: String
  injectConstraint(IO(loc = id))
