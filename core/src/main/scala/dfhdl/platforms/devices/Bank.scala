package dfhdl.platforms.devices
import dfhdl.platforms.resources.*

abstract class Bank extends ResourceGroup:
  type This <: Bank
  protected given Bank: This = this.asInstanceOf[This]
