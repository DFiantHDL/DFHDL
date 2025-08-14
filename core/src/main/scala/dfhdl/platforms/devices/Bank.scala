package dfhdl.platforms.devices
import dfhdl.platforms.resources.*
import Resource.CanConnect

abstract class Bank extends ResourceGroup:
  type This <: Bank
  protected given Bank: This = this.asInstanceOf[This]
