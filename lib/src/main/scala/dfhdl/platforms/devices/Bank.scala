package dfhdl.platforms.devices
import dfhdl.platforms.resources.*
import Resource.CanConnect

abstract class Bank extends ResourceGroup:
  type This <: Bank
  protected given Bank: This = this.asInstanceOf[This]

object Bank:
  // power is unidirectional, but the connection is commutative.
  // always constraints travel from power to bank, not the other way around.
  given [B <: Bank, P <: Power]: CanConnect[B, P] = (bank, power) => bank.connectFrom(power)
  given [P <: Power, B <: Bank]: CanConnect[P, B] = (power, bank) => bank.connectFrom(power)
