package dfhdl.platforms.resources
import dfhdl.compiler.ir.constraints
import dfhdl.compiler.ir.ConfigN
import dfhdl.platforms.devices.Bank
import Resource.CanConnect

class Power(val levelVolt: ConfigN[constraints.IO.LevelVolt] = None) extends IO:
  injectConstraint(constraints.IO(levelVolt = levelVolt))

  override def checkConnection(res: Resource): Unit =
    res match
      case p: Power =>
        if (p.levelVolt != levelVolt && (p.levelVolt != None && levelVolt != None))
          throw new IllegalArgumentException(
            s"Power level voltage connection mismatch between `${p.getFullId} @ ${p.levelVolt}` and `${getFullId} @ ${levelVolt}`."
          )
      case connPin: Connector.Pin => // do nothing
      case unexpected             =>
        throw new IllegalArgumentException(
          s"Unexpected resource type `${unexpected.getFullId}` connected to power `${getFullId}`."
        )
end Power

object Power:
  // power is unidirectional, but the connection is commutative.
  // always constraints travel from power to bank, not the other way around.
  given [B <: Bank, P <: Power]: CanConnect[B, P] = (bank, power) => bank.connectFrom(power)
  given [P <: Power, B <: Bank]: CanConnect[P, B] = (power, bank) => bank.connectFrom(power)
