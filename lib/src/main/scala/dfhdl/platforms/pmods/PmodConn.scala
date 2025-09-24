package dfhdl.platforms.pmods
import dfhdl.platforms.resources.*
import Resource.CanConnect
import dfhdl.DFC

object PmodConn extends Connector.Companion(12):
  private def standardPmodPinToSPPmodPin(i: Int): Int =
    11 - 2 * ((i - 1) % 6) + (i - 1) / 6
  private def standardPmodPinToSPPmodPinStanding(i: Int): Int =
    14 - 2 * i + 11 * ((i - 1) / 6)

  /** Sipeed 90 degrees male version of the connector (different pin numbering). */
  def SPMale90Deg()(using DFC): Male = Male(standardPmodPinToSPPmodPin)

  /** Sipeed 90 degrees female version of the connector (different pin numbering). */
  def SPFemale90Deg()(using DFC): Female = Female(standardPmodPinToSPPmodPin)

  /** Sipeed standing female version of the connector (different pin numbering). */
  def SPFemaleStanding()(using DFC): Female = Female(standardPmodPinToSPPmodPinStanding)

object PmodDualConn:
  class Male(val pm1: PmodConn.Male, val pm2: PmodConn.Male) extends ResourceDeps:
    lazy val upstreamDeps: List[Resource] = List(pm1, pm2)
  class Female(val pm1: PmodConn.Female, val pm2: PmodConn.Female) extends ResourceDeps:
    lazy val upstreamDeps: List[Resource] = List(pm1, pm2)

  given [M <: Male, F <: Female]: CanConnect[M, F] with
    def connect(male: M, female: F)(using DFC): Unit =
      male.pm1 <> female.pm1
      male.pm2 <> female.pm2
end PmodDualConn
