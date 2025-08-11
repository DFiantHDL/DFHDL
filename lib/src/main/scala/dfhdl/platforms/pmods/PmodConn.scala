package dfhdl.platforms.pmods
import dfhdl.platforms.resources.*
import Resource.CanConnect
import dfhdl.DFC

object PmodConn extends Connector.Companion(12)

object PmodDualConn:
  class Male(val pm1: PmodConn.Male, val pm2: PmodConn.Male) extends ResourceDeps:
    lazy val upstreamDeps: List[Resource] = List(pm1, pm2)
  class Female(val pm1: PmodConn.Female, val pm2: PmodConn.Female) extends ResourceDeps:
    lazy val upstreamDeps: List[Resource] = List(pm1, pm2)

  given [M <: Male, F <: Female]: CanConnect[M, F] = (m, f) =>
    m.connectFrom(f)
    f.connectFrom(m)
end PmodDualConn
