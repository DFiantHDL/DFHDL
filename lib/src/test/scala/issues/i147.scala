// format: off
package issues.i147

import dfhdl.*

val clkCfg1 = ClkCfg(ClkCfg.Edge.Rising)
val rstCfg1 = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.High)
val rtcfg = RTDomainCfg(clkCfg1, rstCfg1)

class INV() extends RTDesign:
    val a = Bit <> IN
    val c = Bit <> VAR.REG init 0
    val b = Bit <> OUT
    c.din := a
    b := !a && c
    
class ClockRstConnection() extends RTDesign(rtcfg):
    val a = Bit <> IN
    val b = Bit <> OUT
    
    val inv0 = new INV()
    
    inv0.a <> a
    inv0.b <> b
