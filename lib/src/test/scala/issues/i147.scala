// format: off
package issues.i147

import dfhdl.*

class INV() extends RTDesign:
    val a = Bit <> IN
    val c = Bit <> VAR.REG init 0
    val b = Bit <> OUT
    c.din := a
    b := !a && c

@hw.constraints.timing.clock(grpName = "rt")
@hw.constraints.timing.reset()
@top(false) class ClockRstConnection() extends RTDesign:
    val a = Bit <> IN
    val b = Bit <> OUT

    val inv0 = new INV()

    inv0.a <> a
    inv0.b <> b
