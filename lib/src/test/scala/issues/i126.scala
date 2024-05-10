// format: off
package issues.i126

import dfhdl.*

class TypeConvertIssue() extends RTDesign:
    val a = Bit <> IN
    val b = Bit <> IN
    val c = UInt(8) <> IN
    val d = Bit <> OUT
    val e = Bit <> OUT
    
    d := 0
    if (c < 3 && b)
        d := 1

    e := 0
    if (b && c < 3)
        e := 1
