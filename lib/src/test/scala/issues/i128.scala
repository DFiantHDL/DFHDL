// format: off
package issues.i128

import dfhdl.*

@top(false) class ArrayIssue() extends RTDesign:
    val a = Bit <> IN
    val b = Bit X 6 <> VAR
    val c = Bit X 5 X 4 <> VAR
    val d = Bits(4) X 3 X 2 <> VAR
    
    b(0) := a
    c(0)(0) := a
    d(0)(0)(0) := a
