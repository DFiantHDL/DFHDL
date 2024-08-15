// format: off
package issues.i142

import dfhdl._

@top(false) class IntegerIndexingIssue() extends RTDesign:
    val a = Bits(4) <> IN
    val b = Bits(12) X 16 <> VAR.REG init all(all(0))
    val c = Bits(12) <> OUT
    
    c := b(a)