// format: off
package issues.i135

import dfhdl._
@top(false) class VerilogSRA() extends RTDesign:
    val a = SInt(10) <> IN
    val b = SInt(10) <> VAR
    
    b := a >> 1
