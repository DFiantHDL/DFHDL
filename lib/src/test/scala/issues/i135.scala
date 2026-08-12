// format: off
package issues.i135

import dfhdl._
import dfhdl.hw.annotation.top
@top(false) class VerilogSRA() extends RTDesign:
    val a = SInt(10) <> IN
    val b = SInt(10) <> VAR
    
    b := a >> 1
