// format: off
package issues.i129

import dfhdl.*

class StdLogicConvIssue() extends RTDesign:
    val a = Bits(10) <> IN
    val e = Bits(10) <> OUT

    e := (a(0), a(3), b"0", false.bit, b"000000")

