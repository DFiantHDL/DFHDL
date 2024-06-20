// format: off
package issues.i133

import dfhdl.*

@top(false) class Width0Issue(val width : Int <> CONST) extends RTDesign:
    val d = Bit <> IN
    val a = Bits(width) <> VAR
    a(0) := d // works only when width > 1




