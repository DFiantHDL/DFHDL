// format: off
package issues.i146

import dfhdl.*

case class InStruct (
    a : Bit <> VAL,
    b : Bit <> VAL
) extends Struct

@top(false) class DoubleStructDecl() extends RTDesign():
    val sin = InStruct <> IN
    val svar = InStruct <> VAR
    svar := sin
