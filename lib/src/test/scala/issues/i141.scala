// format: off
package issues.i141

import dfhdl._

case class EmbeddedArray (
    a : Bits[8]X(3) <> VAL
) extends Struct
    
@top(false) class StructArrayIssue() extends RTDesign:
    val a = Bit <> IN
    val e = EmbeddedArray <> VAR
    e.a(0)(0) := a