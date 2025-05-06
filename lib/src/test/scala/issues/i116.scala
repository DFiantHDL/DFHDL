// format: off
package issues.i116

import dfhdl.*

case class Test (
  a : Bit <> VAL,
  b : Bit <> VAL
) extends Struct

@top(false) class GlobCounter (val width: Int <> CONST) extends RTDesign:
  val req = Bit <> IN
  val req2 = Bit <> IN
  val t = Test <> IN
  val t_b = Bit <> VAR
  t_b := t.b
  val t_int = Test <> VAR
  t_int := t
  
  val cnt = UInt(width) <> OUT.REG init 0
   
  if (t.a) {
    if (t.b) {
        cnt.din := cnt + 1
    }
  }
  
  if (req) {
    if (t.b) {
        cnt.din := cnt + 1
    }
  }

  if (t.a && t.b) {
    cnt.din := cnt + 1
  }
  
  if (t.a) {
    if (t_b) {
        cnt.din := cnt + 1
    }
  }

  if (t_int.a) {
    if (t_int.b) {
        cnt.din := cnt + 1
    }
  }
  
  if (req) {
    if (req2) {
      cnt.din := cnt + 1
    }
  }
end GlobCounter
