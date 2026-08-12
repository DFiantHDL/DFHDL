package issues.i375

import dfhdl.*
import dfhdl.hw.annotation.top

@top(false) class draw_line(val CORDW: Int <> CONST = 16) extends EDDesign:
  val clk = Bit <> IN
  val x0 = SInt(CORDW) <> IN
  val x1 = SInt(CORDW) <> IN
  val y0 = SInt(CORDW) <> IN
  val y1 = SInt(CORDW) <> IN
  val swap = Bit <> VAR
  val xa = SInt(CORDW) <> VAR
  val xb = SInt(CORDW) <> VAR

  process(all):
    swap := y0 > y1
    xa := (if (swap) x1 else x0).resize(CORDW) // <-- crash here
    xb := (if (swap) x0 else x1).resize(CORDW)
