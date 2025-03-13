import dfhdl._

@df class Conc extends DFDesign:
  val i, j = UInt(32) <> IN
  val a, b, c, d, e = UInt(32) <> OUT
  a := i + 5
  b := a * 3
  c := a + b
  d := i - 1
  e := j / 4
