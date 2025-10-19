import dfhdl._

@df class Fibo extends DFDesign:
  val o = UInt(32) <> OUT
  val f = UInt(32) <> VAR init (1, 0)
  f := f.prev + f.prev(2)
  o := f.prev(2) // start from 0
