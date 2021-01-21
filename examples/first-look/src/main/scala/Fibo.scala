import DFiant._

@df class Fibo extends DFDesign {
  val o = DFUInt(32) <> OUT
  val f = DFUInt(32) <> VAR init(1, 0)
  f := f.prev + f.prev(2)
  o := f.prev(2) //start from 0
}