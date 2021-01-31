import DFiant._
import lib.sequential._
@df final class PatternGen extends DFDesign {
  val sel     = DFBit <> IN
  val tick    = DFBit <> IN
  val p       = DFBit <> OUT

  val PatternChoice : FSM = FSM {
    ifdf(sel) {Pattern1.goto()}
    .elsedf   {Pattern0.goto()}
    p := ?
  }
  val Pattern0 : FSM =
    doFor(0 until 20, tick){i => p := 0} ==>
    doFor(0 until 40, tick){i => p := 1} ==> PatternChoice
  val Pattern1 : FSM =
    doFor(0 until 30, tick){i => p := 0} ==>
    doFor(0 until 30, tick){i => p := 1} ==> PatternChoice
}