import DFiant._

import lib.sequential._
@df final class TrafficLightController(
    RSec: Int,
    RYSec: Int,
    YSec: Int,
    GSec: Int
) extends DFDesign {
  val R, Y, G  = DFBit() <> OUT := 0 //outputs to traffic lights
  val timer_1s = DFBit() <> IN //active for a single token every 1 second
  val light: FSM =
    doFor(0 until RSec, timer_1s) { i => R := 1 } ==>
      doFor(0 until RYSec, timer_1s) { i => R := 1; Y := 1 } ==>
      doFor(0 until GSec, timer_1s) { i => G := 1 } ==>
      doFor(0 until YSec, timer_1s) { i => Y := 1 } ==>
      light
}

object TrafficLightControllerApp extends App {
  val tlc = new TrafficLightController(60, 5, 3, 30)
  tlc.printCodeString
}
