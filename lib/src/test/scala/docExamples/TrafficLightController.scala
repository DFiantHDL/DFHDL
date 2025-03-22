package docExamples
import dfhdl.*

case class TrafficLight(
    red: Bit <> VAL,
    yellow: Bit <> VAL,
    green: Bit <> VAL
) extends Struct
object TrafficLight:
  val OFF = TrafficLight(red = 0, yellow = 0, green = 0)
  val RED = TrafficLight(red = 1, yellow = 0, green = 0)
  val YELLOW = TrafficLight(red = 0, yellow = 1, green = 0)
  val GREEN = TrafficLight(red = 0, yellow = 0, green = 1)
  val RED_YELLOW = TrafficLight(red = 1, yellow = 1, green = 0)

@top class TrafficLightController(
    val RED_TIME: Time <> CONST = 1.min,
    val YELLOW_TIME: Time <> CONST = 5.sec,
    val RED_YELLOW_TIME: Time <> CONST = 3.sec,
    val GREEN_TIME: Time <> CONST = 1.min,
    val GREEN_BLINK_TIME: Time <> CONST = 1.sec,
    val GREEN_BLINK_CNT: Int <> CONST = 3
) extends RTDesign:
  val trafficLight = TrafficLight <> OUT.REG init TrafficLight.RED

  process:
    def S_RED: Step =
      trafficLight.din := TrafficLight.RED
      RED_TIME.wait
      S_RED_YELLOW
    def S_RED_YELLOW: Step =
      trafficLight.din := TrafficLight.RED_YELLOW
      RED_YELLOW_TIME.wait
      S_GREEN
    def S_GREEN: Step =
      trafficLight.din := TrafficLight.GREEN
      GREEN_TIME.wait
      S_GREEN_BLINK
    def S_GREEN_BLINK: Step =
      for (i <- 0 until GREEN_BLINK_CNT)
        trafficLight.din := TrafficLight.OFF
        GREEN_BLINK_TIME.wait
        trafficLight.din := TrafficLight.GREEN
        GREEN_BLINK_TIME.wait
      S_YELLOW
    def S_YELLOW: Step =
      trafficLight.din := TrafficLight.YELLOW
      YELLOW_TIME.wait
      S_RED
end TrafficLightController
