import DFiant.*
import munit.*
import internals.Inlined

import scala.annotation.implicitNotFound

class DFDecimalSpec extends DFSpec:
  val u8 = DFUInt(8) <> VAR
  val u7 = DFUInt(8) <> VAR
  val u4 = DFUInt(4) <> VAR
  assertCodeString(
    """|val t7 = u8.resize(4) === u8.resize(4)
       |""".stripMargin
  ) {
    val t7 = u8.resize(4) == u7.resize(4)

  }
end DFDecimalSpec
