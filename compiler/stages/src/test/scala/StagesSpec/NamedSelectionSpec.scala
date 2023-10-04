package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.namedSelection
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NamedSelectionSpec extends StageSpec:
  test("Anonymous conditional expressions") {
    class Mux extends DFDesign:
      val c = Boolean <> IN
      val i = Byte    <> IN
      val o = Byte    <> OUT
      val z = Byte    <> OUT
      o := ((if (c) i else i): Byte <> VAL)
      z := ((i match
        case all(0) => i
        case _      => i
      ): Byte <> VAL)

    val id = (new Mux).namedSelection
    assertCodeString(
      id,
      """|class Mux extends DFDesign:
         |  val c = Boolean <> IN
         |  val i = Bits(8) <> IN
         |  val o = Bits(8) <> OUT
         |  val z = Bits(8) <> OUT
         |  val o_part: Bits[8] <> VAL =
         |    if (c) i
         |    else i
         |  o := o_part
         |  val z_part: Bits[8] <> VAL =
         |    i match
         |      case h"00" => i
         |      case _ => i
         |  z := z_part
         |end Mux
         |""".stripMargin
    )
  }
  test("Named selection multiple references") {
    class ID extends DFDesign:
      val x = UInt(16) <> IN
      val y = Bits(8)  <> OUT
      if (x > 5)
        y := (x + 1).bits(7, 0) | (x + 1).bits(15, 8)
      else
        y := (x + 1).bits(15, 8)
      y   := (x + 2).bits(11, 4)
      if (x < 5)
        y := (x + 2).bits(7, 0) | (x + 2).bits(15, 8)
      else
        y := (x + 2).bits(15, 8)

    val id = (new ID).namedSelection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = UInt(16) <> IN
         |  val y = Bits(8) <> OUT
         |  if (x > d"16'5")
         |    val y_part = (x + d"16'1").bits
         |    y := y_part(7, 0) | y_part(15, 8)
         |  else
         |    val y_part = (x + d"16'1").bits
         |    y := y_part(15, 8)
         |  val y_part = (x + d"16'2").bits
         |  y := y_part(11, 4)
         |  if (x < d"16'5") y := y_part(7, 0) | y_part(15, 8)
         |  else y := y_part(15, 8)
         |end ID
         |""".stripMargin
    )
  }
  test("Ignore opaque type actual selection") {
    case class Wrapper() extends Opaque(Bits(16) X 4)
    class ID extends DFDesign:
      val x = Wrapper <> IN
      val y = Bits(8) <> OUT
      y := x.actual(0).bits(7, 0)

    val id = (new ID).namedSelection
    assertCodeString(
      id,
      """|case class Wrapper() extends Opaque(Bits(16) X 4)
         |
         |class ID extends DFDesign:
         |  val x = Wrapper <> IN
         |  val y = Bits(8) <> OUT
         |  val y_part = x.actual(0)
         |  y := y_part(7, 0)
         |end ID
         |""".stripMargin
    )
  }
end NamedSelectionSpec
