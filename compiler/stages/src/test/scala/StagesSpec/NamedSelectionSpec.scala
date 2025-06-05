package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.verilogNamedSelection
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NamedSelectionSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("Anonymous conditional expressions") {
    class Mux extends DFDesign:
      val c = Boolean <> IN
      val i = Byte    <> IN
      val o = Byte    <> OUT
      val z = Byte    <> OUT
      o := (if (c) i else i)
      o := i | ((if (c) i else i): Byte <> VAL)
      z := i match
        case all(0) => i
        case _      => i

    val id = (new Mux).verilogNamedSelection
    assertCodeString(
      id,
      """|class Mux extends DFDesign:
         |  val c = Boolean <> IN
         |  val i = Bits(8) <> IN
         |  val o = Bits(8) <> OUT
         |  val z = Bits(8) <> OUT
         |  o := ((
         |    if (c) i
         |    else i
         |  ): Bits[8] <> VAL)
         |  val o_part: Bits[8] <> VAL =
         |    if (c) i
         |    else i
         |  o := i | o_part
         |  z := ((
         |    i match
         |      case h"00" => i
         |      case _ => i
         |    end match
         |  ): Bits[8] <> VAL)
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

    val id = (new ID).verilogNamedSelection
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

    val id = (new ID).verilogNamedSelection
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
  test("Named selection with default parameter values") {
    class Foo(val width: Int <> CONST = 16) extends DFDesign:
      val x = SInt(width) <> IN
      val y = SInt(width) <> OUT
      y <> x

    class Top extends RTDesign:
      val x1   = SInt(16) <> IN
      val x2   = SInt(8)  <> IN
      val foo1 = Foo()
      val foo2 = Foo()
      foo1.x <> x1
      foo2.x <> x2

    val top = (new Top).verilogNamedSelection
    assertCodeString(
      top,
      """|class Foo(val width: Int <> CONST = 16) extends DFDesign:
         |  val x = SInt(width) <> IN
         |  val y = SInt(width) <> OUT
         |  y <> x
         |end Foo
         |
         |class Top extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val x2 = SInt(8) <> IN
         |  val foo1 = Foo(width = 16)
         |  val foo2 = Foo(width = 16)
         |  foo1.x <> x1
         |  foo2.x <> x2.resize(16)
         |end Top
         |""".stripMargin
    )
  }
end NamedSelectionSpec
