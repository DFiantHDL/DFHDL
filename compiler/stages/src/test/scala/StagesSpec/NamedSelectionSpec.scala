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
  test("Named selection with functions under system verilog") {
    class ID extends DFDesign:
      val x                  = UInt(6) <> IN
      val y: UInt[5] <> VAL  = (x min x).resize
      val z: UInt[5] <> VAL  = (x + x).resize
      val w: UInt[20] <> VAL = (x + x) + x

    val id = (new ID).verilogNamedSelection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = UInt(6) <> IN
         |  val y = (x min x).resize(5)
         |  val z = (x + x).resize(5)
         |  val w = ((x + x) +^ x).resize(20)
         |end ID""".stripMargin
    )
  }
  test("Named selection with functions under basic verilog") {
    given options.CompilerOptions.Backend = _.verilog.v95
    class ID extends DFDesign:
      val x                  = UInt(6) <> IN
      val y: UInt[5] <> VAL  = (x min x).resize
      val z: UInt[5] <> VAL  = (x + x).resize
      val w: UInt[20] <> VAL = (x + x) + x

    val id = (new ID).verilogNamedSelection
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = UInt(6) <> IN
         |  val y_part = x min x
         |  val y = y_part.resize(5)
         |  val z_part = x + x
         |  val z = z_part.resize(5)
         |  val w_part = x + x
         |  val w = (w_part +^ x).resize(20)
         |end ID""".stripMargin
    )
  }
  // An ED domain body is a concurrent scope, so a conditional expression branch in it cannot hold
  // a name (see `DB.condExprNamedValCheck`). The value is named and relocated before the
  // conditional in the same patch, rather than named where it was built.
  test("Named selection hoisted out of a concurrent conditional expression") {
    class ID extends EDDesign:
      val c = Bit     <> IN
      val x = SInt(9) <> IN
      val y = UInt(8) <> OUT
      y <> (if (c) (~x.bits).uint.lsbits(8) else x.bits.uint.lsbits(8))

    val id = (new ID).verilogNamedSelection
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val c = Bit <> IN
         |  val x = SInt(9) <> IN
         |  val y = UInt(8) <> OUT
         |  val anon = (~x.bits).uint
         |  val anon = x.bits.uint
         |  y <> ((
         |    if (c) anon(7, 0)
         |    else anon(7, 0)
         |  ): UInt[8] <> VAL)
         |end ID
         |""".stripMargin
    )
  }
  // In a process the branch is a procedural block, so the name stays where it was built.
  test("Named selection kept inside a sequential conditional expression") {
    class ID extends EDDesign:
      val c = Bit     <> IN
      val x = SInt(9) <> IN
      val y = UInt(8) <> OUT
      process(all):
        y := (if (c) (~x.bits).uint.lsbits(8) else x.bits.uint.lsbits(8))

    val id = (new ID).verilogNamedSelection
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val c = Bit <> IN
         |  val x = SInt(9) <> IN
         |  val y = UInt(8) <> OUT
         |  process(all):
         |    y := ((
         |      if (c)
         |        val anon = (~x.bits).uint
         |        anon(7, 0)
         |      else
         |        val anon = x.bits.uint
         |        anon(7, 0)
         |    ): UInt[8] <> VAL)
         |end ID
         |""".stripMargin
    )
  }
  // The value to name reads a conditional expression that is a naming group of its own, so both
  // must relocate. They cannot move in one pass without duplicating the shared sub-tree, so the
  // innermost is named and hoisted first and the outer one follows on the next pass.
  test("Named selection hoisted along with a nested conditional expression") {
    class ID extends EDDesign:
      val c, d = Bit     <> IN
      val p, q = UInt(9) <> IN
      val y    = UInt(8) <> OUT
      y <> (
        if (c) p.lsbits(8)
        else ((if (d) p else q) + d"9'1").lsbits(8)
      )

    val id = (new ID).verilogNamedSelection
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val c = Bit <> IN
         |  val d = Bit <> IN
         |  val p = UInt(9) <> IN
         |  val q = UInt(9) <> IN
         |  val y = UInt(8) <> OUT
         |  val anon_part: UInt[9] <> VAL =
         |    if (d) p
         |    else q
         |  val anon = anon_part + d"9'1"
         |  y <> ((
         |    if (c) p(7, 0)
         |    else anon(7, 0)
         |  ): UInt[8] <> VAL)
         |end ID
         |""".stripMargin
    )
  }
  // a carry-widened func consumed by an unsigned-to-signed conversion is named, so the
  // Verilog `{1'b0, ...}` sign-extension concat (whose operands are self-determined)
  // sees a declared identifier instead of an inline func pinned at its narrow operand
  // width (issue #452)
  test("Sign-converted carry func is named") {
    class SignedCarry extends EDDesign:
      val a = UInt(8)  <> IN
      val b = UInt(8)  <> IN
      val o = SInt(10) <> OUT
      o <> (a +^ b).signed

    val id = (new SignedCarry).verilogNamedSelection
    assertCodeString(
      id,
      """|class SignedCarry extends EDDesign:
         |  val a = UInt(8) <> IN
         |  val b = UInt(8) <> IN
         |  val o = SInt(10) <> OUT
         |  val o_part = a +^ b
         |  o <> o_part.signed
         |end SignedCarry
         |""".stripMargin
    )
  }
end NamedSelectionSpec
