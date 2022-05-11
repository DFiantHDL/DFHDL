package StagesSpec

import DFiant.*
import DFiant.compiler.stages.explicitPrev
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitPrevSpec extends StageSpec:
  test("Basic explicit prev") {
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT init 0
      y := y + 1
      val y2 = SInt(16) <> OUT init 0
      y := 1
      y := y + 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT init sd"16'0"
         |  y := y.prev
         |  y := y + sd"2'1"
         |  val y2 = SInt(16) <> OUT init sd"16'0"
         |  y := sd"16'1"
         |  y := y + sd"2'1"
         |end ID
         |""".stripMargin
    )
  }
  test("If-else coverage") {
    class ID extends DFDesign:
      val x  = SInt(16) <> IN
      val y  = SInt(16) <> OUT
      val y2 = SInt(16) <> OUT
      if (x > 0)
        y := 1
      y   := y + 1
      if (x > 0)
        y2 := 1
      else
        y2 := 2
      y2   := y2 + 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT init ?
         |  y := y.prev
         |  val y2 = SInt(16) <> OUT
         |  if (x > d"16'0") y := sd"16'1"
         |  y := y + sd"2'1"
         |  if (x > d"16'0") y2 := sd"16'1"
         |  else y2 := sd"16'2"
         |  y2 := y2 + sd"2'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Partial assignment coverage") {
    class ID extends DFDesign:
      val x  = SInt(16) <> IN
      val y  = Bits(16) <> OUT
      val y2 = Bits(16) <> OUT
      y(7, 0)   := all(0)
      y         := y << 1
      y2(7, 0)  := all(0)
      y2(15, 8) := all(0)
      y2        := y2 << 1
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = Bits(16) <> OUT init h"????"
         |  y := y.prev
         |  val y2 = Bits(16) <> OUT
         |  y(7, 0) := h"00"
         |  y := y << 1
         |  y2(7, 0) := h"00"
         |  y2(15, 8) := h"00"
         |  y2 := y2 << 1
         |end ID
         |""".stripMargin
    )
  }
  test("DFDecimal match pattern coverage") {
    class ID extends DFDesign:
      val x  = UInt(3) <> IN
      val y  = UInt(8) <> OUT init 0
      val y2 = UInt(8) <> OUT init 0
      val y3 = UInt(8) <> OUT init 0
      x match
        case 0 | 1 | 2 => y := 1
        case 3 | 4 | 5 => y := 1
        case 6         => y := 1
      y := y + 1
      x match
        case 0 | 1 | 2 => y2 := 1
        case 3 | 4 | 5 => y2 := 1
        case 6 | 7     => y2 := 1
      y2 := y2 + 1
      x match
        case 0 | 1 | 2 => y3 := 1
        case 3 | 4 | 5 => y3 := 1
        case _         => y3 := 1
      y3 := y3 + 1
    end ID
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = UInt(3) <> IN
         |  val y = UInt(8) <> OUT init d"8'0"
         |  y := y.prev
         |  val y2 = UInt(8) <> OUT init d"8'0"
         |  val y3 = UInt(8) <> OUT init d"8'0"
         |  x match
         |    case d"3'0" | d"3'1" | d"3'2" => y := d"8'1"
         |    case d"3'3" | d"3'4" | d"3'5" => y := d"8'1"
         |    case d"3'6" => y := d"8'1"
         |  y := y + d"1'1"
         |  x match
         |    case d"3'0" | d"3'1" | d"3'2" => y2 := d"8'1"
         |    case d"3'3" | d"3'4" | d"3'5" => y2 := d"8'1"
         |    case d"3'6" | d"3'7" => y2 := d"8'1"
         |  y2 := y2 + d"1'1"
         |  x match
         |    case d"3'0" | d"3'1" | d"3'2" => y3 := d"8'1"
         |    case d"3'3" | d"3'4" | d"3'5" => y3 := d"8'1"
         |    case _ => y3 := d"8'1"
         |  y3 := y3 + d"1'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Bits match pattern coverage") {
    class ID extends DFDesign:
      val x  = Bits(3) <> IN
      val y  = UInt(8) <> OUT init 0
      val y2 = UInt(8) <> OUT init 0
      val y3 = UInt(8) <> OUT init 0
      x match
        case b"000" | b"001" | b"010" => y := 1
        case b"011" | b"100" | b"101" => y := 1
        case b"110"                   => y := 1
      y := y + 1
      x match
        case b"000" | b"001" | b"010" => y2 := 1
        case b"011" | b"100" | b"101" => y2 := 1
        case b"110" | b"111"          => y2 := 1
      y2 := y2 + 1
      // although this is fully covered, the current implementation does not check it
      x match
        case b"?1?" => y3 := 1
        case b"?0?" => y3 := 1
      y3 := y3 + 1
    end ID
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = Bits(3) <> IN
         |  val y = UInt(8) <> OUT init d"8'0"
         |  y := y.prev
         |  val y2 = UInt(8) <> OUT init d"8'0"
         |  val y3 = UInt(8) <> OUT init d"8'0"
         |  y3 := y3.prev
         |  x match
         |    case b"000" | b"001" | b"010" => y := d"8'1"
         |    case b"011" | b"100" | b"101" => y := d"8'1"
         |    case b"110" => y := d"8'1"
         |  y := y + d"1'1"
         |  x match
         |    case b"000" | b"001" | b"010" => y2 := d"8'1"
         |    case b"011" | b"100" | b"101" => y2 := d"8'1"
         |    case b"110" | b"111" => y2 := d"8'1"
         |  y2 := y2 + d"1'1"
         |  x match
         |    case b"?1?" => y3 := d"8'1"
         |    case b"?0?" => y3 := d"8'1"
         |  y3 := y3 + d"1'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Encode match pattern coverage") {
    class ID extends DFDesign:
      enum MyEnum extends Encode:
        case Foo, Baz, Bar
      import MyEnum.*
      val x  = MyEnum  <> IN
      val y  = UInt(8) <> OUT init 0
      val y2 = UInt(8) <> OUT init 0
      x match
        case Foo | Baz => y := 1
      y := y + 1
      x match
        case Foo | Baz => y2 := 1
        case Bar       => y2 := 1
      y2 := y2 + 1
    end ID
    val id = (new ID).explicitPrev
    assertCodeString(
      id,
      """|enum MyEnum(val value: UInt[2] <> TOKEN) extends Encode.Manual(2):
         |  case Foo extends MyEnum(d"2'0")
         |  case Baz extends MyEnum(d"2'1")
         |  case Bar extends MyEnum(d"2'2")
         |
         |class ID extends DFDesign:
         |  val x = MyEnum <> IN
         |  val y = UInt(8) <> OUT init d"8'0"
         |  y := y.prev
         |  val y2 = UInt(8) <> OUT init d"8'0"
         |  x match
         |    case MyEnum.Foo | MyEnum.Baz => y := d"8'1"
         |  y := y + d"1'1"
         |  x match
         |    case MyEnum.Foo | MyEnum.Baz => y2 := d"8'1"
         |    case MyEnum.Bar => y2 := d"8'1"
         |  y2 := y2 + d"1'1"
         |end ID
         |""".stripMargin
    )
  }
end ExplicitPrevSpec
