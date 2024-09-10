package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.matchToIf
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class MatchToIfSpec extends StageSpec:
  test("Basic pattern match remains the same"):
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      x match
        case 22           => y := 0
        case 11 | 33 | 44 => y := 1
        case _            => y := 2

    end ID
    val id = (new ID).matchToIf
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  x match
         |    case sd"16'22" => y := sd"16'0"
         |    case sd"16'11" | sd"16'33" | sd"16'44" => y := sd"16'1"
         |    case _ => y := sd"16'2"
         |  end match
         |end ID
         |""".stripMargin
    )

  test("Basic pattern match with a guard"):
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      x match
        case 22           => y := 0
        case 11 | 33 | 44 => y := 1
        case _ if x == 55 => y := 2
        case _            => y := 3

    end ID
    val id = (new ID).matchToIf
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  if (x == sd"16'22") y := sd"16'0"
         |  else if ((x == sd"16'11") || ((x == sd"16'33") || (x == sd"16'44"))) y := sd"16'1"
         |  else if (x == sd"16'55") y := sd"16'2"
         |  else y := sd"16'3"
         |end ID
         |""".stripMargin
    )

  test("Basic pattern match with a guard, anonymous selector"):
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      (x + 1) match
        case 22           => y := 0
        case 11 | 33 | 44 => y := 1
        case _ if x == 55 => y := 2
        case _            => y := 3

    end ID
    val id = (new ID).matchToIf
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val match_sel = x + sd"16'1"
         |  if (match_sel == sd"16'22") y := sd"16'0"
         |  else if ((match_sel == sd"16'11") || ((match_sel == sd"16'33") || (match_sel == sd"16'44"))) y := sd"16'1"
         |  else if (x == sd"16'55") y := sd"16'2"
         |  else y := sd"16'3"
         |end ID
         |""".stripMargin
    )

  test("Struct pattern match"):
    case class Pixel(x: SInt[16] <> VAL, y: SInt[16] <> VAL) extends Struct
    class ID extends DFDesign:
      val x = Pixel    <> IN
      val y = SInt(16) <> OUT
      x match
        case Pixel(5, 22 | 17)      => y := 0
        case Pixel(10, py)          => y := py
        case Pixel(px, _) if px > 4 => y := 2
        case _                      => y := 3

    end ID
    val id = (new ID).matchToIf
    assertCodeString(
      id,
      """|final case class Pixel(
         |    x: SInt[16] <> VAL
         |    y: SInt[16] <> VAL
         |) extends Struct
         |
         |class ID extends DFDesign:
         |  val x = Pixel <> IN
         |  val y = SInt(16) <> OUT
         |  val py = x.y
         |  val px = x.x
         |  if ((x.x == sd"16'5") && ((x.y == sd"16'22") || (x.y == sd"16'17"))) y := sd"16'0"
         |  else if (x.x == sd"16'10") y := py
         |  else if (px > sd"16'4") y := sd"16'2"
         |  else y := sd"16'3"
         |end ID
         |""".stripMargin
    )
end MatchToIfSpec
