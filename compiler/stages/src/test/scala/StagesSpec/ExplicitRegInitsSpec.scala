package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitRegInits
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitRegInitsSpec extends StageSpec:
  test("Regs that require explicit init") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN init 0
      val y1 = SInt(16) <> OUT
      val x2 = SInt(16) <> IN init 5
      val y2 = SInt(16) <> OUT
      y1 := x1.reg
      y2 := x2.reg(2) + x2.reg(2)
    // applying `nameRegAliases` to also check that existing
    // named reg aliases should be ignored
    val id = (new ID).explicitRegInits
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  y1 := x1.reg(1, init = sd"16'0")
         |  y2 := x2.reg(2, init = sd"16'5") + x2.reg(2, init = sd"16'5")
         |end ID
         |""".stripMargin
    )
  }
  test("Regs that have explicit init") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN init 0
      val y1 = SInt(16) <> OUT
      val x2 = Bits(16) <> IN init all(1)
      val y2 = Bits(16) <> OUT
      y1 := (x1 + 1).reg(1, init = ?)
      val z = (x2 << 1).reg(1, init = ?)
      y2 := x2(7, 0).reg(1, init = ?).resize(16).reg(2, init = ?) | z
    val id = (new ID).explicitRegInits
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = Bits(16) <> IN
         |  val y2 = Bits(16) <> OUT
         |  y1 := (x1 + sd"16'1").reg(1, init = ?)
         |  val z = (x2 << 1).reg(1, init = h"????")
         |  y2 := x2(7, 0).reg(1, init = h"??").resize(16).reg(2, init = h"????") | z
         |end ID
         |""".stripMargin
    )
  }
  test("Constant variable") {
    class ID extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      val v = SInt(16) <> VAR init 44
      y := x + v
    val id = (new ID).explicitRegInits
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init sd"16'44"
         |  y := x + v
         |end ID
         |""".stripMargin
    )
  }
end ExplicitRegInitsSpec
