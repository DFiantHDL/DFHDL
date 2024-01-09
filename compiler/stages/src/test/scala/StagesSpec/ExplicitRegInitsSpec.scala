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
         |  val x1 = SInt(16) <> IN init sd"16'0"
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN init sd"16'5"
         |  val y2 = SInt(16) <> OUT
         |  y1 := x1.reg(1, sd"16'0")
         |  y2 := x2.reg(2, sd"16'5") + x2.reg(2, sd"16'5")
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
         |  val x1 = SInt(16) <> IN init sd"16'0"
         |  val y1 = SInt(16) <> OUT
         |  val x2 = Bits(16) <> IN init h"ffff"
         |  val y2 = Bits(16) <> OUT
         |  y1 := (x1 + sd"16'1").reg(1, ?)
         |  val z = (x2 << 1).reg(1, h"????")
         |  y2 := x2(7, 0).reg(1, h"??").resize(16).reg(2, h"????") | z
         |end ID
         |""".stripMargin
    )
  }
end ExplicitRegInitsSpec
