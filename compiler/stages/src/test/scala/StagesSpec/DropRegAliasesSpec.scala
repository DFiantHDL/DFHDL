package StagesSpec

import DFiant.*
import DFiant.compiler.stages.dropRegAliases
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropRegAliasesSpec extends StageSpec:
  test("Basic reg alias") {
    class ID extends RTDesign:
      val x1 = DFSInt(16) <> IN
      val y1 = DFSInt(16) <> OUT
      val x2 = DFSInt(16) <> IN
      val y2 = DFSInt(16) <> OUT
      y1 := x1.reg
      y2 := x2.reg(2) + x2.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val y1 = DFSInt(16) <> OUT
         |  val x2 = DFSInt(16) <> IN
         |  val y2 = DFSInt(16) <> OUT
         |  val x1_reg = DFSInt(16) <> REG
         |  val x2_reg1 = DFSInt(16) <> REG
         |  val x2_reg2 = DFSInt(16) <> REG
         |  x1_reg.din := x1
         |  y1 := x1_reg
         |  x2_reg1.din := x2
         |  x2_reg2.din := x2_reg1
         |  y2 := x2_reg2 + x2_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("Anonymous value reg aliases") {
    class ID extends RTDesign:
      val x1 = DFSInt(16) <> IN
      val y1 = DFSInt(16) <> OUT
      val x2 = DFBits(16) <> IN
      val y2 = DFBits(16) <> OUT
      y1 := (x1 + 1).reg
      val z = (x2 << 1).reg
      y2 := x2(7, 0).reg.resize(16).reg(2) | z
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val y1 = DFSInt(16) <> OUT
         |  val x2 = DFBits(16) <> IN
         |  val y2 = DFBits(16) <> OUT
         |  val y1_part_reg = DFSInt(16) <> REG
         |  val z = DFBits(16) <> REG
         |  val y2_part1_reg = DFBits(8) <> REG
         |  val y2_part2_reg1 = DFBits(16) <> REG
         |  val y2_part2_reg2 = DFBits(16) <> REG
         |  y1_part_reg.din := x1 + sd"2'1"
         |  y1 := y1_part_reg
         |  z.din := x2 << 1
         |  y2_part1_reg.din := x2(7, 0)
         |  y2_part2_reg1.din := y2_part1_reg.resize(16)
         |  y2_part2_reg2.din := y2_part2_reg1
         |  y2 := y2_part2_reg2 | z
         |end ID
         |""".stripMargin
    )
  }
  test("Reg alias of mutating wire") {
    class ID extends RTDesign:
      val x1 = DFSInt(16) <> IN
      val y1 = DFSInt(16) <> OUT
      val x2 = DFSInt(16) <> IN
      val y2 = DFSInt(16) <> OUT
      val v  = DFSInt(16) <> WIRE
      v  := x1
      y1 := v.reg
      v  := x2
      v  := v.reg
      y2 := v.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val y1 = DFSInt(16) <> OUT
         |  val x2 = DFSInt(16) <> IN
         |  val y2 = DFSInt(16) <> OUT
         |  val v = DFSInt(16) <> WIRE
         |  val v_ver1_reg = DFSInt(16) <> REG
         |  val v_ver2_reg = DFSInt(16) <> REG
         |  val v_ver3_reg1 = DFSInt(16) <> REG
         |  val v_ver3_reg2 = DFSInt(16) <> REG
         |  v := x1
         |  v_ver1_reg.din := v
         |  y1 := v_ver1_reg
         |  v := x2
         |  v_ver2_reg.din := v
         |  v := v_ver2_reg
         |  v_ver3_reg1.din := v
         |  v_ver3_reg2.din := v_ver3_reg1
         |  y2 := v_ver3_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("Reg alias inside conditionals") {
    class ID extends RTDesign:
      val x1 = DFSInt(16) <> IN
      val y1 = DFSInt(16) <> OUT
      if (x1 > 0)
        y1 := x1.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val y1 = DFSInt(16) <> OUT
         |  v := v_ver2_reg
         |  v_ver3_reg1.din := v
         |  v_ver3_reg2.din := v_ver3_reg1
         |  y2 := v_ver3_reg2
         |end ID
         |""".stripMargin
    )
  }
end DropRegAliasesSpec
