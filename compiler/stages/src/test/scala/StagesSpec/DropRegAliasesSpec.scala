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
      y2 := x2.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = DFSInt(16) <> IN
         |  val x1_reg = DFSInt(16) <> REG
         |  val y1 = DFSInt(16) <> OUT
         |  val x2 = DFSInt(16) <> IN
         |  val x2_reg1 = DFSInt(16) <> REG
         |  val x2_reg2 = DFSInt(16) <> REG
         |  val y2 = DFSInt(16) <> OUT
         |  x1_reg.din := x1
         |  y1 := x1_reg
         |  x2_reg1.din := x2
         |  x2_reg2.din := x2_reg1
         |  y2 := x2_reg
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
         |  val v  = DFSInt(16) <> WIRE
         |  val v_reg1 = DFSInt(16) <> REG
         |  val v_reg2 = DFSInt(16) <> REG
         |  val v_reg3 = DFSInt(16) <> REG
         |  val v_reg4 = DFSInt(16) <> REG
         |  v := x1
         |  v_reg1.din := v
         |  y1 := v_reg1
         |  v := x2
         |  v_reg2.din := v
         |  v := v_reg2
         |  v_reg3.din := v
         |  v_reg4.din := v_reg3
         |  y2 := v_reg4
         |end ID
         |""".stripMargin
    )
  }
end DropRegAliasesSpec
