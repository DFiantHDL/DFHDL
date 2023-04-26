package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropRegAliases
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropRegAliasesSpec extends StageSpec:
  test("Basic reg alias") {
    val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
    val rstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
    val cfg    = RTDomainCfg(clkCfg, rstCfg)
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN
      val y1 = SInt(16) <> OUT
      val x2 = SInt(16) <> IN
      val y2 = SInt(16) <> OUT
      y1 := x1.reg(cfg)
      y2 := x2.reg(2) + x2.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val x1_reg = SInt(16) <> REG(cfg)
         |  val x2_reg1 = SInt(16) <> REG
         |  val x2_reg2 = SInt(16) <> REG
         |  x1_reg.din := x1
         |  x2_reg1.din := x2
         |  x2_reg2.din := x2_reg1
         |  y1 := x1_reg
         |  y2 := x2_reg2 + x2_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("Anonymous value reg aliases") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN
      val y1 = SInt(16) <> OUT
      val x2 = Bits(16) <> IN
      val y2 = Bits(16) <> OUT
      y1 := (x1 + 1).reg
      val z = (x2 << 1).reg
      y2 := x2(7, 0).reg.resize(16).reg(2) | z
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = Bits(16) <> IN
         |  val y2 = Bits(16) <> OUT
         |  val y1_part_reg = SInt(16) <> REG
         |  val z = Bits(16) <> REG
         |  val y2_part1_reg = Bits(8) <> REG
         |  val y2_part2_reg1 = Bits(16) <> REG
         |  val y2_part2_reg2 = Bits(16) <> REG
         |  y1_part_reg.din := x1 + sd"16'1"
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
      val x1 = SInt(16) <> IN
      val y1 = SInt(16) <> OUT
      val x2 = SInt(16) <> IN
      val y2 = SInt(16) <> OUT
      val v  = SInt(16) <> WIRE
      v  := x1
      y1 := v.reg
      v  := x2
      v  := v.reg
      y2 := v.reg(2)
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val v = SInt(16) <> WIRE
         |  val v_ver1_reg = SInt(16) <> REG
         |  val v_ver2_reg = SInt(16) <> REG
         |  val v_ver3_reg1 = SInt(16) <> REG
         |  val v_ver3_reg2 = SInt(16) <> REG
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
      val x1 = SInt(16) <> IN
      val y1 = SInt(16) <> OUT
      val v  = SInt(16) <> WIRE
      if (x1 > 0)
        y1 := x1.reg
      else
        y1 := x1.reg + 1
    val id = (new ID).dropRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val v = SInt(16) <> WIRE
         |  val x1_reg = SInt(16) <> REG
         |  x1_reg.din := x1
         |  if (x1 > sd"16'0") y1 := x1_reg
         |  else y1 := x1_reg + sd"16'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Reg alias of a reg variable (fibonacci)") {
    class Fib extends RTDesign:
      val f = UInt(32) <> REG init 1
      val o = UInt(32) <> OUT
      f.din := f + f.reg(1, 0)
      o     := f.reg(1, 0)
    val fib = (new Fib).dropRegAliases
    assertCodeString(
      fib,
      """|class Fib extends RTDesign:
         |  val o = UInt(32) <> OUT
         |  val f = UInt(32) <> REG init d"32'1"
         |  val f_reg = UInt(32) <> REG init d"32'0"
         |  f_reg.din := f
         |  f.din := f + f_reg
         |  o := f_reg
         |end Fib
         |""".stripMargin
    )
  }
end DropRegAliasesSpec
