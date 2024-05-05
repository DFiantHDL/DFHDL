package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.nameRegAliases
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NameRegAliasesSpec extends StageSpec:
  test("Basic reg alias + double application") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN init 0
      val y1 = SInt(16) <> OUT
      val x2 = SInt(16) <> IN init 0
      val y2 = SInt(16) <> OUT
      y1 := x1.reg
      y2 := x2.reg(2) + x2.reg(2)
    // applying `nameRegAliases` to also check that existing
    // named reg aliases should be ignored
    val id = (new ID).nameRegAliases.nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN init sd"16'0"
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN init sd"16'0"
         |  val y2 = SInt(16) <> OUT
         |  val x1_reg = SInt(16) <> VAR
         |  val x2_reg1 = SInt(16) <> VAR
         |  val x2_reg2 = SInt(16) <> VAR
         |  x1_reg := x1.reg(1, init = sd"16'0")
         |  x2_reg1 := x2.reg(1, init = sd"16'0")
         |  x2_reg2 := x2_reg1.reg(1, init = sd"16'0")
         |  y1 := x1_reg
         |  y2 := x2_reg2 + x2_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("State reg alias") {
    class Cnt extends RTDesign:
      val c  = UInt(16) <> VAR init 0
      val o1 = UInt(16) <> OUT
      val o2 = UInt(16) <> OUT init 0
      o1 := c
      c  := c.reg + 1
      o2 := o2.reg + 1
    val id = (new Cnt).nameRegAliases
    assertCodeString(
      id,
      """|class Cnt extends RTDesign:
         |  val o1 = UInt(16) <> OUT
         |  val o2 = UInt(16) <> OUT init d"16'0"
         |  val c = UInt(16) <> VAR init d"16'0"
         |  val c_reg = UInt(16) <> VAR
         |  val o2_reg = UInt(16) <> VAR
         |  c_reg := c.reg(1, init = d"16'0")
         |  o2_reg := o2.reg(1, init = d"16'0")
         |  o1 := c
         |  c := c_reg + d"16'1"
         |  o2 := o2_reg + d"16'1"
         |end Cnt
         |""".stripMargin
    )
  }
  test("Anonymous value reg aliases") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN
      val y1 = SInt(16) <> OUT
      val x2 = Bits(16) <> IN
      val y2 = Bits(16) <> OUT
      y1 := (x1 + 1).reg(1, init = ?)
      val z = (x2 << 1).reg(1, init = ?)
      y2 := x2(7, 0).reg(1, init = ?).resize(16).reg(2, init = ?) | z
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = Bits(16) <> IN
         |  val y2 = Bits(16) <> OUT
         |  val y1_part_reg = SInt(16) <> VAR
         |  val z = Bits(16) <> VAR
         |  val y2_part1_reg = Bits(8) <> VAR
         |  val y2_part2_reg1 = Bits(16) <> VAR
         |  val y2_part2_reg2 = Bits(16) <> VAR
         |  y1_part_reg := (x1 + sd"16'1").reg(1, init = ?)
         |  y1 := y1_part_reg
         |  z := (x2 << 1).reg(1, init = h"????")
         |  y2_part1_reg := x2(7, 0).reg(1, init = h"??")
         |  y2_part2_reg1 := y2_part1_reg.resize(16).reg(1, init = h"????")
         |  y2_part2_reg2 := y2_part2_reg1.reg(1, init = h"????")
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
      val v  = SInt(16) <> VAR init 0
      v  := x1
      y1 := v.reg
      v  := x2
      v  := v.reg
      y2 := v.reg(2)
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR init sd"16'0"
         |  val v_ver1_reg = SInt(16) <> VAR
         |  val v_ver2_reg = SInt(16) <> VAR
         |  val v_ver3_reg1 = SInt(16) <> VAR
         |  val v_ver3_reg2 = SInt(16) <> VAR
         |  v := x1
         |  v_ver1_reg := v.reg(1, init = sd"16'0")
         |  y1 := v_ver1_reg
         |  v := x2
         |  v_ver2_reg := v.reg(1, init = sd"16'0")
         |  v := v_ver2_reg
         |  v_ver3_reg1 := v.reg(1, init = sd"16'0")
         |  v_ver3_reg2 := v_ver3_reg1.reg(1, init = sd"16'0")
         |  y2 := v_ver3_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("Reg aliases with param init") {
    val gp: Bit <> CONST      = 1
    val i: SInt[16] <> CONST  = sd"16'0"
    val i2: SInt[16] <> CONST = i + sd"16'5"
    class IDExt(
        val dp: Bit <> CONST    = gp,
        val dpNew: Bit <> CONST = 1
    ) extends RTDesign:
      val c: SInt[16] <> CONST = sd"16'3"
      val x                    = SInt(16) <> IN init i2
      val y                    = SInt(16) <> OUT
      val flag                 = Bit      <> IN init dp || gp
      val z                    = Bit      <> OUT
      y := x.reg(1, i2).reg(2, c - i) - x
      z := dpNew
    end IDExt

    val id = IDExt().nameRegAliases
    assertCodeString(
      id,
      """|val gp: Bit <> CONST = 1
         |val i: SInt[16] <> CONST = sd"16'0"
         |val i2: SInt[16] <> CONST = i + sd"16'5"
         |class IDExt(
         |    val dp: Bit <> CONST = gp,
         |    val dpNew: Bit <> CONST = 1
         |) extends RTDesign:
         |  val c: SInt[16] <> CONST = sd"16'3"
         |  val x = SInt(16) <> IN init i2
         |  val y = SInt(16) <> OUT
         |  val flag = Bit <> IN init dp || gp
         |  val z = Bit <> OUT
         |  val x_reg1 = SInt(16) <> VAR
         |  val x_reg2 = SInt(16) <> VAR
         |  val x_reg3 = SInt(16) <> VAR
         |  x_reg1 := x.reg(1, init = i2)
         |  x_reg2 := x_reg1.reg(1, init = c - i)
         |  x_reg3 := x_reg2.reg(1, init = c - i)
         |  y := x_reg3 - x
         |  z := dpNew
         |end IDExt
         |""".stripMargin
    )
  }
  test("Reg alias inside conditionals") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN init 0
      val y1 = SInt(16) <> OUT
      val v  = SInt(16) <> VAR
      if (x1 > 0)
        y1 := x1.reg
      else
        y1 := x1.reg + 1
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN init sd"16'0"
         |  val y1 = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  val x1_reg = SInt(16) <> VAR
         |  x1_reg := x1.reg(1, init = sd"16'0")
         |  if (x1 > sd"16'0") y1 := x1_reg
         |  else y1 := x1_reg + sd"16'1"
         |end ID
         |""".stripMargin
    )
  }
  // test("Reg alias of a reg variable (fibonacci)") {
  //   class Fib extends RTDesign:
  //     val f = UInt(32) <> VAR init 1
  //     val o = UInt(32) <> OUT
  //     f := f.reg(2, 0) + f.reg(1, 1)
  //     o := f.reg(1, 0)
  //   val fib = (new Fib).nameRegAliases
  //   assertCodeString(
  //     fib,
  //     """|class Fib extends RTDesign:
  //        |  val o = UInt(32) <> OUT
  //        |  val f = UInt(32) <> VAR init d"32'1"
  //        |  val f_reg = UInt(32) <> VAR init d"32'0"
  //        |  f_reg.din := f
  //        |  f.din := f + f_reg
  //        |  o := f_reg
  //        |end Fib
  //        |""".stripMargin
  //   )
  // }
end NameRegAliasesSpec
