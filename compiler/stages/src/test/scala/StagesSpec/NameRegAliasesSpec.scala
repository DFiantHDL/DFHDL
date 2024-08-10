package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.nameRegAliases
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class NameRegAliasesSpec extends StageSpec(stageCreatesUnrefAnons = true):
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
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT.REG init sd"16'0"
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val x2_reg1 = SInt(16) <> VAR.REG init sd"16'0"
         |  val x2_reg2 = SInt(16) <> VAR.REG init sd"16'0"
         |  x2_reg1.din := x2
         |  x2_reg2.din := x2_reg1
         |  y1.din := x1
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
         |  val o2 = UInt(16) <> OUT
         |  val c = UInt(16) <> VAR
         |  val c_reg = UInt(16) <> VAR.REG init d"16'0"
         |  val o2_reg = UInt(16) <> VAR.REG init d"16'0"
         |  c_reg.din := c
         |  o2_reg.din := o2
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
         |  val y1 = SInt(16) <> OUT.REG init ?
         |  val x2 = Bits(16) <> IN
         |  val y2 = Bits(16) <> OUT
         |  val z = Bits(16) <> VAR.REG init h"????"
         |  val y2_part1_reg = Bits(8) <> VAR.REG init h"??"
         |  val y2_part2_reg1 = Bits(16) <> VAR.REG init h"????"
         |  val y2_part2_reg2 = Bits(16) <> VAR.REG init h"????"
         |  y1.din := x1 + sd"16'1"
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
         |  val y1 = SInt(16) <> OUT.REG init sd"16'0"
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  val v_ver1_reg = SInt(16) <> VAR.REG init sd"16'0"
         |  val v_ver2_reg1 = SInt(16) <> VAR.REG init sd"16'0"
         |  val v_ver2_reg2 = SInt(16) <> VAR.REG init sd"16'0"
         |  v := x1
         |  y1.din := v
         |  v := x2
         |  v_ver1_reg.din := v
         |  v := v_ver1_reg
         |  v_ver2_reg1.din := v
         |  v_ver2_reg2.din := v_ver2_reg1
         |  y2 := v_ver2_reg2
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
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val flag = Bit <> IN
         |  val z = Bit <> OUT
         |  val x_reg1 = SInt(16) <> VAR.REG init i2
         |  val x_reg2 = SInt(16) <> VAR.REG init i2
         |  val x_reg3 = SInt(16) <> VAR.REG init i2
         |  x_reg1.din := x
         |  x_reg2.din := x_reg1
         |  x_reg3.din := x_reg2
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
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val v = SInt(16) <> VAR
         |  val x1_reg = SInt(16) <> VAR.REG init sd"16'0"
         |  x1_reg.din := x1
         |  if (x1 > sd"16'0") y1 := x1_reg
         |  else y1 := x1_reg + sd"16'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Reg alias of a REG value has no versions") {
    class ID extends RTDesign:
      val c  = Boolean  <> IN
      val y1 = SInt(16) <> OUT.REG init 0
      y1.din := y1.reg + 1
      y1.din := y1.reg + 1
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val c = Boolean <> IN
         |  val y1 = SInt(16) <> OUT.REG init sd"16'0"
         |  val y1_reg = SInt(16) <> VAR.REG init sd"16'0"
         |  y1_reg.din := y1
         |  y1.din := y1_reg + sd"16'1"
         |  y1.din := y1_reg + sd"16'1"
         |end ID
         |""".stripMargin
    )
  }
  test("Reg alias inside domains") {
    class ID extends EDDesign:
      val dmn1 = new RTDomain:
        val x = SInt(16) <> IN init 0
        val y = SInt(16) <> OUT
        y := x.reg
      val dmn2 = new RTDomain:
        val x = SInt(16) <> IN init 0
        val y = SInt(16) <> OUT
        y := x.reg(2) + x.reg(2)
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends EDDesign:
         |  val dmn1 = new RTDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT.REG init sd"16'0"
         |    y.din := x
         |  val dmn2 = new RTDomain:
         |    val x = SInt(16) <> IN
         |    val y = SInt(16) <> OUT
         |    val x_reg1 = SInt(16) <> VAR.REG init sd"16'0"
         |    val x_reg2 = SInt(16) <> VAR.REG init sd"16'0"
         |    x_reg1.din := x
         |    x_reg2.din := x_reg1
         |    y := x_reg2 + x_reg2
         |end ID
         |""".stripMargin
    )
  }
  test("Alias is already named") {
    class ID extends RTDesign:
      val x1 = SInt(16) <> IN init 0
      val y1 = SInt(16) <> OUT
      val x2 = SInt(16) <> IN init 0
      val y2 = SInt(16) <> OUT
      val r1 = SInt(16) <> VAR
      val x3 = SInt(16) <> IN
      val y3 = SInt(16) <> OUT
      r1 := x1.reg
      y1 := r1
      val r2 = x2.reg
      y2 := r2
      y3 := r2.reg(1, init = 42)
    val id = (new ID).nameRegAliases
    assertCodeString(
      id,
      """|class ID extends RTDesign:
         |  val x1 = SInt(16) <> IN
         |  val y1 = SInt(16) <> OUT
         |  val x2 = SInt(16) <> IN
         |  val y2 = SInt(16) <> OUT
         |  val x3 = SInt(16) <> IN
         |  val y3 = SInt(16) <> OUT.REG init sd"16'42"
         |  val r2 = SInt(16) <> VAR.REG init sd"16'0"
         |  r2.din := x2
         |  val r1 = SInt(16) <> VAR.REG init sd"16'0"
         |  r1.din := x1
         |  y1 := r1
         |  y2 := r2
         |  y3.din := r2
         |end ID
         |""".stripMargin
    )
  }
  // TODO: versioning is all wrong!
  // test("Reg alias inside conditionals with feedback") {
  //   class ID extends RTDesign:
  //     val c  = Boolean  <> IN
  //     val y1 = SInt(16) <> OUT init 0
  //     y1 := y1.reg + 1
  //     y1 := y1.reg + 1
  //     // if (c) y1 := y1.reg + 1
  //     // else y1   := y1.reg - 1
  //   val id = (new ID).nameRegAliases
  //   assertCodeString(
  //     id,
  //     """|class ID extends RTDesign:
  //        |  val x1 = SInt(16) <> IN
  //        |  val y1 = SInt(16) <> OUT
  //        |  val v = SInt(16) <> VAR
  //        |  val x1_reg = SInt(16) <> VAR
  //        |  x1_reg := x1.reg(1, init = sd"16'0")
  //        |  if (x1 > sd"16'0") y1 := x1_reg
  //        |  else y1 := x1_reg + sd"16'1"
  //        |end ID
  //        |""".stripMargin
  //   )
  // }
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
