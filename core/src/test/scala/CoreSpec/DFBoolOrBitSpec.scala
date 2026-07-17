package CoreSpec
import dfhdl.*
import munit.*

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    Bit.verifyWidth(1)
    Boolean.verifyWidth(1)
  }

  test("Assignment") {
    assertCodeString(
      """|val bt = Bit <> VAR
         |val bl = Boolean <> VAR
         |bl := (bt || 1).bool
         |bt := bl.bit
         |bl := bt.bool
         |bt := 1
         |bt := 0
         |bl := true
         |bl := false
         |""".stripMargin
    ) {
      val bt = Bit <> VAR
      val bl = Boolean <> VAR
      bl := (bt || 1)
      bt := bl
      bl := bt
      bt := true
      bt := 0
      bl := 1
      bl := false
    }
  }

  test("Logical Ops") {
    val bt = Bit <> VAR
    val bl = Boolean <> VAR
    assertCodeString(
      """|val t1 = bt && bl.bit
         |val t2 = bt ^ 1
         |val t3 = bl || false
         |val t4a = 0 ^ bt
         |val t4b = 1 || bl.bit
         |val t4c = 0 && bt
         |val t5 = bt && 1
         |val t6 = bl || bt.bool
         |val t7 = (bl ^ false) || (!bt).bool
         |val t8 = (bl && bt.bool) ^ ((!bt) || bl.bit).bool
         |val t9: Bit <> CONST = 1
         |val t10: Bit <> CONST = 0
         |val t11: Bit <> CONST = 1
         |val t12: Bit <> CONST = 0
         |val t13: Boolean <> CONST = true
         |val t14: Boolean <> CONST = false
         |val t15: Boolean <> CONST = true
         |val t16: Boolean <> CONST = false
         |val ifTest =
         |  bt || ((
         |    if (bl) bl
         |    else true
         |  ): Boolean <> VAL).bit
         |""".stripMargin
    ) {
      val t1 = bt && bl
      val t2 = bt ^ 1
      val t3 = bl || false
      val t4a = 0 ^ bt
      val t4b = 1 || bl
      val t4c = 0 && bt
      val t5 = bt && true
      val t6 = bl | bt
      val t7 = bl ^ 0 || !bt
      val t8 = (bl & bt) ^ (~bt || bl)
      val t9: Bit <> CONST = 1
      val t10: Bit <> CONST = 0
      val t11: Bit <> CONST = true
      val t12: Bit <> CONST = false
      val t13: Boolean <> CONST = 1
      val t14: Boolean <> CONST = 0
      val t15: Boolean <> CONST = true
      val t16: Boolean <> CONST = false
      assert(t9.toScalaBitNum == 1)
      assert(t10.toScalaBitNum == 0)
      assert(t11.toScalaBitNum == 1)
      assert(t12.toScalaBitNum == 0)
      assert(t13.toScalaBoolean == true)
      assert(t14.toScalaBoolean == false)
      assert(t15.toScalaBoolean == true)
      assert(t16.toScalaBoolean == false)
      assertCompileError(
        "Only a DFHDL constant is convertible to a Scala value, but this DFHDL value is not a constant."
      )(
        """t1.toScalaBitNum"""
      )
      assertCompileError(
        "Only a DFHDL constant is convertible to a Scala value, but this DFHDL value is not a constant."
      )(
        """t1.toScalaBoolean"""
      )
      val ifTest = bt || (if (bl) bl else 1)
    }
  }

  test("selection operation") {
    val bl = Boolean <> VAR
    val c1: Int <> CONST = 1
    val c2: Int <> CONST = 2
    assertCodeString(
      """|val t1 = bl.sel(d"4'11", d"4'12")
         |val t2 = bl.sel(d"4'12", d"4'11")
         |val t3 = bl.sel(d"4'1", d"4'13")
         |val t4 = bl.sel(d"4'13", d"4'1")
         |val t5 = bl.sel(c1, c2)
         |val t6 = bl.sel(d"4'${c1}", d"4'12")
         |val t7 = bl.sel(d"4'12", d"4'${c2}")
         |""".stripMargin
    ) {
      val t1 = bl.sel(11, d"4'12")
      val t2 = bl.sel(d"4'12", 11)
      val t3 = bl.sel(1, d"4'13")
      val t4 = bl.sel(d"4'13", 1)
      val t5 = bl.sel(c1, c2)
      val t6 = bl.sel(c1, d"4'12")
      val t7 = bl.sel(d"4'12", c2)
    }
  }

  test("Scala Boolean at the LHS of a logical op with a DFHDL value"):
    assertPluginError(
      "Unsupported Scala Boolean primitive at the LHS of `&&` with a DFHDL value.\nConsider switching positions of the arguments."
    )(
      """
      class Foo extends DFDesign:
        val d = Boolean <> VAR
        val sb: Boolean = true
        val r = sb && d
      """
    )

  test("unexpected DFHDL boolean to Scala boolean conversion"):
    assertPluginError(
      "Found unexpected DFHDL boolean to Scala boolean conversion."
    )(
      """
      class Foo extends DFDesign:
        val d = Boolean <> VAR
        val sb: Boolean = d
      """
    )
end DFBoolOrBitSpec
