package CoreSpec
import dfhdl.*
import munit.*

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    Bit.width.verifyInlined(1)
    Boolean.width.verifyInlined(1)
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
         |val t4 = 0 ^ bt
         |val t5 = bt && 1
         |val t6 = bl || bt.bool
         |val t7 = (bl ^ false) || bt.bool
         |val t8 = (bl && bt.bool) ^ (bt || bl.bit).bool
         |val t9: Bit <> CONST = 1
         |val t10: Bit <> CONST = 0
         |val t11: Bit <> CONST = 1
         |val t12: Bit <> CONST = 0
         |val t13: Boolean <> CONST = true
         |val t14: Boolean <> CONST = false
         |val t15: Boolean <> CONST = true
         |val t16: Boolean <> CONST = false
         |""".stripMargin
    ) {
      val t1 = bt && bl
      val t2 = bt ^ 1
      val t3 = bl || false
      val t4 = 0 ^ bt
      val t5 = bt && true
      val t6 = bl || bt
      val t7 = bl ^ 0 || bt
      val t8 = (bl && bt) ^ (bt || bl)
      val t9: Bit <> VAL = 1
      val t10: Bit <> VAL = 0
      val t11: Bit <> VAL = true
      val t12: Bit <> VAL = false
      val t13: Boolean <> VAL = 1
      val t14: Boolean <> VAL = 0
      val t15: Boolean <> VAL = true
      val t16: Boolean <> VAL = false
    }
  }
end DFBoolOrBitSpec
