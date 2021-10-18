import DFiant.*
import munit.*
import internals.Inlined

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    DFBit.width.verifyInlined(1)
    DFBool.width.verifyInlined(1)
  }

  test("Token Construction") {
    val bool = true
    val t1: DFBit <> TOKEN = 0
    val t2: DFBit <> TOKEN = DFBit token 1
    val t3: DFBool <> TOKEN = DFBool token true
    val t4: DFBool <> TOKEN = false
    val t5: DFBit <> TOKEN = DFBit token ?
    val t6: DFBool <> TOKEN = DFBool token t5
    val t7: DFBool <> TOKEN = DFBool token bool
    val t8: DFBit <> TOKEN = t7
    val t9: DFBool <> TOKEN = t8
  }
  test("Token Conversion") {}
  test("Comparison") {
    assertCodeString(
      """|val bt = DFBit <> VAR
         |val bl = DFBool <> VAR
         |val t1 = bt == bl.bit
         |val t2 = bl != bt.bool
         |val t3 = bt == 1
         |val t4 = bt != 0
         |val t5 = bl == true
         |val t6 = bl == false
         |val t7 = bt != 1
         |val t8 = bl == false
         |""".stripMargin
    ) {
      val bt = DFBit <> VAR
      val bl = DFBool <> VAR
      val bit0 = DFBit token 0
      val boolT = DFBool token true
      val t1 = bt == bl
      val t2 = bl != bt
      val t3 = bt == true
      val t4 = bt != 0
      val t5 = bl == 1
      val t6 = bl == false
      val t7 = bt != boolT
      val t8 = bl == bit0
    }
  }
  test("Assignment") {
    assertCodeString(
      """|val bt = DFBit <> VAR
         |val bl = DFBool <> VAR
         |bt := bl.bit
         |bl := bt.bool
         |bt := 1
         |bt := 0
         |bl := true
         |bl := false
         |bt := 1
         |bl := false
         |""".stripMargin
    ) {
      val bt = DFBit <> VAR
      val bl = DFBool <> VAR
      val bit0 = DFBit token 0
      val boolT = DFBool token true
      bt := bl
      bl := bt
      bt := true
      bt := 0
      bl := 1
      bl := false
      bt := boolT
      bl := bit0
    }
  }
  test("Logical Ops") {
    val bit0 = DFBit token 0
    val bit1 = DFBit token 1
    val boolF = DFBool token false
    val boolT = DFBool token true
    assertEquals(!bit0, bit1)
    assertEquals(!bit1, bit0)
    assertEquals(!boolF, boolT)
    assertEquals(!boolT, boolF)
    assertEquals(bit0.bool, boolF)
    assertEquals(boolT.bit, bit1)
    assertEquals(bit0 || 1, bit1)
    assertEquals(1 && bit0, bit0)
    assertEquals(bit0 ^ false, bit0)
    assertEquals(false || bit0, bit0)
    assertEquals(boolF && 1, boolF)
    assertEquals(1 || boolF, boolT)
    assertEquals(boolF && bit0, boolF)
    assertEquals(bit1 ^ boolT, bit0)
    val bt = DFBit <> VAR
    val bl = DFBool <> VAR
    assertCodeString(
      """|val t1 = bt && bl.bit
         |val t2 = bt ^ 1
         |val t3 = bl || false
         |val t4 = 0 ^ bt
         |val t5 = 1 && bt
         |val t6 = bl || bt.bool
         |val t7 = (bl ^ false) || bt.bool
         |val t8 = (bl && bt.bool) ^ (bt || bl.bit).bool
         |""".stripMargin
    ) {
      val t1 = bt && bl
      val t2 = bt ^ 1
      val t3 = bl || false
      val t4 = 0 ^ bt
      val t5 = true && bt
      val t6 = bl || bt
      val t7 = bl ^ 0 || bt
      val t8 = (bl && bt) ^ (bt || bl)
    }

  }
end DFBoolOrBitSpec
