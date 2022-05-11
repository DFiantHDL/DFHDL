package CoreSpec
import DFiant.*
import munit.*
import internals.Inlined

class DFBoolOrBitSpec extends DFSpec:
  test("Inlined width") {
    DFBit.width.verifyInlined(1)
    Boolean.width.verifyInlined(1)
  }

  test("Token Construction") {
    val bool = true
    val t1: DFBit <> TOKEN = 0
    val t2: DFBit <> TOKEN = DFBit token 1
    val t3: Boolean <> TOKEN = Boolean token true
    val t4: Boolean <> TOKEN = false
    val t5: DFBit <> TOKEN = DFBit token ?
    val t6: Boolean <> TOKEN = Boolean token t5
    val t7: Boolean <> TOKEN = Boolean token bool
    // val t8: DFBit <> TOKEN = t7
    // val t9: Boolean <> TOKEN = t8
  }
  test("Token Conversion") {}
  test("Comparison") {
    assertCodeString(
      """|val bt = DFBit <> VAR
         |val bl = Boolean <> VAR
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
      val bl = Boolean <> VAR
      val bit0 = DFBit token 0
      val boolT = Boolean token true
      val t1 = bt == bl
      val t2 = bl != bt
      val t3 = bt == true
      val t4 = bt != 0
      val t5 = bl == 1
      val t6 = bl == false
      val t7 = bt != boolT
      val t8 = bl == bit0
    }
    val bit0 = DFBit token 0
    val bit1 = DFBit token 1
    val boolF = Boolean token false
    val boolT = Boolean token true
    assertEquals(bit0 == bit1, boolF)
    assertEquals(bit1 == boolT, boolT)
    assertEquals(bit0 != 1, boolT)
    assertEquals(bit0 == false, boolT)
    assertEquals(boolF != bit0, boolF)
    assertEquals(boolT == 0, boolF)
  }
  test("Assignment") {
    assertCodeString(
      """|val bt = DFBit <> VAR
         |val bl = Boolean <> VAR
         |bl := (bt || 1).bool
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
      val bl = Boolean <> VAR
      val bit0 = DFBit token 0
      val boolT = Boolean token true
      bl := (bt || 1)
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
    val boolF = Boolean token false
    val boolT = Boolean token true
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
    }

  }
end DFBoolOrBitSpec
