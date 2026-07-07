package CoreSpec
import dfhdl.*
import munit.*

class DFFixSpec extends DFSpec:
  val uf8_10 = UFix(8, 10)
  val sf4_4 = SFix(4, 4)
  val uf0_8 = UFix(0, 8)
  test("Inlined width") {
    uf8_10.verifyWidth(18)
    sf4_4.verifyWidth(8)
    uf0_8.verifyWidth(8)
  }
  test("Type Construction") {
    val negOne = -1
    assertDSLErrorLog(
      "Unsigned magnitude width must be non-negative, but found: -1"
    )(
      """UFix(-1, 8)"""
    ) {
      UFix(negOne, 8)
    }
    val zero = 0
    assertDSLErrorLog(
      "Signed magnitude width must include the sign bit (at least 1), but found: 0"
    )(
      """SFix(0, 8)"""
    ) {
      SFix(zero, 8)
    }
  }
  test("Fixed-point literals") {
    assertCodeString {
      """|val c1: UFix[8, 10] <> CONST = d"8.10'11.22265625"
         |val c2: UFix[1, 1] <> CONST = d"1.1'1.5"
         |val c3: SFix[2, 1] <> CONST = sd"2.1'1.5"
         |val c4: UInt[8] <> CONST = d"8'42"
         |val c5: SFix[4, 4] <> CONST = sd"4.4'-1.5"
         |val c6: UFix[0, 2] <> CONST = d"0.2'0.25"
         |""".stripMargin
    } {
      val c1 = d"8.10'11.223"
      val c2 = d"1.5"
      val c3 = sd"1.5"
      val c4 = d"8.0'42"
      val c5 = sd"4.4'-1.5"
      val c6 = d"0.25"
    }
  }
  test("ShowType rendering") {
    // forces the `ShowType[T]` macro to render the fixed-point receiver type in the
    // "Unsupported value" error (exercises the ShowType path, not the IR type printer)
    assertCompileError(
      "Unsupported value of type `\"str\"` for DFHDL receiver type `UFix[8, 10]`."
    )("""val x = UFix(8, 10) <> VAR; x := "str"""")
    assertCompileError(
      "Unsupported value of type `\"str\"` for DFHDL receiver type `SFix[4, 4]`."
    )("""val y = SFix(4, 4) <> VAR; y := "str"""")
  }
  test("Literal errors") {
    assertCompileError(
      """|The value 11.223 is not exactly representable in binary.
         |To Fix: use an explicit `M.F'` width format to opt into rounding.""".stripMargin
    )("""d"11.223"""")
    assertCompileError(
      "The value 11.223 requires a magnitude width of at least 4, but found: 1"
    )("""d"1.10'11.223"""")
    assertCompileError(
      "Scaled formats (`p` binary-exponent notation) are not yet supported."
    )("""d"8p8'42"""")
    assertCompileError(
      "Negative value in unsigned `d\"\"` interpolation. Use `sd\"\"` for signed values."
    )("""d"4.4'-1.5"""")
  }
  test("Code String") {
    assertCodeString {
      """|val x = UFix(8, 10) <> VAR
         |val y = SFix(4, 4) <> VAR
         |val c: UFix[8, 10] <> CONST = d"8.10'11.22265625"
         |val d1: UFix[8, 10] <> CONST = d"8.10'0.25"
         |x := c
         |x := d1
         |""".stripMargin
    } {
      val x = UFix(8, 10) <> VAR
      val y = SFix(4, 4) <> VAR
      val c: UFix[8, 10] <> CONST = d"8.10'11.223"
      val d1: UFix[8, 10] <> CONST = 0.25
      x := c
      x := d1
    }
  }
  test("Fixed-point conversion") {
    val x = UFix(8, 10) <> VAR
    val y = SFix(4, 4) <> VAR
    val u4 = UInt(4) <> VAR
    val s4 = SInt(4) <> VAR
    val u3 = UInt(3) <> VAR
    assertCodeString {
      """|x := d"8.10'1.5"
         |y := sd"4.4'1.5"
         |x := u4.resize(8, 10)
         |y := s4.resize(4, 4)
         |y := u3.signed.resize(4, 4)
         |""".stripMargin
    } {
      x := d"1.5"
      y := sd"1.5"
      x := u4
      y := s4
      y := u3
    }
  }
  test("Assignment errors") {
    val x = UFix(4, 4) <> VAR
    val y = SFix(4, 4) <> VAR
    val wideF = UFix(4, 8) <> VAR
    val wideM = UFix(8, 4) <> VAR
    assertRuntimeErrorLog(
      """|The applied value's fraction width (8) is larger than the fixed-point receiver's fraction width (4) and would lose precision.
         |An explicit conversion must be applied.""".stripMargin
    ) {
      x := wideF
    }
    assertRuntimeErrorLog(
      "The applied value's magnitude width (8) is larger than the fixed-point receiver's magnitude width (4)."
    ) {
      x := wideM
    }
    assertRuntimeErrorLog(
      """|Cannot apply a signed value to an unsigned fixed-point receiver.
         |An explicit conversion must be applied.""".stripMargin
    ) {
      x := y
    }
    assertRuntimeErrorLog(
      "The Double value 0.1 requires a fraction width larger than the fixed-point receiver's fraction width (4).\nUse an explicit `M.F'` formatted literal to opt into rounding."
    ) {
      x := 0.1
    }
    assertRuntimeErrorLog(
      "The Double value 100.0 requires a magnitude width of at least 7, but the fixed-point receiver's magnitude width is 4."
    ) {
      x := 100.0
    }
  }
end DFFixSpec
