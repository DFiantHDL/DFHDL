package CoreSpec
import dfhdl.*
import munit.*

class DFDoubleSpec extends DFSpec:
  test("Inlined width") {
    Double.verifyWidth(64)
  }

  test("Assignment") {
    assertCodeString(
      """|val d1 = Double <> VAR
         |val d2 = Double <> VAR
         |d1 := 1.0
         |d2 := d1
         |d1 := 3.14159
         |d2 := -2.718
         |""".stripMargin
    ) {
      val d1 = Double <> VAR
      val d2 = Double <> VAR
      d1 := 1.0
      d2 := d1
      d1 := 3.14159
      d2 := -2.718
    }
  }

  test("Arithmetic Operations") {
    val d1 = Double <> VAR
    val d2 = Double <> VAR
    assertCodeString(
      """|val t1 = d1 + d2
         |val t2 = d1 - 2.0
         |val t3 = 3.0 * d2
         |val t4 = d1 / d2
         |val t5 = -d1
         |val t6 = d1 max d2
         |val t7 = d1 min d2
         |val t8: Double <> CONST = 1.0
         |val t9: Double <> CONST = -2.5
         |""".stripMargin
    ) {
      val t1 = d1 + d2
      val t2 = d1 - 2.0
      val t3 = 3.0 * d2
      val t4 = d1 / d2
      val t5 = -d1
      val t6 = d1 max d2
      val t7 = d1 min d2
      val t8: Double <> CONST = 1.0
      val t9: Double <> CONST = -2.5
      assert(t8.toScalaDouble == 1.0)
      assert(t9.toScalaDouble == -2.5)
      assertCompileError(
        "Only a DFHDL constant is convertible to a Scala value, but this DFHDL value is not a constant."
      )(
        """t1.toScalaDouble"""
      )
    }
  }

  test("Comparison Operations") {
    val d1 = Double <> VAR
    val d2 = Double <> VAR
    assertCodeString(
      """|val t1 = d1 < d2
         |val t2 = d1 <= 2.0
         |val t3 = 3.0 > d2
         |val t4 = d1 >= d2
         |val t5 = d1 == d2
         |val t6 = d1 != 1.0
         |""".stripMargin
    ) {
      val t1 = d1 < d2
      val t2 = d1 <= 2.0
      val t3 = 3.0 > d2
      val t4 = d1 >= d2
      val t5 = d1 == d2
      val t6 = d1 != 1.0
    }
  }

  test("Type Conversion") {
    val d = Double <> VAR
    val b = Bits(64) <> VAR
    assertCodeString(
      """|b := d.bits
         |d := b.as(Double)
         |""".stripMargin
    ) {
      b := d.bits
      d := b.as(Double)
    }
  }
end DFDoubleSpec
