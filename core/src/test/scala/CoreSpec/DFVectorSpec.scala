package CoreSpec
import DFiant.*
import munit.*

class DFVectorSpec extends DFSpec:
  test("Assignment") {
    assertCodeString(
      """|val v1 = UInt(8) X 5 <> VAR init Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val v2 = UInt(8) X 6 <> VAR init Vector(d"8'55", d"8'55", d"8'55", d"8'55", d"8'55", d"8'55")
         |val x = UInt(8) <> VAR
         |v1 := Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |v1 := Vector(x, x, x, x, x)
         |v2 := Vector(d"8'55", d"8'55", d"8'55", d"8'55", d"8'55", d"8'55")
         |val t1 = v1 == Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val t2 = v1 != Vector(x, x, x, x, x)
         |val t3 = v1(3)
         |val i = UInt(3) <> VAR
         |val i2 = UInt(4) <> VAR
         |val t4 = v1(i)
         |val v3 = UInt(8) X 4 X 4 <> VAR
         |v3 := Vector(Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"))
         |v3(3)(1) := d"8'25"
         |v3 := v3
         |""".stripMargin
    ) {
      val v1 = UInt(8) X 5 <> VAR init Vector.fill(5)(d"8'22")
      val v2 = UInt(8) X 6 <> VAR init all(d"8'55")
      val x = UInt(8) <> VAR
      v1 := Vector.fill(5)(d"8'22")
      v1 := Vector.fill(5)(x)
      v2 := all(d"8'55")
      val t1 = v1 == Vector.fill(5)(d"8'22")
      val t2 = v1 != Vector.fill(5)(x)
      val t3 = v1(3)
      val i = UInt(3) <> VAR
      val i2 = UInt(4) <> VAR
      val t4 = v1(i)
      assertCompileError(
        "Expected argument width 3 but found: 4"
      )(
        """v1(i2)"""
      )
      assertCompileError(
        "The argument must be smaller than the upper-bound 5 but found: 5"
      )(
        """v1(5)"""
      )
      val v3 = UInt(8) X 4 X 4 <> VAR
      v3 := all(all(0))
      v3(3)(1) := 25
      val t: UInt[8] X 4 X 4 <> VAL = v3
      v3 := t
    }
  }
end DFVectorSpec
