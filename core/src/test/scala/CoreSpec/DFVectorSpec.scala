package CoreSpec
import dfhdl.*
import munit.*

class DFVectorSpec extends DFSpec:
  test("Assignment") {
    assertCodeString(
      """|val v1 = UInt(8) X 5 <> VAR init Vector(d"8'22", d"8'23", d"8'24", d"8'25", d"8'26")
         |val v2 = UInt(8) X 6 <> VAR init all(d"8'55")
         |val x = UInt(8) <> VAR
         |v1 := all(d"8'22")
         |v1 := all(x)
         |v2 := all(d"8'55")
         |v2 := Vector(v1(0), v1(1), v1(2), v1(3), v1(4))
         |val t1 = v1 == all(d"8'22")
         |val t2 = v1 != all(x)
         |val t3 = v1(3)
         |val i = UInt(3) <> VAR
         |val i2 = UInt(4) <> VAR
         |val t4 = v1(i)
         |val v3 = UInt(8) X 4 X 4 <> VAR
         |v3 := all(all(d"8'0"))
         |v3(3)(1) := d"8'25"
         |v3 := v3
         |""".stripMargin
    ) {
      val v1 = UInt(8) X 5 <> VAR init Vector.tabulate(5)(d"8'22" + _)
      val v2 = UInt(8) X 6 <> VAR init all(d"8'55")
      val x = UInt(8) <> VAR
      v1 := Vector.fill(5)(d"8'22")
      v1 := Vector.fill(5)(x)
      v2 := all(d"8'55")
      v2 := v1.elements
      val t1 = v1 == Vector.fill(5)(d"8'22")
      val t2 = v1 != Vector.fill(5)(x)
      val t3 = v1(3)
      val i = UInt(3) <> VAR
      val i2 = UInt(4) <> VAR
      val t4 = v1(i)
      assertCompileError(
        "The argument vector length (6) is different than the receiver vector length (5)."
      )(
        """v1 := v2"""
      )
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
