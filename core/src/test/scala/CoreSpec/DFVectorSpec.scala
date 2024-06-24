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
         |v2 := Vector(v1(0), v1(1), v1(2), v1(3), v1(4), v2(0))
         |val t1 = v1 == all(d"8'22")
         |val t2 = v1 != all(x)
         |val t3 = v1(3)
         |val i = UInt(3) <> VAR
         |val i2 = UInt(4) <> VAR
         |val t4 = v1(i.toInt)
         |val v3 = UInt(8) X 4 X 4 <> VAR
         |v3 := all(all(d"8'0"))
         |v3(3)(1) := d"8'25"
         |v3 := v3
         |val len: Int <> CONST = 3
         |val v4 = UInt(8) X len <> VAR init all(d"8'0")
         |val v5: UInt[4] X len <> CONST = all(d"4'0")
         |val v6 = UInt(4) X len <> VAR init v5
         |v6 := all(d"4'0")
         |val zeroP: Int <> CONST = 0
         |""".stripMargin
    ) {
      val v1 = UInt(8) X 5 <> VAR init Vector.tabulate(5)(22 + _)
      val v2 = UInt(8) X 6 <> VAR init all(d"8'55")
      val x = UInt(8) <> VAR
      v1 := Vector.fill(5)(d"8'22")
      v1 := List.fill(5)(x)
      v2 := all(d"8'55")
      assertRuntimeErrorLog(
        "The argument vector length (5) is different than the receiver vector length (6)."
      ) {
        v2 := v1.elements
      }
      v2 := v1.elements.appended(v2(0))
      assertRuntimeErrorLog(
        "The argument vector length (5) is different than the receiver vector length (6)."
      ) {
        val comp = v2 == v1.elements
      }
      val t1 = v1 == List.fill(5)(d"8'22")
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
      val len: Int <> CONST = 3
      val v4 = UInt(8) X len <> VAR init all(0)
      val v5: UInt[4] X len.type <> CONST = all(0)
      val v6 = UInt(4) X len <> VAR init v5
      v6 := all(0)
      val zero = 0
      assertDSLError(
        "The vector length must be positive but found: 0"
      )(
        """val v7 = UInt(4) X 0 <> VAR"""
      ) {
        val v7 = UInt(4) X zero <> VAR
      }
      assertDSLError(
        "The vector length must be positive but found: 0"
      )(
        """val v7 = UInt(4) X 0 X 5 <> VAR"""
      ) {
        val v7 = UInt(4) X zero X 5 <> VAR
      }
      assertCompileError(
        "The vector length must be positive but found: 0"
      )(
        """val v7: UInt[4] X 0 <> CONST = all(0)"""
      )
      // TODO: does not compile, since (zero.type <> CONST) is not considered for vector composition.
      // Attempts to fix it resulted in match type failure to reduce. Maybe in the future this can be
      // resolved.
      // assertRuntimeError(
      //   "The vector length must be positive but found: 0"
      // ){
      //   val v7: UInt[4] X zero.type <> CONST = all(0)
      // }
      val zeroP: Int <> CONST = 0
      assertRuntimeError(
        "The vector length must be positive but found: 0"
      ) {
        val v7: UInt[4] X zeroP.type <> CONST = all(0)
      }
      assertRuntimeError(
        "The vector length must be positive but found: 0"
      ) {
        val v7: UInt[4] X zeroP.type X 5 <> CONST = all(all(0))
      }
    }
  }
  test("Big Endian Packed Order") {
    val v: (Bits[8] X 4) <> CONST = Vector(h"12", h"34", h"56", h"78")
    val b = h"12345678"
    assert((b == v.bits).toScalaBoolean)
    assert((b.as(Bits[8] X 4) == v).toScalaBoolean)
  }
end DFVectorSpec
