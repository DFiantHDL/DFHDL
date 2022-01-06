import DFiant.*
import munit.*

class DFVectorSpec extends DFSpec:
  test("Assignment") {
    assertCodeString(
      """|val v1 = DFUInt(8).X(5) <> VAR init Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val v2 = DFUInt(8).X(6) <> VAR init Vector(d"8'55", d"8'55", d"8'55", d"8'55", d"8'55", d"8'55")
         |val x = DFUInt(8) <> VAR
         |v1 := Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |v1 := Vector(x, x, x, x, x)
         |v2 := Vector(d"8'55", d"8'55", d"8'55", d"8'55", d"8'55", d"8'55")
         |val t1 = v1 == Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val t2 = v1 != Vector(x, x, x, x, x)
         |val t3 = v1(3)
         |val i = DFUInt(3) <> VAR
         |val i2 = DFUInt(4) <> VAR
         |val t4 = v1(i)
         |val v3 = DFUInt(8).X(4).X(4) <> VAR
         |v3 := Vector(Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"), Vector(d"8'0", d"8'0", d"8'0", d"8'0"))
         |v3(3)(1) := d"8'25"
         |""".stripMargin
    ) {
      val v1 = DFUInt(8).X(5) <> VAR init Vector.fill(5)(d"8'22")
      val v2 = DFUInt(8).X(6) <> VAR init all(d"8'55")
      val x = DFUInt(8) <> VAR
      v1 := Vector.fill(5)(d"8'22")
      v1 := Vector.fill(5)(x)
      v2 := all(d"8'55")
      val t1 = v1 == Vector.fill(5)(d"8'22")
      val t2 = v1 != Vector.fill(5)(x)
      val t3 = v1(3)
      val i = DFUInt(3) <> VAR
      val i2 = DFUInt(4) <> VAR
      val t4 = v1(i)
      assertCompileError(
        "The index width 4 is different than the expected width of the vector address 3"
      )(
        """v1(i2)"""
      )
      assertCompileError(
        "Index 5 is out of range of width/length 5"
      )(
        """v1(5)"""
      )
      val v3 = DFUInt(8).X(4).X(4) <> VAR
      v3 := all(all(0))
      v3(3)(1) := 25
    }
  }
end DFVectorSpec
