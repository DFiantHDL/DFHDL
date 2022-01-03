import DFiant.*
import munit.*

class DFVectorSpec extends DFSpec:
  test("Assignment") {
    assertCodeString(
      """|val v1 = DFUInt(8).X(5) <> VAR init Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val v2 = DFUInt(8).X(6) <> VAR
         |val x = DFUInt(8) <> VAR
         |v1 := Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |v1 := Vector(x, x, x, x, x)
         |val c = Vector(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val t1 = v1 == c
         |val c = Vector(x, x, x, x, x)
         |val t2 = v1 != c
         |val t3 = v1(3)
         |val i = DFUInt(3) <> VAR
         |val i2 = DFUInt(4) <> VAR
         |val t4 = v1(i)
         |""".stripMargin
    ) {
      val v1 = DFUInt(8).X(5) <> VAR init Vector.fill(5)(d"8'22")
      val v2 = DFUInt(8).X(6) <> VAR
      val x = DFUInt(8) <> VAR
      v1 := Vector.fill(5)(d"8'22")
      v1 := Vector.fill(5)(x)
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
    }
  }
end DFVectorSpec
