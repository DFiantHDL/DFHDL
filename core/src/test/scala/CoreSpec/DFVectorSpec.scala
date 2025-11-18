package CoreSpec
import dfhdl.*
import munit.*

class DFVectorSpec extends DFSpec:
  test("Assignment") {
    assertCodeString(
      """|val v1 = UInt(8) X 5 <> VAR init DFVector(UInt(8) X 5)(d"8'22", d"8'23", d"8'24", d"8'25", d"8'26")
         |val v2 = UInt(8) X 6 <> VAR init all(d"8'55")
         |val x = UInt(8) <> VAR
         |v1 := DFVector(UInt(8) X 5)(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |v1 := DFVector(UInt(8) X 5)(x, x, x, x, x)
         |v2 := all(d"8'55")
         |v2 := DFVector(UInt(8) X 6)(v1(0), v1(1), v1(2), v1(3), v1(4), v2(0))
         |val t1 = v1 == DFVector(UInt(8) X 5)(d"8'22", d"8'22", d"8'22", d"8'22", d"8'22")
         |val t2 = v1 != DFVector(UInt(8) X 5)(x, x, x, x, x)
         |val t3 = v1(3)
         |val i = UInt(3) <> VAR
         |val i2 = UInt(2) <> VAR
         |val i4 = UInt(4) <> VAR
         |val t4 = v1(i.toInt)
         |val o2 = v1(i2.resize(3).toInt)
         |val o4 = v1(i4.resize(3).toInt)
         |val v3 = UInt(8) X 4 X 4 <> VAR
         |v3 := all(all(d"8'0"))
         |v3(3)(1) := d"8'25"
         |v3 := v3
         |val len: Int <> CONST = 3
         |val v4 = UInt(8) X len <> VAR init all(d"8'0")
         |val v5: UInt[4] X len.type <> CONST = all(d"4'0")
         |val v6 = UInt(4) X len <> VAR init v5
         |v6 := all(d"4'0")
         |val zeroP: Int <> CONST = 0
         |val w: Int <> CONST = 4
         |val v8: Bits[w.type] X len.type <> CONST = all(b"0".repeat(w))
         |val v9 = UInt(5) X len <> VAR
         |val v10 = UInt(4) X len <> VAR
         |v10 := v6
         |val v11 = v10(1, 2)
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
      val i2 = UInt(2) <> VAR
      val i4 = UInt(4) <> VAR
      val t4 = v1(i)
      assertCompileError(
        "The argument vector length (6) is different than the receiver vector length (5)."
      )(
        """v1 := v2"""
      )
      assertCompileError(
        """|Expected argument width 3 but found: 2
           |To Fix:
           |Use `.extend` to match the width automatically.""".stripMargin
      )(
        """v1(i2)"""
      )
      val o2 = v1(i2.extend)
      assertCompileError(
        """|Expected argument width 3 but found: 4
           |To Fix:
           |Use `.truncate` to match the width automatically.""".stripMargin
      )(
        """v1(i4)"""
      )
      val o4 = v1(i4.truncate)
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
      // Note: compiler plugin does not work inside the scala test compilation code,
      // so we need to directly apply braces to set the proper operator precedence.
      assertDSLErrorLog(
        "The vector length must be positive but found: 0"
      )(
        """val v7 = (UInt(4) X 0) <> VAR"""
      ) {
        val v7 = UInt(4) X zero <> VAR
      }
      assertDSLErrorLog(
        "The vector length must be positive but found: 0"
      )(
        """val v7 = (UInt(4) X 0 X 5) <> VAR"""
      ) {
        val v7 = UInt(4) X zero X 5 <> VAR
      }
      assertCompileError(
        "The vector length must be positive but found: 0"
      )(
        """val v7: (UInt[4] X 0) <> CONST = all(0)"""
      )
      assertRuntimeErrorLog(
        "The vector length must be positive but found: 0"
      ) {
        val v7: UInt[4] X zero.type <> CONST = all(0)
      }
      val zeroP: Int <> CONST = 0
      assertRuntimeErrorLog(
        "The vector length must be positive but found: 0"
      ) {
        val v7: UInt[4] X zeroP.type <> CONST = all(0)
      }
      assertRuntimeErrorLog(
        "The vector length must be positive but found: 0"
      ) {
        val v7: UInt[4] X zeroP.type X 5 <> CONST = all(all(0))
      }
      val w: Int <> CONST = 4
      val v8: Bits[w.type] X len.type <> CONST = all(all(0))
      val five = 5
      val v9 = UInt(five) X len <> VAR
      val v10 = UInt(five - 1) X len <> VAR
      v10 := v6
      assertRuntimeErrorLog(
        """|Vector types must be the same when applying one vector onto another.
           |Expected type: UInt(5) X len
           |Found type:    UInt(4) X len
           |""".stripMargin
      ) {
        v9 := v6
      }
      val v11 = v10(1, 2)
      assertDSLErrorLog(
        "Index 5 is out of range of width/length 5"
      )(
        """v1(1, 5)"""
      ) {
        v1(1, five)
      }
    }
  }
  test("Big Endian Packed Order") {
    val v: Bits[8] X 4 <> CONST = Vector(h"12", h"34", h"56", h"78")
    val v2: Bits[8] X Int <> CONST = Vector(h"12", h"34", h"56", h"78")
    val v3 = "Hello".toByteVector
    val v4: Bits[8] X 5 <> CONST = Vector(h"48", h"65", h"6c", h"6c", h"6f")
    val v5: Bits[8] X 3 <> CONST = Vector(h"65", h"6c", h"6c")
    val v6 = v4(1, 3)
    val b = h"12345678"
    assert((v(0) == h"12").toScalaBoolean)
    assert((v == v2).toScalaBoolean)
    assert((v3 == v4).toScalaBoolean)
    assert((v5 == v6).toScalaBoolean)
    assert(v.lengthInt == 4)
    assert(v2.length.toScalaInt == 4)
    assert((b == v.bits).toScalaBoolean)
    assert((b.as(Bits[8] X 4) == v).toScalaBoolean)
  }
  // TODO: missing address and don't care value check
  test("Vector file initialization") {
    assertCodeString(
      """|val v1 = Bits(8) X 4 <> VAR init DFVector(Bits(8) X 4)(h"18", h"24", h"42", h"81")
         |val v2 = Bits(8) X 4 <> VAR init DFVector(Bits(8) X 4)(h"??", h"??", h"??", h"??")
         |val v3 = Bits(8) X 4 <> VAR init DFVector(Bits(8) X 4)(h"18", h"24", h"42", h"81")
         |val v4 = Bits(8) X 4 <> VAR init DFVector(Bits(8) X 4)(h"00", h"00", h"00", h"00")
         |""".stripMargin
    ) {
      val v1 = Bits(8) X 4 <> VAR initFile "bits8x4.bin"
      val v2 = Bits(8) X 4 <> VAR initFile (
        "bits8x4.empty",
        InitFileFormat.VerilogBin,
        InitFileUndefinedValue.Bubbles
      )
      val v3 = Bits(8) X 4 <> VAR initFile "bits8x4.hex"
      val v4 = Bits(8) X 4 <> VAR initFile ("bits8x4.empty", InitFileFormat.VerilogBin)
      assertRuntimeErrorLog(
        """|Init file not found: bits8x4.nofile
           |make sure either to place the file in your Scala project resource folder or provide a proper relative/absolute path.
           |""".stripMargin
      ) {
        val v1 = Bits(8) X 4 <> VAR initFile "bits8x4.nofile"
      }
      assertRuntimeErrorLog(
        "Could not automatically detect the init file format of bits8x4.empty"
      ) {
        val v1 = Bits(8) X 4 <> VAR initFile "bits8x4.empty"
      }
      assertRuntimeErrorLog(
        """|Init file error detected in VerilogHex formatted bits8x4.bin:0
           |Invalid data width detected (expected 8 bits but found 17 bits): 00011000
           |""".stripMargin
      ) {
        val v1 = Bits(8) X 4 <> VAR initFile ("bits8x4.bin", InitFileFormat.VerilogHex)
      }
      assertRuntimeErrorLog(
        """|Init file error detected in VerilogBin formatted bits8x4.hex:0
           |Invalid data character detected: 18
           |""".stripMargin
      ) {
        val v1 = Bits(8) X 4 <> VAR initFile ("bits8x4.hex", InitFileFormat.VerilogBin)
      }
    }
  }
end DFVectorSpec
