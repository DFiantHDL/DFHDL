package CoreSpec
import dfhdl.*
import munit.*
import internals.Inlined
import compiler.printing.{DefaultPrinter, Printer}

import scala.annotation.internal.sharable
import collection.immutable.ListMap

class DFTypeSpec extends DFSpec:
  val b8 = Bits(8)
  val u7 = UInt(7)
  val s5 = SInt(5)
  val bit = Bit
  val bool = Boolean
  val tpl = (bool, bit, b8)
  val vec_b8x10 = b8 X 10
  enum MyEnum1 extends Encoded:
    case Foo, Bar, Baz
  enum MyEnum2 extends Encoded.StartAt(20):
    case Foo, Bar, Baz
  enum MyEnum3 extends Encoded.OneHot:
    case Foo, Bar, Baz
  enum MyEnum4 extends Encoded.Grey:
    case Foo, Bar, Baz
  enum MyEnum5(val value: UInt[8] <> CONST) extends Encoded.Manual(8):
    case Foo extends MyEnum5(200)
    case Bar extends MyEnum5(100)
    case Baz extends MyEnum5(0)
  case class MyStruct(x: UInt[5] <> VAL, y: Bits[4] <> VAL) extends Struct
  case class MyOpaque() extends Opaque(u7)

  test("Inlined width") {
    b8.verifyWidth(8)
    bit.verifyWidth(1)
    bool.verifyWidth(1)
    tpl.verifyWidth(10)
    vec_b8x10.verifyWidth(80)
    MyEnum1.verifyWidth(2)
    MyEnum2.verifyWidth(5)
    MyEnum3.verifyWidth(3)
    MyEnum4.verifyWidth(2)
    MyEnum5.verifyWidth(8)
    MyStruct.verifyWidth(9)
    MyOpaque.verifyWidth(7)
  }

  given Printer = DefaultPrinter(using dfc.getSet)
  test("codeString") {
    assertEquals(b8.codeString, "Bits(8)")
    assertEquals(bit.codeString, "Bit")
    assertEquals(tpl.dfType.codeString, "(Boolean, Bit, Bits(8))")
    assertEquals(vec_b8x10.codeString, "Bits(8) X 10")
    assertEquals(MyEnum1.dfType.codeString, "MyEnum1")
    assertEquals(MyEnum2.dfType.codeString, "MyEnum2")
    assertEquals(MyEnum3.dfType.codeString, "MyEnum3")
    assertEquals(MyEnum4.dfType.codeString, "MyEnum4")
    assertEquals(MyEnum5.dfType.codeString, "MyEnum5")
  }

  test("value modifier limitations") {
    assertCompileError(
      "Can only initialize a DFHDL port or variable that are not already initialized."
    )(
      """val x = UInt(8) <> VAR init 0 init 0"""
    )
    val y = UInt(8) <> VAR
    assertCompileError(
      "Can only initialize a DFHDL port or variable that are not already initialized."
    )(
      """(y + 1) init 0"""
    )
    val z = Bits(8) <> VAR init all(0)
    val zprev = z.prev
    assertCompileError(
      """|Value must be an initialized declaration or `.prev` must have an initialization argument.
         |E.g.: `x.prev(step, init)`.
         |It's possible to apply a bubble initialization with `init = ?`
         |""".stripMargin
    )(
      """(y + 1).prev"""
    )
    assertCompileError(
      """|Value must be an initialized declaration or `.prev` must have an initialization argument.
         |E.g.: `x.prev(step, init)`.
         |It's possible to apply a bubble initialization with `init = ?`
         |""".stripMargin
    )(
      """z(3, 0).prev"""
    )
    assertCompileError(
      "The LHS of a connection must be a connectable DFHDL value (var/port)."
    )(
      """z(3, 0) <> all(0)"""
    )
    val tplx = tpl <> VAR
    tplx._1 := 1
    assertCompileError(
      "The LHS of a connection must be a connectable DFHDL value (var/port)."
    )(
      """tplx._1 <> 1"""
    )
  }
end DFTypeSpec
