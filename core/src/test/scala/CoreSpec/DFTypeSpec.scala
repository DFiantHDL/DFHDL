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
  enum MyEnum1 extends Encode:
    case Foo, Bar, Baz
  enum MyEnum2 extends Encode.StartAt(20):
    case Foo, Bar, Baz
  enum MyEnum3 extends Encode.OneHot:
    case Foo, Bar, Baz
  enum MyEnum4 extends Encode.Grey:
    case Foo, Bar, Baz
  enum MyEnum5(val value: UInt[8] <> TOKEN) extends Encode.Manual(8):
    case Foo extends MyEnum5(200)
    case Bar extends MyEnum5(100)
    case Baz extends MyEnum5(0)
  case class MyStruct(x: UInt[5] <> VAL, y: Bits[4] <> VAL) extends Struct
  case class MyOpaque() extends Opaque(u7)

  test("Inlined width") {
    val a: Inlined[8] = b8.width
    assert(b8.width.value == 8)
    val b: Inlined[1] = bit.width
    assert(bit.width.value == 1)
    val c: Inlined[1] = bool.width
    assert(bool.width.value == 1)
    val d: Inlined[10] = tpl.width
    assert(tpl.width.value == 10)
    val e: Inlined[80] = vec_b8x10.width
    assert(vec_b8x10.width.value == 80)
    val f: Inlined[2] = MyEnum1.width
    assert(MyEnum1.width.value == 2)
    val g: Inlined[5] = MyEnum2.width
    assert(MyEnum2.width.value == 5)
    val h: Inlined[3] = MyEnum3.width
    assert(MyEnum3.width.value == 3)
    val i: Inlined[2] = MyEnum4.width
    assert(MyEnum4.width.value == 2)
    val j: Inlined[8] = MyEnum5.width
    assert(MyEnum5.width.value == 8)
    val k: Inlined[9] = MyStruct.width
    assert(MyStruct.width.value == 9)
    val l: Inlined[7] = MyOpaque.width
    assert(MyOpaque.width.value == 7)
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
