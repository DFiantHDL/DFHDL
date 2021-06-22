import DFiant.*
import munit.*
import internals.Inlined
import compiler.printing.{Printer, DefaultPrinter}
import scala.annotation.internal.sharable
import collection.immutable.ListMap

class DFTypeSpec extends FunSuite:
  val b8 = DFBits(8)
  val u7 = DFUInt(7)
  val s5 = DFSInt(5)
  val bit = DFBit
  val bool = DFBool
//  val tpl = (bool, bit, b8)
  val vec_b8x10 = b8.X(10)
  enum MyEnum1 extends DFEncoding.Default:
    case Foo, Bar, Baz
  enum MyEnum2 extends DFEncoding.StartAt(20):
    case Foo, Bar, Baz
  enum MyEnum3 extends DFEncoding.OneHot:
    case Foo, Bar, Baz
  enum MyEnum4 extends DFEncoding.Grey:
    case Foo, Bar, Baz
  enum MyEnum5(val value: BigInt) extends DFEncoding.Manual(8):
    case Foo extends MyEnum5(200)
    case Bar extends MyEnum5(100)
    case Baz extends MyEnum5(0)

  test("Inlined width") {
    val a: Inlined.Int[8] = b8.width
    assert(b8.width.value == 8)
    val b: Inlined.Int[1] = bit.width
    assert(bit.width.value == 1)
    val c: Inlined.Int[1] = bool.width
    assert(bool.width.value == 1)
//    val d: Inlined.Int[10] = tpl.width
//    assert(tpl.width.value == 10)
    val e: Inlined.Int[80] = vec_b8x10.width
    assert(vec_b8x10.width.value == 80)
    val f: Inlined.Int[2] = MyEnum1.width
    assert(MyEnum1.width.value == 2)
    val g: Inlined.Int[5] = MyEnum2.width
    assert(MyEnum2.width.value == 5)
    val h: Inlined.Int[3] = MyEnum3.width
    assert(MyEnum3.width.value == 3)
    val i: Inlined.Int[2] = MyEnum4.width
    assert(MyEnum4.width.value == 2)
    val j: Inlined.Int[8] = MyEnum5.width
    assert(MyEnum5.width.value == 8)
    val k: Inlined.Int[7] = u7.width
    assert(u7.width.value == 7)
    val l: Inlined.Int[5] = s5.width
    assert(s5.width.value == 5)
  }

  given Printer = DefaultPrinter
  test("codeString") {
    assertEquals(b8.codeString, "DFBits(8)")
    assertEquals(bit.codeString, "DFBit")
    assertEquals(bool.codeString, "DFBool")
//    assertEquals(tpl.dfType.codeString, "(DFBool, DFBit, DFBits(8))")
    assertEquals(vec_b8x10.codeString, "DFBits(8).X(10)")
    assertEquals(MyEnum1.dfType.codeString, "MyEnum1")
    assertEquals(MyEnum2.dfType.codeString, "MyEnum2")
    assertEquals(MyEnum3.dfType.codeString, "MyEnum3")
    assertEquals(MyEnum4.dfType.codeString, "MyEnum4")
    assertEquals(MyEnum5.dfType.codeString, "MyEnum5")
  }

  test("Enumeration Entries") {
    assertEquals(
      MyEnum1.dfType.asIR.entries,
      ListMap("Foo" -> BigInt(0), "Bar" -> BigInt(1), "Baz" -> BigInt(2))
    )
    assertEquals(
      MyEnum2.dfType.asIR.entries,
      ListMap("Foo" -> BigInt(20), "Bar" -> BigInt(21), "Baz" -> BigInt(22))
    )
    assertEquals(
      MyEnum3.dfType.asIR.entries,
      ListMap("Foo" -> BigInt(1), "Bar" -> BigInt(2), "Baz" -> BigInt(4))
    )
    assertEquals(
      MyEnum4.dfType.asIR.entries,
      ListMap("Foo" -> BigInt(0), "Bar" -> BigInt(1), "Baz" -> BigInt(3))
    )
    assertEquals(
      MyEnum5.dfType.asIR.entries,
      ListMap("Foo" -> BigInt(200), "Bar" -> BigInt(100), "Baz" -> BigInt(0))
    )
  }
