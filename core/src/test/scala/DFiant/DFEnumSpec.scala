import DFiant.*
import munit.*
import internals.Inlined
import collection.immutable.ListMap

class DFEnumSpec extends DFSpec:
  enum MyEnum1 extends DFEnum.Default:
    case Foo, Bar, Baz
  enum MyEnum2 extends DFEnum.StartAt(20):
    case Foo, Bar, Baz
  enum MyEnum3 extends DFEnum.OneHot:
    case Foo, Bar, Baz
  enum MyEnum4 extends DFEnum.Grey:
    case Foo, Bar, Baz
  enum MyEnum5(val value: BigInt) extends DFEnum.Manual(8):
    case Foo extends MyEnum5(200)
    case Bar extends MyEnum5(100)
    case Baz extends MyEnum5(0)

  test("Type Construction") {}
  test("Inlined width") {
    MyEnum1.width.verifyInlined(2)
    MyEnum2.width.verifyInlined(5)
    MyEnum3.width.verifyInlined(3)
    MyEnum4.width.verifyInlined(2)
    MyEnum5.width.verifyInlined(8)
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

  test("Token Construction") {
    val t1: MyEnum1 <> TOKEN = MyEnum1 token MyEnum1.Bar
    val t2: MyEnum2 <> TOKEN = MyEnum2 token MyEnum2.Bar
    assertEquals(t1.bits, DFBits(2) token b"01")
    assertEquals(t2.bits, DFBits(5) token h"5'15")
  }
  test("DFVal Conversion") {
    val t1: MyEnum1 <> TOKEN = MyEnum1.Bar
  }
  test("Assignment") {
    assertCodeString {
      """|val x = MyEnum1 <> VAR init MyEnum1.Bar
         |x := MyEnum1.Foo
         |""".stripMargin
    } {
      val x = MyEnum1 <> VAR init MyEnum1.Bar
      x := MyEnum1.Foo
    }
  }
  test("Comparison") {
    val t1Bar = MyEnum1 token MyEnum1.Bar
    val t1Baz = MyEnum1 token MyEnum1.Baz
    assertEquals(t1Bar == t1Bar, DFBool token true)
    assertEquals(t1Bar != t1Baz, DFBool token true)
    assertEquals(t1Bar != MyEnum1.Baz, DFBool token true)
  }
end DFEnumSpec
