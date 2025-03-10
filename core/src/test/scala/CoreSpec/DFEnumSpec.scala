package CoreSpec
import dfhdl.*
import munit.*
import collection.immutable.ListMap

class DFEnumSpec extends DFSpec:
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

  test("Type Construction") {}
  test("Inlined width") {
    MyEnum1.verifyWidth(2)
    MyEnum2.verifyWidth(5)
    MyEnum3.verifyWidth(3)
    MyEnum4.verifyWidth(2)
    MyEnum5.verifyWidth(8)
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
    val t1 = MyEnum1 <> VAR
    t1 == MyEnum1.Bar
  }
end DFEnumSpec
