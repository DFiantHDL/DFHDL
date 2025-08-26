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

  enum BinEnum extends Encoded.Toggle:
    case Zero, One

  test("Type Construction") {}
  test("Inlined width") {
    MyEnum1.verifyWidth(2)
    MyEnum2.verifyWidth(5)
    MyEnum3.verifyWidth(3)
    MyEnum4.verifyWidth(2)
    MyEnum5.verifyWidth(8)
    BinEnum.verifyWidth(1)
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

  test("Binary enum: conversions to Bit and Boolean") {
    val e0: BinEnum <> CONST = BinEnum.Zero
    val e1: BinEnum <> CONST = BinEnum.One
    val b0: Bit <> CONST = e0.bit
    val b1: Bit <> CONST = e1.bit
    val bool0: Boolean <> CONST = e0.bool
    val bool1: Boolean <> CONST = e1.bool
    assertEquals(b0.toScalaBitNum, 0)
    assertEquals(b1.toScalaBitNum, 1)
    assertEquals(bool0.toScalaBoolean, false)
    assertEquals(bool1.toScalaBoolean, true)
  }

  test("Binary enum: conversions from Bit and Boolean") {
    val b0: Bit <> CONST = 0
    val b1: Bit <> CONST = 1
    val bool0: Boolean <> CONST = false
    val bool1: Boolean <> CONST = true
    val e0: BinEnum <> CONST = b0.as(BinEnum)
    val e1: BinEnum <> CONST = b1.as(BinEnum)
    val e0b: BinEnum <> CONST = bool0.as(BinEnum)
    val e1b: BinEnum <> CONST = bool1.as(BinEnum)
    assert((e0 == BinEnum.Zero).toScalaBoolean)
    assert((e1 == BinEnum.One).toScalaBoolean)
    assert((e0b == BinEnum.Zero).toScalaBoolean)
    assert((e1b == BinEnum.One).toScalaBoolean)
  }

  test("Binary enum: logical not operation") {
    val e0: BinEnum <> CONST = BinEnum.Zero
    val e1: BinEnum <> CONST = BinEnum.One
    assertEquals((e0.toggle).bit.toScalaBitNum, 1)
    assertEquals((e1.toggle).bit.toScalaBitNum, 0)
  }

  test("Binary enum: comparison operations") {
    val e0: BinEnum <> CONST = BinEnum.Zero
    val e1: BinEnum <> CONST = BinEnum.One
    assert((e0 == BinEnum.Zero).toScalaBoolean)
    assert((e1 == BinEnum.One).toScalaBoolean)
    assert((e0 != BinEnum.One).toScalaBoolean)
    assert((e1 != BinEnum.Zero).toScalaBoolean)
  }
end DFEnumSpec
