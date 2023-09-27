package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.uniqueNames
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueNamesSpec extends StageSpec:
  class ID extends DFDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    val z = Bits(8)  <> IN
    y := x
    object Temp:
      val x = Bit     <> VAR init 1
      val Y = Boolean <> VAR init 0
    Temp.x // touch to force evaluation

  test("Unique names case-sensitive") {
    val id = (new ID).uniqueNames(Set("z"), true)
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x_0 = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val z_0 = Bits(8) <> IN
         |  y := x_0
         |  val x_1 = Bit <> VAR init 1
         |  val Y = Boolean <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  test("Unique names case-insensitive") {
    val id = (new ID).uniqueNames(Set(), false)
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x_0 = SInt(16) <> IN
         |  val y_0 = SInt(16) <> OUT
         |  val z = Bits(8) <> IN
         |  y_0 := x_0
         |  val x_1 = Bit <> VAR init 1
         |  val Y_1 = Boolean <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  class SomeEnums extends DFDesign:
    enum MyEnumGlbl extends Encode:
      case Bar, Baz
    enum MyEnumLcl extends Encode:
      case Bar, Baz
    case class Pixel(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
    case class MyByte() extends Opaque(Bits(8))
    val x     = MyEnumGlbl <> IN
    val y     = MyEnumLcl  <> VAR init MyEnumLcl.Bar
    val pixel = Pixel      <> VAR init Pixel(0, 0)
    val byte  = MyByte     <> VAR init all(0).as(MyByte)
    object Temp:
      enum MyEnumLcl extends Encode:
        case Baz, Bar
      val y = (MyEnumLcl, MyEnumLcl) <> VAR init (MyEnumLcl.Bar, MyEnumLcl.Baz)
    Temp.y // touch to force evaluation
  end SomeEnums
  test("Unique named DFHDL types") {
    val top = (new SomeEnums).uniqueNames(Set(), true)
    assertCodeString(
      top,
      """|enum MyEnumGlbl(val value: UInt[1] <> TOKEN) extends Encode.Manual(1):
         |  case Bar extends MyEnumGlbl(d"1'0")
         |  case Baz extends MyEnumGlbl(d"1'1")
         |
         |class SomeEnums extends DFDesign:
         |  enum MyEnumLcl_0(val value: UInt[1] <> TOKEN) extends Encode.Manual(1):
         |    case Bar extends MyEnumLcl_0(d"1'0")
         |    case Baz extends MyEnumLcl_0(d"1'1")
         |  final case class Pixel(
         |      x: UInt[8] <> VAL
         |      y: UInt[8] <> VAL
         |  ) extends Struct
         |  case class MyByte() extends Opaque(Bits(8))
         |  enum MyEnumLcl_1(val value: UInt[1] <> TOKEN) extends Encode.Manual(1):
         |    case Baz extends MyEnumLcl_1(d"1'0")
         |    case Bar extends MyEnumLcl_1(d"1'1")
         |
         |  val x = MyEnumGlbl <> IN
         |  val y_0 = MyEnumLcl_0 <> VAR init MyEnumLcl_0.Bar
         |  val pixel = Pixel <> VAR init Pixel(x = d"8'0", y = d"8'0")
         |  val byte = MyByte <> VAR init h"00".as(MyByte)
         |  val y_1 = (MyEnumLcl_1, MyEnumLcl_1) <> VAR init (MyEnumLcl_1.Bar, MyEnumLcl_1.Baz)
         |end SomeEnums
         |""".stripMargin
    )
  }

end UniqueNamesSpec
