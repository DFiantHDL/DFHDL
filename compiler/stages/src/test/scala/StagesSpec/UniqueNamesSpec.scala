package StagesSpec

import DFiant.*
import DFiant.compiler.stages.uniqueNames
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueNamesSpec extends StageSpec:
  class ID extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    val z = DFBits(8)  <> IN
    y := x
    object Temp:
      val x = DFBit   <> VAR init 1
      val Y = Boolean <> VAR init 0
    Temp.x // touch to force evaluation

  test("Unique names case-sensitive") {
    val id = (new ID).uniqueNames(Set("z"), true)
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x_0 = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z_0 = DFBits(8) <> IN
         |  y := x_0
         |  val x_1 = DFBit <> VAR init 1
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
         |  val x_0 = DFSInt(16) <> IN
         |  val y_0 = DFSInt(16) <> OUT
         |  val z = DFBits(8) <> IN
         |  y_0 := x_0
         |  val x_1 = DFBit <> VAR init 1
         |  val Y_1 = Boolean <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  class SomeEnums extends DFDesign:
    enum MyEnumGlbl extends DFEnum:
      case Bar, Baz
    enum MyEnumLcl extends DFEnum:
      case Bar, Baz
    case class Pixel(x: DFUInt[8] <> VAL, y: DFUInt[8] <> VAL) extends DFStruct
    object MyByte extends DFOpaque(DFBits(8))
    val x     = MyEnumGlbl <> IN
    val y     = MyEnumLcl  <> VAR init MyEnumLcl.Bar
    val pixel = Pixel      <> VAR init Pixel(0, 0)
    val byte  = MyByte     <> VAR init all(0).as(MyByte)
    object Temp:
      enum MyEnumLcl extends DFEnum:
        case Baz, Bar
      val y = (MyEnumLcl, MyEnumLcl) <> VAR init (MyEnumLcl.Bar, MyEnumLcl.Baz)
    Temp.y // touch to force evaluation
  end SomeEnums
  test("Unique named dataflow types") {
    val top = (new SomeEnums).uniqueNames(Set(), true)
    assertCodeString(
      top,
      """|enum MyEnumGlbl(val value: DFUInt[1] <> TOKEN) extends DFEnum.Manual(1):
         |  case Bar extends MyEnumGlbl(d"1'0")
         |  case Baz extends MyEnumGlbl(d"1'1")
         |
         |class SomeEnums extends DFDesign:
         |  object MyByte extends DFOpaque(DFBits(8))
         |  enum MyEnumLcl_0(val value: DFUInt[1] <> TOKEN) extends DFEnum.Manual(1):
         |    case Baz extends MyEnumLcl_0(d"1'0")
         |    case Bar extends MyEnumLcl_0(d"1'1")
         |  enum MyEnumLcl_1(val value: DFUInt[1] <> TOKEN) extends DFEnum.Manual(1):
         |    case Bar extends MyEnumLcl_1(d"1'0")
         |    case Baz extends MyEnumLcl_1(d"1'1")
         |  final case class Pixel(
         |      x: DFUInt[8] <> VAL
         |      y: DFUInt[8] <> VAL
         |  ) extends DFStruct
         |
         |  val x = MyEnumGlbl <> IN
         |  val y_0 = MyEnumLcl_1 <> VAR init MyEnumLcl_1.Bar
         |  val pixel = Pixel <> VAR init Pixel(x = d"8'0", y = d"8'0")
         |  val byte = MyByte <> VAR init h"00".as(MyByte)
         |  val y_1 = (MyEnumLcl_0, MyEnumLcl_0) <> VAR init (MyEnumLcl_0.Bar, MyEnumLcl_0.Baz)
         |end SomeEnums
         |""".stripMargin
    )
  }

end UniqueNamesSpec
