package StagesSpec

import DFiant.*
import DFiant.compiler.stages.uniqueNames
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class UniqueNames extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    val z = DFBits(8)  <> IN
    y := x
    object Temp:
      val x = DFBit  <> VAR init 1
      val Y = DFBool <> VAR init 0
    Temp.x // touch to force evaluation

  test("Unique names case-sensitive") {
    val id = (new ID).uniqueNames(Set("z"), true)
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x_0 = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z_0 = DFBits(8) <> IN
         |  y := x_0
         |  val x_1 = DFBit <> VAR init 1
         |  val Y = DFBool <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  test("Unique names case-insensitive") {
    val id = (new ID).uniqueNames(Set(), false)
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x_0 = DFSInt(16) <> IN
         |  val y_0 = DFSInt(16) <> OUT
         |  val z = DFBits(8) <> IN
         |  y_0 := x_0
         |  val x_1 = DFBit <> VAR init 1
         |  val Y_1 = DFBool <> VAR init false
         |end ID
         |""".stripMargin
    )
  }
  class SomeEnums(using DFC) extends DFDesign:
    enum MyEnumGlbl extends DFEnum:
      case Bar, Baz
    enum MyEnumLcl extends DFEnum:
      case Bar, Baz
    val x = MyEnumGlbl <> IN
    val y = MyEnumLcl  <> VAR init MyEnumLcl.Bar
    object Temp:
      enum MyEnumLcl extends DFEnum:
        case Baz, Bar
      val y = (MyEnumLcl, MyEnumLcl) <> VAR init (MyEnumLcl.Bar, MyEnumLcl.Baz)
    Temp.y // touch to force evaluation
  test("Unique names enumerations") {
    val top = (new SomeEnums).uniqueNames(Set(), true)
    assertCodeString(
      top,
      """|class SomeEnums(using DFC) extends DFDesign:
         |  val x = MyEnumGlbl <> IN
         |  val y_0 = MyEnumLcl_0 <> VAR init MyEnumLcl_0.Bar
         |  val y_1 = (MyEnumLcl_1, MyEnumLcl_1) <> VAR init (MyEnumLcl_1.Bar, MyEnumLcl_1.Baz)
         |end SomeEnums
         |""".stripMargin
    )
  }

end UniqueNames
