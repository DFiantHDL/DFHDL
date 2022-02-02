package StagesSpec

import DFiant.*
import DFiant.compiler.stages.explicitNamedVars
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitNamedVarsSpec extends StageSpec:
  test("Basic named variable") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z = x + 1
      y := z
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = DFSInt(16) <> VAR
         |  z := x + sd"2'1"
         |  y := z
         |end ID
         |""".stripMargin
    )
  }
  test("Named conditional expression") {
    class ID(using DFC) extends DFDesign:
      val x = DFSInt(16) <> IN
      val y = DFSInt(16) <> OUT
      val z: DFSInt[16] <> VAL =
        if (x > 0) 5
        else if (x < 0) x + 1
        else x
      val z2: DFSInt[16] <> VAL =
        z match
          case 1 | 2 => 17
          case _     => z + 12
      y := z2
    val id = (new ID).explicitNamedVars
    assertCodeString(
      id,
      """|class ID(using DFC) extends DFDesign:
         |  val x = DFSInt(16) <> IN
         |  val y = DFSInt(16) <> OUT
         |  val z = DFSInt(16) <> VAR
         |  if (x > d"16'0") z := sd"16'5"
         |  else if (x < d"16'0") z := x + sd"2'1"
         |  else z := x
         |  val z2 = DFSInt(16) <> VAR
         |  z match
         |    case sd"16'1" | sd"16'2" => z2 := sd"16'17"
         |    case _ => z2 := z + sd"5'12"
         |  y := z2
         |end ID
         |""".stripMargin
    )
  }
//  test("Nested named conditional expression") {
//    class ID(using DFC) extends DFDesign:
//      val x = DFSInt(16) <> IN
//      val y = DFSInt(16) <> OUT
//      val z: DFSInt[16] <> VAL =
//        if (x > 0)
//          if (x > 5) 5
//          else -5
//        else if (x < 0) x + 1
//        else x
////      val z2: DFSInt[16] <> VAL =
////        z match
////          case 1 | 2 => 17
////          case _     => z + 12
//      y := z
//    val id = (new ID).explicitNamedVars
//    assertCodeString(
//      id,
//      """|class ID(using DFC) extends DFDesign:
//         |  val x = DFSInt(16) <> IN
//         |  val y = DFSInt(16) <> OUT
//         |  val z = DFSInt(16) <> VAR
//         |  if (x > d"16'0") z := sd"16'5"
//         |  else if (x < d"16'0") z := x + sd"2'1"
//         |  else z := x
//         |  val z2 = DFSInt(16) <> VAR
//         |  z match
//         |    case sd"16'1" | sd"16'2" => z2 := sd"16'17"
//         |    case _ => z2 := z + sd"5'12"
//         |  y := z2
//         |end ID
//         |""".stripMargin
//    )
//  }

end ExplicitNamedVarsSpec
