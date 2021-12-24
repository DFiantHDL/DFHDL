import DFiant.*
import munit.*
import core.DFMatch
import DFMatch.Pattern
import core.__For_Plugin.*

class DFMatchSpec extends DFSpec:
  val i = DFBool <> IN
  val x = DFUInt(8) <> VAR
  val e = MyEnum1 <> VAR

  e match
    case MyEnum1.Bar() =>

  test("No ret val") {
    assertCodeString(
      """|x match
         |  case d"8'77" | d"8'11" => x := d"8'1"
         |  case d"8'22" =>
         |  case _ =>
         |    x := d"8'3"
         |    x := d"8'4"
         |e match
         |  case MyEnum1.Bar() =>
         |""".stripMargin
    ) {
      x match
        case 77 | 11 =>
          x := 1
        case d"8'22" =>
        case _ =>
          x := 3
          x := 4
      e match
        case MyEnum1.Bar() =>
    }
  }
//
//  test("With ret val") {
//    assertCodeString(
//      """|val res: DFUInt[8] <> VAL =
//         |  x match
//         |    case d"4'11" => d"8'1"
//         |    case d"5'22" if i =>
//         |      x := d"8'2"
//         |      x
//         |    case _ => x
//         |""".stripMargin
//    ) {
//      val case1 =
//        (Pattern.Singleton(d"11"), None, toFunc1 { 1: (DFUInt[8] <> VAL) })
//      val case2 = (Pattern.Singleton(d"22"), Some(i), toFunc1 { x := 2; x })
//      val case3 = (Pattern.CatchAll, None, toFunc1({ x }))
//      val res = DFMatch.fromCases(x, case1 :: case2 :: case3 :: Nil)
//    }
//
//  }
end DFMatchSpec
