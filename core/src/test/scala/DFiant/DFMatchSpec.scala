import DFiant.*
import munit.*
import core.DFMatch
import DFMatch.Pattern
import core.__For_Plugin.*

class DFMatchSpec extends DFSpec:
  enum MyEnum1 extends DFEnum.Default:
    case Foo, Bar, Baz

  case class Pixel(x: DFUInt[8] <> VAL, y: DFUInt[8] <> VAL)
  case class PixelB(xy: core.DFStruct[Pixel] <> VAL, z: DFUInt[8] <> VAL)
  val i = DFBool <> IN
  val x = DFUInt(8) <> VAR
  val e = MyEnum1 <> VAR
  val y = DFBits(64) <> VAR
  val p = Pixel <> VAR

  test("No ret val") {
    assertCodeString(
      """|x match
         |  case d"8'77" | d"8'11" => x := d"8'1"
         |  case d"8'22" =>
         |  case d"8'150" =>
         |  case d"8'34" =>
         |  case _ =>
         |    x := d"8'3"
         |    x := d"8'4"
         |e match
         |  case MyEnum1.Bar() =>
         |(x, e) match
         |  case (d"8'0", MyEnum1.Bar()) =>
         |  case (v, MyEnum1.Baz()) if v > d"8'20" =>
         |  case (v, MyEnum1.Baz()) if v < d"8'15" =>
         |y match
         |  case h"DEAD${secret: B[32]}BEEF" =>
         |  case h"DE${secret1: B[16]}AD${secret2: B[16]}BEEF" =>
         |  case h"64'0000000000000000" =>
         |  case h"64'ffffffffffffffff" =>
         |Pixel(x = x, y = x) match
         |  case Pixel(d"8'1", d"8'2") =>
         |p match
         |  case Pixel(d"8'1", d"8'2") =>
         |PixelB(xy = Pixel(x = x, y = x), z = x) match
         |  case PixelB(Pixel(d"8'1", d"8'2"), d"8'3") =>
         |(Pixel(x = x, y = x), x) match
         |  case (Pixel(d"8'1", d"8'2"), d"8'3") =>
         |""".stripMargin
    ) {
      x match
        case 77 | 11 =>
          x := 1
        case d"8'22"     =>
        case b"10010110" =>
        case h"8'22"     =>
        case _ =>
          x := 3
          x := 4

      e match
        case MyEnum1.Bar() =>
      (x, e) match
        case (0, MyEnum1.Bar())           =>
        case (v, MyEnum1.Baz()) if v > 20 =>
        case (v, MyEnum1.Baz()) if v < 15 =>

      y match
        case h"DEAD${secret: B[32]}BEEF"                   =>
        case h"DE${secret1: B[16]}AD${secret2: B[16]}BEEF" =>
        case all(0)                                        =>
        case all(1)                                        =>

      Pixel(x, x) match
        case Pixel(1, 2) =>

      p match
        case Pixel(1, 2) =>

      PixelB(Pixel(x, x), x) match
        case PixelB(Pixel(1, 2), 3) =>

      (Pixel(x, x), x) match
        case (Pixel(1, 2), 3) =>
    }
  }

  test("With ret val") {
    assertCodeString(
      """|val res: DFUInt[8] <> VAL =
         |  x match
         |    case d"8'0" | d"8'1" | d"8'2" | d"8'3" => d"8'77"
         |    case _ => d"8'22"
         |""".stripMargin
    ) {
      val res: DFUInt[8] <> VAL =
        x match
          case 0 | 1 | 2 | 3 => 77
          case _             => 22
    }

  }
end DFMatchSpec
