package CoreSpec
import dfhdl.*
import munit.*

class DFMatchSpec extends DFSpec:
  enum MyEnum1 extends Encode:
    case Foo, Bar, Baz

  case class Pixel(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
  case class PixelB(xy: Pixel <> VAL, z: UInt[8] <> VAL) extends Struct
  val i = Boolean <> IN
  val x = UInt(8) <> VAR
  val e = MyEnum1 <> VAR
  val y = Bits(64) <> VAR
  val p = Pixel <> VAR
  val pB = PixelB <> VAR

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
         |end match
         |e match
         |  case MyEnum1.Bar =>
         |end match
         |(x, e) match
         |  case (d"8'0", MyEnum1.Bar) =>
         |  case (v, MyEnum1.Baz) if v > d"8'20" =>
         |  case (v, MyEnum1.Baz) if v < d"8'15" =>
         |end match
         |y match
         |  case h"DEAD${secret: B[32]}BEEF" =>
         |  case h"DE${secret1: B[16]}AD${secret2: B[16]}BEEF" =>
         |  case h"0000000000000000" =>
         |  case h"ffffffffffffffff" =>
         |end match
         |Pixel(x = x, y = x) match
         |  case Pixel(d"8'1", d"8'2") =>
         |end match
         |p match
         |  case Pixel(d"8'1", d"8'2") =>
         |end match
         |PixelB(xy = Pixel(x = x, y = x), z = x) match
         |  case PixelB(Pixel(d"8'1", d"8'2"), d"8'3") =>
         |end match
         |(Pixel(x = x, y = x), x) match
         |  case (Pixel(d"8'1", d"8'2"), d"8'3") =>
         |end match
         |val t10: UInt[8] <> VAL =
         |  p match
         |    case Pixel(t10, d"8'55") => t10
         |  end match
         |val t11 = UInt(8) <> VAR
         |t11 := ?
         |val t12 = UInt(8) <> VAR
         |t12 := ?
         |pB match
         |  case PixelB(Pixel(_t11, _t12), d"8'55") =>
         |    t11 := _t11
         |    t12 := _t12
         |end match
         |val t13: Bits[32] <> VAL =
         |  y match
         |    case h"DEAD${t13: B[32]}BEEF" => t13
         |  end match
         |val t14 = Bits(16) <> VAR
         |t14 := h"????"
         |val t15 = Bits(16) <> VAR
         |t15 := h"????"
         |y match
         |  case h"DE${_t14: B[16]}ADBE${_t15: B[16]}EF" =>
         |    t14 := _t14
         |    t15 := _t15
         |end match
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
        case MyEnum1.Bar =>
      (x, e) match
        case (0, MyEnum1.Bar)           =>
        case (v, MyEnum1.Baz) if v > 20 =>
        case (v, MyEnum1.Baz) if v < 15 =>

      y match
        case h"DEAD${secret: B[32]}BEEF"                   =>
        case h"DE${secret1: B[16]}AD${secret2: B[16]}BEEF" =>
        case all(0)                                        =>
        case all(true)                                     =>

      Pixel(x, x) match
        case Pixel(1, 2) =>

      p match
        case Pixel(1, 2) =>

      PixelB(Pixel(x, x), x) match
        case PixelB(Pixel(1, 2), 3) =>

      (Pixel(x, x), x) match
        case (Pixel(1, 2), 3) =>

      val Pixel(t10, 55) = p: @unchecked

      val PixelB(Pixel(t11, t12), 55) = pB: @unchecked

      val h"DEAD${t13: B[32]}BEEF" = y: @unchecked

      val h"DE${t14: B[16]}ADBE${t15: B[16]}EF" = y: @unchecked
    }
  }

  test("With ret val") {
    assertCodeString(
      """|val res: UInt[8] <> VAL =
         |  x match
         |    case d"8'0" | d"8'1" | d"8'2" | d"8'3" => d"8'77"
         |    case _ => d"8'22"
         |  end match
         |val res2 = UInt(8) <> VAR
         |res2 := ((
         |  x match
         |    case d"8'0" | d"8'1" | d"8'2" | d"8'3" => d"8'77"
         |    case _ => d"8'22"
         |  end match
         |): UInt[8] <> VAL)""".stripMargin
    ) {
      val res: UInt[8] <> VAL =
        x match
          case 0 | 1 | 2 | 3 => 77
          case _             => 22
      val res2 = UInt(8) <> VAR
      res2 := x match
        case 0 | 1 | 2 | 3 => 77
        case _             => 22
    }
  }

  test("Trivial tuple match skip") {
    assertCodeString("") {
      val (ret, _) =
        (0 until 8).foldLeft[(Byte <> VAL, Byte <> VAL)]((all(0), all(0))) { case ((p, a), _) =>
          (p, a)
        }
    }
  }

  test("Different return widths error") {
    assertRuntimeErrorLog(
      """|This DFHDL `match` expression has different return types for cases.
         |These are its branch types in order:
         |Bits(2)
         |Bits(3)
         |""".stripMargin
    ) {
      val res: Bits[Int] <> VAL =
        i match
          case 0 => b"11"
          case 1 => b"111"
    }
  }
end DFMatchSpec
