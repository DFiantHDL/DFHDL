package CoreSpec
import dfhdl.*
import munit.*

class DFMatchSpec extends DFSpec:
  enum MyEnum1 extends Encoded:
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
         |p match
         |  case Pixel(x = d"8'1", y = d"8'2") =>
         |end match
         |p match
         |  case Pixel(x = d"8'1") =>
         |end match
         |p match
         |  case Pixel(y = d"8'2") =>
         |end match
         |p match
         |  case Pixel(_, d"8'2") =>
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
        case _           =>
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

      p match
        case Pixel(x = 1, y = 2) =>

      p match
        case Pixel(x = 1) =>

      p match
        case Pixel(y = 2) =>

      p match
        case Pixel(_, 2) =>

      PixelB(Pixel(x, x), x) match
        case PixelB(Pixel(1, 2), 3) =>

      (Pixel(x, x), x) match
        case (Pixel(1, 2), 3) =>

      val Pixel(t10, 55) = p.runtimeChecked

      val PixelB(Pixel(t11, t12), 55) = pB.runtimeChecked

      val h"DEAD${t13: B[32]}BEEF" = y.runtimeChecked

      val h"DE${t14: B[16]}ADBE${t15: B[16]}EF" = y.runtimeChecked
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
    import dfhdl.hw.flag.scalaRanges
    assertCodeString("") {
      val (ret, _) = (0 until 8).foldLeft[(Byte <> VAL, Byte <> VAL)]((all(0), all(0))) {
        case ((p, a), _) =>
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

  test("signed literal pattern on an unsigned selector"):
    assertPluginError(
      "Cannot compare a signed literal value with an unsigned DFHDL variable.\nAn explicit conversion must be applied."
    )(
      """
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        x match
          case -1 =>
          case _  =>
      """
    )

  test("wider Int literal pattern than the selector"):
    assertPluginError(
      "Cannot compare a DFHDL value (width = 8) with a Scala `Int` argument that is wider (width = 9).\nAn explicit conversion must be applied."
    )(
      """
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        x match
          case 300 =>
          case _   =>
      """
    )

  test("unsupported literal pattern for the selector type"):
    assertPluginError(
      "Unsupported literal 5 for the DFHDL variable type dfhdl.core.DFBit"
    )(
      """
      class Foo extends DFDesign:
        val x = Bit <> VAR
        x match
          case 5 =>
          case _ =>
      """
    )

  test("tuple pattern arity mismatch"):
    assertPluginError(
      "The number of patterns in the pattern (3) tuple does not match the number of fields in the selector (2)"
    )(
      """
      class Foo extends DFDesign:
        val t = (UInt(8), Bit) <> VAR
        t match
          case (a, b, c) =>
          case _         =>
      """
    )

  test("tuple pattern on a non-tuple selector"):
    assertPluginError(
      "Found a tuple pattern but the match selector is not a tuple."
    )(
      """
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        x match
          case (a, b) =>
          case _      =>
      """
    )

  test("`all` pattern on a non-Bits selector"):
    assertPluginError(
      "`all` pattern is allowed for a Bits DFHDL value only."
    )(
      """
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        x match
          case all(0) =>
          case _      =>
      """
    )

  test("wrong enum entry pattern"):
    assertPluginError(
      """|Wrong enum entry type.
         |Expecting: MyEnum1
         |Found: (MyEnum2.Foo2 : MyEnum2)""".stripMargin
    )(
      """
      enum MyEnum1 extends Encoded:
        case Foo, Bar, Baz
      enum MyEnum2 extends Encoded:
        case Foo2, Bar2, Baz2
      class Foo extends DFDesign:
        val e = MyEnum1 <> VAR
        e match
          case MyEnum2.Foo2 =>
          case _            =>
      """
    )

  test("enum pattern on a non-enum selector"):
    assertPluginError(
      "Found an enum pattern but the match selector is not an enum."
    )(
      """
      enum MyEnum1 extends Encoded:
        case Foo, Bar, Baz
      class Foo extends DFDesign:
        val x = UInt(8) <> VAR
        x match
          case MyEnum1.Foo =>
          case _           =>
      """
    )

  test("string interpolation pattern on an unsupported selector"):
    assertPluginError(
      "String interpolation pattern is only allowed for Bits, UInt, or SInt DFHDL values."
    )(
      """
      class Foo extends DFDesign:
        val b = Boolean <> VAR
        b match
          case b"1" =>
          case _    =>
      """
    )

  test("string interpolation value extraction on an SInt selector"):
    assertPluginError(
      "Value extraction with a string interpolation pattern is only allowed for Bits or UInt DFHDL values."
    )(
      """
      class Foo extends DFDesign:
        val s = SInt(8) <> VAR
        s match
          case h"A${v: B[4]}" =>
          case _              =>
      """
    )

  test("string interpolation bind without a Bits annotation"):
    assertPluginError(
      "The bind `v` must have a Bits value type annotation `: B[<width>]`"
    )(
      """
      class Foo extends DFDesign:
        val y = Bits(8) <> VAR
        y match
          case h"A$v" =>
          case _      =>
      """
    )

  test("string interpolation pattern width mismatch"):
    assertPluginError(
      "Cannot compare a value of 8 bits width (LHS) to a value of 6 bits width (RHS).\nAn explicit conversion must be applied."
    )(
      """
      class Foo extends DFDesign:
        val y = Bits(8) <> VAR
        y match
          case b"1010${v: B[2]}" =>
          case _                 =>
      """
    )

  test("invalid struct pattern for the selector"):
    assertPluginError(
      "Invalid pattern of type PixelB for the given selector."
    )(
      """
      case class Pixel(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
      case class PixelB(x: UInt[8] <> VAL, y: UInt[8] <> VAL) extends Struct
      class Foo extends DFDesign:
        val p = Pixel <> VAR
        p match
          case PixelB(a, b) =>
          case _            =>
      """
    )
end DFMatchSpec
