package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.simplifyMatchSel
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class SimplifyMatchSelSpec extends StageSpec:
  test("UInt/SInt match selectors are converted to Bits match selectors"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class Foo extends DFDesign:
      val iu = UInt(8) <> IN
      val is = SInt(9) <> IN
      val o  = SInt(9) <> OUT

      iu match
        case 0 =>
        case 1 =>
        case _ =>

      is match
        case -1 =>
        case 1  =>
        case _  =>

      o := is + iu.signed
    end Foo
    val top = (new Foo).simplifyMatchSel
    assertCodeString(
      top,
      """|class Foo extends DFDesign:
         |  val iu = UInt(8) <> IN
         |  val is = SInt(9) <> IN
         |  val o = SInt(9) <> OUT
         |  val iu_slv = iu.bits
         |  iu_slv match
         |    case h"00" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |  val is_slv = is.bits
         |  is_slv match
         |    case h"9'1ff" =>
         |    case h"9'001" =>
         |    case _ =>
         |  end match
         |  o := is + iu.signed
         |end Foo""".stripMargin
    )

  test("Design-parameterized UInt/SInt match selectors are converted to Bits match selectors"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    val WG: Int <> CONST                  = 8
    class Foo(val WP: Int <> CONST = 8) extends RTDesign:
      val iu  = UInt(WP)       <> IN
      val is  = SInt(WP)       <> IN
      val iug = UInt.until(WG) <> IN
      val isg = SInt(WG)       <> IN

      iu match
        case 0 =>
        case 1 =>
        case _ =>

      is match
        case -1 =>
        case 1  =>
        case _  =>

      iug match
        case 0 =>
        case 1 =>
        case _ =>

      isg match
        case -1 =>
        case 1  =>
        case _  =>
    end Foo
    val top = (new Foo).simplifyMatchSel
    assertCodeString(
      top,
      """|val WG: Int <> CONST = 8
         |class Foo(val WP: Int <> CONST = 8) extends RTDesign:
         |  val iu = UInt(WP) <> IN
         |  val is = SInt(WP) <> IN
         |  val iug = UInt(clog2(WG)) <> IN
         |  val isg = SInt(WG) <> IN
         |  val iu_slv = iu.bits(7, 0)
         |  iu_slv match
         |    case h"00" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |  val is_slv = is.bits(7, 0)
         |  is_slv match
         |    case h"ff" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |  val iug_slv = iug.bits(2, 0)
         |  iug_slv match
         |    case b"000" =>
         |    case b"001" =>
         |    case _ =>
         |  end match
         |  val isg_slv = isg.bits
         |  isg_slv match
         |    case h"ff" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |end Foo""".stripMargin
    )

  test(
    "Bits global/local parameter selector is not dropped, but design parameter selection is dropped"
  ):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    val WG: Int <> CONST                  = 8
    class Foo(val WP: Int <> CONST = 8) extends RTDesign:
      val i                = Bits(WG) <> IN
      val i2               = Bits(WP) <> IN
      val WL: Int <> CONST = 8
      val v                = Bits(WL) <> VAR

      i match
        case h"00" =>
        case h"01" =>
        case _     =>

      i2 match
        case h"00" =>
        case h"01" =>
        case _     =>

      v match
        case h"00" =>
        case h"01" =>
        case _     =>
    end Foo
    val top = (new Foo).simplifyMatchSel
    assertCodeString(
      top,
      """|val WG: Int <> CONST = 8
         |class Foo(val WP: Int <> CONST = 8) extends RTDesign:
         |  val i = Bits(WG) <> IN
         |  val i2 = Bits(WP) <> IN
         |  val WL: Int <> CONST = 8
         |  val v = Bits(WL) <> VAR
         |  i match
         |    case h"00" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |  val i2_slv = i2(7, 0)
         |  i2_slv match
         |    case h"00" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |  v match
         |    case h"00" =>
         |    case h"01" =>
         |    case _ =>
         |  end match
         |end Foo""".stripMargin
    )
end SimplifyMatchSelSpec
