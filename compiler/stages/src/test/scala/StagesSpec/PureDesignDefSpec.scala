package StagesSpec

import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PureDesignDefSpec extends StageSpec:
  // `@hw.annotation.pure` is a performance hint (skip re-running the body), never a semantics
  // change: a pure design def must elaborate to the exact same code as its non-pure twin,
  // including the per-call applied design parameters.
  val expectedTwoCallsCodeString =
    """|def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
       |  (arg + arg) - constArg
       |end test
       |
       |class IDWithDesignDef extends DFDesign:
       |  val data = UInt(32) <> IN
       |  val o = UInt(32) <> OUT
       |  o := test(constArg = d"32'7")(data + d"32'1")
       |  val x = test(constArg = d"32'10")(data)
       |  o := x
       |end IDWithDesignDef
       |""".stripMargin

  test("baseline: non-pure design def applies per-call const params") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        (arg + arg) - constArg
      o := test(7)(data + 1)
      val x = test(10)(data)
      o := x
    end IDWithDesignDef
    assertCodeString(new IDWithDesignDef, expectedTwoCallsCodeString)
  }

  test("pure design def applies per-call const params") {
    class IDWithDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      @hw.annotation.pure
      def test(constArg: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        (arg + arg) - constArg
      o := test(7)(data + 1)
      val x = test(10)(data)
      o := x
    end IDWithDesignDef
    assertCodeString(new IDWithDesignDef, s"@hw.annotation.pure\n$expectedTwoCallsCodeString")
  }

  test("pure design def scala args are part of the cache key") {
    class ScalaArgDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      // the structure legitimately depends on the plain Scala arg `n`, so distinct `n`
      // values must elaborate distinct designs, while repeated `n` values share one
      @hw.annotation.pure
      def test(arg: UInt[32] <> VAL, n: Int): UInt[32] <> DFRET =
        if (n > 5) arg + 1 else arg - 1
      o := test(data, 1)
      o := test(data, 10)
      o := test(data, 10)
    end ScalaArgDesignDef
    assertCodeString(
      new ScalaArgDesignDef,
      """|@hw.annotation.pure
         |def test_0(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg - d"32'1"
         |end test_0
         |
         |@hw.annotation.pure
         |def test_1(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test_1
         |
         |class ScalaArgDesignDef extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(data)
         |  o := test_1(data)
         |  o := test_1(data)
         |end ScalaArgDesignDef
         |""".stripMargin
    )
  }

  // A def capturing a design-local (non-global) value gets an AUTO-created design parameter
  // (see `cloneUnreachable`). Such a parameter only comes into existence by running the body,
  // so a pure cache hit cannot bind it; the sound behavior is to treat the call as a miss and
  // re-elaborate (structural dedup still unifies the identical bodies afterwards).
  val expectedCaptureCodeString =
    """|def test(localConst: UInt[32] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
       |  arg + localConst
       |end test
       |
       |class CaptureDesignDef extends DFDesign:
       |  val data = UInt(32) <> IN
       |  val o = UInt(32) <> OUT
       |  val localConst: UInt[32] <> CONST = d"32'42"
       |  o := test(localConst = localConst)(data)
       |  val x = test(localConst = localConst)(data + d"32'1")
       |  o := x
       |end CaptureDesignDef
       |""".stripMargin

  test("baseline: non-pure design def with a captured design-local value") {
    class CaptureDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      val localConst: UInt[32] <> CONST                 = 42
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + localConst
      o := test(data)
      val x = test(data + 1)
      o := x
    end CaptureDesignDef
    assertCodeString(new CaptureDesignDef, expectedCaptureCodeString)
  }

  test("pure design def with a captured design-local value") {
    class CaptureDesignDef extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      val localConst: UInt[32] <> CONST = 42
      @hw.annotation.pure
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + localConst
      o := test(data)
      val x = test(data + 1)
      o := x
    end CaptureDesignDef
    assertCodeString(new CaptureDesignDef, s"@hw.annotation.pure\n$expectedCaptureCodeString")
  }
end PureDesignDefSpec
