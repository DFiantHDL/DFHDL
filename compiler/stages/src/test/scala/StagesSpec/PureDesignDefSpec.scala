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

  // A def capturing a design-local (non-global) value gets a PHANTOM design parameter,
  // created by the harness (outside the body) and bound fresh at every call, so a pure
  // cache hit binds it like any explicit parameter. Phantoms are hidden in the design-def
  // view form, and the def declaration prints locally in the host design's body (just
  // before its first instance), so the printout matches the user-written source.
  def expectedCaptureCodeString(defAnnot: String) =
    s"""|class CaptureDesignDef extends DFDesign:
        |  val data = UInt(32) <> IN
        |  val o = UInt(32) <> OUT
        |  val localConst: UInt[32] <> CONST = d"32'42"
        |$defAnnot  def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        |    arg + localConst
        |  end test
        |  o := test(data)
        |  val x = test(data + d"32'1")
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
    assertCodeString(new CaptureDesignDef, expectedCaptureCodeString(""))
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
    assertCodeString(new CaptureDesignDef, expectedCaptureCodeString("  @hw.annotation.pure\n"))
  }

  test("design def capturing a value, called from another design def") {
    // `inner` captures `b`, a value of the DESIGN, but is called from `outer`'s body — a scope
    // that cannot reference `b` at all (outer's own design sits between them). The capture is
    // propagated inward through a phantom port of `outer`.
    class NestedCapture extends DFDesign:
      val b                                          = UInt(8) <> IN
      val a                                          = UInt(8) <> IN
      val y                                          = UInt(8) <> OUT
      def inner(l: UInt[8] <> VAL): UInt[8] <> DFRET = l + b
      def outer(l: UInt[8] <> VAL): UInt[8] <> DFRET = inner(l) + 1
      y := outer(a)
    end NestedCapture
    assertCodeString(
      new NestedCapture,
      // both defs carry phantoms, so each prints locally, just before its first call: `outer`
      // in the class body, and `inner` inside `outer`'s body (where it is first called)
      """|class NestedCapture extends DFDesign:
         |  val b = UInt(8) <> IN
         |  val a = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  def outer(l: UInt[8] <> VAL): UInt[8] <> DFRET =
         |    def inner(l: UInt[8] <> VAL): UInt[8] <> DFRET =
         |      l + b
         |    end inner
         |    inner(l) + d"8'1"
         |  end outer
         |  y := outer(a)
         |end NestedCapture
         |""".stripMargin
    )
  }
  test("design def captures propagated through two nested calls") {
    // `l1`'s captures are evaluated wherever `l1` is CALLED, so every def between the capture's
    // design and the call gets the capture too (a value as a phantom port, a constant as a
    // phantom parameter), each one binding to the next one out
    class DeepCapture extends DFDesign:
      val b                                       = UInt(8) <> IN
      val a                                       = UInt(8) <> IN
      val y                                       = UInt(8) <> OUT
      val c: UInt[8] <> CONST                     = 3
      def l1(l: UInt[8] <> VAL): UInt[8] <> DFRET = l + b + c
      def l2(l: UInt[8] <> VAL): UInt[8] <> DFRET = l1(l) + 1
      def l3(l: UInt[8] <> VAL): UInt[8] <> DFRET = l2(l) + 1
      y := l3(a)
    end DeepCapture
    assertCodeString(
      new DeepCapture,
      """|class DeepCapture extends DFDesign:
         |  val b = UInt(8) <> IN
         |  val a = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  val c: UInt[8] <> CONST = d"8'3"
         |  def l3(l: UInt[8] <> VAL): UInt[8] <> DFRET =
         |    def l2(l: UInt[8] <> VAL): UInt[8] <> DFRET =
         |      def l1(l: UInt[8] <> VAL): UInt[8] <> DFRET =
         |        l + b + c
         |      end l1
         |      l1(l) + d"8'1"
         |    end l2
         |    l2(l) + d"8'1"
         |  end l3
         |  y := l3(a)
         |end DeepCapture
         |""".stripMargin
    )
  }
end PureDesignDefSpec
