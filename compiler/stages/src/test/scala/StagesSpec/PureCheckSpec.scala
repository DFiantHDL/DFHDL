package StagesSpec

import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** Tests for the `PureCheck` plugin phase: elaboration is pure by default, and the phase
  * transitively synthesizes `@hw.annotation.pure(false)` (impure marking) for detectably impure
  * code, which disables elaboration caching (impure bodies always re-elaborate and only unify
  * through structural dedup).
  */
class PureCheckSpec extends StageSpec:
  test("toScalaXYZ forcing rooted at a design param marks the param, not the design") {
    class ToScalaDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + const.toScalaInt
      o := test(1)(data)
      o := test(10)(data)
      o := test(1)(data)
    end ToScalaDesign
    // The forced data derives from the design param `const`, so only that PARAM is marked
    // data-impure, recorded by name on the def's own annotation (`pure(true, "const")`,
    // printed at the declaration), and the design def stays pure and cacheable: the cache
    // key gains the impure param's applied data. Different applied values elaborate separate
    // bodies (test_0/test_1 with their folded constants), while a repeated value hits the
    // cache (the third call reuses test_0).
    assertCodeString(
      new ToScalaDesign,
      """|@hw.annotation.pure(impureParams = "const")
         |def test_0(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test_0
         |
         |@hw.annotation.pure(impureParams = "const")
         |def test_1(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'10"
         |end test_1
         |
         |class ToScalaDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(const = d"8'1")(data)
         |  o := test_1(const = d"8'10")(data)
         |  o := test_0(const = d"8'1")(data)
         |end ToScalaDesign
         |""".stripMargin
    )
  }

  test("forcing rooted at multiple design params marks them all") {
    class MultiDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(const1: UInt[8] <> CONST, const2: UInt[8] <> CONST)(
          arg: UInt[32] <> VAL
      ): UInt[32] <> DFRET =
        val sum = const1 + const2
        arg + sum.toScalaInt
      o := test(1, 2)(data)
      o := test(1, 3)(data)
      o := test(1, 2)(data)
    end MultiDesign
    // The forced value traces through the local `sum` to BOTH params, so both are marked
    // and both applied values join the cache key: (1,2) and (1,3) elaborate separate
    // bodies, while the repeated (1,2) application hits the cache (the third call reuses
    // test_0).
    assertCodeString(
      new MultiDesign,
      """|@hw.annotation.pure(impureParams = "const1", "const2")
         |def test_0(
         |    const1: UInt[8] <> CONST,
         |    const2: UInt[8] <> CONST
         |)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  val sum: UInt[8] <> CONST = const1 + const2
         |  arg + d"32'3"
         |end test_0
         |
         |@hw.annotation.pure(impureParams = "const1", "const2")
         |def test_1(
         |    const1: UInt[8] <> CONST,
         |    const2: UInt[8] <> CONST
         |)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  val sum: UInt[8] <> CONST = const1 + const2
         |  arg + d"32'4"
         |end test_1
         |
         |class MultiDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(
         |      const1 = d"8'1",
         |      const2 = d"8'2"
         |  )(data)
         |  o := test_1(
         |      const1 = d"8'1",
         |      const2 = d"8'3"
         |  )(data)
         |  o := test_0(
         |      const1 = d"8'1",
         |      const2 = d"8'2"
         |  )(data)
         |end MultiDesign
         |""".stripMargin
    )
  }

  test("toScalaXYZ forcing rooted at per-instance data keeps design-level impurity") {
    class CaptureDesign(offset: Int) extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      val localConst: UInt[8] <> CONST                  = 5 + offset
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + localConst.toScalaInt
      o := test(data)
    end CaptureDesign
    // The forced data traces through the captured `localConst` to the Scala constructor
    // argument `offset`, per-instance data that is not part of any cache key (forcing
    // bypasses member referencing, so no auto-param is created either), so the attribution
    // falls back to design-level impurity. Note: a capture whose definition is fully
    // code-determined (e.g. initialized by a literal) traces as PURE instead.
    assertCodeString(
      new CaptureDesign(1),
      """|@hw.annotation.pure(false)
         |def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'6"
         |end test
         |
         |@hw.annotation.pure(false)
         |class CaptureDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val localConst: UInt[8] <> CONST = d"8'6"
         |  o := test(data)
         |end CaptureDesign
         |""".stripMargin
    )
  }

  test("toScalaXYZ forcing rooted at an input's type stays fully pure") {
    class WidthDesign extends DFDesign:
      val data = Bits(32) <> IN
      val o    = UInt(32) <> OUT

      def test(arg: Bits[32] <> VAL): UInt[32] <> DFRET =
        arg.uint + arg.width.toScalaInt
      o := test(data)
      o := test(data)
    end WidthDesign
    // The forced data derives from the input's TYPE (its width), which is already part of
    // the cache key (input DFTypes), so nothing is marked and both calls share one cached
    // body.
    assertCodeString(
      new WidthDesign,
      """|def test(arg: Bits[32] <> VAL): UInt[32] <> DFRET =
         |  arg.uint + d"32'32"
         |end test
         |
         |class WidthDesign extends DFDesign:
         |  val data = Bits(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test(data)
         |  o := test(data)
         |end WidthDesign
         |""".stripMargin
    )
  }

  test("param forcing propagates through nested design def application") {
    class NestedDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def inner(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + const.toScalaInt
      def outer(const2: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        inner(const2)(arg)
      o := outer(1)(data)
      o := outer(10)(data)
    end NestedDesign
    // `inner` forces its own param `const`, so that param is marked data-impure on `inner`'s
    // annotation. `outer` applies its own param `const2` to the impure param of `inner`, so
    // the forcing propagates: `const2` is marked on `outer` as well and joins `outer`'s cache
    // key. Both defs remain design-level pure, and each applied value gets its own
    // correctly-folded body.
    assertCodeString(
      new NestedDesign,
      """|@hw.annotation.pure(impureParams = "const")
         |def inner_0(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end inner_0
         |
         |@hw.annotation.pure(impureParams = "const2")
         |def outer_0(const2: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  inner_0(const = const2)(arg)
         |end outer_0
         |
         |@hw.annotation.pure(impureParams = "const")
         |def inner_1(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'10"
         |end inner_1
         |
         |@hw.annotation.pure(impureParams = "const2")
         |def outer_1(const2: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  inner_1(const = const2)(arg)
         |end outer_1
         |
         |class NestedDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := outer_0(const2 = d"8'1")(data)
         |  o := outer_1(const2 = d"8'10")(data)
         |end NestedDesign
         |""".stripMargin
    )
  }

  test("outer var access forces synthesized impurity") {
    class VarDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      var offset: Int                                   = 1
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        val r = arg + offset
        offset = offset + 10
        r
      o := test(data)
      o := test(data)
    end VarDesign
    assertCodeString(
      new VarDesign,
      """|@hw.annotation.pure(false)
         |def test_0(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test_0
         |
         |@hw.annotation.pure(false)
         |def test_1(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'11"
         |end test_1
         |
         |@hw.annotation.pure(false)
         |class VarDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(data)
         |  o := test_1(data)
         |end VarDesign
         |""".stripMargin
    )
  }

  test("blacklisted calls force synthesized impurity") {
    class BlacklistDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        val unused = scala.util.Random.nextInt(1) // deterministic 0, structure unaffected
        arg + 1
      o := test(data)
      o := test(data)
    end BlacklistDesign
    assertCodeString(
      new BlacklistDesign,
      """|@hw.annotation.pure(false)
         |def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test
         |
         |@hw.annotation.pure(false)
         |class BlacklistDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test(data)
         |  o := test(data)
         |end BlacklistDesign
         |""".stripMargin
    )
  }

  test("impurity propagates transitively through helper methods") {
    class TransitiveDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def amount: Int = scala.util.Random.nextInt(1) + 5 // deterministic 5
      def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + amount
      o := test(data)
    end TransitiveDesign
    assertCodeString(
      new TransitiveDesign,
      """|@hw.annotation.pure(false)
         |def test(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'5"
         |end test
         |
         |@hw.annotation.pure(false)
         |class TransitiveDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test(data)
         |end TransitiveDesign
         |""".stripMargin
    )
  }

  test("explicit @pure overrides the detection (trusted, cached; user's responsibility)") {
    class PureOverrideDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      @hw.annotation.pure
      def test(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + const.toScalaInt
      o := test(1)(data)
      o := test(10)(data)
    end PureOverrideDesign
    // The override makes the def cacheable despite the forced parameter data: the second
    // call HITS the cache and shares the first call's body (folded d"32'1"), while its
    // instance still applies its own parameter value. This mismatch is exactly the
    // documented contract: overriding the detection makes correctness the user's
    // responsibility.
    assertCodeString(
      new PureOverrideDesign,
      """|@hw.annotation.pure
         |def test(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test
         |
         |class PureOverrideDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test(const = d"8'1")(data)
         |  o := test(const = d"8'10")(data)
         |end PureOverrideDesign
         |""".stripMargin
    )
  }

  test("impurity is inherited through design class inheritance") {
    abstract class ImpureBase extends DFDesign:
      def seed: Int = scala.util.Random.nextInt(1) // deterministic 0, marks the base impure
    class Sub extends ImpureBase:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT
      o := data + seed
    assertCodeString(
      new Sub,
      """|@hw.annotation.pure(false)
         |class Sub extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := data + d"32'0"
         |end Sub
         |""".stripMargin
    )
  }

  test("impurity of a sub-design poisons the design instance hierarchy upwards") {
    class ImpureChild extends DFDesign:
      val data   = UInt(32) <> IN
      val o      = UInt(32) <> OUT
      val unused = scala.util.Random.nextInt(1) // deterministic 0, marks the child impure
      o := data
    class ParentDesign extends DFDesign:
      val data  = UInt(32) <> IN
      val o     = UInt(32) <> OUT
      val child = new ImpureChild
      child.data <> data
      o          := child.o
    assertCodeString(
      new ParentDesign,
      """|@hw.annotation.pure(false)
         |class ImpureChild extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := data
         |end ImpureChild
         |
         |@hw.annotation.pure(false)
         |class ParentDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  val child = ImpureChild()
         |  child.data <> data
         |  o := child.o
         |end ParentDesign
         |""".stripMargin
    )
  }
end PureCheckSpec
