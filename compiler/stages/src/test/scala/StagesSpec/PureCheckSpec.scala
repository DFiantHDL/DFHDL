package StagesSpec

import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** Tests for the `PureCheck` plugin phase: elaboration is pure by default, and the phase
  * transitively synthesizes `@hw.annotation.pure(false)` (impure marking) for detectably impure
  * code, which disables elaboration caching (impure bodies always re-elaborate and only unify
  * through structural dedup).
  */
class PureCheckSpec extends StageSpec:
  test("toScalaXYZ on a design param forces synthesized impurity") {
    class ToScalaDesign extends DFDesign:
      val data = UInt(32) <> IN
      val o    = UInt(32) <> OUT

      def test(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
        arg + const.toScalaInt
      o := test(1)(data)
      o := test(10)(data)
    end ToScalaDesign
    assertCodeString(
      new ToScalaDesign,
      """|@hw.annotation.pure(false)
         |def test_0(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'1"
         |end test_0
         |
         |@hw.annotation.pure(false)
         |def test_1(const: UInt[8] <> CONST)(arg: UInt[32] <> VAL): UInt[32] <> DFRET =
         |  arg + d"32'10"
         |end test_1
         |
         |@hw.annotation.pure(false)
         |class ToScalaDesign extends DFDesign:
         |  val data = UInt(32) <> IN
         |  val o = UInt(32) <> OUT
         |  o := test_0(const = d"8'1")(data)
         |  o := test_1(const = d"8'10")(data)
         |end ToScalaDesign
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
