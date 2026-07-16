package CoreSpec
import dfhdl.*
import munit.*

// Static functions (`T <> CONSTRET`) — see devdocs/static-domain-plan.md.
class StaticFunctionSpec extends DFSpec:
  test("static function elaboration"):
    class FooTwice extends RTDesign:
      val o = UInt(8) <> OUT
      def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = n + n
      o <> twice(d"8'3")
    val top = FooTwice()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  // decision 7: a static function body may declare and mutate local variables. They are bounded
  // by the body, so from the outside the result is still a constant.
  test("static function with a local static variable"):
    class FooSum3 extends RTDesign:
      val o = UInt(8) <> OUT
      def sum3(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
        val acc = UInt(8) <> VAR
        acc := n
        acc := acc + n
        acc := acc + n
        acc
      o <> sum3(d"8'3")
    val top = FooSum3()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  // a static function is callable from EVERY domain (its `DomainType.Static` context parameter is
  // the ambient given), unlike an ED method, whose `DomainType.ED` restricts it to ED domains
  test("static function called from a DF, an RT and an ED design"):
    def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = n + n
    class FooDF extends DFDesign:
      val o = UInt(8) <> OUT
      o <> twice(d"8'3")
    class FooRT extends RTDesign:
      val o = UInt(8) <> OUT
      o <> twice(d"8'3")
    class FooED extends EDDesign:
      val o = UInt(8) <> OUT
      o <> twice(d"8'3")
    FooDF(); FooRT(); FooED()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  // ~~~ scope guards (see devdocs/scoping.md) ~~~
  // A static function body is a `Scope.Function` body, so it lacks `HasTextOut` and `HasWait`.
  // Text output is load-bearing for purity, so its rejection must not regress.

  test("text output is rejected inside a static function body"):
    assertCompileError(
      """|Text output is not allowed here.
         |`print`/`println`/`report`/`assert`/`debug`/`finish` are allowed inside a design, a domain, a process, an `initial` block, or a procedural (task) method body.
         |They are NOT allowed inside a function method body, which must remain pure.""".stripMargin
    )(
      """
      class Foo extends RTDesign:
        val o = UInt(8) <> OUT
        def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          println("impure")
          n + n
        o <> twice(d"8'3")
      """
    )

  test("`wait` is rejected inside a static function body"):
    assertCompileError(
      """|`wait` statements are only allowed inside a process or a procedural (task) method body.
         |They are not allowed in a design or domain body, in an `initial` block, or in a function method body.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val o = UInt(8) <> OUT
        def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          wait(1.ns)
          n + n
        o <> twice(d"8'3")
      """
    )

  // NOTE: the DesignDefsPhase and PureCheckPhase errors cannot be asserted via `assertCompileError`
  // (it runs `typeCheckErrors`, which stops at the typer, so plugin phases never run). These were
  // verified manually:
  //   * a non-constant argument: "Non-constant arguments are not supported for static functions. ..."
  //   * a `Unit` return: "A static function must return a value. ..."
  //   * a captured non-constant: "Non-constant captured values are not supported for static
  //     functions. ..."
  //   * recursion: "Recursion is not allowed for static functions."
  //   * an impure body (randomness/IO/time/outer `var`/impure callee): "A static function
  //     (`<> CONSTRET`) must be pure, and this one's elaboration depends on an effect. ..."
end StaticFunctionSpec
