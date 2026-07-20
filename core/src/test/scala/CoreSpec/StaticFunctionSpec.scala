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
        acc.asInstanceOf[UInt[8] <> CONST]
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

  test("static function with a non-constant argument"):
    assertPluginError(
      """|Non-constant arguments are not supported for static functions.
         |The `n` argument is a `<> VAL` value, but every value in a static function is constant.
         |Use a `<> CONST` argument instead.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def twice(n: UInt[8] <> VAL): UInt[8] <> CONSTRET = n + n
        y <> twice(d"8'2")
      """
    )

  test("static function with a Unit return"):
    assertPluginError(
      "A static function must return a value. A `Unit` return type (a procedure) is not supported with `<> CONSTRET`."
    )(
      """
      class Foo extends EDDesign:
        def bad(n: UInt[8] <> CONST): Unit <> CONSTRET = ???
      """
    )

  test("static function with a non-constant capture"):
    assertPluginError(
      """|Non-constant captured values are not supported for static functions.
         |The captured `a` value is not a `<> CONST`, but every value in a static function is constant.
         |Capture a constant instead, or pass it in as a `<> CONST` argument.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = 
          val t = n + a
          1
        y <> f(d"8'1")
      """
    )

  test("static function direct recursion"):
    assertPluginError(
      "Recursion is not allowed for static functions."
    )(
      """
      class Foo extends RTDesign:
        val o = UInt(8) <> OUT
        def rec(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = rec(n)
        o <> rec(d"8'1")
      """
    )

  test("ED method call inside a static function body"):
    assertPluginError(
      "ED method calls are not allowed inside a static function. A static function is callable from any domain, so it may only call other static functions."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def em(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + 1
        def sf(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          val t = em(n)
          n + n
        y <> sf(d"8'1")
      """
    )

  test("process block inside a static function body"):
    assertPluginError(
      "Process blocks are not allowed inside a static function."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def sf(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          process {}
          n + n
        y <> sf(d"8'1")
      """
    )

  test("design instance inside a static function body"):
    assertPluginError(
      "Design instances are not allowed inside a static function. Only calls to other static functions are."
    )(
      """
      class Inner extends EDDesign:
        val x = UInt(8) <> IN
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def sf(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          val i = Inner()
          n + n
        y <> sf(d"8'1")
      """
    )

  test("static function with an impure body"):
    assertPluginError(
      """|A static function (`<> CONSTRET`) must be pure, and this one's elaboration depends on an effect.
         |Effects are randomness, IO, time, system state, a `var` declared outside the function, or a call to an impure definition.
         |Note that CAPTURED CONSTANTS are pure: they become phantom design parameters and only enter the elaboration cache key.""".stripMargin
    )(
      """
      class Foo extends RTDesign:
        val o = UInt(8) <> OUT
        def bad(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
          val x = scala.util.Random.nextInt(8)
          n + x
        o <> bad(d"8'3")
      """
    )
end StaticFunctionSpec
