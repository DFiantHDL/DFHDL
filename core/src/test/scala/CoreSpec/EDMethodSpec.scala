package CoreSpec
import dfhdl.*
import munit.*

class EDMethodSpec extends DFSpec:
  test("ED function elaboration"):
    class FooFn extends EDDesign:
      val a = UInt(8) <> IN
      val b = UInt(8) <> IN
      val y = UInt(8) <> OUT
      val z = UInt(8) <> OUT
      def add(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
        val tmp = UInt(8) <> VAR
        tmp := l + r
        tmp
      def zero(): UInt[8] <> EDRET = d"8'0"
      y <> add(a, b)
      process(all):
        z := add(a, b) + zero()
    val top = FooFn()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("ED function phantom capture"):
    class FooCap extends EDDesign:
      val a = UInt(8) <> IN
      val b = UInt(8) <> IN
      val y = UInt(8) <> OUT
      // `b` is not an explicit argument — it is captured as a phantom input
      def addB(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + b
      y <> addB(a)
    // two instances of the enclosing design: the pure cache hits for the second
    // instance, yet each instance's phantom must connect to its own `b`
    class Wrapper extends EDDesign:
      val i1 = UInt(8) <> IN
      val i2 = UInt(8) <> IN
      val o1 = UInt(8) <> OUT
      val o2 = UInt(8) <> OUT
      val f1 = FooCap()
      val f2 = FooCap()
      f1.a <> i1
      f1.b <> i2
      o1 <> f1.y
      f2.a <> i1
      f2.b <> i2
      o2 <> f2.y
    val top = Wrapper()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("valid ED method snippet reports no plugin errors"):
    assertPluginError("No error found")(
      """
      class Ok extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def add(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET = l + r
        y <> add(a, a)
      """
    )

  test("ED method without an explicit parameter block"):
    assertPluginError(
      "An ED method must declare an explicit parameter block. Use an empty `()` parameter block if the method has no arguments."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def zero: UInt[8] <> EDRET = d"8'0"
        y <> zero
      """
    )

  test("ED method direct recursion"):
    assertPluginError(
      "Recursion is not allowed for ED methods."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def rec(): UInt[8] <> EDRET = rec()
        y <> rec()
      """
    )

  test("ED method with a constant argument"):
    assertPluginError(
      """|Constant arguments are not supported for ED methods.
         |The `c` argument is a `<> CONST` value, which an ED method cannot take as a parameter.
         |Use a `<> VAL` argument instead, reference a constant declared outside the method, or declare a static function (`<> CONSTRET`).""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def bad(l: UInt[8] <> VAL, c: UInt[8] <> CONST): UInt[8] <> EDRET = l + c
        y <> bad(a, d"8'1")
      """
    )

  test("process block inside an ED method body"):
    assertPluginError(
      "Process blocks are not allowed inside an ED method."
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          process(all) {}
          l
        y <> f(a)
      """
    )

  test("design instance inside an ED method body"):
    assertPluginError(
      "Design instances are not allowed inside an ED method. Only calls to other ED methods and to static functions are."
    )(
      """
      class Inner extends EDDesign:
        val x = UInt(8) <> IN
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          val i = Inner()
          l
        y <> f(a)
      """
    )

  test("DF method call inside an ED method body"):
    assertPluginError(
      "Design instances are not allowed inside an ED method. Only calls to other ED methods and to static functions are."
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def df(l: UInt[8] <> VAL): UInt[8] <> DFRET = l + 1
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET = df(l)
        y <> f(a)
      """
    )

  test("non-blocking assignment inside an ED function body"):
    assertPluginError(
      "Non-blocking assignments `:==` are not allowed inside an ED function."
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          val v = UInt(8) <> VAR
          v :== l
          v
        y <> f(a)
      """
    )

  test("domain block inside an ED method body"):
    assertPluginError(
      "This construct is not allowed inside an ED method."
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          val d = new RTDomain {}
          l
        y <> f(a)
      """
    )

  test("ambiguous captured value name"):
    assertPluginError(
      """|Ambiguous captured value name `a` in a DFHDL method.
         |Every captured external value must have a name distinct from the method's arguments and from other captured values.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def f(a: UInt[8] <> VAL): UInt[8] <> EDRET = a + this.a
        y <> f(a)
      """
    )

  test("procedural ED method (task) elaboration"):
    class FooProc extends EDDesign:
      val a = UInt(8) <> IN
      def show(l: UInt[8] <> IN): Unit <> EDRET =
        val tmp = UInt(8) <> VAR
        tmp := l
        report(s"value is $tmp")
        wait(1.ns)
      process:
        show(a)
    val top = FooProc()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("procedural ED method with an OUT argument elaboration"):
    class FooOut extends EDDesign:
      val a = UInt(8) <> IN
      val y = UInt(8) <> OUT
      def addOne(l: UInt[8] <> IN, o: UInt[8] <> OUT): Unit <> EDRET =
        o := l + 1
      process:
        addOne(a, y)
    val top = FooOut()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("procedural ED method with a non-blocking OUT.NB argument elaboration"):
    class FooOutNB extends EDDesign:
      val a = UInt(8) <> IN
      val y = UInt(8) <> OUT
      def addOne(l: UInt[8] <> IN, o: UInt[8] <> OUT.NB): Unit <> EDRET =
        o :== l + 1
      process:
        addOne(a, y)
    val top = FooOutNB()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("non-blocking `:==` to a copy-out OUT argument is rejected"):
    assertPluginError(
      "Non-blocking assignments `:==` are not allowed inside an ED method, except to an `<> OUT.NB` output argument."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def store(o: UInt[8] <> OUT): Unit <> EDRET =
          o :== d"8'0"
      """
    )

  test("procedural ED method call is rejected outside a process"):
    assertCompileError(
      "A procedural ED method (`Unit <> EDRET`) can only be invoked inside a process or another procedural ED method body"
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        def show(l: UInt[8] <> IN): Unit <> EDRET =
          report("hello")
        show(a)
      """
    )

  test("non-blocking assignment is rejected inside an ED function body"):
    assertCompileError(
      "Non-blocking assignments `:==` are only allowed inside a process under an event-driven (ED) domain.\nChange the assignment to a connection `<>` or place it in a process."
    )(
      """
      class Foo extends EDDesign:
        val y = UInt(8) <> OUT
        def bad(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          val tmp = UInt(8) <> VAR
          tmp :== l
          tmp
        y <> bad(y)
      """
    )

  test("ED function call is rejected outside the ED domain"):
    assertCompileError(
      "An ED method can only be invoked inside an event-driven (ED) domain."
    )(
      """
      class Foo extends RTDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def add(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET = l + r
        y := add(a, a)
      """
    )

  test("valid procedure with an IN argument reports no plugin errors"):
    assertPluginError("No error found")(
      """
      class Ok extends EDDesign:
        val a = UInt(8) <> IN
        def show(l: UInt[8] <> IN): Unit <> EDRET =
          report(s"value is $l")
        process:
          show(a)
      """
    )

  test("procedure argument must be IN or OUT, not VAL"):
    assertPluginError(
      """|A procedural ED method's arguments must be `<> IN` or `<> OUT`.
         |The `l` argument is a `<> VAL`, which is only valid for a function (a non-`Unit` return).
         |Use `<> IN` for an input the call reads, or `<> OUT` for an output the call writes.""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        def show(l: UInt[8] <> VAL): Unit <> EDRET =
          report(s"value is $l")
        process:
          show(a)
      """
    )

  test("ED function argument must be VAL, not a directional port"):
    assertPluginError(
      """|An ED function's arguments must be `<> VAL`.
         |The `l` argument is a directional port (`<> IN`/`<> OUT`), which is only valid for a procedure (a `Unit` return).""".stripMargin
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val y = UInt(8) <> OUT
        def add(l: UInt[8] <> IN): UInt[8] <> EDRET = l + 1
        y <> add(a)
      """
    )

end EDMethodSpec
