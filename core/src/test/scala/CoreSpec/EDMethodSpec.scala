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

  // NOTE: plugin-reported errors cannot be asserted via `assertCompileError` (it runs
  // `typeCheckErrors`, which stops at the typer — plugin phases never run). The following
  // DesignDefsPhase errors were verified manually (see the ed-methods plan):
  //   * missing explicit parameter block:
  //     "An ED method must declare an explicit parameter block. Use an empty `()`
  //      parameter block if the method has no arguments."
  //   * direct recursion: "Recursion is not allowed for ED methods."
  //   * explicit `<> CONST` argument: "Constant arguments are not supported for ED
  //     methods. ..." (an HDL subprogram takes no constant parameter; captured outer
  //     constants are supported instead, as phantom parameters)

  test("procedural ED method (task) elaboration"):
    class FooProc extends EDDesign:
      val a = UInt(8) <> IN
      def show(l: UInt[8] <> VAL): Unit <> EDRET =
        val tmp = UInt(8) <> VAR
        tmp := l
        report(s"value is $tmp")
        wait(1.ns)
      process.forever:
        show(a)
    val top = FooProc()
    val errs = dfc.getErrors
    assert(errs.isEmpty, errs.mkString("\n"))

  test("procedural ED method call is rejected outside a process"):
    assertCompileError(
      "A procedural ED method (`Unit <> EDRET`) can only be invoked inside a process or another procedural ED method body"
    )(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        def show(l: UInt[8] <> VAL): Unit <> EDRET =
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
end EDMethodSpec
