package CoreSpec
import dfhdl.*
import munit.*

// Compile-time coverage for the `DFC.Scope` capability lattice (see devdocs/scoping.md).
//
// Each scope is a PLACE that mixes capability building blocks (`HasVars`, `HasWait`, `HasTextOut`,
// ...), and every construct is guarded by summoning exactly the block it needs. These tests pin the
// resulting rejections, since a capability leak would silently ACCEPT the code rather than fail
// loudly: a broken guard shows up here as "No error found".
class ScopeChecksSpec extends DFSpec:
  private val waitErr =
    """|`wait` statements are only allowed inside a process or a procedural (task) method body.
       |They are not allowed in a design or domain body, in an `initial` block, or in a function method body.""".stripMargin

  private val textOutErr =
    """|Text output is not allowed here.
       |`print`/`println`/`report`/`assert`/`debug`/`finish` are allowed inside a design, a domain, a process, an `initial` block, or a procedural (task) method body.
       |They are NOT allowed inside a function method body, which must remain pure.""".stripMargin

  // ~~~ HasWait: only `Process` and `Procedural` have it ~~~

  test("`wait` is rejected inside an `initial` block"):
    assertCompileError(waitErr)(
      """
      class Foo extends EDDesign:
        val a = Bit <> VAR
        initial:
          a := 1
          wait
      """
    )

  test("`wait` is rejected in a design body"):
    assertCompileError(waitErr)(
      """
      class Foo extends EDDesign:
        val a = Bit <> VAR
        a := 1
        wait
      """
    )

  test("`wait(_.cy)` is rejected in an RT design body, outside a process"):
    assertCompileError(waitErr)(
      """
      class Foo extends RTDesign:
        val a = Bit <> OUT.REG
        a.din := 1
        wait(1.cy)
      """
    )

  test("`waitUntil` is rejected in a design body"):
    assertCompileError(waitErr)(
      """
      class Foo extends EDDesign:
        val a = Bit <> IN
        waitUntil(a)
      """
    )

  // `wait` IS allowed inside a process, under both ED and RT. These are the positive controls: if
  // the `HasWait` guard were too strict, the designs below would stop compiling.

  test("`wait` is allowed inside a process (ED)"):
    class Foo extends EDDesign:
      val a = Bit <> VAR
      process:
        a := 1
        wait(10.ns)
    Foo()

  test("`wait` is allowed inside a process (RT)"):
    class Foo extends RTDesign:
      val a = Bit <> OUT.REG
      process:
        a.din := 1
        wait(1.cy)
    Foo()

  // ~~~ HasTextOut: `Function` does not have it, because a function must stay pure ~~~

  test("text output is rejected inside an ED function method body"):
    assertCompileError(textOutErr)(
      """
      class Foo extends EDDesign:
        val a = UInt(8) <> IN
        val o = UInt(8) <> OUT
        def f(l: UInt[8] <> VAL): UInt[8] <> EDRET =
          println("impure")
          l
        o <> f(a)
      """
    )

  // ~~~ HasProcesses: nesting prohibitions, which must stay NEGATIVE guards ~~~
  // A positive `AssertGiven[HasProcesses]` would reach the ENCLOSING design's given and silently
  // accept these, so a regression here reads as "No error found".

  test("a process cannot be nested inside another process"):
    assertCompileError("A process cannot be nested inside another process.")(
      """
      class Foo extends EDDesign:
        val a = Bit <> VAR
        process(all):
          process(all):
            a := 1
      """
    )
end ScopeChecksSpec
