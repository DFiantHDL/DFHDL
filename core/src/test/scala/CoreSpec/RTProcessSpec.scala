package CoreSpec
import dfhdl.*
import munit.*

class RTProcessSpec extends NoDFCSpec:
  test("valid RT process steps report no plugin errors"):
    assertPluginError("No error found")(
      """
      class Foo extends RTDesign:
        val x = Bit <> IN
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            y.din := 0
            if (x) S1 else S0
          def S1: Step =
            y.din := 1
            S0
          S0
      """
    )

  test("non-step def inside an RT process"):
    assertPluginError(
      "Unexpected register-transfer (RT) process `def` syntax. Must be `def xyz: Step = ...`"
    )(
      """
      class Foo extends RTDesign:
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            y.din := 0
            S0
          def helper: Unit = {}
          S0
      """
    )

  test("def inside onEntry"):
    assertPluginError(
      "onEntry/onExit/fallThrough must not contain any other `def`s."
    )(
      """
      class Foo extends RTDesign:
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            def onEntry =
              def nested: Unit = {}
              y.din := 1
            y.din := 0
            S0
          S0
      """
    )

  test("step goto inside onEntry"):
    assertPluginError(
      "onEntry/onExit/fallThrough `def`s cannot have `wait` or step goto statements."
    )(
      """
      class Foo extends RTDesign:
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            def onEntry =
              y.din := 1
              S0
            y.din := 0
            S0
          S0
      """
    )

  test("onEntry with arguments"):
    assertPluginError(
      "`def onEntry` must not have arguments."
    )(
      """
      class Foo extends RTDesign:
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            def onEntry() =
              y.din := 1
            y.din := 0
            S0
          S0
      """
    )

  test("fallThrough with a non-Boolean return"):
    assertPluginError(
      "`def fallThrough` must return a DFHDL Boolean or Bit value."
    )(
      """
      class Foo extends RTDesign:
        val y = Bit <> OUT.REG init 0
        val fsm = process:
          def S0: Step =
            def fallThrough = d"8'0"
            y.din := 0
            S0
          S0
      """
    )

  test("Java Object.wait call"):
    assertPluginError(
      "Did you mean to call DFHDL's `wait`? If so, use a bare `wait` (endless), `<time>.wait`, or `wait(<time>)` instead (e.g., `5.ns.wait` or `wait(5.ns)`).\nDid you mean to call Java's `wait`? if so, use `java_wait` instead."
    )(
      """
      class Foo extends EDDesign:
        val y = Bit <> OUT
        process:
          y := 1
          this.wait(100)
      """
    )

  private val fallThroughPosErr =
    "`FALL_THROUGH` must mark a loop directly: write it as the whole `while` condition, " +
      "`while (FALL_THROUGH(cond))`, or as the `for` range, `for (i <- FALL_THROUGH(range))`."

  test("FALL_THROUGH on a while condition and on a for range"):
    assertPluginError("No error found")(
      """
      class Foo extends RTDesign:
        val go = Bit <> IN
        val n = Int <> IN
        val x = Bit <> OUT.REG init 0
        process:
          while (FALL_THROUGH(go))
            x.din := !x
            1.cy.wait
          for (i <- FALL_THROUGH(0 until n))
            x.din := !x
            1.cy.wait
      """
    )

  // a `for` guard becomes a plain `if` inside the loop body, so it is a body predicate with no
  // loop to mark: skipping the filtered iterations for free would need an unbounded number of
  // iterator increments in one cycle
  test("FALL_THROUGH on a for-comprehension guard"):
    assertPluginError(fallThroughPosErr)(
      """
      class Foo extends RTDesign:
        val p = Bit <> IN
        val x = Bit <> OUT.REG init 0
        process:
          for (i <- 0 until 4 if FALL_THROUGH(p))
            x.din := !x
            1.cy.wait
      """
    )

  test("FALL_THROUGH on part of a while condition"):
    assertPluginError(fallThroughPosErr)(
      """
      class Foo extends RTDesign:
        val a = Bit <> IN
        val b = Bit <> IN
        val x = Bit <> OUT.REG init 0
        process:
          while (FALL_THROUGH(a) && b)
            x.din := !x
            1.cy.wait
      """
    )

  test("FALL_THROUGH reaching its loop through a val"):
    assertPluginError(fallThroughPosErr)(
      """
      class Foo extends RTDesign:
        val go = Bit <> IN
        val x = Bit <> OUT.REG init 0
        process:
          val c = FALL_THROUGH(go)
          while (c)
            x.din := !x
            1.cy.wait
      """
    )
end RTProcessSpec
