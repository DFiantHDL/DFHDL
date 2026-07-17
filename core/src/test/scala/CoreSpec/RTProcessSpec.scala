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
        process.forever:
          y := 1
          this.wait(100)
      """
    )
end RTProcessSpec
