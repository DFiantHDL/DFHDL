package CoreSpec
import dfhdl.*

class GlobalsSpec extends NoDFCSpec:
  test("Global errors") {
    assertCompileError(
      "Missing local design context.\nEither this operation is not supported in global context or `using DFC` is missing."
    )(
      """val x = Bit <> VAR"""
    )
    assertCompileError(
      "Missing local design context.\nEither this operation is not supported in global context or `using DFC` is missing."
    )(
      """val x = Bit <> IN"""
    )
  }

  // regression: tuple destructuring of DFHDL values outside any design
  // context (no dfc in scope) must not crash the CustomControl plugin phase
  // (previously threw `head of empty list`). The mere fact that this test
  // compiles is the regression coverage, since the crash was in a plugin
  // phase that runs after the typer.
  test("Global tuple destructuring") {
    val tpl = (h"5a", h"a5", h"0f")
    val (a, b, c) = tpl
    assertEquals(List(a, b, c).size, 3)
  }
end GlobalsSpec
