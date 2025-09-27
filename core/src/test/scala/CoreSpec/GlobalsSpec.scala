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
