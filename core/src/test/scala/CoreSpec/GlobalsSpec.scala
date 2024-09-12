package CoreSpec
import dfhdl.*

class GlobalsSpec extends NoDFCSpec:
  test("Global errors") {
    assertCompileError("Port/Variable declarations cannot be global")(
      """val x = Bit <> VAR"""
    )
    assertCompileError("Port/Variable declarations cannot be global")(
      """val x = Bit <> IN"""
    )
  }
