package CoreSpec
import dfhdl.*

class ConnectAssignSpec extends NoDFCSpec:
  test("OPEN assignment is not allowed") {
    class Foo extends DFDesign:
      val x = Bit <> OUT
      assertCompileError("OPEN cannot be used here")(
        """x := OPEN"""
      )
    val foo = Foo()
  }
