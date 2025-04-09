package CoreSpec
import dfhdl.*

class ConnectAssignSpec extends NoDFCSpec:
  test("OPEN assignment is not allowed") {
    class Foo extends DFDesign:
      val x = Bit <> OUT
      x := 1
      assertCompileError("`OPEN` cannot be used here.")(
        """x := OPEN"""
      )
    val foo = Foo()
  }

  test("NOTHING is not a constant") {
    class Foo extends DFDesign:
      assertCompileError("Applied argument must be a constant.")(
        """val x: Bit <> CONST = NOTHING"""
      )
    val foo = Foo()
  }

  test("NOTHING constraints") {
    class Foo extends RTDesign:
      val x = UInt(8) <> OUT
      val ok1 = Bits(8) <> OUT
      val ok2 = Bit <> OUT
      x := 0
      ok1 := NOTHING
      ok2 := NOTHING
      assertCompileError(
        "`NOTHING` can only be assigned to either `Bits` or `Bit` DFHDL values outside of a dataflow (DF) domain."
      )(
        """x := NOTHING"""
      )
    val foo = Foo()

    class Bar extends EDDesign:
      val x = UInt(8) <> OUT
      val ok1 = Bits(8) <> OUT
      val ok2 = Bit <> OUT
      process(all):
        x := 0
        ok1 := NOTHING
        ok2 := NOTHING
        assertCompileError(
          "`NOTHING` can only be assigned to either `Bits` or `Bit` DFHDL values outside of a dataflow (DF) domain."
        )(
          """x := NOTHING"""
        )
        assertCompileError(
          "`NOTHING` can only be assigned to either `Bits` or `Bit` DFHDL values outside of a dataflow (DF) domain."
        )(
          """x :== NOTHING"""
        )
    end Bar
    val bar = Bar()

    class Baz extends DFDesign:
      val x = UInt(8) <> OUT
      val ok1 = Bits(8) <> OUT
      val ok2 = Bit <> OUT
      ok1 := NOTHING
      ok2 := NOTHING
      x := NOTHING // also ok because this is DFDesign
    val baz = Baz()
  }
end ConnectAssignSpec
