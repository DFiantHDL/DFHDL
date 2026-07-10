package CoreSpec
import dfhdl.*
import munit.*

class InitialSpec extends DFSpec:
  test("initial block is rejected under DF"):
    assertCompileError(
      "An `initial` block is not supported under dataflow (DF) domains."
    )(
      """
      class Foo extends DFDesign:
        val a = Bit <> OUT
        initial:
          a := 1
      """
    )

  test("initial block is rejected inside a process"):
    assertCompileError(
      "An `initial` block cannot be nested inside a process."
    )(
      """
      class Foo extends EDDesign:
        val a = Bit <> OUT
        process(all):
          initial:
            a := 1
      """
    )

  test("initial block is rejected inside another initial block"):
    assertCompileError(
      "A process or an `initial` block cannot be nested inside an `initial` block."
    )(
      """
      class Foo extends EDDesign:
        val a = Bit <> OUT
        initial:
          initial:
            a := 1
      """
    )

  test("process is rejected inside an initial block"):
    assertCompileError(
      "A process or an `initial` block cannot be nested inside an `initial` block."
    )(
      """
      class Foo extends EDDesign:
        val a = Bit <> OUT
        initial:
          process(all):
            a := 1
      """
    )

  test("non-blocking assignment is rejected inside an initial block"):
    assertCompileError(
      "Non-blocking assignments `:==` are not allowed inside an `initial` block.\nChange the assignment to a blocking assignment `:=`."
    )(
      """
      class Foo extends EDDesign:
        val a = Bit <> VAR
        initial:
          a :== 1
      """
    )
end InitialSpec
