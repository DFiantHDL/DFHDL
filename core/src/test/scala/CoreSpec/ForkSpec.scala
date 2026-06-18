package CoreSpec
import dfhdl.*
import munit.*

class ForkSpec extends DFSpec:
  test("forkJoinAny is rejected under RT"):
    assertCompileError(
      "forkJoinAny and forkJoinNone are only allowed under event-driven (ED) domains."
    )(
      """
      class FJ extends RTDesign:
        val a = Bit <> OUT.REG
        process:
          forkJoinAny:
            locally:
              a.din := 1
            locally:
              a.din := 0
      """
    )

  test("forkJoinNone is rejected under RT"):
    assertCompileError(
      "forkJoinAny and forkJoinNone are only allowed under event-driven (ED) domains."
    )(
      """
      class FJ extends RTDesign:
        val a = Bit <> OUT.REG
        process:
          forkJoinNone:
            locally:
              a.din := 1
            locally:
              a.din := 0
      """
    )
end ForkSpec
