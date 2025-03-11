package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropTimedRTWaits

class DropTimedRTWaitsSpec extends StageSpec():
  test("wait with time duration") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      process:
        x.din := 1
        1.sec.wait
        x.din := 0
        2.ms.wait
    end Foo
    val top = (new Foo).dropTimedRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  process:
         |    x.din := 1
         |    50000000.cy.wait
         |    x.din := 0
         |    100000.cy.wait
         |end Foo""".stripMargin
    )
  }
  test("ED timed waits are not dropped") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      process:
        x :== 1
        1.sec.wait
        x :== 0
        2.ms.wait
    end Foo
    val top = (new Foo).dropTimedRTWaits
    assertCodeString(
      top,
      """|class Foo extends EDDesign:
         |  val x = Bit <> OUT
         |  process:
         |    x :== 1
         |    1.sec.wait
         |    x :== 0
         |    2.ms.wait
         |end Foo""".stripMargin
    )
  }
end DropTimedRTWaitsSpec
