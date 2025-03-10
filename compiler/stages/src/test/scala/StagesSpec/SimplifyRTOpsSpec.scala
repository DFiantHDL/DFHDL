package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.simplifyRTOps

class SimplifyRTOpsSpec extends StageSpec(stageCreatesUnrefAnons = true):
  test("waitWhile to while loop transformation") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val i = Bit <> IN
      process:
        x.din := 1
        waitWhile(i)
        x.din := 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val i = Bit <> IN
         |  process:
         |    x.din := 1
         |    while (i)
         |      1.cy.wait
         |    end while
         |    x.din := 0
         |end Foo""".stripMargin
    )
  }

  test("ED domain is untouched") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      val i = Bit <> IN
      process:
        x :== 1
        waitUntil(i)
        waitUntil(i.falling)
        x :== 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends EDDesign:
         |  val x = Bit <> OUT
         |  val i = Bit <> IN
         |  process:
         |    x :== 1
         |    waitUntil(i)
         |    waitUntil(i.falling)
         |    x :== 0
         |end Foo""".stripMargin
    )
  }

  test("waitUntil to while loop and rising/falling edge transformation") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val i = Bit <> IN
      process:
        x.din := 1
        x.din := i.rising
        x.din := i.falling
        waitUntil(i)
        waitUntil(i.falling)
        waitUntil(i.rising)
        val temp = i.rising
        waitUntil(temp)
        x.din := 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val i = Bit <> IN
         |  process:
         |    x.din := 1
         |    x.din := (!i.reg(1, init = 1)) && i
         |    x.din := i.reg(1, init = 0) && (!i)
         |    while (!i)
         |      1.cy.wait
         |    end while
         |    while ((!i.reg(1, init = 0)) || i)
         |      1.cy.wait
         |    end while
         |    while (i.reg(1, init = 1) || (!i))
         |      1.cy.wait
         |    end while
         |    val temp = (!i.reg(1, init = 1)) && i
         |    while (!temp)
         |      1.cy.wait
         |    end while
         |    x.din := 0
         |end Foo""".stripMargin
    )
  }

  test("multiple waitWhile transformations") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val i = Bit <> IN
      val j = Bit <> IN
      process:
        x.din := 1
        waitWhile(i)
        x.din := 0
        waitWhile(j)
        x.din := 1
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val i = Bit <> IN
         |  val j = Bit <> IN
         |  process:
         |    x.din := 1
         |    while (i)
         |      1.cy.wait
         |    end while
         |    x.din := 0
         |    while (j)
         |      1.cy.wait
         |    end while
         |    x.din := 1
         |end Foo""".stripMargin
    )
  }
end SimplifyRTOpsSpec
