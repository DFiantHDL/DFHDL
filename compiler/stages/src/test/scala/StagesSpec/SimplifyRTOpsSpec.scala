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
        val MyWait = waitUntil(i.falling)
        waitUntil(i.rising)
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
         |    end while
         |    val MyWait = while ((!i.reg(1, init = 0)) || i)
         |    end MyWait
         |    while (i.reg(1, init = 1) || (!i))
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
        val MyWait = waitWhile(j)
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
         |    end while
         |    x.din := 0
         |    val MyWait = while (j)
         |    end MyWait
         |    x.din := 1
         |end Foo""".stripMargin
    )
  }

  test("wait with cycle duration") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitParam: UInt[Int] <> CONST = 50000000
      process:
        x.din := 1
        50000000.cy.wait
        x.din := 0
        val MyWait = waitParam.cy.wait
        1.cy.wait
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitParam: UInt[26] <> CONST = d"26'50000000"
         |  process:
         |    x.din := 1
         |    val waitCnt = UInt(26) <> VAR.REG
         |    waitCnt.din := d"26'0"
         |    while (waitCnt != d"26'49999999")
         |      waitCnt.din := waitCnt + d"26'1"
         |    end while
         |    x.din := 0
         |    val MyWait_waitCnt = UInt(26) <> VAR.REG
         |    MyWait_waitCnt.din := d"26'0"
         |    val MyWait = while (MyWait_waitCnt != (waitParam - d"26'1"))
         |      MyWait_waitCnt.din := MyWait_waitCnt + d"26'1"
         |    end MyWait
         |    1.cy.wait
         |end Foo""".stripMargin
    )
  }
  test("RT for loop with until is converted to while loop") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      process:
        x.din := 1
        for (i <- 0 until 4)
          x.din := 0
        x.din := 1
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  process:
         |    x.din := 1
         |    val i = Int <> VAR.REG
         |    i.din := 0
         |    while (i < 4)
         |      x.din := 0
         |      i.din := i + 1
         |    end while
         |    x.din := 1
         |end Foo""".stripMargin
    )
  }

  test("RT for loop with to is converted to while loop with <= guard") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      process:
        val MyFor = for (i <- 0 to 3)
          x.din := 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  process:
         |    val MyFor_i = Int <> VAR.REG
         |    MyFor_i.din := 0
         |    val MyFor = while (MyFor_i <= 3)
         |      x.din := 0
         |      MyFor_i.din := MyFor_i + 1
         |    end MyFor
         |end Foo""".stripMargin
    )
  }

  test("RT for loop with explicit step is converted to while loop with step increment") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      process:
        for (i <- 0 until 8 by 2)
          x.din := 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  process:
         |    val i = Int <> VAR.REG
         |    i.din := 0
         |    while (i < 8)
         |      x.din := 0
         |      i.din := i + 2
         |    end while
         |end Foo""".stripMargin
    )
  }

  test("RT combinational for loop is untouched") {
    class Foo extends RTDesign:
      val x = Bits(4) <> OUT.REG
      process:
        x.din := all(0)
        for (i <- 0 until 4)
          COMB_LOOP
          x(i).din := 1
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bits(4) <> OUT.REG
         |  process:
         |    x.din := h"0"
         |    for (i <- 0 until 4)
         |      COMB_LOOP
         |      x(i).din := 1
         |    end for
         |end Foo""".stripMargin
    )
  }

  test("RT for loop outside a process is untouched") {
    class Foo(val WIDTH: Int <> CONST = 4) extends RTDesign:
      val r = Bits(WIDTH) <> OUT.REG init all(0)
      for (i <- 0 until WIDTH)
        r(i).din := 1
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo(val WIDTH: Int <> CONST = 4) extends RTDesign:
         |  val r = Bits(WIDTH) <> OUT.REG init b"0".repeat(WIDTH)
         |  for (i <- 0 until WIDTH)
         |    r(i).din := 1
         |  end for
         |end Foo""".stripMargin
    )
  }

  test("ED domain for loop is untouched") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      process:
        for (i <- 0 until 4)
          x :== 0
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends EDDesign:
         |  val x = Bit <> OUT
         |  process:
         |    for (i <- 0 until 4)
         |      x :== 0
         |    end for
         |end Foo""".stripMargin
    )
  }

  test("nested RT for loops are converted to nested while loops") {
    class Foo extends RTDesign:
      val o = Int <> OUT.REG
      process:
        for (i <- 0 until 10)
          for (j <- 0 until 10)
            o.din := i + j
    end Foo
    val top = (new Foo).simplifyRTOps
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val o = Int <> OUT.REG
         |  process:
         |    val i = Int <> VAR.REG
         |    i.din := 0
         |    while (i < 10)
         |      val j = Int <> VAR.REG
         |      j.din := 0
         |      while (j < 10)
         |        o.din := i + j
         |        j.din := j + 1
         |      end while
         |      i.din := i + 1
         |    end while
         |end Foo""".stripMargin
    )
  }

end SimplifyRTOpsSpec
