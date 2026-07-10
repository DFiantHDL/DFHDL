package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropRTWaits

class DropRTWaitsSpec extends StageSpec():
  test("empty RT process block") {
    class Foo extends RTDesign:
      process {}
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |
         |end Foo""".stripMargin
    )
  }
  test("single statement in process block") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        x.din := i
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    x.din := i
         |end Foo""".stripMargin
    )
  }
  test("endless wait halts the FSM") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        x.din := i
        1.cy.wait
        x.din := i
        wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    x.din := i
         |    def S_1: Step =
         |      NextStep
         |    end S_1
         |    x.din := i
         |    def S_2: Step =
         |      ThisStep
         |    end S_2
         |end Foo""".stripMargin
    )
  }
  test("endless wait as the only process member") {
    class Foo extends RTDesign:
      process:
        wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |    def S_0: Step =
         |      ThisStep
         |    end S_0
         |end Foo""".stripMargin
    )
  }
  test("basic single cycle wait before assignment") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        1.cy.wait
        x.din := i
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    x.din := i
         |end Foo""".stripMargin
    )
  }
  test("basic single cycle wait after assignment") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        x.din := i
        1.cy.wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    x.din := i
         |    def S_1: Step =
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }
  test("basic multiple single cycle waits") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        x.din := 1
        x.din := i
        1.cy.wait
        x.din := !x
        1.cy.wait
        x.din := 0
        1.cy.wait
        x.din := !x
        1.cy.wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    x.din := 1
         |    x.din := i
         |    def S_1: Step =
         |      NextStep
         |    end S_1
         |    x.din := !x
         |    def S_2: Step =
         |      NextStep
         |    end S_2
         |    x.din := 0
         |    def S_3: Step =
         |      NextStep
         |    end S_3
         |    x.din := !x
         |    def S_4: Step =
         |      NextStep
         |    end S_4
         |end Foo""".stripMargin
    )
  }
  test("basic single while loop") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      process:
        while (waitCnt1 != 149)
          waitCnt1.din := waitCnt1 + 1
        waitCnt1.din := 0
        x.din := !x
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      if (waitCnt1 != d"8'149")
         |        waitCnt1.din := waitCnt1 + d"8'1"
         |        ThisStep
         |      else NextStep
         |    end S_0
         |    waitCnt1.din := d"8'0"
         |    x.din := !x
         |end Foo""".stripMargin
    )
  }
  test("basic single while loop with nested waits") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      process:
        while (waitCnt1 != 149)
          waitCnt1.din := waitCnt1 + 1
          1.cy.wait
          1.cy.wait
          1.cy.wait
        waitCnt1.din := 0
        x.din := !x
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      if (waitCnt1 != d"8'149")
         |        waitCnt1.din := waitCnt1 + d"8'1"
         |        def S_0_0: Step =
         |          NextStep
         |        end S_0_0
         |        def S_0_1: Step =
         |          NextStep
         |        end S_0_1
         |        def S_0_2: Step =
         |          NextStep
         |        end S_0_2
         |        ThisStep
         |      else NextStep
         |      end if
         |    end S_0
         |    waitCnt1.din := d"8'0"
         |    x.din := !x
         |end Foo""".stripMargin
    )
  }
  test("basic multiple while loops") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      val waitCnt2 = UInt(8) <> VAR.REG init 0
      process:
        while (waitCnt1 != 149)
          waitCnt1.din := waitCnt1 + 1
        waitCnt1.din := 0
        x.din := !x
        while (waitCnt2 != 149)
          waitCnt2.din := waitCnt2 + 1
        waitCnt2.din := 0
        x.din := 1
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  val waitCnt2 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      if (waitCnt1 != d"8'149")
         |        waitCnt1.din := waitCnt1 + d"8'1"
         |        ThisStep
         |      else NextStep
         |    end S_0
         |    waitCnt1.din := d"8'0"
         |    x.din := !x
         |    def S_1: Step =
         |      if (waitCnt2 != d"8'149")
         |        waitCnt2.din := waitCnt2 + d"8'1"
         |        ThisStep
         |      else NextStep
         |    end S_1
         |    waitCnt2.din := d"8'0"
         |    x.din := 1
         |end Foo""".stripMargin
    )
  }
  test("basic multiple while loops with fall-through") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      val waitCnt2 = UInt(8) <> VAR.REG init 0
      process:
        while (waitCnt1 != 149)
          FALL_THROUGH
          waitCnt1.din := waitCnt1 + 1
        waitCnt1.din := 0
        x.din := !x
        while (waitCnt2 != 149)
          waitCnt2.din := waitCnt2 + 1
        waitCnt2.din := 0
        x.din := 1
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  val waitCnt2 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      def fallThrough: Boolean <> VAL =
         |        !(waitCnt1 != d"8'149")
         |      end fallThrough
         |      if (waitCnt1 != d"8'149")
         |        waitCnt1.din := waitCnt1 + d"8'1"
         |        ThisStep
         |      else NextStep
         |    end S_0
         |    waitCnt1.din := d"8'0"
         |    x.din := !x
         |    def S_1: Step =
         |      if (waitCnt2 != d"8'149")
         |        waitCnt2.din := waitCnt2 + d"8'1"
         |        ThisStep
         |      else NextStep
         |    end S_1
         |    waitCnt2.din := d"8'0"
         |    x.din := 1
         |end Foo""".stripMargin
    )
  }
  test("basic while loops with nested while loops") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      val waitCnt2 = UInt(8) <> VAR.REG init 0
      process:
        while (waitCnt1 != 149)
          while (waitCnt2 != 149)
            waitCnt2.din := waitCnt2 + 1
          waitCnt2.din := 0
          x.din := !x
          waitCnt1.din := waitCnt1 + 1
        waitCnt1.din := 0
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  val waitCnt2 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      if (waitCnt1 != d"8'149")
         |        def S_0_0: Step =
         |          if (waitCnt2 != d"8'149")
         |            waitCnt2.din := waitCnt2 + d"8'1"
         |            ThisStep
         |          else NextStep
         |        end S_0_0
         |        waitCnt2.din := d"8'0"
         |        x.din := !x
         |        waitCnt1.din := waitCnt1 + d"8'1"
         |        ThisStep
         |      else NextStep
         |      end if
         |    end S_0
         |    waitCnt1.din := d"8'0"
         |end Foo""".stripMargin
    )
  }
  test("basic if condition with waits") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        if (x)
          x.din := !x
          1.cy.wait
        else
          x.din := !x
          1.cy.wait
          1.cy.wait
          1.cy.wait
        x.din := !x
        1.cy.wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    if (x)
         |      x.din := !x
         |      def S_1: Step =
         |        NextStep
         |      end S_1
         |    else
         |      x.din := !x
         |      def S_2: Step =
         |        NextStep
         |      end S_2
         |      def S_3: Step =
         |        NextStep
         |      end S_3
         |      def S_4: Step =
         |        NextStep
         |      end S_4
         |    end if
         |    x.din := !x
         |    def S_5: Step =
         |      NextStep
         |    end S_5
         |end Foo""".stripMargin
    )
  }
  test("basic if condition with while loops") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      val waitCnt1 = UInt(8) <> VAR.REG init 0
      process:
        if (x)
          x.din := !x
          while (waitCnt1 != 149)
            waitCnt1.din := waitCnt1 + 1
          waitCnt1.din := 0
        else
          x.din := !x
          1.cy.wait
          1.cy.wait
          1.cy.wait
        x.din := !x
        1.cy.wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    if (x)
         |      x.din := !x
         |      def S_1: Step =
         |        if (waitCnt1 != d"8'149")
         |          waitCnt1.din := waitCnt1 + d"8'1"
         |          ThisStep
         |        else NextStep
         |      end S_1
         |      waitCnt1.din := d"8'0"
         |    else
         |      x.din := !x
         |      def S_2: Step =
         |        NextStep
         |      end S_2
         |      def S_3: Step =
         |        NextStep
         |      end S_3
         |      def S_4: Step =
         |        NextStep
         |      end S_4
         |    end if
         |    x.din := !x
         |    def S_5: Step =
         |      NextStep
         |    end S_5
         |end Foo""".stripMargin
    )
  }
  test("basic named steps") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        x.din := 1
        def MyStep: Step =
          NextStep
        end MyStep
        1.cy.wait
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      // `x.din := 1` is an initial-convertible prologue (const RHS to a REG), so no
      // bootstrap step (S_0) is inserted; DropRTProcess lowers it into an `initial` block
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  process:
         |    x.din := 1
         |    def MyStep: Step =
         |      NextStep
         |    end MyStep
         |    def S_1: Step =
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }
  test("basic named steps with nested steps") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        def MyStep: Step =
          1.cy.wait
          def Internal: Step =
            NextStep
          end Internal
          1.cy.wait
          NextStep
        end MyStep
    end Foo
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  process:
         |    def MyStep: Step =
         |      def MyStep_0: Step =
         |        NextStep
         |      end MyStep_0
         |      def MyStep_Internal: Step =
         |        NextStep
         |      end MyStep_Internal
         |      def MyStep_2: Step =
         |        NextStep
         |      end MyStep_2
         |      NextStep
         |    end MyStep
         |end Foo""".stripMargin
    )
  }
  test("complex named steps with nested steps, loops, and waits") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        def MyStep: Step =
          1.cy.wait
          def Internal: Step =
            def Deeper: Step =
              NextStep
            end Deeper
            1.cy.wait
            NextStep
          end Internal
          1.cy.wait
          NextStep
        end MyStep
        x.din := !x
        def MyStepB: Step =
          def MyStepB_Internal: Step =
            NextStep
          end MyStepB_Internal
          1.cy.wait
          val MyWait = 1.cy.wait
          1.cy.wait
          NextStep
        end MyStepB
        x.din := !x
        val MyWhile = while (x)
          x.din := !x
          def GoGo: Step =
            NextStep
          end GoGo
          1.cy.wait
        end MyWhile
        x.din := !x
    end Foo
    // run dropRTWaits twice to test the nested step name handling (nothing should change after the first run)
    val top = (new Foo).dropRTWaits.dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  process:
         |    def MyStep: Step =
         |      def MyStep_0: Step =
         |        NextStep
         |      end MyStep_0
         |      def MyStep_Internal: Step =
         |        def MyStep_Internal_Deeper: Step =
         |          NextStep
         |        end MyStep_Internal_Deeper
         |        def MyStep_Internal_1: Step =
         |          NextStep
         |        end MyStep_Internal_1
         |        NextStep
         |      end MyStep_Internal
         |      def MyStep_2: Step =
         |        NextStep
         |      end MyStep_2
         |      NextStep
         |    end MyStep
         |    x.din := !x
         |    def MyStepB: Step =
         |      def MyStepB_Internal: Step =
         |        NextStep
         |      end MyStepB_Internal
         |      def MyStepB_1: Step =
         |        NextStep
         |      end MyStepB_1
         |      def MyStepB_MyWait: Step =
         |        NextStep
         |      end MyStepB_MyWait
         |      def MyStepB_3: Step =
         |        NextStep
         |      end MyStepB_3
         |      NextStep
         |    end MyStepB
         |    x.din := !x
         |    def MyWhile: Step =
         |      if (x)
         |        x.din := !x
         |        def MyWhile_GoGo: Step =
         |          NextStep
         |        end MyWhile_GoGo
         |        def MyWhile_1: Step =
         |          NextStep
         |        end MyWhile_1
         |        ThisStep
         |      else NextStep
         |      end if
         |    end MyWhile
         |    x.din := !x
         |end Foo""".stripMargin
    )
  }
  test("empty loop") {
    class Foo extends RTDesign:
      process:
        while (true) {}
    val top = (new Foo).dropRTWaits
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |    def S_0: Step =
         |      if (true) ThisStep
         |      else NextStep
         |    end S_0
         |end Foo""".stripMargin
    )
  }
end DropRTWaitsSpec
