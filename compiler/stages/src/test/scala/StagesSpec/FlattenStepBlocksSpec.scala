package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.flattenStepBlocks
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class FlattenStepBlocksSpec extends StageSpec():

  test("single flat step") {
    class Foo extends RTDesign:
      process:
        def S_0: Step =
          NextStep
        end S_0
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |    def S_0: Step =
         |      S_0
         |    end S_0
         |end Foo""".stripMargin
    )
  }

  test("two flat steps") {
    class Foo extends RTDesign:
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          y.din := 0
          NextStep
        end S0
        def S1: Step =
          y.din := 1
          NextStep
        end S1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val y = Bit <> OUT.REG init 0
         |  process:
         |    def S0: Step =
         |      y.din := 0
         |      S1
         |    end S0
         |    def S1: Step =
         |      y.din := 1
         |      S0
         |    end S1
         |end Foo""".stripMargin
    )
  }

  test("two flat steps with inter-step statement") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        end S_0
        x.din := i
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      x.din := i
         |      S_1
         |    end S_0
         |    def S_1: Step =
         |      S_0
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("one level of nesting") {
    class Foo extends RTDesign:
      process:
        def MyStep: Step =
          def MyStep_0: Step =
            NextStep
          end MyStep_0
          NextStep
        end MyStep
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |    def MyStep_0: Step =
         |      MyStep_0
         |    end MyStep_0
         |end Foo""".stripMargin
    )
  }

  test("three flat steps with inter-step statements") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      val y = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        end S_0
        x.din := 0
        def S_1: Step =
          NextStep
        end S_1
        y.din := 1
        def S_2: Step =
          NextStep
        end S_2
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      x.din := 0
         |      S_1
         |    end S_0
         |    def S_1: Step =
         |      y.din := 1
         |      S_2
         |    end S_1
         |    def S_2: Step =
         |      S_0
         |    end S_2
         |end Foo""".stripMargin
    )
  }

  test("nested siblings with inter-step statements") {
    class Foo extends RTDesign:
      val a = Int <> OUT.REG
      val b = Int <> OUT.REG
      process:
        def S_0: Step =
          a.din := 1
          def S_0_0: Step =
            NextStep
          end S_0_0
          b.din := 2
          def S_0_1: Step =
            NextStep
          end S_0_1
          NextStep
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val a = Int <> OUT.REG
         |  val b = Int <> OUT.REG
         |  process:
         |    a.din := 1
         |    def S_0_0: Step =
         |      b.din := 2
         |      S_0_1
         |    end S_0_0
         |    def S_0_1: Step =
         |      S_1
         |    end S_0_1
         |    def S_1: Step =
         |      a.din := 1
         |      S_0_0
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("two levels of nesting with inner-to-outer statement relocation") {
    class Foo extends RTDesign:
      val a = Int <> OUT.REG
      val b = Int <> OUT.REG
      val c = Int <> OUT.REG
      process:
        def S_0: Step =
          def S_0_0: Step =
            a.din := 1
            def S_0_0_0: Step =
              NextStep
            end S_0_0_0
            b.din := 2
            NextStep
          end S_0_0
          c.din := 3
          NextStep
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val a = Int <> OUT.REG
         |  val b = Int <> OUT.REG
         |  val c = Int <> OUT.REG
         |  process:
         |    a.din := 1
         |    def S_0_0_0: Step =
         |      b.din := 2
         |      c.din := 3
         |      S_1
         |    end S_0_0_0
         |    def S_1: Step =
         |      a.din := 1
         |      S_0_0_0
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("ThisStep and FirstStep resolution") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      process:
        def S_0: Step =
          if (i)
            ThisStep
          else
            NextStep
          end if
        end S_0
        def S_1: Step =
          if (i)
            FirstStep
          else
            NextStep
          end if
        end S_1
        def S_2: Step =
          NextStep
        end S_2
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  process:
         |    def S_0: Step =
         |      if (i) S_0
         |      else S_1
         |    end S_0
         |    def S_1: Step =
         |      if (i) S_0
         |      else S_2
         |    end S_1
         |    def S_2: Step =
         |      S_0
         |    end S_2
         |end Foo""".stripMargin
    )
  }

  test("FirstStep resolves past the bootstrap step, the wrap-around does not") {
    class Foo extends RTDesign:
      val x = Bit     <> IN
      val y = UInt(8) <> OUT.REG init 0
      val z = UInt(8) <> OUT.REG init 0
      process:
        z.din := z + 1 // non-constant RHS: not initial-convertible, so a bootstrap step is added
        def Accum: Step =
          y.din := y + 1
          NextStep
        def Flush: Step =
          y.din := y + 16
          if (x) FirstStep // explicit jump: no prologue re-run
          else NextStep // wrap-around: the prologue re-runs
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      // S_boot is the synthesized bootstrap carrying the prologue. Flush's two branches are the
      // whole point: the sequential wrap-around goes through S_boot and so re-runs
      // `z.din := z + 1`, while `FirstStep` lands on Accum, the process's actual first step, and
      // re-runs neither the prologue nor the bootstrap's cycle.
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  val z = UInt(8) <> OUT.REG init d"8'0"
         |  process:
         |    def S_boot: Step =
         |      z.din := z + d"8'1"
         |      Accum
         |    end S_boot
         |    def Accum: Step =
         |      y.din := y + d"8'1"
         |      Flush
         |    end Accum
         |    def Flush: Step =
         |      y.din := y + d"8'16"
         |      if (x) Accum
         |      else S_boot
         |    end Flush
         |end Foo""".stripMargin
    )
  }

  test("FirstStep targets a generated first step, not the bootstrap") {
    class Foo extends RTDesign:
      val x = Bit     <> IN
      val y = UInt(8) <> OUT.REG init 0
      val z = UInt(8) <> OUT.REG init 0
      process:
        z.din := z + 1 // non-convertible prologue: a bootstrap step is added
        1.cy.wait // the process's first step, yielded by a wait rather than a `def`
        y.din := y + 1
        def Check: Step =
          if (x) FirstStep
          else NextStep
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      // How the first step was written has no bearing on `FirstStep`: it targets S_0, the wait's
      // step, because the bootstrap is the prologue's carrier rather than a step of the process.
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val y = UInt(8) <> OUT.REG init d"8'0"
         |  val z = UInt(8) <> OUT.REG init d"8'0"
         |  process:
         |    def S_boot: Step =
         |      z.din := z + d"8'1"
         |      S_0
         |    end S_boot
         |    def S_0: Step =
         |      y.din := y + d"8'1"
         |      Check
         |    end S_0
         |    def Check: Step =
         |      if (x) S_0
         |      else S_boot
         |    end Check
         |end Foo""".stripMargin
    )
  }

  test("step nested inside conditional branch") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      process:
        def S_0: Step =
          if (i)
            def S_0_0: Step =
              NextStep
            end S_0_0
            ThisStep
          else
            NextStep
          end if
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  process:
         |    def S_0: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_0
         |    def S_0_0: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_0_0
         |    def S_1: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("step nested inside conditional branch with inter-step statement") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        def S_0: Step =
          if (i)
            def S_0_0: Step =
              NextStep
            end S_0_0
            x.din := 1
            ThisStep
          else
            NextStep
          end if
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_0
         |    def S_0_0: Step =
         |      x.din := 1
         |      if (i) S_0_0
         |      else S_1
         |    end S_0_0
         |    def S_1: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("nested steps inside nested conditional branches") {
    class Foo extends RTDesign:
      val i = Bit <> IN
      val x = Bit <> OUT.REG
      process:
        def S_0: Step =
          if (i)
            def S_0_0: Step =
              if (i)
                def S_0_0_0: Step =
                  NextStep
                end S_0_0_0
                x.din := 1
                ThisStep
              else
                NextStep
              end if
            end S_0_0
            ThisStep
          else
            NextStep
          end if
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Bit <> IN
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_0
         |    def S_0_0: Step =
         |      if (i) S_0_0_0
         |      else
         |        if (i) S_0_0
         |        else S_1
         |      end if
         |    end S_0_0
         |    def S_0_0_0: Step =
         |      x.din := 1
         |      S_0_0
         |    end S_0_0_0
         |    def S_1: Step =
         |      if (i) S_0_0
         |      else S_1
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("onEntry and onExit blocks move with their parent step during flattening") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG
      process:
        def S_0: Step =
          def onEntry =
            x.din := 1
          end onEntry
          def S_0_0: Step =
            def onExit =
              x.din := 0
            end onExit
            NextStep
          end S_0_0
          NextStep
        end S_0
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      def onEntry: Unit =
         |        x.din := 1
         |      end onEntry
         |      S_0_0
         |    end S_0
         |    def S_0_0: Step =
         |      def onExit: Unit =
         |        x.din := 0
         |      end onExit
         |      S_1
         |    end S_0_0
         |    def S_1: Step =
         |      S_0
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test(
    "multiple inter-step statements from nested and conditional scopes collected before single NextStep"
  ) {
    class Foo extends RTDesign:
      val a = Int <> OUT.REG
      val b = Int <> OUT.REG
      val c = Int <> OUT.REG
      val i = Bit <> IN
      process:
        def S_0: Step =
          def S_0_0: Step =
            if (i)
              a.din := 1
            else
              a.din := 0
            end if
            NextStep
          end S_0_0
          b.din := 2
          NextStep
        end S_0
        c.din := 3
        def S_1: Step =
          NextStep
        end S_1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val a = Int <> OUT.REG
         |  val b = Int <> OUT.REG
         |  val c = Int <> OUT.REG
         |  val i = Bit <> IN
         |  process:
         |    def S_0_0: Step =
         |      if (i) a.din := 1
         |      else a.din := 0
         |      b.din := 2
         |      c.din := 3
         |      S_1
         |    end S_0_0
         |    def S_1: Step =
         |      S_0_0
         |    end S_1
         |end Foo""".stripMargin
    )
  }
  test("multiple steps nested inside the same conditional branch with inter-step statements") {
    class Foo extends RTDesign:
      val i = Int <> VAR.REG
      process:
        def S_0: Step =
          NextStep
        end S_0
        i.din := 0
        def S_1: Step =
          if (i < 3)
            println(s"Hello")
            def S_1_0: Step =
              NextStep
            end S_1_0
            println(s"World")
            def S_1_1: Step =
              NextStep
            end S_1_1
            println(s"!")
            i.din := i + 1
            ThisStep
          else NextStep
          end if
        end S_1
        finish()
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val i = Int <> VAR.REG
         |  process:
         |    def S_0: Step =
         |      i.din := 0
         |      println(s"Hello")
         |      S_1_0
         |    end S_0
         |    def S_1_0: Step =
         |      println(s"World")
         |      S_1_1
         |    end S_1_0
         |    def S_1_1: Step =
         |      println(s"!")
         |      i.din := i + 1
         |      if ((i + 1) < 3)
         |        println(s"Hello")
         |        S_1_0
         |      else
         |        finish()
         |        S_0
         |    end S_1_1
         |end Foo""".stripMargin
    )
  }

  test("fusion: deeply nested first steps fuse into a single state") {
    class Foo extends RTDesign:
      process:
        def S1: Step =
          def S2: Step =
            def S3: Step =
              NextStep
            end S3
            NextStep
          end S2
          NextStep
        end S1
        finish()
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  process:
         |    def S1_S2_S3: Step =
         |      finish()
         |      S1_S2_S3
         |    end S1_S2_S3
         |end Foo""".stripMargin
    )
  }

  test("fusion: loop control step fuses into its wait with guard forwarding") {
    class Foo extends RTDesign:
      val x       = Bit     <> OUT.REG
      val i       = Int     <> VAR.REG
      val waitCnt = UInt(8) <> VAR.REG init 0
      process:
        def S_0: Step =
          NextStep
        end S_0
        i.din := 0
        def S_1: Step =
          if (i < 4)
            waitCnt.din := 0
            def S_1_0: Step =
              if (waitCnt != 9)
                waitCnt.din := waitCnt + 1
                ThisStep
              else NextStep
              end if
            end S_1_0
            i.din := i + 1
            ThisStep
          else NextStep
          end if
        end S_1
        x.din := 1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val i = Int <> VAR.REG
         |  val waitCnt = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      i.din := 0
         |      waitCnt.din := d"8'0"
         |      S_1_0
         |    end S_0
         |    def S_1_0: Step =
         |      if (waitCnt != d"8'9")
         |        waitCnt.din := waitCnt + d"8'1"
         |        S_1_0
         |      else
         |        i.din := i + 1
         |        if ((i + 1) < 4)
         |          waitCnt.din := d"8'0"
         |          S_1_0
         |        else
         |          x.din := 1
         |          S_0
         |      end if
         |    end S_1_0
         |end Foo""".stripMargin
    )
  }

  test("fusion: nested loop control steps fuse into the innermost wait") {
    class Foo extends RTDesign:
      val x       = Bit     <> OUT.REG
      val i       = Int     <> VAR.REG
      val j       = Int     <> VAR.REG
      val waitCnt = UInt(8) <> VAR.REG init 0
      process:
        def S_0: Step =
          NextStep
        end S_0
        i.din := 0
        def S_1: Step =
          if (i < 2)
            j.din := 0
            def S_1_0: Step =
              if (j < 2)
                waitCnt.din := 0
                def S_1_0_0: Step =
                  if (waitCnt != 9)
                    waitCnt.din := waitCnt + 1
                    ThisStep
                  else NextStep
                  end if
                end S_1_0_0
                j.din := j + 1
                ThisStep
              else NextStep
              end if
            end S_1_0
            i.din := i + 1
            ThisStep
          else NextStep
          end if
        end S_1
        x.din := 1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG
         |  val i = Int <> VAR.REG
         |  val j = Int <> VAR.REG
         |  val waitCnt = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      i.din := 0
         |      j.din := 0
         |      waitCnt.din := d"8'0"
         |      S_1_0_0
         |    end S_0
         |    def S_1_0_0: Step =
         |      if (waitCnt != d"8'9")
         |        waitCnt.din := waitCnt + d"8'1"
         |        S_1_0_0
         |      else
         |        j.din := j + 1
         |        if ((j + 1) < 2)
         |          waitCnt.din := d"8'0"
         |          S_1_0_0
         |        else
         |          i.din := i + 1
         |          if ((i + 1) < 2)
         |            j.din := 0
         |            waitCnt.din := d"8'0"
         |            S_1_0_0
         |          else
         |            x.din := 1
         |            S_0
         |          end if
         |        end if
         |      end if
         |    end S_1_0_0
         |end Foo""".stripMargin
    )
  }

  test("fusion: dynamic re-entry guard falls back to a control state for the inner loop") {
    class Foo extends RTDesign:
      val n       = Int     <> IN
      val x       = Bit     <> OUT.REG
      val i       = Int     <> VAR.REG
      val j       = Int     <> VAR.REG
      val waitCnt = UInt(8) <> VAR.REG init 0
      process:
        def S_0: Step =
          NextStep
        end S_0
        i.din := 0
        def S_1: Step =
          if (i < 2)
            j.din := 0
            def S_1_0: Step =
              if (j < n)
                waitCnt.din := 0
                def S_1_0_0: Step =
                  if (waitCnt != 9)
                    waitCnt.din := waitCnt + 1
                    ThisStep
                  else NextStep
                  end if
                end S_1_0_0
                j.din := j + 1
                ThisStep
              else NextStep
              end if
            end S_1_0
            i.din := i + 1
            ThisStep
          else NextStep
          end if
        end S_1
        x.din := 1
    end Foo
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val n = Int <> IN
         |  val x = Bit <> OUT.REG
         |  val i = Int <> VAR.REG
         |  val j = Int <> VAR.REG
         |  val waitCnt = UInt(8) <> VAR.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      i.din := 0
         |      S_1
         |    end S_0
         |    def S_1: Step =
         |      if (i < 2)
         |        j.din := 0
         |        if (0 < n)
         |          waitCnt.din := d"8'0"
         |          S_1_0_0
         |        else
         |          i.din := i + 1
         |          S_1
         |      else
         |        x.din := 1
         |        S_0
         |      end if
         |    end S_1
         |    def S_1_0_0: Step =
         |      if (waitCnt != d"8'9")
         |        waitCnt.din := waitCnt + d"8'1"
         |        S_1_0_0
         |      else
         |        j.din := j + 1
         |        if ((j + 1) < n)
         |          waitCnt.din := d"8'0"
         |          S_1_0_0
         |        else
         |          i.din := i + 1
         |          S_1
         |      end if
         |    end S_1_0_0
         |end Foo""".stripMargin
    )
  }

  test("forever-loop rotation clones the prologue before the wrap-around NextStep") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG
      process:
        y.din := 1
        def S0: Step =
          NextStep
        def S1: Step =
          NextStep
    end Foo
    // the rotation clone is triggered ONLY by the relative NextStep goto that wraps past
    // the last step — after resolution it is a named goto, so re-running the stage creates
    // no further copies (fix-point), verified by applying the stage twice
    val expected =
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG
         |  process:
         |    y.din := 1
         |    def S0: Step =
         |      S1
         |    end S0
         |    def S1: Step =
         |      y.din := 1
         |      S0
         |    end S1
         |end Foo""".stripMargin
    val top = (new Foo).flattenStepBlocks
    assertCodeString(top, expected)
    assertCodeString(top.flattenStepBlocks, expected)
  }
  test("forever-loop rotation clones a combinational loop prologue before the wrap-around") {
    class Foo extends RTDesign:
      val vec = Bits(8) X 4 <> OUT.REG
      process:
        COMB_LOOP:
          for (i <- 0 until 4)
            vec(i).din := all(0)
        def S0: Step =
          NextStep
        def S1: Step =
          NextStep
    end Foo
    // the rotation clone carries the whole prologue statement closure (the combinational
    // for loop with its iterator/range bookkeeping), matching what DropRTProcess
    // subsequently lowers into the generated `initial` block; inside the process the
    // clone keeps its COMB_LOOP marker
    val expected =
      """|class Foo extends RTDesign:
         |  val vec = Bits(8) X 4 <> OUT.REG
         |  process:
         |    COMB_LOOP:
         |      for (i <- 0 until 4)
         |        vec(i).din := h"00"
         |      end for
         |    def S0: Step =
         |      S1
         |    end S0
         |    def S1: Step =
         |      COMB_LOOP:
         |        for (i <- 0 until 4)
         |          vec(i).din := h"00"
         |        end for
         |      S0
         |    end S1
         |end Foo""".stripMargin
    val top = (new Foo).flattenStepBlocks
    assertCodeString(top, expected)
    assertCodeString(top.flattenStepBlocks, expected)
  }

  test("fusion: first-step loop control with wrap-around self-goto fuses and folds at reset") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        for (i <- 0 until 4)
          3.cy.wait
        x.din := 1
    end Foo
    // the loop control step is the process's first flat step, so the forever-loop rotation
    // plants the wrap-around self-goto (with the re-initializing `i := 0` clone) inside its
    // exit branch. The control step still fuses: the loop-back site forwards the incremented
    // iterator into the guard, the wrap re-entry const-folds through the re-initialization,
    // and the reset-site fold then drops the bootstrap state entirely, extending the prologue
    // with the folded wait-counter clear
    val expected =
      """|class Foo extends RTDesign:
         |  val x = Bit <> OUT.REG init 0
         |  val i = Int <> VAR.REG
         |  val waitCnt = UInt(2) <> VAR.REG
         |  process:
         |    i.din := 0
         |    waitCnt.din := d"2'0"
         |    def S_0_0: Step =
         |      if (waitCnt != d"2'2")
         |        waitCnt.din := waitCnt + d"2'1"
         |        S_0_0
         |      else
         |        i.din := i + 1
         |        if ((i + 1) < 4)
         |          waitCnt.din := d"2'0"
         |          S_0_0
         |        else
         |          x.din := 1
         |          i.din := 0
         |          waitCnt.din := d"2'0"
         |          S_0_0
         |        end if
         |      end if
         |    end S_0_0
         |end Foo""".stripMargin
    val top = (new Foo).flattenStepBlocks
    assertCodeString(top, expected)
    assertCodeString(top.flattenStepBlocks, expected)
  }

  test("trailing nested conditional") {
    class Foo extends RTDesign:
      val sel = Bit     <> IN
      val acc = UInt(8) <> OUT.REG init d"8'0"
      process:
        def S_0: Step =
          if (acc < d"8'10")
            def S_0_0: Step =
              NextStep
            end S_0_0
            if (sel) acc.din := acc + d"8'3"
            ThisStep
          else NextStep
          end if
        end S_0
    end Foo
    // the trailing conditional (a partial assignment under `sel`) relocates into S_0_0 as a
    // whole-block closure, and its conditional assignment to `acc` blocks fusing S_0's
    // dispatch (which reads `acc`) at S_0_0's exit site, so both steps remain real states
    val top = (new Foo).flattenStepBlocks
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val sel = Bit <> IN
         |  val acc = UInt(8) <> OUT.REG init d"8'0"
         |  process:
         |    def S_0: Step =
         |      if (acc < d"8'10") S_0_0
         |      else S_0
         |    end S_0
         |    def S_0_0: Step =
         |      if (sel) acc.din := acc + d"8'3"
         |      S_0
         |    end S_0_0
         |end Foo""".stripMargin
    )
  }
end FlattenStepBlocksSpec
