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
         |    def MyStep: Step =
         |      MyStep_0
         |    end MyStep
         |    def MyStep_0: Step =
         |      MyStep
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
         |    def S_0: Step =
         |      a.din := 1
         |      S_0_0
         |    end S_0
         |    def S_0_0: Step =
         |      b.din := 2
         |      S_0_1
         |    end S_0_0
         |    def S_0_1: Step =
         |      S_1
         |    end S_0_1
         |    def S_1: Step =
         |      S_0
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
         |    def S_0: Step =
         |      S_0_0
         |    end S_0
         |    def S_0_0: Step =
         |      a.din := 1
         |      S_0_0_0
         |    end S_0_0
         |    def S_0_0_0: Step =
         |      b.din := 2
         |      c.din := 3
         |      S_1
         |    end S_0_0_0
         |    def S_1: Step =
         |      S_0
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
         |      S_0
         |    end S_0_0
         |    def S_1: Step =
         |      S_0
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
         |      S_0
         |    end S_0_0
         |    def S_1: Step =
         |      S_0
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
         |      else S_0
         |    end S_0_0
         |    def S_0_0_0: Step =
         |      x.din := 1
         |      S_0_0
         |    end S_0_0_0
         |    def S_1: Step =
         |      S_0
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
         |    def S_0: Step =
         |      S_0_0
         |    end S_0
         |    def S_0_0: Step =
         |      if (i) a.din := 1
         |      else a.din := 0
         |      b.din := 2
         |      c.din := 3
         |      S_1
         |    end S_0_0
         |    def S_1: Step =
         |      S_0
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
         |      S_1
         |    end S_0
         |    def S_1: Step =
         |      if (i < 3)
         |        println(s"Hello")
         |        S_1_0
         |      else
         |        finish()
         |        S_0
         |    end S_1
         |    def S_1_0: Step =
         |      println(s"World")
         |      S_1_1
         |    end S_1_0
         |    def S_1_1: Step =
         |      println(s"!")
         |      i.din := i + 1
         |      S_1
         |    end S_1_1
         |end Foo""".stripMargin
    )
  }

end FlattenStepBlocksSpec
