package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.explicitFallThroughDIN
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class ExplicitFallThroughDINSpec extends StageSpec:

  test("register read in a fallThrough condition becomes a din read") {
    class Foo extends RTDesign:
      val x     = Bit <> IN
      val armed = Bit <> VAR.REG init 0
      val y     = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        def S_1: Step =
          def onEntry =
            armed.din := x
          def fallThrough = !armed
          y.din := !y
          NextStep
    end Foo
    val top = (new Foo).explicitFallThroughDIN
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val armed = Bit <> VAR.REG init 0
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    def S_1: Step =
         |      def onEntry: Unit =
         |        armed.din := x
         |      end onEntry
         |      def fallThrough: Bit <> VAL =
         |        !armed.din
         |      end fallThrough
         |      y.din := !y
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("a condition with no register read is left alone") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        def S_1: Step =
          def fallThrough = x
          y.din := !y
          NextStep
    end Foo
    val top = (new Foo).explicitFallThroughDIN
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    def S_1: Step =
         |      def fallThrough: Bit <> VAL =
         |        x
         |      end fallThrough
         |      y.din := !y
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("a named intermediate inside the block is rewritten too") {
    class Foo extends RTDesign:
      val x     = Bit <> IN
      val armed = Bit <> VAR.REG init 0
      val y     = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        def S_1: Step =
          def fallThrough =
            val edge = x && !armed
            edge || armed
          y.din := !y
          NextStep
    end Foo
    val top = (new Foo).explicitFallThroughDIN
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val x = Bit <> IN
         |  val armed = Bit <> VAR.REG init 0
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    def S_1: Step =
         |      def fallThrough: Bit <> VAL =
         |        val edge = x && (!armed.din)
         |        edge || armed.din
         |      end fallThrough
         |      y.din := !y
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("a partial register read is wrapped at its outermost point") {
    class Foo extends RTDesign:
      val cnt = Bits(8) <> VAR.REG init all(0)
      val y   = Bit     <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        def S_1: Step =
          def fallThrough = cnt(3, 0) == h"4'0"
          y.din := !y
          NextStep
    end Foo
    val top = (new Foo).explicitFallThroughDIN
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val cnt = Bits(8) <> VAR.REG init h"00"
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    def S_1: Step =
         |      def fallThrough: Boolean <> VAL =
         |        cnt(3, 0).din == h"0"
         |      end fallThrough
         |      y.din := !y
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }

  test("onEntry and onExit blocks keep their ordinary register reads") {
    class Foo extends RTDesign:
      val armed = Bit <> VAR.REG init 0
      val y     = Bit <> OUT.REG
      process:
        def S_0: Step =
          NextStep
        def S_1: Step =
          def onEntry =
            y.din := armed
          def onExit =
            y.din := !armed
          NextStep
    end Foo
    val top = (new Foo).explicitFallThroughDIN
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  val armed = Bit <> VAR.REG init 0
         |  val y = Bit <> OUT.REG
         |  process:
         |    def S_0: Step =
         |      NextStep
         |    end S_0
         |    def S_1: Step =
         |      def onEntry: Unit =
         |        y.din := armed
         |      end onEntry
         |      def onExit: Unit =
         |        y.din := !armed
         |      end onExit
         |      NextStep
         |    end S_1
         |end Foo""".stripMargin
    )
  }

end ExplicitFallThroughDINSpec
