package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropRTProcess

class DropRTProcessSpec extends StageSpec():
  test("named FSM steps") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      val my_fsm = process:
        def S0: Step =
          y.din := 0
          if (x) S1 else S0
        def S1: Step =
          def onEntry =
            y.din := 1
          if (x) S2 else S0
        def S2: Step =
          def onExit =
            y.din := 0
          if (x) S2 else S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum my_fsm_State(val value: UInt[2] <> CONST) extends Encoded.Manual(2):
         |    case S0 extends my_fsm_State(d"2'0")
         |    case S1 extends my_fsm_State(d"2'1")
         |    case S2 extends my_fsm_State(d"2'2")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val my_fsm_state = my_fsm_State <> VAR.REG init my_fsm_State.S0
         |  my_fsm_state match
         |    case my_fsm_State.S0 =>
         |      y.din := 0
         |      if (x)
         |        y.din := 1
         |        my_fsm_state.din := my_fsm_State.S1
         |      else my_fsm_state.din := my_fsm_State.S0
         |      end if
         |    case my_fsm_State.S1 =>
         |      if (x) my_fsm_state.din := my_fsm_State.S2
         |      else my_fsm_state.din := my_fsm_State.S0
         |    case my_fsm_State.S2 =>
         |      if (x) my_fsm_state.din := my_fsm_State.S2
         |      else
         |        y.din := 0
         |        my_fsm_state.din := my_fsm_State.S0
         |      end if
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("onEntry is not fired on self-transition") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          if (x) S1 else S0
        def S1: Step =
          def onEntry =
            y.din := 1
          if (x) S1 else S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |    case S1 extends State(d"1'1")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      if (x)
         |        y.din := 1
         |        state.din := State.S1
         |      else state.din := State.S0
         |      end if
         |    case State.S1 =>
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("unnamed FSM steps") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          y.din := 0
          if (x) S1 else S0
        def S1: Step =
          y.din := 1
          if (x) S2 else S0
        def S2: Step =
          y.din := 0
          if (x) S2 else S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[2] <> CONST) extends Encoded.Manual(2):
         |    case S0 extends State(d"2'0")
         |    case S1 extends State(d"2'1")
         |    case S2 extends State(d"2'2")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      y.din := 0
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |    case State.S1 =>
         |      y.din := 1
         |      if (x) state.din := State.S2
         |      else state.din := State.S0
         |    case State.S2 =>
         |      y.din := 0
         |      if (x) state.din := State.S2
         |      else state.din := State.S0
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("process with a single step") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          y.din := 0
          S0
        end S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      y.din := 0
         |      state.din := State.S0
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("fall-through steps") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          y.din := 0
          S1
        def S1: Step =
          def fallThrough = x
          def onEntry =
            y.din := 1
          S2
        def S2: Step =
          def fallThrough = !x
          def onEntry =
            y.din := !y
          S3
        def S3: Step =
          def fallThrough = x ^ x.reg(1, init = 0)
          def onEntry =
            y.din := y ^ y.reg
          S4
        def S4: Step =
          y.din := 0
          if (x) S2 else S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[3] <> CONST) extends Encoded.Manual(3):
         |    case S0 extends State(d"3'0")
         |    case S1 extends State(d"3'1")
         |    case S2 extends State(d"3'2")
         |    case S3 extends State(d"3'3")
         |    case S4 extends State(d"3'4")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      y.din := 0
         |      y.din := 1
         |      state.din := State.S1
         |      if (x)
         |        y.din := !y
         |        state.din := State.S2
         |        if (!x)
         |          y.din := y ^ y.reg
         |          state.din := State.S3
         |          if (x ^ x.reg(1, init = 0)) state.din := State.S4
         |        end if
         |      end if
         |    case State.S1 =>
         |      y.din := !y
         |      state.din := State.S2
         |      if (!x)
         |        y.din := y ^ y.reg
         |        state.din := State.S3
         |        if (x ^ x.reg(1, init = 0)) state.din := State.S4
         |      end if
         |    case State.S2 =>
         |      y.din := y ^ y.reg
         |      state.din := State.S3
         |      if (x ^ x.reg(1, init = 0)) state.din := State.S4
         |    case State.S3 => state.din := State.S4
         |    case State.S4 =>
         |      y.din := 0
         |      if (x)
         |        y.din := !y
         |        state.din := State.S2
         |        if (!x)
         |          y.din := y ^ y.reg
         |          state.din := State.S3
         |          if (x ^ x.reg(1, init = 0)) state.din := State.S4
         |        end if
         |      else state.din := State.S0
         |      end if
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("circular fall-through steps") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        def S0: Step =
          def fallThrough = x
          def onEntry =
            y.din := y
          S1
        def S1: Step =
          def fallThrough = !x
          def onEntry =
            y.din := !y
          S2
        def S2: Step =
          def fallThrough = x ^ x.reg(1, init = 0)
          def onEntry =
            y.din := y ^ y.reg
          S0
    end Foo
    val top = (new Foo).dropRTProcess
    assertCodeString(
      top,
      // The first step's onEntry is not initial-convertible (`y.din := y` reads y), so a
      // bootstrap S_0 step is generated (DropRTWaits Rule 6) and correctly fires S0's
      // onEntry on reset entry (previously it was silently lost); the wrap-around and the
      // circular fall-through cascade now pass through S_0.
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[2] <> CONST) extends Encoded.Manual(2):
         |    case S_0 extends State(d"2'0")
         |    case S0 extends State(d"2'1")
         |    case S1 extends State(d"2'2")
         |    case S2 extends State(d"2'3")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG init 0
         |  val state = State <> VAR.REG init State.S_0
         |  state match
         |    case State.S_0 =>
         |      y.din := y
         |      state.din := State.S0
         |      if (x)
         |        y.din := !y
         |        state.din := State.S1
         |        if (!x)
         |          y.din := y ^ y.reg
         |          state.din := State.S2
         |          if (x ^ x.reg(1, init = 0)) state.din := State.S_0
         |        end if
         |      end if
         |    case State.S0 =>
         |      y.din := !y
         |      state.din := State.S1
         |      if (!x)
         |        y.din := y ^ y.reg
         |        state.din := State.S2
         |        if (x ^ x.reg(1, init = 0)) state.din := State.S_0
         |      end if
         |    case State.S1 =>
         |      y.din := y ^ y.reg
         |      state.din := State.S2
         |      if (x ^ x.reg(1, init = 0)) state.din := State.S_0
         |    case State.S2 =>
         |      y.din := y
         |      state.din := State.S0
         |      if (x)
         |        y.din := !y
         |        state.din := State.S1
         |        if (!x)
         |          y.din := y ^ y.reg
         |          state.din := State.S2
         |        end if
         |      end if
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("convertible prologue moves into a generated initial block") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG init 0
      process:
        y.din := 1
        def S0: Step =
          if (x) S1 else S0
        def S1: Step =
          if (x) S0 else S1
    end Foo
    val top = (new Foo).dropRTProcess
    // the prologue `y.din := 1` moves into a generated initial block (superseding the
    // decl init — `init 0` is stripped); no bootstrap step is added. Both steps exit via
    // explicit gotos (no wrap-around NextStep), so the prologue runs only at initialization
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |    case S1 extends State(d"1'1")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG
         |  initial:
         |    y.din := 1
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |    case State.S1 =>
         |      if (x) state.din := State.S0
         |      else state.din := State.S1
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("prologue combinational loop moves into the generated initial block") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val vec = Bits(8) X 4 <> VAR.REG
      process:
        COMB_LOOP:
          for (i <- 0 until 4)
            vec(i).din := all(0)
        def S0: Step =
          if (x) S1 else S0
        def S1: Step =
          if (x) S0 else S1
    end Foo
    val top = (new Foo).dropRTProcess
    // the combinational for loop prologue moves into the generated initial block; the
    // COMB_LOOP marker is dropped there (the content runs once) and no bootstrap step is
    // added. Both steps exit via explicit gotos (no wrap-around NextStep), so the
    // prologue runs only at initialization
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |    case S1 extends State(d"1'1")
         |
         |  val x = Bit <> IN
         |  val vec = Bits(8) X 4 <> VAR.REG
         |  initial:
         |    for (i <- 0 until 4)
         |      vec(i).din := h"00"
         |    end for
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |    case State.S1 =>
         |      if (x) state.din := State.S0
         |      else state.din := State.S1
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("constant-guarded conditional prologue moves into the generated initial block") {
    class Foo extends RTDesign:
      val EN: Boolean <> CONST = true
      val x = Bit <> IN
      val y = Bit <> OUT.REG
      process:
        if (EN) y.din := 0
        else y.din := 1
        def S0: Step =
          if (x) S1 else S0
        def S1: Step =
          if (x) S0 else S1
    end Foo
    val top = (new Foo).dropRTProcess
    // a conditional chain with constant guards is initial-convertible, so the whole chain
    // moves into the generated initial block and no bootstrap step is added
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |    case S1 extends State(d"1'1")
         |
         |  val EN: Boolean <> CONST = true
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG
         |  initial:
         |    if (EN) y.din := 0
         |    else y.din := 1
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |    case State.S1 =>
         |      if (x) state.din := State.S0
         |      else state.din := State.S1
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("first-step onEntry is cloned into the generated initial block") {
    class Foo extends RTDesign:
      val x = Bit <> IN
      val y = Bit <> OUT.REG
      process:
        def S0: Step =
          def onEntry =
            y.din := 0
          if (x) S1 else S0
        def S1: Step =
          if (x) S0 else S1
    end Foo
    val top = (new Foo).dropRTProcess
    // reset entry into S0 runs onEntry via the generated initial block; the S1->S0
    // transition still inlines it at the goto site
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S0 extends State(d"1'0")
         |    case S1 extends State(d"1'1")
         |
         |  val x = Bit <> IN
         |  val y = Bit <> OUT.REG
         |  initial:
         |    y.din := 0
         |  val state = State <> VAR.REG init State.S0
         |  state match
         |    case State.S0 =>
         |      if (x) state.din := State.S1
         |      else state.din := State.S0
         |    case State.S1 =>
         |      if (x)
         |        y.din := 0
         |        state.din := State.S0
         |      else state.din := State.S1
         |      end if
         |  end match
         |end Foo""".stripMargin
    )
  }
  test("waiting loop lowers to a single-state FSM with no bootstrap cycle") {
    class Foo extends RTDesign:
      val x = Bit <> OUT.REG init 0
      process:
        for (i <- 0 until 4)
          3.cy.wait
        x.din := 1
    end Foo
    val top = (new Foo).dropRTProcess
    // the loop control step fuses into the wait's exit sites (forwarded `(i + 1) < 4`
    // guard) and the reset-site fold drops the bootstrap state: the generated initial
    // block provides the iterator and wait-counter values, so the FSM resets directly
    // into the (single) wait state and the whole loop costs exactly 4 x 3 cycles
    assertCodeString(
      top,
      """|class Foo extends RTDesign:
         |  enum State(val value: UInt[1] <> CONST) extends Encoded.Manual(1):
         |    case S_0_0 extends State(d"1'0")
         |
         |  val x = Bit <> OUT.REG init 0
         |  val i = Int <> VAR.REG
         |  val waitCnt = UInt(2) <> VAR.REG
         |  initial:
         |    i.din := 0
         |    waitCnt.din := d"2'0"
         |  val state = State <> VAR.REG init State.S_0_0
         |  state match
         |    case State.S_0_0 =>
         |      if (waitCnt != d"2'2")
         |        waitCnt.din := waitCnt + d"2'1"
         |        state.din := State.S_0_0
         |      else
         |        i.din := i + 1
         |        if ((i + 1) < 4)
         |          waitCnt.din := d"2'0"
         |          state.din := State.S_0_0
         |        else
         |          x.din := 1
         |          i.din := 0
         |          waitCnt.din := d"2'0"
         |          state.din := State.S_0_0
         |        end if
         |      end if
         |  end match
         |end Foo""".stripMargin
    )
  }
end DropRTProcessSpec
