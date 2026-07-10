package stepFusion

import munit.*
import dfhdl.*

// End-to-end full-compilation coverage for RT first-step fusion (FlattenStepBlocks Rule 6):
// a flat wait, a loop of waits, and nested loops of waits, all with the same total wait time,
// compile through the complete pipeline (fused FSM generation, ED lowering, and both backends)
// with every stage sanity check passing.
class WaitFlat extends RTDesign:
  val done = Bit <> OUT.REG init 0
  process:
    40.cy.wait
    done.din := 1

class WaitLoop extends RTDesign:
  val done = Bit <> OUT.REG init 0
  process:
    for (i <- 0 until 4) 10.cy.wait
    done.din := 1

class WaitNested extends RTDesign:
  val done = Bit <> OUT.REG init 0
  process:
    for (i <- 0 until 2)
      for (j <- 0 until 2)
        10.cy.wait
    done.din := 1

class StepFusionCompileSpec extends FunSuite:
  given options.OnError = _.Exception
  test("flat wait compiles to verilog and vhdl"):
    locally:
      given options.CompilerOptions.Backend = _.verilog
      WaitFlat().compile
    locally:
      given options.CompilerOptions.Backend = _.vhdl
      WaitFlat().compile
  test("loop of waits (fused control step) compiles to verilog and vhdl"):
    locally:
      given options.CompilerOptions.Backend = _.verilog
      WaitLoop().compile
    locally:
      given options.CompilerOptions.Backend = _.vhdl
      WaitLoop().compile
  test("nested loops of waits (chained fused dispatch) compiles to verilog and vhdl"):
    locally:
      given options.CompilerOptions.Backend = _.verilog
      WaitNested().compile
    locally:
      given options.CompilerOptions.Backend = _.vhdl
      WaitNested().compile
end StepFusionCompileSpec
