package dfhdl.sim
import dfhdl.*

/** The canonical typed-API example (locked decision 10): typed poke with a DFHDL constant,
  * const-vs-const assertEquals through the SimSpec Compare, and settle-on-peek semantics, i.e.
  * combinational results observable without any clock step.
  */
class Foo(val WIDTH: Int <> CONST) extends RTDesign:
  val x = Bits(WIDTH) <> IN
  val y = Bits(WIDTH) <> OUT
  y := x

class SimulationApiSpec extends SimSpec:
  bothTiers("typed peek/poke wire-through, settle-on-peek"): tier =>
    Foo(8).simulation { dut =>
      dut.x.poke(h"42")
      assertEquals(dut.y.peek, h"42") // no step: peek observes settled comb state
      dut.x.poke(h"7f")
      assertEquals(dut.y.peek, h"7f")
      simCtx.step()
      assertEquals(dut.y.peek, h"7f") // stable across a clock step (pure wire)
    }.withTier(tier).run()
end SimulationApiSpec
