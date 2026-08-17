package dfhdl.sim
import dfhdl.*
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** The canonical typed-API example (locked decision 10): typed poke with a DFHDL constant,
  * const-vs-const assertEquals through the SimSpec Compare, and settle-on-peek semantics, i.e.
  * combinational results observable without any clock step.
  */
class Foo(val WIDTH: Int <> CONST) extends RTDesign:
  val x = Bits(WIDTH) <> IN
  val y = Bits(WIDTH) <> OUT
  y := x

/** a nonzero-low bit vector: selection and partial assignment use ABSOLUTE indices in [L, L+W-1],
  * while the underlying data offsets are relative to the low index
  */
class BitsHLFoo extends RTDesign:
  val i8 = Bits(8)      <> IN
  val i4 = Bits(4)      <> IN
  val y8 = Bits(8)      <> OUT
  val lo = Bits(4)      <> OUT
  val hb = Bit          <> OUT
  val yp = Bits(8)      <> OUT
  val v  = BitsHL(9, 2) <> VAR
  val vp = BitsHL(9, 2) <> VAR
  v        := i8
  y8       := v
  lo       := v(5, 2)
  hb       := v(9)
  vp       := i8
  vp(5, 2) := i4
  yp       := vp
end BitsHLFoo

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
  bothTiers("nonzero-low bit vector selection and partial-assignment offsets"): tier =>
    BitsHLFoo().simulation { dut =>
      dut.i8.poke(h"a5")
      dut.i4.poke(h"c")
      assertEquals(dut.y8.peek, h"a5")
      assertEquals(dut.lo.peek, h"5")
      assertEquals(dut.hb.peek, 1)
      assertEquals(dut.yp.peek, h"ac")
    }.withTier(tier).run()
end SimulationApiSpec
