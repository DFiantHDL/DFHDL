package dfhdl.sim
import dfhdl.*

/** A concurrent (event-driven) design whose wires are all driven by a connection that appears AFTER
  * the connection reading them (a forward reference in source order). `w` is whole-driven and `p`
  * is driven bit-by-bit; both are read by the output connections above their drivers. Connections
  * are continuous, so the simulator must resolve this dataflow regardless of source order.
  */
class OrderFreeConnectDut extends EDDesign:
  val a, b = Bits(4) <> IN
  val outW = Bits(4) <> OUT
  val outP = Bits(4) <> OUT
  val w = Bits(4) <> VAR
  val p = Bits(4) <> VAR
  // forward references: the outputs read `w`/`p` before their drivers appear below
  outW <> w
  outP <> p
  // drivers, out of source order relative to the reads above
  w <> a & b
  for (i <- 0 until 4)
    p(i) <> (a(i) ^ b(i))
end OrderFreeConnectDut

/** A multi-hop forward chain: every wire is driven from the next one down, and every driver appears
  * after the connection that reads it. Resolving `out` requires seeing through the whole chain
  * (`out` <- `s0` <- `s1` <- `s2` <- `in`) independently of connection order. `out` == `in` xor 7.
  */
class ForwardChainDut extends EDDesign:
  val in = Bits(4) <> IN
  val out = Bits(4) <> OUT
  val s0, s1, s2 = Bits(4) <> VAR
  out <> s0
  s0 <> s1 ^ h"1"
  s1 <> s2 ^ h"2"
  s2 <> in ^ h"4"
end ForwardChainDut

/** Order-independence of concurrent connections in the DFacsimile engine: a connection may read a
  * value whose sole driver is a later connection. These designs have no hierarchy and no registers,
  * so they isolate the forward-reference resolution from the FullAdderN docExample coverage.
  */
class OrderFreeConnectSimSpec extends SimSpec:
  bothTiers("forward whole and partial connections"): tier =>
    (new OrderFreeConnectDut).simulation { dut =>
      for
        a <- 0 until 16
        b <- 0 until 16
      do
        dut.a.poke(d"4'$a".bits)
        dut.b.poke(d"4'$b".bits)
        simCtx.step()
        assertEquals(dut.outW.peek, d"4'${a & b}".bits, s"outW (a=$a b=$b)")
        assertEquals(dut.outP.peek, d"4'${a ^ b}".bits, s"outP (a=$a b=$b)")
    }.withTier(tier).run()

  bothTiers("multi-hop forward connection chain"): tier =>
    (new ForwardChainDut).simulation { dut =>
      for x <- 0 until 16 do
        dut.in.poke(d"4'$x".bits)
        simCtx.step()
        assertEquals(dut.out.peek, d"4'${x ^ 7}".bits, s"out (x=$x)")
    }.withTier(tier).run()
end OrderFreeConnectSimSpec
