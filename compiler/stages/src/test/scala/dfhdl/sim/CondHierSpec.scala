package dfhdl.sim
import dfhdl.*

/** DFacsimile conditional + hierarchy tests through the typed simulation API: if/else-if with
  * register hold semantics (CondCounter), and a two-instance sub-design hierarchy with a `match`
  * selector (AluPair) including a typed hierarchical expect (`dut.alu0.res`), each checked against
  * a plain Scala reference model on both kernel tiers.
  */
class CondHierSpec extends SimSpec:
  bothTiers("CondCounter if/else-if with hold"): tier =>
    (new CondCounter).simulation { dut =>
      // the reference model is a DFHDL constant, updated with constant arithmetic
      var model: UInt[8] <> CONST = d"8'0"
      // (en, clear) drive pattern covering all branch combinations, incl. counter activity
      val drive = Seq(
        (1, 0),
        (1, 0),
        (0, 0),
        (1, 0),
        (1, 1),
        (1, 0),
        (0, 1),
        (1, 0),
        (1, 0),
        (1, 0),
        (0, 0),
        (0, 0),
        (1, 0),
        (1, 0),
        (1, 1),
        (0, 0)
      )
      for ((en, clear), cyc) <- (drive ++ drive ++ drive).zipWithIndex do
        dut.en.poke(en == 1)
        dut.clear.poke(clear == 1)
        simCtx.step()
        model =
          if clear == 1 then d"8'0"
          else if en == 1 then model + 1
          else model
        assertEquals(dut.count.peek, model, s"cycle $cyc (en=$en clear=$clear)")
    }.withTier(tier).run()

  bothTiers("AluPair hierarchy with match"): tier =>
    (new AluPair).simulation { dut =>
      // reference model over DFHDL constants (constant-folded ops)
      def aluModel(op: Int, a: Bits[8] <> CONST, b: Bits[8] <> CONST): Bits[8] <> CONST =
        op match
          case 0 => (a.uint + b.uint).bits
          case 1 => a ^ b
          case 2 => a & b
          case _ => a | b
      val operands = Seq((h"5a", h"a5", h"0f"), (h"ff", h"01", h"80"), (h"13", h"37", h"ce"))
      for
        op <- 0 to 3
        abc <- operands
      do
        val (a, b, c) = abc
        dut.op.poke(op)
        dut.a.poke(a)
        dut.b.poke(b)
        dut.c.poke(c)
        simCtx.step()
        val r0 = aluModel(op, a, b)
        val expected = aluModel(op, r0, c)
        // typed hierarchical peek through the sub-design instance object
        assertEquals(dut.alu0.res.peek, r0, s"alu0.res (op=$op a=$a b=$b)")
        assertEquals(dut.out.peek, expected, s"out (op=$op a=$a b=$b c=$c)")
      end for
    }.withTier(tier).run()

  bothTiers("per-instance CONST params in reg inits and const folds"): tier =>
    val run = (new SeededPair).simulation.withTier(tier).run()
    // time zero: r = 0f ^ seed per instance
    run.inspect { dut =>
      assertEquals(dut.o0.peek, h"0f", "o0 at time zero")
      assertEquals(dut.o1.peek, h"ff", "o1 at time zero")
    }
    // each cycle adds bias = seed ^ a5 per instance
    run.continue(1)
    run.inspect { dut =>
      assertEquals(dut.o0.peek, h"b4", "o0 after 1 cycle") // 0f + a5
      assertEquals(dut.o1.peek, h"54", "o1 after 1 cycle") // ff + 55 (mod 256)
    }
    run.continue(1)
    run.inspect { dut =>
      assertEquals(dut.o0.peek, h"59", "o0 after 2 cycles") // b4 + a5
      assertEquals(dut.o1.peek, h"a9", "o1 after 2 cycles") // 54 + 55
    }
end CondHierSpec
