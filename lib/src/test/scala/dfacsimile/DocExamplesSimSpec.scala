package dfacsimile
import dfhdl.*
import dfhdl.sim.*
import docExamples.alu.ALUSel

/** DFacsimile verification of stock docExamples designs, unmodified, through the typed simulation
  * API:
  *   - Counter: design param + if-without-else register hold
  *   - Blinker: design params driving widths/constants + if/else with `===` compare
  *   - ALU: expression-form `match` on an Encoded enum selector (poked with typed enum values),
  *     subtraction, dynamic logical and arithmetic shifts, signed/unsigned compares, and a `?`
  *     (don't-care) default
  */
class DocExamplesSimSpec extends SimSpec:
  bothTiers("Counter docExample"): tier =>
    (new docExamples.counter.Counter()).simulation { dut =>
      // the reference model is a DFHDL constant, updated with constant arithmetic
      var model: UInt[8] <> CONST = d"8'0"
      val drive = Seq[Bit](1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1)
      for (en, cyc) <- (drive ++ drive).zipWithIndex do
        dut.en.poke(en == 1)
        simCtx.step()
        if en == 1 then model = model + 1
        assertEquals(dut.cnt.peek, model, s"cycle $cyc")
    }.withTier(tier).run()

  bothTiers("Blinker docExample"): tier =>
    // CLK_FREQ_KHz=1, LED_FREQ_Hz=100 -> HALF_PERIOD = 5 cycles per led toggle
    (new docExamples.led_blinker.Blinker(1, 100)).simulation { dut =>
      var led: Bit = 1
      var cnt = 0
      for cyc <- 0 until 40 do
        simCtx.step()
        if cnt == 4 then
          cnt = 0
          led = ~led
        else cnt += 1
        assertEquals(dut.led.peek, led, s"cycle $cyc")
        assertEquals(dut.cnt.peek, d"3'$cnt", s"cycle $cyc")
    }.withTier(tier).run()

  bothTiers("ALU docExample"): tier =>
    (new docExamples.alu.ALU).simulation { dut =>
      // reference model over DFHDL constants (constant-folded ops), mirroring RISC-V semantics
      def model(sel: ALUSel, op1: Bits[32] <> CONST, op2: Bits[32] <> CONST): Bits[32] <> CONST =
        val shamt = op2(4, 0)
        // ordinal matching (Encoded enums have no CanEqual under strictEquality):
        // ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1
        sel.ordinal match
          case 0 => (op1.uint + op2.uint).bits
          case 1 => (op1.uint - op2.uint).bits
          case 2 => op1 << shamt
          case 3 => op1 >> shamt
          case 4 => (op1.sint >> shamt).bits
          case 5 => op1 & op2
          case 6 => op1 | op2
          case 7 => op1 ^ op2
          case 8 => if (op1.sint < op2.sint).toScalaBoolean then h"00000001" else h"00000000"
          case 9 => if (op1.uint < op2.uint).toScalaBoolean then h"00000001" else h"00000000"
          case _ => op1
      end model
      val operands = Seq(
        (h"00000000", h"00000000"),
        (h"5a5a5a5a", h"a5a5a5a5"),
        (h"ffffffff", h"00000001"),
        (h"80000000", h"7fffffff"),
        (h"12345678", h"9abcdef0"),
        (h"00000007", h"ffffffe0"), // shamt = 0
        (h"80000001", h"0000001f") // shamt = 31
      )
      for
        sel <- ALUSel.values
        (op1, op2) <- operands
      do
        dut.op1.poke(op1)
        dut.op2.poke(op2)
        dut.aluSel.poke(sel) // typed enum poke
        simCtx.step()
        assertEquals(dut.aluOut.peek, model(sel, op1, op2), s"sel=$sel op1=$op1 op2=$op2")
    }.withTier(tier).run()
end DocExamplesSimSpec
