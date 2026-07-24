package dfacsimile
import dfhdl.*
import dfhdl.sim.*
import docExamples.fullAdderN.FullAdderN

/** DFacsimile verification of the FullAdderN docExample through the typed simulation API: a real
  * hierarchical design with four deduplicated FullAdder1 instances (shared sub-DB, per-instance
  * state), PortByNameSelect connections, a sibling carry chain, and per-bit partial connections
  * into the sum output. Verified exhaustively (n=4: all 512 input combinations) against a
  * carry-preserving addition reference in DFHDL constant arithmetic.
  */
class FullAdderNSimSpec extends SimSpec:
  bothTiers("FullAdderN(4) exhaustive"): tier =>
    (new FullAdderN(4)).simulation { dut =>
      for
        a <- 0 until 16
        b <- 0 until 16
        cin <- Seq[Bit](0, 1)
      do
        val aC = d"4'$a"
        val bC = d"4'$b"
        dut.a.poke(aC)
        dut.b.poke(bC)
        dut.c_in.poke(cin)
        simCtx.step()
        // reference model in DFHDL constant arithmetic: carry-preserving addition
        val base = aC +^ bC
        val total = (if cin == 1 then base + 1 else base).bits
        assertEquals(dut.sum.peek, total(3, 0), s"sum (a=$a b=$b cin=$cin)")
        assertEquals(dut.c_out.peek, total(4), s"c_out (a=$a b=$b cin=$cin)")
        // internal carry chain through the typed hierarchy: all four instances share the
        // val name `adder`; the member bridge disambiguates them by sibling rank
        val lowBase = aC.bits(2, 0).uint +^ bC.bits(2, 0).uint
        val low3 = (if cin == 1 then lowBase + 1 else lowBase).bits
        assertEquals(dut.adder(3).c_in.peek, low3(3), s"carry (a=$a b=$b)")
    }.withTier(tier).run()
end FullAdderNSimSpec
