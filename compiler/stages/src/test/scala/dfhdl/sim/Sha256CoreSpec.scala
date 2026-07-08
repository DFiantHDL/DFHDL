package dfhdl.sim
import dfhdl.*

/** End-to-end DFacsimile test: elaborate the SHA-256 DFHDL design, lower its IR to the simulation
  * netlist, and run it on both kernel tiers through the typed simulation API. After 64 rounds,
  * `IV + state` (computed with DFHDL constant arithmetic on the peeked state) must equal the NIST
  * FIPS 180 test vector for SHA-256("abc").
  */
class Sha256CoreSpec extends SimSpec:
  private val IV = Vector(
    h"6a09e667",
    h"bb67ae85",
    h"3c6ef372",
    h"a54ff53a",
    h"510e527f",
    h"9b05688c",
    h"1f83d9ab",
    h"5be0cd19"
  )
  // NIST FIPS 180 test vector: SHA-256("abc")
  private val expectedDigest = Vector(
    h"ba7816bf",
    h"8f01cfea",
    h"414140de",
    h"5dae2223",
    h"b00361a3",
    h"96177a9c",
    h"b410ff61",
    h"f20015ad"
  )

  bothTiers("SHA-256 DFHDL design digest of \"abc\""): tier =>
    (new SHA256Core).simulation { dut =>
      simCtx.step(64)
      val state = Vector(dut.a, dut.b, dut.c, dut.d, dut.e, dut.f, dut.g, dut.h)
      for i <- 0 until 8 do
        assertEquals(IV(i) + state(i).peek, expectedDigest(i), s"digest word $i")
    }.withTier(tier).run()

  test("Interpreter and Codegen stay in lockstep (raw kernel equivalence)"):
    // kernel-level equivalence check — deliberately uses the raw layer
    val db = (new SHA256Core).getDB
    val sim0 = DFacsimile.simulate(db, SimTier.Interpreter)
    val sim1 = DFacsimile.simulate(db, SimTier.Codegen)
    val stateNames = Vector("a", "b", "c", "d", "e", "f", "g", "h", "t")
    for cyc <- 1 to 200 do
      sim0.step()
      sim1.step()
      for name <- stateNames do
        assertEquals(sim0.peek(name), sim1.peek(name), s"mismatch on '$name' at cycle $cyc")
end Sha256CoreSpec
