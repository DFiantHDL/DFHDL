package dfhdl.sim
import dfhdl.*

/** `.din` read coverage DUT: a read before any assignment (the register itself), a
  * read-modify-write chain (two increments advance by two per cycle), and a read after the chain
  * (the pending value).
  */
class RegDINDut extends RTDesign:
  val i = UInt(8) <> IN
  val r = UInt(8) <> OUT.REG init 0
  val pre = UInt(8) <> OUT
  val post = UInt(8) <> OUT
  pre := r.din // nothing assigned yet: reads the register
  r.din := r.din + d"8'1"
  r.din := r.din + i
  post := r.din // the pending next-cycle value
end RegDINDut

/** A partial `.din` read reads the corresponding slice of the pending value. */
class RegDINPartialDut extends RTDesign:
  val i = Bits(8) <> IN
  val r = Bits(8) <> OUT.REG init all(0)
  val lo = Bits(4) <> OUT
  val hi = Bits(4) <> OUT
  r.din := r | i
  lo := r(3, 0).din
  hi := r(7, 4).din
end RegDINPartialDut

/** A `.din` read after a conditional assignment sees the pending value of whichever branch ran. */
class RegDINCondDut extends RTDesign:
  val en = Bit <> IN
  val r = UInt(8) <> OUT.REG init 0
  val pend = UInt(8) <> OUT
  if (en) r.din := r + d"8'10"
  pend := r.din
end RegDINCondDut

/** A `.din` read inside an RT process, across states: each state sees only its own pending writes,
  * and a read taken before the state's write still yields the register.
  */
class RegDINProcDut extends RTDesign:
  val r = UInt(8) <> OUT.REG init 0
  val seen = UInt(8) <> OUT.REG init 0
  process:
    seen.din := r.din // state 0, before the write: the register
    r.din := r + d"8'1"
    1.cy.wait
    r.din := r + d"8'2"
    seen.din := r.din // state 1, after the write: the pending value
    1.cy.wait
end RegDINProcDut
