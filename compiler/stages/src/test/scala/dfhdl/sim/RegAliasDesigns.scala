package dfhdl.sim
import dfhdl.*

/** `.reg` alias coverage DUT: single and multi-step delays with inits, chained aliases, reg-of-reg
  * (bare `.reg`, no init), and versioned sampling of a multiply-assigned wire (each `.reg` samples
  * the wire's value at its position).
  */
class RegAliasDut extends RTDesign:
  val i = UInt(8) <> IN
  val d1 = UInt(8) <> OUT
  val d2 = UInt(8) <> OUT
  val d3 = UInt(8) <> OUT
  val verA = UInt(8) <> OUT
  val verB = UInt(8) <> OUT
  val acc = UInt(8) <> OUT.REG init 0
  val accD = UInt(8) <> OUT
  d1 := i.reg(1, init = 55)
  d2 := i.reg(2, init = 0)
  d3 := i.reg(1, init = 0).reg(2, init = 0) // chained aliases: a 3-cycle delay
  val v = UInt(8) <> VAR
  v := i
  verA := v.reg(1, init = 0) // samples i
  v := v + 1
  verB := v.reg(1, init = 0) // samples i + 1
  acc.din := acc + i
  accD := acc.reg // bare `.reg` (no init) on a REG dcl: one extra delay stage
end RegAliasDut

/** Wide (>64-bit) `.reg` alias: the history register chain must lane-split like any register. */
class WideRegAliasDut extends RTDesign:
  val i = UInt(100) <> IN
  val d2 = UInt(100) <> OUT
  d2 := i.reg(2, init = 0)
end WideRegAliasDut

/** Rejection case: `.reg` of a mutable wire inside a conditional branch needs a conditional
  * (hold-when-untaken) register update, which the minimum lowering does not model.
  */
class CondWireRegDut extends RTDesign:
  val i = UInt(8) <> IN
  val o = UInt(8) <> OUT
  val v = UInt(8) <> VAR
  v := i
  if (i > 5) o := v.reg(1, init = 0)
  else o := i
end CondWireRegDut
