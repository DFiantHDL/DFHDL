package dfhdl.sim
import dfhdl.*

/** Conditional-assignment test design: an enabled/clearable counter. Exercises if/else-if chains,
  * comparison ops, and the register hold-when-unassigned default (no final else).
  */
class CondCounter extends RTDesign:
  val en = Bit <> IN
  val clear = Bit <> IN
  val count = UInt(8) <> OUT.REG init 0
  if (clear) count.din := 0
  else if (en) count.din := count + 1

/** Combinational ALU cell with a `match` selector, used as the hierarchy sub-design. */
class MiniAlu extends RTDesign:
  val op = UInt(2) <> IN
  val a = Bits(8) <> IN
  val b = Bits(8) <> IN
  val res = Bits(8) <> OUT
  op match
    case 0 => res := (a.uint + b.uint).bits
    case 1 => res := a ^ b
    case 2 => res := a & b
    case _ => res := a | b

/** A sub-design whose register init and a named constant both fold a per-instance CONST param:
  * regression for shared-sub-DB param resolution (all instances used to get the first elaboration's
  * baked value in constant folds).
  */
class SeededCounter(val seed: Bits[8] <> CONST) extends RTDesign:
  val out = Bits(8) <> OUT
  val bias: Bits[8] <> CONST = seed ^ h"a5"
  val r = Bits(8) <> VAR.REG init (h"0f" ^ seed)
  r.din := (r.uint + bias.uint).bits
  out := r

/** Two [[SeededCounter]] instances with different seeds over one shared sub-DB. */
class SeededPair extends RTDesign:
  val o0 = Bits(8) <> OUT
  val o1 = Bits(8) <> OUT
  val u0 = new SeededCounter(h"00")
  val u1 = new SeededCounter(h"f0")
  o0 := u0.out
  o1 := u1.out

/** Hierarchy test design: two MiniAlu instances chained through ports. */
class AluPair extends RTDesign:
  val op = UInt(2) <> IN
  val a = Bits(8) <> IN
  val b = Bits(8) <> IN
  val c = Bits(8) <> IN
  val out = Bits(8) <> OUT
  val alu0 = new MiniAlu
  val alu1 = new MiniAlu
  alu0.op <> op
  alu0.a <> a
  alu0.b <> b
  alu1.op <> op
  alu1.a <> alu0.res
  alu1.b <> c
  out := alu1.res
end AluPair
