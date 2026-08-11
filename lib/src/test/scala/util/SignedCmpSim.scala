package util
import dfhdl.*

/** Conformance check for SIGNED ordering, run by `testApps` against every installed tool and
  * dialect.
  *
  * It earns its place on Verilog-95, which has no `signed` keyword and so implements `<`, `>`, `<=`
  * and `>=` over signed values as macros in `dfhdl_defs.vh` rather than as native operators (see
  * `VerilogValPrinter.csDFValFuncExpr`). Nothing else executes those macros, so their arithmetic
  * was unverified: `>=` was defined as "greater OR NOT EQUAL" and answered true for every operand
  * pair, and reading an operand's sign bit by bit-select made a negative literal operand (`a <
  * sd"4'-2"`) illegal Verilog.
  *
  * Every one of the 256 operand pairs is checked against an independent reference. Flipping the
  * sign bit maps two's complement onto offset binary, where an UNSIGNED comparison answers the
  * signed one; unsigned comparisons emit plain operators in every dialect, so the reference shares
  * no machinery with the operators under test.
  */
class SignedCmpSim extends RTDesign:
  val cnt = UInt(8) <> VAR.REG init 0
  cnt.din := cnt + 1
  val a = SInt(4) <> VAR
  val b = SInt(4) <> VAR
  a := cnt.bits(3, 0).sint
  b := cnt.bits(7, 4).sint
  val ao = (a.bits ^ b"1000").uint
  val bo = (b.bits ^ b"1000").uint
  assert((a < b) == (ao < bo), s"lt $a $b")
  assert((a > b) == (ao > bo), s"gt $a $b")
  assert((a <= b) == (ao <= bo), s"le $a $b")
  assert((a >= b) == (ao >= bo), s"ge $a $b")
  // the same four against a NEGATIVE literal, whose emitted form is not an indexable primary.
  // -2 is 4'b1110, so its offset-binary image is 4'b0110 = 6.
  assert((a < sd"4'-2") == (ao < d"4'6"), s"nlt $a")
  assert((a > sd"4'-2") == (ao > d"4'6"), s"ngt $a")
  assert((a <= sd"4'-2") == (ao <= d"4'6"), s"nle $a")
  assert((a >= sd"4'-2") == (ao >= d"4'6"), s"nge $a")
  if (cnt == d"8'255") finish()
end SignedCmpSim
