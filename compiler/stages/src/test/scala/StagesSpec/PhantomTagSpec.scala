package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDesignDefs
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** Tests for phantom ports and parameters: a design def referencing values from outside its own
  * scope gets compiler-created (PhantomTag-tagged) members that make the generated design
  * self-contained: captured DFHDL constants become phantom design parameters and captured
  * non-constant DFHDL values become phantom input ports, all named after the captured values. The
  * DFHDL printer hides phantoms in the design-def VIEW form only, so the printed def matches the
  * user-written source; once the def is dropped to a regular design (`dropDesignDefs`), phantoms
  * print like any other port/parameter.
  */
class PhantomTagSpec extends StageSpec(stageCreatesUnrefAnons = true):
  // a design def within a host design, capturing the host's local values: the port `phIn`
  // (a phantom input port) and the constant `phW` (a phantom design parameter)
  class Host extends DFDesign:
    val data                                        = UInt(8) <> IN
    val phIn                                        = UInt(8) <> IN
    val phW: UInt[8] <> CONST                       = 7
    val o                                           = UInt(8) <> OUT
    def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
      arg + phIn + phW
    o := calc(data)
  end Host

  test("Phantom ports and parameters are hidden in the design-def view form") {
    val id = new Host
    // the def declaration prints locally in the host design's body (just before its
    // first instance), since its body references the host's values by name
    assertCodeString(
      id,
      """|class Host extends DFDesign:
         |  val data = UInt(8) <> IN
         |  val phIn = UInt(8) <> IN
         |  val phW: UInt[8] <> CONST = d"8'7"
         |  val o = UInt(8) <> OUT
         |  def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
         |    arg + phIn + phW
         |  end calc
         |  o := calc(data)
         |end Host
         |""".stripMargin
    )
  }
  test("Phantom ports and parameters are visible once the design def is dropped") {
    val id = (new Host).dropDesignDefs
    assertCodeString(
      id,
      """|class calc(val phW: UInt[8] <> CONST) extends DFDesign:
         |  val arg = UInt(8) <> IN
         |  val phIn = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  o <> (arg + phIn + phW)
         |end calc
         |
         |class Host extends DFDesign:
         |  val data = UInt(8) <> IN
         |  val phIn = UInt(8) <> IN
         |  val phW: UInt[8] <> CONST = d"8'7"
         |  val o = UInt(8) <> OUT
         |  val o_part_calc_inst = calc(phW = phW)
         |  o_part_calc_inst.arg <> data
         |  o_part_calc_inst.phIn <> phIn
         |  o := o_part_calc_inst.o
         |end Host
         |""".stripMargin
    )
  }
end PhantomTagSpec
