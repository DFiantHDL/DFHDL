package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropDFMethods
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

/** Tests for phantom ports and parameters: a method referencing values from outside its own scope
  * gets compiler-created (PhantomTag-tagged) members that make the generated design self-contained:
  * captured DFHDL constants become phantom design parameters and captured non-constant DFHDL values
  * become phantom input ports, all named after the captured values. The DFHDL printer hides
  * phantoms in the method VIEW form only, so the printed def matches the user-written source; once
  * the def is dropped to a regular design (`dropDFMethods`), phantoms print like any other
  * port/parameter.
  */
class PhantomTagSpec extends StageSpec(stageCreatesUnrefAnons = true):
  // a method within a host design, capturing the host's local values: the port `phIn`
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

  test("Phantom ports and parameters are hidden in the method view form") {
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
  test("Phantom ports and parameters are visible once the method is dropped") {
    val id = (new Host).dropDFMethods
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
  // a method capturing the host design's `<> CONST` CONSTRUCTOR parameter: the body reference
  // is rewritten to the generated design-parameter member (a class field), which the method
  // captures as a phantom design parameter named after the original parameter (issue #416)
  class HostParam(val phW: UInt[8] <> CONST = 7) extends DFDesign:
    val data                                        = UInt(8) <> IN
    val o                                           = UInt(8) <> OUT
    def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
      arg + phW
    o := calc(data)
  end HostParam

  test("A phantom capture of a `<> CONST` class parameter is hidden in the method view form") {
    val id = new HostParam
    assertCodeString(
      id,
      """|class HostParam(val phW: UInt[8] <> CONST = d"8'7") extends DFDesign:
         |  val data = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  def calc(arg: UInt[8] <> VAL): UInt[8] <> DFRET =
         |    arg + phW
         |  end calc
         |  o := calc(data)
         |end HostParam
         |""".stripMargin
    )
  }
  test("A phantom capture of a `<> CONST` class parameter is visible once the method is dropped") {
    val id = (new HostParam).dropDFMethods
    assertCodeString(
      id,
      """|class calc(val phW: UInt[8] <> CONST) extends DFDesign:
         |  val arg = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  o <> (arg + phW)
         |end calc
         |
         |class HostParam(val phW: UInt[8] <> CONST = d"8'7") extends DFDesign:
         |  val data = UInt(8) <> IN
         |  val o = UInt(8) <> OUT
         |  val o_part_calc_inst = calc(phW = phW)
         |  o_part_calc_inst.arg <> data
         |  o := o_part_calc_inst.o
         |end HostParam
         |""".stripMargin
    )
  }
  // a method capturing a member of an anonymous DOMAIN instance: the body reference `r.q`
  // is a REFLECTIVE (structural) selection on the domain's refinement type, with no member
  // symbol, which the capture discovery must recognize like a stable symbol select and turn
  // into a phantom input port. Before that, the domain OBJECT was captured as a plain Scala
  // value and the member reference stayed in the body as an illegal direct cross-design
  // reference (issue #493).
  class HostDomain extends RTDesign:
    val x = Bit <> IN
    val y = Bit <> OUT
    val r = new RTDomain:
      val q = Bit <> VAR.REG init 0
      q.din := x
    def f(a: Bit <> VAL): Bit <> DFRET =
      a & r.q
    y := f(x)
  end HostDomain

  test("A phantom capture of a domain member is hidden in the method view form") {
    val id = new HostDomain
    assertCodeString(
      id,
      """|class HostDomain extends RTDesign:
         |  val x = Bit <> IN
         |  val y = Bit <> OUT
         |  val r = new RTDomain:
         |    val q = Bit <> VAR.REG init 0
         |    q.din := x
         |  end r
         |  def f(a: Bit <> VAL): Bit <> DFRET =
         |    a && r.q
         |  end f
         |  y := f(x)
         |end HostDomain
         |""".stripMargin
    )
  }
  test("A phantom capture of a domain member is visible once the method is dropped") {
    val id = (new HostDomain).dropDFMethods
    assertCodeString(
      id,
      """|class f extends DFDesign:
         |  val a = Bit <> IN
         |  val q = Bit <> IN
         |  val o = Bit <> OUT
         |  o <> (a && q)
         |end f
         |
         |class HostDomain extends RTDesign:
         |  val x = Bit <> IN
         |  val y = Bit <> OUT
         |  val r = new RTDomain:
         |    val q = Bit <> VAR.REG init 0
         |    q.din := x
         |  end r
         |  val o_part_f_inst = f()
         |  o_part_f_inst.a <> x
         |  o_part_f_inst.q <> r.q
         |  y := o_part_f_inst.o
         |end HostDomain
         |""".stripMargin
    )
  }
end PhantomTagSpec
