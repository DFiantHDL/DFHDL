package dfhdl
import munit.*
import dfhdl.hw.annotation.top
import dfhdl.compiler.ir.MemberGetSet
import dfhdl.tools.toolsCore.{VivadoIPPrinter, QuartusPrimeIPPrinter}

// The vendor IP generation scripts (Vivado `create_ip` tcl, Quartus qsys tcl) are emitted from the
// IP block's design parameters. An IP block is a sub-design, so the values it was instantiated
// with live at the instantiation site and must be resolved through it; asking the parameter
// under the default cache policy leaves it opaque and used to crash the printer (`None.get`).
class VendorIPPrinterSpec extends FunSuite:
  class VivadoCounter(
      val WIDTH: Int <> CONST = 8,
      val CLK_PORT: String <> CONST = "clk",
      val ENABLE: Boolean <> CONST = false,
      val version: String <> CONST = ""
  ) extends EDBlackBox.VivadoIP:
    val clk = Bit <> IN
    val cnt = Bits(WIDTH) <> OUT

  class QsysCounter(
      val WIDTH: Int <> CONST = 8,
      val CLK_PORT: String <> CONST = "clk",
      val version: String <> CONST = "1.0"
  ) extends EDBlackBox.QsysIP:
    val clk = Bit <> IN
    val cnt = Bits(WIDTH) <> OUT

  @top(false) class Top extends EDDesign:
    val clk = Bit <> IN
    val cntV = Bits(16) <> OUT
    val cntQ = Bits(12) <> OUT
    val vivadoCounter = new VivadoCounter(WIDTH = 16, CLK_PORT = "clock", ENABLE = true)
    val qsysCounter = new QsysCounter(WIDTH = 12, version = "2.5")
    vivadoCounter.clk <> clk
    cntV <> vivadoCounter.cnt
    qsysCounter.clk <> clk
    cntQ <> qsysCounter.cnt

  test("vendor IP scripts carry the applied parameter values"):
    val cd = Top().compile
    given MemberGetSet = cd.stagedDB.getSet
    val vivadoTcl = new VivadoIPPrinter().getSourceFiles
      .find(_.path == "ips/VivadoCounter.tcl").get.contents
    assertNoDiff(
      vivadoTcl,
      """|create_ip -name VivadoCounter -module_name VivadoCounter
         |set_property -dict [list \
         |  CONFIG.WIDTH {16} \
         |  CONFIG.CLK_PORT {clock} \
         |  CONFIG.ENABLE {true} \
         |] [get_ips VivadoCounter]
         |""".stripMargin
    )
    val qsysTcl = new QuartusPrimeIPPrinter().getSourceFiles
      .find(_.path == "ips/QsysCounter.tcl").get.contents
    assert(qsysTcl.contains("QsysCounter 2.5"), qsysTcl)
    assert(
      qsysTcl.contains(
        """|set_instance_parameter_value QsysCounter_inst {WIDTH} {12}
           |set_instance_parameter_value QsysCounter_inst {CLK_PORT} {clk}""".stripMargin
      ),
      qsysTcl
    )
end VendorIPPrinterSpec
