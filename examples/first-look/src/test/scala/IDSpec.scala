import DFiant._

import TestUtils._
import compiler.printer.formatter._

class IDSpec extends DFTopSpec {
  val id = new ID

  val expectedCodeString: String =
    """|@df class ID extends DFDesign {
       |  final val x = DFSInt(16) <> IN
       |  final val y = DFSInt(16) <> OUT
       |  y           := x
       |}""".stripMargin

  test("codeString generation") {
    assert(id.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    id.compile.toFolder("sandbox/id/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    id.compile.toFolder("sandbox/id/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    id.compile.toFolder("sandbox/id/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    id.compile.toFolder("sandbox/id/verilog95")
    true
  }
}
