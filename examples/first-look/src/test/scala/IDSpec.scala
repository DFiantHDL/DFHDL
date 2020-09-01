import DFiant._

import TestUtils._
import compiler.printer.formatter._

class IDSpec extends DFTopSpec {
  val id = new ID

  val expectedCodeString : String =
    """|@df class ID extends DFDesign {
       |  final val x = DFSInt(16) <> IN
       |  final val y = DFSInt(16) <> OUT
       |  y           := x
       |}""".stripMargin

  "codeString generation" should "match" in {
    assert(id.codeString =@= expectedCodeString)
  }

  "vhdl2008 generation" should "match" in {
    import compiler.backend.vhdl.v2008
    id.compile.toFolder("sandbox/id/vhdl2008")
    true
  }
  "vhdl93 generation" should "match" in {
    import compiler.backend.vhdl.v93
    id.compile.toFolder("sandbox/id/vhdl93")
    true
  }
  "verilog2001 generation" should "match" in {
    import compiler.backend.verilog.v2005
    id.compile.toFolder("sandbox/id/verilog2001")
    true
  }
  "verilog95 generation" should "match" in {
    import compiler.backend.verilog.v95
    id.compile.toFolder("sandbox/id/verilog95")
    true
  }
}

