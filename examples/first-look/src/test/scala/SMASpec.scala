import DFiant.TestUtils._
import DFiant._

class SMASpec extends DFTopSpec {
  val sma = new SMA

  val expectedCodeString: String =
    """|@df class SMA extends DFDesign {
       |  final val x   = DFSInt(16) <> IN  init 0
       |  final val y   = DFSInt(16) <> OUT
       |  final val sum = (x +^ x.prev) +^ (x.prev(2) +^ x.prev(3))
       |  y             := (sum >> 2).resize(16)
       |}""".stripMargin

  test("codeString generation") {
    assert(sma.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    sma.compile.toFolder("sandbox/sma/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    sma.compile.toFolder("sandbox/sma/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    sma.compile.toFolder("sandbox/sma/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    sma.compile.toFolder("sandbox/sma/verilog95")
    true
  }
}
