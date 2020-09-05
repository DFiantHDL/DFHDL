import DFiant.TestUtils._
import DFiant._

class SMASpec extends DFTopSpec {
  val sma = new SMA

  val expectedCodeString : String =
    """|@df class SMA extends DFDesign {
       |  final val x   = DFSInt(16) <> IN  init 0
       |  final val y   = DFSInt(16) <> OUT
       |  final val sum = (x +^ x.prev) +^ (x.prev(2) +^ x.prev(3))
       |  y             := sum.resize(16)
       |}""".stripMargin

  "codeString generation" should "match" in {
    assert(sma.codeString =@= expectedCodeString)
  }

  "vhdl2008 generation" should "match" in {
    import compiler.backend.vhdl.v2008
    sma.compile.toFolder("sandbox/sma/vhdl2008")
    true
  }
  "vhdl93 generation" should "match" in {
    import compiler.backend.vhdl.v93
    sma.compile.toFolder("sandbox/sma/vhdl93")
    true
  }
  "verilog2001 generation" should "match" in {
    import compiler.backend.verilog.v2005
    sma.compile.toFolder("sandbox/sma/verilog2001")
    true
  }
  "verilog95 generation" should "match" in {
    import compiler.backend.verilog.v95
    sma.compile.toFolder("sandbox/sma/verilog95")
    true
  }
}

