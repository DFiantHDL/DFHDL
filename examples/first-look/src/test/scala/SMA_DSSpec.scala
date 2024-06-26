import dfhdl.TestUtils._
import dfhdl._

class SMA_DSSpec extends DFTopSpec {
  val sma = new SMA_DS

  val expectedCodeString : String =
    """|@df final class SMA_DS extends DFDesign {
       |  val x   = SInt(16) <> IN  init 0
       |  val y   = SInt(16) <> OUT
       |  val s0  = x +^ x.prev
       |  val s2  = x.prev(2) +^ x.prev(3)
       |  val sum = s0 +^ s2
       |  y       := (sum / 4).resize(16)
       |}""".stripMargin

  test("codeString generation") {
    assert(sma.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    sma.compile.toFolder("sandbox/SMA_DS/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    sma.compile.toFolder("sandbox/SMA_DS/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    sma.compile.toFolder("sandbox/SMA_DS/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    sma.compile.toFolder("sandbox/SMA_DS/verilog95")
    true
  }
}

