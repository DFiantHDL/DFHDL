import dfhdl.TestUtils._
import dfhdl._

class SMA_CSSpec extends DFTopSpec:
  val sma = new SMA_CS

  val expectedCodeString: String =
    """|@df final class SMA_CS extends DFDesign {
       |  val x   = SInt(16) <> IN  init 0
       |  val y   = SInt(16) <> OUT
       |  val acc = SInt(18) <> VAR init 0
       |  acc     := (acc - x.prev(4)) + x
       |  y       := (acc / 4).resize(16)
       |}""".stripMargin

  test("codeString generation") {
    assert(sma.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    sma.compile.toFolder("sandbox/SMA_CS/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    sma.compile.toFolder("sandbox/SMA_CS/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    sma.compile.toFolder("sandbox/SMA_CS/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    sma.compile.toFolder("sandbox/SMA_CS/verilog95")
    true
  }
end SMA_CSSpec
