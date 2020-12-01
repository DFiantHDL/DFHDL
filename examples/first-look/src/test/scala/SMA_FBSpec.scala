import DFiant.TestUtils._
import DFiant._

class SMA_FBSpec extends DFTopSpec {
  val sma_fb = new SMA_FB

  val expectedCodeString : String =
    """|@df class SMA_FB extends DFDesign {
       |  final val x   = DFSInt(16) <> IN  init 0
       |  final val y   = DFSInt(16) <> OUT
       |  final val acc = DFSInt(18) init 0
       |  acc           := (acc - x.prev(4)) + x
       |  y             := (acc >> 2).resize(16)
       |}""".stripMargin

  test("codeString generation") {
    assert(sma_fb.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    sma_fb.compile.toFolder("sandbox/sma_fb/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    sma_fb.compile.toFolder("sandbox/sma_fb/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    sma_fb.compile.toFolder("sandbox/sma_fb/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    sma_fb.compile.toFolder("sandbox/sma_fb/verilog95")
    true
  }
}

