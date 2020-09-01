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

  "codeString generation" should "match" in {
    assert(sma_fb.codeString =@= expectedCodeString)
  }

  "vhdl2008 generation" should "match" in {
    import compiler.backend.vhdl.v2008
    sma_fb.compile.toFolder("sandbox/sma_fb/vhdl2008")
    true
  }
  "vhdl93 generation" should "match" in {
    import compiler.backend.vhdl.v93
    sma_fb.compile.toFolder("sandbox/sma_fb/vhdl93")
    true
  }
  "verilog2001 generation" should "match" in {
    import compiler.backend.verilog.v2005
    sma_fb.compile.toFolder("sandbox/sma_fb/verilog2001")
    true
  }
  "verilog95 generation" should "match" in {
    import compiler.backend.verilog.v95
    sma_fb.compile.toFolder("sandbox/sma_fb/verilog95")
    true
  }
}

