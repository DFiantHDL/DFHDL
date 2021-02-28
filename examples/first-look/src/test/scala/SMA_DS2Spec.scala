import DFiant.TestUtils._
import DFiant._

class SMA_DS2Spec extends DFTopSpec {
  val sma = new SMA_DS2

  val expectedCodeString : String =
    """|@df final class SMA_DS2 extends DFDesign {
       |  val x   = DFSInt(16) <> IN  init 0
       |  val y   = DFSInt(16) <> OUT
       |  val s0  = x +^ x.prev
       |  val s2  = s0.prev(2)
       |  val sum = s0 +^ s2
       |  y       := (sum / 4).resize(16)
       |}""".stripMargin

  test("codeString generation") {
    assert(sma.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    sma.compile.toFolder("sandbox/SMA_DS2/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    sma.compile.toFolder("sandbox/SMA_DS2/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    sma.compile.toFolder("sandbox/SMA_DS2/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    sma.compile.toFolder("sandbox/SMA_DS2/verilog95")
    true
  }
}

