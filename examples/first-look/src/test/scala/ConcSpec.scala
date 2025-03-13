import dfhdl.TestUtils._
import dfhdl._

class ConcSpec extends DFTopSpec:
  val conc = new Conc

  val expectedCodeString: String =
    """|@df final class Conc extends DFDesign {
       |  val i = UInt(32) <> IN
       |  val j = UInt(32) <> IN
       |  val a = UInt(32) <> OUT
       |  val b = UInt(32) <> OUT
       |  val c = UInt(32) <> OUT
       |  val d = UInt(32) <> OUT
       |  val e = UInt(32) <> OUT
       |  a     := i + 5
       |  b     := a * 3
       |  c     := a + b
       |  d     := i - 1
       |  e     := j / 4
       |}""".stripMargin

  test("codeString generation") {
    assert(conc.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    conc.compile.toFolder("sandbox/Conc/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    conc.compile.toFolder("sandbox/Conc/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    conc.compile.toFolder("sandbox/Conc/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    conc.compile.toFolder("sandbox/Conc/verilog95")
    true
  }
end ConcSpec
