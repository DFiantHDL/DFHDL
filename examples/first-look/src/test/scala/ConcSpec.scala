import DFiant.TestUtils._
import DFiant._

class ConcSpec extends DFTopSpec {
  val conc = new Conc

  val expectedCodeString : String =
    """|@df final class Conc extends DFDesign {
       |  val i = DFUInt(32) <> IN
       |  val j = DFUInt(32) <> IN
       |  val a = DFUInt(32) <> OUT
       |  val b = DFUInt(32) <> OUT
       |  val c = DFUInt(32) <> OUT
       |  val d = DFUInt(32) <> OUT
       |  val e = DFUInt(32) <> OUT
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
}

