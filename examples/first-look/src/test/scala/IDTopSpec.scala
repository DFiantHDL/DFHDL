import dfhdl.TestUtils._
import dfhdl._

class IDTopSpec extends DFTopSpec:
  val idTop = new IDTop

  val expectedCodeString: String =
    """|@df final class ID extends DFDesign {
       |  val x   = SInt(16) <> IN
       |  val y   = SInt(16) <> OUT
       |  y       := x
       |}
       |
       |@df final class IDTop extends DFDesign {
       |  val x   = SInt(16) <> IN
       |  val y   = SInt(16) <> OUT
       |  val id1 = new ID {}
       |  val id2 = new ID {}
       |  id1.x   <> x
       |  id2.x   <> id1.y
       |  y       <> id2.y
       |}""".stripMargin

  test("codeString generation") {
    assert(idTop.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    idTop.compile.toFolder("sandbox/idTop/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    idTop.compile.toFolder("sandbox/idTop/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    idTop.compile.toFolder("sandbox/idTop/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    idTop.compile.toFolder("sandbox/idTop/verilog95")
    true
  }
end IDTopSpec
