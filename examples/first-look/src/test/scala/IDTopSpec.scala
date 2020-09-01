import DFiant.TestUtils._
import DFiant._

class IDTopSpec extends DFTopSpec {
  val idTop = new IDTop

  val expectedCodeString : String =
    """|@df class ID extends DFDesign {
       |  final val x   = DFSInt(16) <> IN
       |  final val y   = DFSInt(16) <> OUT
       |  y             := x
       |}
       |
       |@df class IDTop extends DFDesign {
       |  final val x   = DFSInt(16) <> IN
       |  final val y   = DFSInt(16) <> OUT
       |  final val id1 = new ID {}
       |  final val id2 = new ID {}
       |  id1.x         <> x
       |  id2.x         <> id1.y
       |  y             <> id2.y
       |}""".stripMargin

  "codeString generation" should "match" in {
    assert(idTop.codeString =@= expectedCodeString)
  }

  "vhdl2008 generation" should "match" in {
    import compiler.backend.vhdl.v2008
    idTop.compile.toFolder("sandbox/idTop/vhdl2008")
    true
  }
  "vhdl93 generation" should "match" in {
    import compiler.backend.vhdl.v93
    idTop.compile.toFolder("sandbox/idTop/vhdl93")
    true
  }
  "verilog2001 generation" should "match" in {
    import compiler.backend.verilog.v2005
    idTop.compile.toFolder("sandbox/idTop/verilog2001")
    true
  }
  "verilog95 generation" should "match" in {
    import compiler.backend.verilog.v95
    idTop.compile.toFolder("sandbox/idTop/verilog95")
    true
  }
}

