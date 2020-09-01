import DFiant.TestUtils._
import DFiant._

class SeqDetSpec extends DFTopSpec {
  val seqDet = new SeqDet

  val expectedCodeString : String =
    """|@df class SeqDet extends DFDesign {
       |  object detFsm_states extends EnumType.Auto {
       |    val S0,S1,S10,S100,S1001 = Entry()
       |  }
       |  final val seqIn        = DFBool() <> IN
       |  final val detOut       = DFBool() <> OUT
       |  final val detFsm_state = DFEnum(detFsm_states) init detFsm_states.S0
       |  matchdf(detFsm_state)
       |  .casedf(detFsm_states.S0) {
       |    detOut               := 0
       |    ifdf(seqIn) {detFsm_state := detFsm_states.S1}
       |    .elsedf {detFsm_state := detFsm_states.S0}
       |  }
       |  .casedf(detFsm_states.S1) {
       |    detOut               := 0
       |    ifdf(seqIn) {detFsm_state := detFsm_states.S1}
       |    .elsedf {detFsm_state := detFsm_states.S10}
       |  }
       |  .casedf(detFsm_states.S10) {
       |    detOut               := 0
       |    ifdf(seqIn) {detFsm_state := detFsm_states.S1}
       |    .elsedf {detFsm_state := detFsm_states.S100}
       |  }
       |  .casedf(detFsm_states.S100) {
       |    detOut               := 0
       |    ifdf(seqIn) {detFsm_state := detFsm_states.S1001}
       |    .elsedf {detFsm_state := detFsm_states.S0}
       |  }
       |  .casedf(detFsm_states.S1001) {
       |    detOut               := 1
       |    ifdf(seqIn) {detFsm_state := detFsm_states.S1}
       |    .elsedf {detFsm_state := detFsm_states.S10}
       |  }
       |}""".stripMargin

  "codeString generation" should "match" in {
    assert(seqDet.codeString =@= expectedCodeString)
  }

  "vhdl2008 generation" should "match" in {
    import compiler.backend.vhdl.v2008
    seqDet.compile.toFolder("sandbox/seqDet/vhdl2008")
    true
  }
  "vhdl93 generation" should "match" in {
    import compiler.backend.vhdl.v93
    seqDet.compile.toFolder("sandbox/seqDet/vhdl93")
    true
  }
  "verilog2001 generation" should "match" in {
    import compiler.backend.verilog.v2005
    seqDet.compile.toFolder("sandbox/seqDet/verilog2001")
    true
  }
  "verilog95 generation" should "match" in {
    import compiler.backend.verilog.v95
    seqDet.compile.toFolder("sandbox/seqDet/verilog95")
    true
  }
}

