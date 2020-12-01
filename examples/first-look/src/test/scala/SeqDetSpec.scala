import DFiant.TestUtils._
import DFiant._

class SeqDetSpec extends DFTopSpec {
  val seqDet = new SeqDet

  val expectedCodeString : String =
    """|@df class SeqDet extends DFDesign {
       |  object fsm_states extends EnumType.Auto {
       |    val S0,S1,S10,S100,S1001 = Entry()
       |  }
       |  final val seqIn     = DFBit() <> IN
       |  final val detOut    = DFBit() <> OUT
       |  final val fsm_state = DFEnum(fsm_states) init fsm_states.S0
       |  matchdf(fsm_state)
       |  .casedf(fsm_states.S0) {
       |    detOut            := 0
       |    ifdf(seqIn) {fsm_state := fsm_states.S1}
       |    .elsedf {fsm_state := fsm_states.S0}
       |  }
       |  .casedf(fsm_states.S1) {
       |    detOut            := 0
       |    ifdf(seqIn) {fsm_state := fsm_states.S1}
       |    .elsedf {fsm_state := fsm_states.S10}
       |  }
       |  .casedf(fsm_states.S10) {
       |    detOut            := 0
       |    ifdf(seqIn) {fsm_state := fsm_states.S1}
       |    .elsedf {fsm_state := fsm_states.S100}
       |  }
       |  .casedf(fsm_states.S100) {
       |    detOut            := 0
       |    ifdf(seqIn) {fsm_state := fsm_states.S1001}
       |    .elsedf {fsm_state := fsm_states.S0}
       |  }
       |  .casedf(fsm_states.S1001) {
       |    detOut            := 1
       |    ifdf(seqIn) {fsm_state := fsm_states.S1}
       |    .elsedf {fsm_state := fsm_states.S10}
       |  }
       |}""".stripMargin

  test("codeString generation") {
    assert(seqDet.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    seqDet.compile.toFolder("sandbox/seqDet/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    seqDet.compile.toFolder("sandbox/seqDet/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    seqDet.compile.toFolder("sandbox/seqDet/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    seqDet.compile.toFolder("sandbox/seqDet/verilog95")
    true
  }
}

