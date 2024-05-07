import dfhdl.TestUtils._
import dfhdl._

class ParityCheckSpec extends DFTopSpec {
  val parityCheck = new ParityCheck

  val expectedCodeString : String =
    """|@df final class ParityCheck extends DFDesign {
       |  object fsm_states extends DFEnum.Auto {
       |    val Even,Odd = Entry()
       |  }
       |  val seqIn     = Bit      <> IN
       |  val detOut    = Bit      <> OUT
       |  val fsm_state = fsm_states <> VAR init fsm_states.Even
       |  matchdf(fsm_state)
       |  .casedf(fsm_states.Even) {
       |    detOut      := 1
       |    ifdf(seqIn) {fsm_state := fsm_states.Odd}
       |  }
       |  .casedf(fsm_states.Odd) {
       |    detOut      := 0
       |    ifdf(seqIn) {fsm_state := fsm_states.Even}
       |  }
       |}""".stripMargin

  test("codeString generation") {
    assert(parityCheck.codeString =@= expectedCodeString)
  }

  test("vhdl2008 generation") {
    import compiler.backend.vhdl.v2008
    parityCheck.compile.toFolder("sandbox/parityCheck/vhdl2008")
    true
  }
  test("vhdl93 generation") {
    import compiler.backend.vhdl.v93
    parityCheck.compile.toFolder("sandbox/parityCheck/vhdl93")
    true
  }
  test("verilog2001 generation") {
    import compiler.backend.verilog.v2001
    parityCheck.compile.toFolder("sandbox/parityCheck/verilog2001")
    true
  }
  test("verilog95 generation") {
    import compiler.backend.verilog.v95
    parityCheck.compile.toFolder("sandbox/parityCheck/verilog95")
    true
  }
}

