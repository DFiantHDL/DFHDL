package fpga2019
import DFiant._

trait SeqDet extends DFDesign {
  val seqIn  = DFBool() <> IN
  val detOut = DFBool() <> OUT
  object State extends Enum.Auto {
    val S0, S1, S10, S100, S1001 = Entry
  }
  val state = DFEnum(State) init State.S0
  matchdf(state)
    .casedf(State.S0) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S0}
    }.casedf(State.S1) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S10}
    }.casedf(State.S10) {
      detOut := 0
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S100}
    }.casedf(State.S100) {
      detOut := 0
      ifdf (seqIn) {state := State.S1001}
      .elsedf      {state := State.S0}
    }.casedf(State.S1001) {
      detOut := 1
      ifdf (seqIn) {state := State.S1}
      .elsedf      {state := State.S10}
    }
}
