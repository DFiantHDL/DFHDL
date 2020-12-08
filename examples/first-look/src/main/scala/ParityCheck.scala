import DFiant._

@df class ParityCheck extends DFDesign {
  val seqIn     = DFBit() <> IN
  val detOut    = DFBit() <> OUT
  val Even: FSM = { detOut := 1 } =?> seqIn ==> Odd
  val Odd: FSM  = { detOut := 0 } =?> seqIn ==> Even
}
