import dfhdl._

@df class ParityCheck extends DFDesign:
  val seqIn = Bit <> IN
  val detOut = Bit <> OUT
  val Even: FSM = { detOut := 1 } =?> seqIn ==> Odd
  val Odd: FSM = { detOut := 0 } =?> seqIn ==> Even
