import DFiant._

@df class SeqDet extends DFDesign {
  val seqIn  = DFBit <> IN
  val detOut = DFBit <> OUT
  @df def detStep(
    out : Int, trueNS : => FSM, falseNS : => FSM
  ) : FSM = FSM {
    detOut := out
    ifdf(seqIn){
      trueNS.goto()
    }.elsedf {
      falseNS.goto()
    }
  }
  val S0     : FSM = detStep(0, S1, S0)
  val S1     : FSM = detStep(0, S1, S10)
  val S10    : FSM = detStep(0, S1, S100)
  val S100   : FSM = detStep(0, S1001, S0)
  val S1001  : FSM = detStep(1, S1, S10)
}
