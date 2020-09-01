import DFiant._
import DFiant.fsm._

@df class SeqDet extends DFDesign {
  val seqIn  = DFBool() <> IN
  val detOut = DFBool() <> OUT
  val detFsm : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> detFsm
  val S1     : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> S10
  val S10    : FSM = step {detOut := 0} =?> seqIn =!> S1     =!> S100
  val S100   : FSM = step {detOut := 0} =?> seqIn =!> S1001  =!> detFsm
  val S1001  : FSM = step {detOut := 1} =?> seqIn =!> S1     =!> S10
}