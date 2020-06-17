package DFiant
package lib.ompss

trait StartFSM {
  val start : DFBool <> VAR
  val done  : DFBool <> VAR
  val idle  : DFBool <> VAR
  val ready : DFBool <> VAR

  import fsm._
  def startFSM(implicit ctx : DFBlock.Context) = doUntil(start) {
    done := 1
    idle := 1
  } =^> {
    done := 0
    idle := 0
  }
  def finishFSM(cond : DFBool)(onExit : => Unit)(implicit ctx : DFBlock.Context) = waitUntil(cond) =^> {
    onExit
    done := 1
    ready := 1
  }
}

@df class AP_Interface extends DFInterface with StartFSM {
  final val start   = DFBit() <> IN
  final val done    = DFBit() <> OUT
  final val idle    = DFBit() <> OUT
  final val ready   = DFBit() <> OUT
}


