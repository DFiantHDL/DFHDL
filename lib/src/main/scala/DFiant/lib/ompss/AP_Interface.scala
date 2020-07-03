package DFiant
package lib.ompss

@df class AP_Interface extends DFInterface {
  final val start   = DFBit() <> IN
  final val done    = DFBit() <> OUT
  final val idle    = DFBit() <> OUT
  final val ready   = DFBit() <> OUT

  import fsm._
  @fsm def startFSM = doUntil(start) {
    done := 1
    idle := 1
  } =^> {
    done := 0
    idle := 0
  }
//  @fsm def startFSM = step {
//    done := 0
//    idle := 0
//  } =?> start =^> {
//    done := 1
//    idle := 1
//  } =!> nextStep

  @fsm def finishFSM(cond : DFBool)(onExit : => Unit) = waitUntil(cond) =^> {
    onExit
    done := 1
    ready := 1
  }

  //  @fsm def finishFSM = prevStep =^> {
  //    done := 1
  //    ready := 1
  //  }
  if (this.hasNativeDir) {
    done := 0
    idle := 0
    ready := 0
  }
}


