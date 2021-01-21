package DFiant
package lib.ompss

import DFiant.internals.Exact

/**
  * Provides the standard AP interface used by the Vivado HLS kernels.
  * DFiant integrates with ompss via the kernel interface.
  */
@df class AP_Interface extends DFInterface {
  final val start   = DFBit <> IN
  final val done    = DFBit <> OUT
  final val idle    = DFBit <> OUT
  final val ready   = DFBit <> OUT

  /**
    * Use to start an FSM that handles a kernel run.
    * @return an FSM with a dangling next step edge that should be connected to the rest of the kernel running FSM.
    */
  @df def startFSM: FSM =
    FSM {
      ifdf(start) {
        done := 0
        idle := 0
        nextStep.goto()
      }.elsedf {
        done := 1
        idle := 1
      }
    }

  /**
    * Use to finish an FSM that handles a kernel run.
    * @param cond The final condition to wait for before completing the FSM and going back to the beginning.
    * @return an FSM with a dangling edge to completes the kernel run.
    *         This should be connected to the initial FSM step that was created with [[startFSM]].
    */
  @df def finishFSM[C](cond: => Exact[C])(implicit arg: DFBool.Arg[C]): FSM =
    FSM {
      ifdf(cond) {
        done  := 1
        ready := 1
        nextStep.goto()
      }
    }

  if (this.hasNativeDir) {
    done  := 0
    idle  := 0
    ready := 0
  }
}
