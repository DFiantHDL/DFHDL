package DFiant
package lib.ompss

@df class AP_Interface extends DFInterface {
  final val start   = DFBit() <> IN
  final val done    = DFBit() <> OUT
  final val idle    = DFBit() <> OUT
  final val ready   = DFBit() <> OUT
}

@df class OmpssKernelDesign extends DFDesign {
  final val ap      = new AP_Interface
  final val size    = DFBits(32) <> IN

  //TODO:
  //need to have size 32-bit input
  //inputs may be should be cast to/from DFBits.
}


