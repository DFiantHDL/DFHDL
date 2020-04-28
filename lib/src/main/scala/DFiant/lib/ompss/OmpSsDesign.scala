package DFiant
package lib.ompss

abstract class OmpSsDesign(
  implicit ctx : ContextOf[OmpSsDesign]
) extends DFDesign {
  final val ap = new DFInterface {
    final val start   = DFBit() <> IN
    final val done    = DFBit() <> OUT
    final val idle    = DFBit() <> OUT
    final val ready   = DFBit() <> OUT
  }
  //TODO:
  //need to replace DF-AXI inputs => AXI4(AXI4.Config(true, false)) + an input <name>_offset(64)
  //need to replace DF-AXI outputs => AXI4(AXI4.Config(false, true)) + an input <name>_offset(64)
  //need to have size 32-bit input
  //inputs may be should be cast to/from DFBits.
}


