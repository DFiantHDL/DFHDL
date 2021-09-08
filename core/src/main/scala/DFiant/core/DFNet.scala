package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

type DFNet = OpaqueDFNet.DFNet
val DFNet = OpaqueDFNet.DFNet

private object OpaqueDFNet:
  opaque type DFNet <: DFMember.Of[ir.DFNet] = DFMember.Of[ir.DFNet]
  object DFNet:
    extension (net: ir.DFNet) def asFE: DFNet = net.asInstanceOf[DFNet]
    export ir.DFNet.Op

    def apply(toVal: ir.DFVal, op: Op, fromVal: ir.DFVal)(using DFC): DFNet =
      lazy val net: ir.DFNet = ir.DFNet(
        toVal.refTW(net),
        op,
        fromVal.refTW(net),
        dfc.owner.ref,
        dfc.getMeta,
        ir.DFTags.empty
      )
      net.addMember.asFE
end OpaqueDFNet
