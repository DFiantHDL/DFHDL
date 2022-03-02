package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

final class DFNet(val value: ir.DFNet | DFError) extends AnyVal with DFMember[ir.DFNet]
object DFNet:
  export ir.DFNet.Op
  extension (net: ir.DFNet) def asFE: DFNet = new DFNet(net)

  def apply(toVal: ir.DFVal, op: Op, fromVal: ir.DFVal)(using DFC): DFNet =
    lazy val net: ir.DFNet = ir.DFNet(
      toVal.refTW(net),
      op,
      fromVal.refTW(net),
      dfc.lateConstruction,
      dfc.owner.ref,
      dfc.getMeta,
      ir.DFTags.empty
    )
    net.addMember.asFE
end DFNet
