package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*

final class DFNet(val irValue: ir.DFNet | DFError) extends DFMember[ir.DFNet] //AnyVal with
object DFNet:
  export ir.DFNet.Op
  extension (net: ir.DFNet) def asFE: DFNet = new DFNet(net)

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
end DFNet
