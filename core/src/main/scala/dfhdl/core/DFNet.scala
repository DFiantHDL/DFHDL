package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*

final class DFNet(val irValue: ir.DFNet | DFError) extends AnyVal with DFMember[ir.DFNet]
object DFNet:
  export ir.DFNet.Op
  extension (net: ir.DFNet) def asFE: DFNet = new DFNet(net)

  def apply(toVal: ir.DFVal, op: Op, fromVal: ir.DFVal)(using DFC): DFNet =
    val net: ir.DFNet = ir.DFNet(
      toVal.refTW[ir.DFNet],
      op,
      fromVal.refTW[ir.DFNet],
      dfc.owner.ref,
      dfc.getMeta,
      ir.DFTags.empty
    )
    net.addMember.asFE
end DFNet
