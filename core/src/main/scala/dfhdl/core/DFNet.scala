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
      dfc.tags
    )
    net.addMember.asFE
end DFNet

sealed trait ConnectPlaceholder
object ConnectPlaceholder extends ConnectPlaceholder:
  transparent inline def errMsg =
    "Found syntax error between a connector `<>` and an operation due to Scala operator precedence.\nTo fix, apply bracket around the RHS of the connection operation.\nE.g., `x <> (y & z)`"
  object Ops:
    extension (cp: ConnectPlaceholder)
      inline def &(arg: Any): Nothing = compiletime.error(errMsg)
      inline def &&(arg: Any): Nothing = compiletime.error(errMsg)
      inline def |(arg: Any): Nothing = compiletime.error(errMsg)
      inline def ||(arg: Any): Nothing = compiletime.error(errMsg)
      inline def ^(arg: Any): Nothing = compiletime.error(errMsg)
