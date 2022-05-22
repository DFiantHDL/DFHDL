package dfhdl.core

import dfhdl.compiler.ir
import ir.Time
import dfhdl.internals.*

sealed class Wait private (val irValue: ir.Wait | DFError) extends DFMember[ir.Wait]
object Wait:
  extension (wait: ir.Wait) private def asFE: Wait = new Wait(wait)
  object Duration:
    def apply(timeOpt: Option[Time])(using DFC): Wait =
      lazy val wait: ir.Wait = ir.Wait
        .Duration(
          timeOpt,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      wait.asFE
  object Until:
    def apply(timeOpt: Option[Time])(using DFC): Wait =
      lazy val wait: ir.Wait = ir.Wait
        .Duration(
          timeOpt,
          dfc.owner.ref,
          dfc.getMeta,
          ir.DFTags.empty
        )
        .addMember
      wait.asFE
  object Ops
end Wait
