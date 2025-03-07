package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFRange.Op as RangeOp
final class DFRange[P](val irValue: ir.DFRange | DFError) extends AnyVal with DFMember[ir.DFRange]:
  // TODO: foreach iterator could be const (DFValTP[DFInt32, P]), but for now the IR internals do not
  // differentiate between a constant interator and constant literal. So disabling the frontend
  // possibility for now. If for loops will be used for generate emulation, we need to change how
  // iterator constant derived parameters are checked.
  def foreach[T <: DFValTP[DFInt32, NOTCONST]](f: T => Unit)(using DFC): Unit =
    throw new IllegalArgumentException(
      "foreach is not meant to be run directly, the DFHDL compiler plugin should have replaced its call."
    )
  def by[SP](step: DFValTP[DFInt32, SP])(using dfc: DFC): DFRange[P | SP] =
    import dfc.getSet
    this.asIR.stepRef.get.replaceMemberWith(step.asIR)
    this.asInstanceOf[DFRange[P | SP]]
object DFRange:
  extension (range: ir.DFRange) def asFE[P]: DFRange[P] = new DFRange[P](range)
  extension (range: DFRange[?]) def asFE[P]: DFRange[P] = range.asInstanceOf[DFRange[P]]
  def apply[P](
      start: DFValTP[DFInt32, P],
      end: DFValTP[DFInt32, P],
      op: RangeOp,
      step: DFValTP[DFInt32, P] = DFConstInt32(1)
  )(using DFC): DFRange[P] =
    val member = ir.DFRange(
      startRef = start.asIR.refTW[ir.DFRange],
      endRef = end.asIR.refTW[ir.DFRange],
      op = op,
      stepRef = step.asIR.refTW[ir.DFRange],
      ownerRef = dfc.owner.ref,
      meta = dfc.getMeta,
      tags = dfc.tags
    )
    member.addMember.asFE[P]
  end apply
  object Ops:
    extension (start: Int)
      private[core] def untilOrig(end: Int): Range = Range.Exclusive(start, end, 1)
      private[core] def untilDF(end: Int)(using DFC): DFRange[CONST] =
        DFRange(DFConstInt32(start), DFConstInt32(end), RangeOp.Until)
      // until is selected at compile time, according to the context
      transparent inline def until(end: Int): Range | DFRange[CONST] =
        compiletime.summonFrom {
          case given DFC.Scope.Process => untilDF(end)(using compiletime.summonInline[DFC])
          case _                       => untilOrig(end)
        }
      def until[P](end: DFValTP[DFInt32, P])(using DFC): DFRange[P] =
        DFRange(DFConstInt32(start), end, RangeOp.Until).asFE[P]
    extension [SP](start: DFValTP[DFInt32, SP])
      def until[EP](end: DFValTP[DFInt32, EP])(using DFC): DFRange[SP | EP] =
        DFRange(start, end, RangeOp.Until).asFE[SP | EP]
    extension (start: Int)
      private[core] def toOrig(end: Int): Range = Range.Inclusive(start, end, 1)
      private[core] def toDF(end: Int)(using DFC): DFRange[CONST] =
        DFRange(DFConstInt32(start), DFConstInt32(end), RangeOp.To)
      // to is selected at compile time, according to the context
      transparent inline def to(end: Int): Range | DFRange[CONST] =
        compiletime.summonFrom {
          case given DFC.Scope.Process => toDF(end)(using compiletime.summonInline[DFC])
          case _                       => toOrig(end)
        }
      def to[P](end: DFValTP[DFInt32, P])(using DFC): DFRange[P] =
        DFRange(DFConstInt32(start), end, RangeOp.To).asFE[P]
    extension [SP](start: DFValTP[DFInt32, SP])
      def to[EP](end: DFValTP[DFInt32, EP])(using DFC): DFRange[SP | EP] =
        DFRange(start, end, RangeOp.To).asFE[SP | EP]
  end Ops
end DFRange
