package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFRange.Op as RangeOp
import DFXInt.Val.Ops.toScalaInt

final class DFRange[P](val irValue: ir.DFRange | DFError) extends AnyVal with DFMember[ir.DFRange]:
  def foreach[T <: DFValTP[DFInt32, P]](f: T => Unit)(using DFC): Unit =
    throw new IllegalArgumentException(
      "foreach is not meant to be run directly, the DFHDL compiler plugin should have replaced its call."
    )
  def withFilter[FP](f: DFValTP[DFInt32, P] => DFValTP[DFBoolOrBit, FP]): DFRange[P] =
    throw new IllegalArgumentException(
      "withFilter is not meant to be run directly, the DFHDL compiler plugin should have replaced its call."
    )
  def by[SP](step: DFValTP[DFInt32, SP])(using dfc: DFC): DFRange[P | SP] =
    import dfc.getSet
    this.asIR.stepRef.get.replaceMemberWith(step.asIR)
    this.asInstanceOf[DFRange[P | SP]]
end DFRange
object DFRange:
  extension (range: ir.DFRange) def asFE[P]: DFRange[P] = new DFRange[P](range)
  extension (range: DFRange[?]) def asFE[P]: DFRange[P] = range.asInstanceOf[DFRange[P]]
  def apply[P](using
      DFC
  )(
      start: DFValTP[DFInt32, P],
      end: DFValTP[DFInt32, P],
      op: RangeOp,
      step: DFValTP[DFInt32, P] = DFConstInt32(1)
  ): DFRange[P] =
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

  trait ScalaRangesFlag
  // DFHDL for loop will materialize if ScalaRangesFlag is not in scope and DFC.Scope.HasLoops is in scope.
  // otherwise, it will materialize as a Scala for loop.
  trait HasDFRange
  object HasDFRange:
    given [S <: DFC.Scope](using
        s: S
    )(using util.NotGiven[ScalaRangesFlag], s.type <:< DFC.Scope.HasLoops): HasDFRange with {}

  object Ops:
    extension (start: Int)
      private[core] def untilOrig(end: Int): Range = Range.Exclusive(start, end, 1)
      private[core] def untilDF(end: Int)(using DFC): DFRange[CONST] =
        DFRange(DFConstInt32(start), DFConstInt32(end), RangeOp.Until)
      // until is selected at compile time, according to the context
      transparent inline def until(end: Int): Range | DFRange[CONST] =
        compiletime.summonFrom {
          case given HasDFRange => untilDF(end)(using compiletime.summonInline[DFC])
          case _                => untilOrig(end)
        }
      transparent inline def until[P](end: DFValTP[DFInt32, P])(using DFC): Range | DFRange[P] =
        compiletime.summonFrom {
          case given HasDFRange => DFRange(DFConstInt32(start), end, RangeOp.Until).asFE[P]
          case _                => untilOrig(end.toScalaInt)
        }
    end extension
    extension [SP](start: DFValTP[DFInt32, SP])
      transparent inline def until[EP](end: DFValTP[DFInt32, EP])(using
          DFC
      ): Range | DFRange[SP | EP] =
        compiletime.summonFrom {
          case given HasDFRange => DFRange(start, end, RangeOp.Until).asFE[SP | EP]
          case _                => start.toScalaInt.untilOrig(end.toScalaInt)
        }
    extension (start: Int)
      private[core] def toOrig(end: Int): Range = Range.Inclusive(start, end, 1)
      private[core] def toDF(end: Int)(using DFC): DFRange[CONST] =
        DFRange(DFConstInt32(start), DFConstInt32(end), RangeOp.To)
      // to is selected at compile time, according to the context
      transparent inline def to(end: Int): Range | DFRange[CONST] =
        compiletime.summonFrom {
          case given HasDFRange => toDF(end)(using compiletime.summonInline[DFC])
          case _                => toOrig(end)
        }
      transparent inline def to[P](end: DFValTP[DFInt32, P])(using DFC): Range | DFRange[P] =
        compiletime.summonFrom {
          case given HasDFRange => DFRange(DFConstInt32(start), end, RangeOp.To).asFE[P]
          case _                => toOrig(end.toScalaInt)
        }
    end extension
    extension [SP](start: DFValTP[DFInt32, SP])
      transparent inline def to[EP](end: DFValTP[DFInt32, EP])(using
          DFC
      ): Range | DFRange[SP | EP] =
        compiletime.summonFrom {
          case given HasDFRange => DFRange(start, end, RangeOp.To).asFE[SP | EP]
          case _                => start.toScalaInt.toOrig(end.toScalaInt)
        }
  end Ops
end DFRange
