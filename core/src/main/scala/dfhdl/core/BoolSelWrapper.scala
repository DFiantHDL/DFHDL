package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import DFVal.TC
final class BoolSelWrapper[SP, OT, OF](
    val sel: DFValTP[DFBoolOrBit, SP],
    val onTrue: OT,
    val onFalse: OF
):
  private var memoizedSelection: Option[DFValAny] = None
  protected def setMemoizedSelection[T <: DFTypeAny](
      dfType: T,
      onTrue: DFValOf[T],
      onFalse: DFValOf[T]
  )(using DFC): Unit =
    if (memoizedSelection.isEmpty)
      memoizedSelection = Some(
        DFVal.Func(dfType, FuncOp.sel, List(sel, onTrue, onFalse))
      )
  protected def getMemoizedSelection: DFValAny = memoizedSelection.get
end BoolSelWrapper

object BoolSelWrapper:
  given [T <: DFTypeAny, SP, OT, OF](using
      tcOT: TC[T, OT],
      tcOF: TC[T, OF]
  ): TC[T, BoolSelWrapper[SP, OT, OF]] with
    type OutP = SP | tcOT.OutP | tcOF.OutP
    def conv(dfType: T, value: BoolSelWrapper[SP, OT, OF])(using DFC): Out =
      value.setMemoizedSelection(dfType, tcOT(dfType, value.onTrue), tcOF(dfType, value.onFalse))
      value.getMemoizedSelection.asValTP[T, OutP]
