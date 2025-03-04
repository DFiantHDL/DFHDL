// package dfhdl.core
// import dfhdl.compiler.ir
// import ir.Timer.Func.Op as FuncOp
// import ir.Ratio
// import dfhdl.internals.*
// import scala.annotation.targetName

// sealed class Timer private (val irValue: ir.Timer | DFError) extends DFMember[ir.Timer]
// object Timer:
//   extension (timer: ir.Timer) private def asFE: Timer = new Timer(timer)

//   def apply()(using DFC): Timer = Periodic(None, None)
//   @targetName("applyPeriod")
//   def apply(rate: Rate)(using DFC): Timer = Periodic(None, Some(rate))
//   def apply(trigger: DFValOf[DFBit])(using DFC): Timer = Periodic(Some(trigger), None)
//   def apply(trigger: DFValOf[DFBit], rate: Rate)(using DFC): Timer =
//     Periodic(Some(trigger), Some(rate))
//   object Ops:
//     extension (timer: Timer)
//       def *(ratio: Int | Double)(using DFC): Timer =
//         Timer.Func(timer, FuncOp.`*`, Ratio(BigDecimal(ratio)))
//       def /(ratio: Int | Double)(using DFC): Timer =
//         Timer.Func(timer, FuncOp./, Ratio(BigDecimal(ratio)))
//       def delay(arg: Time)(using DFC): Timer =
//         Timer.Func(timer, FuncOp.Delay, arg)
//       def isActive(using DFC): DFValOf[DFBool] =
//         Timer.IsActive(timer)
//   end Ops

//   object Periodic:
//     def apply(trigger: Option[DFValOf[DFBit]], rateOpt: Option[Rate])(using DFC): Timer =
//       val triggerRef: ir.Timer.TriggerRef = trigger match
//         case Some(value) => value.asIR.refTW[ir.Timer]
//         case None        => ir.DFRef.TwoWay.Empty
//       val timer: ir.Timer = ir.Timer.Periodic(
//         triggerRef, rateOpt.map(_.asIR), dfc.owner.ref, dfc.getMeta, dfc.tags
//       ).addMember
//       timer.asFE
//   end Periodic
//   object Func:
//     def apply(source: Timer, op: FuncOp, arg: Time | Ratio)(using DFC): Timer =
//       val timer: ir.Timer = ir.Timer.Func(
//         source.asIR.refTW[ir.Timer],
//         op,
//         arg,
//         dfc.owner.ref,
//         dfc.getMeta,
//         dfc.tags
//       ).addMember
//       timer.asFE

//   object IsActive:
//     def apply(timer: Timer)(using DFC): DFValOf[DFBool] =
//       val dfVal: ir.DFVal = ir.Timer.IsActive(
//         timer.asIR.refTW[ir.DFVal],
//         dfc.owner.ref,
//         dfc.getMeta,
//         dfc.tags
//       ).addMember
//       dfVal.asValOf[DFBool]
// end Timer
