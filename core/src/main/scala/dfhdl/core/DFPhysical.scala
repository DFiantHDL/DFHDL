package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName
import scala.annotation.implicitNotFound

extension (bd: BigDecimal.type)
  private def apply(arg: Int | Long | Double): BigDecimal = arg match
    case i: Int    => BigDecimal(i)
    case l: Long   => BigDecimal(l)
    case d: Double => BigDecimal(d)

type DFPhysical[U <: ir.DFPhysical.Unit] = DFType[ir.DFPhysical[U], NoArgs]
object DFPhysical:
  given DFTime = DFTime
  given DFFreq = DFFreq
  given DFNumber = DFNumber
  object Val:
    object Ops:
      extension (lhs: Int | Double)
        def fs(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.fs), named = true)
        def ps(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.ps), named = true)
        def ns(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.ns), named = true)
        def us(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.us), named = true)
        def ms(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.ms), named = true)
        def sec(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.sec), named = true)
        def min(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.min), named = true)
        def hr(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), ir.DFTime.Unit.hr), named = true)
        def Hz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), ir.DFFreq.Unit.Hz), named = true)
        def KHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), ir.DFFreq.Unit.KHz), named = true)
        def MHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), ir.DFFreq.Unit.MHz), named = true)
        def GHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), ir.DFFreq.Unit.GHz), named = true)
      end extension
      // extension [U <: PhysicalUnit, LP](lhs: DFValTP[DFPhysical[U], LP])
      //   def +[RP](rhs: DFValTP[DFPhysical[U], RP])(using DFC): DFValTP[DFPhysical[U], LP | RP] =
      //     DFVal.Func(lhs.dfType, FuncOp.+, List(lhs, rhs))
      //   def -[RP](rhs: DFValTP[DFPhysical[U], RP])(using DFC): DFValTP[DFPhysical[U], LP | RP] =
      //     DFVal.Func(lhs.dfType, FuncOp.-, List(lhs, rhs))
      //   // division of the same physical types results in a number
      //   def /[RP](rhs: DFValTP[DFPhysical[U], RP])(using DFC): DFValTP[DFNumber, LP | RP] =
      //     DFVal.Func(DFNumber, FuncOp./, List(lhs, rhs))
      //   def *[RP](rhs: Int | Double)(using DFC): DFValTP[DFPhysical[U], LP] =
      //     val rhsVal = DFVal.Const(DFNumber, (BigDecimal(rhs), ir.DFPhysical.Unit.Number))
      //     DFVal.Func(lhs.dfType, FuncOp.*, List(lhs, rhsVal)).asValTP[DFPhysical[U], LP]
      //   def *[RP](rhs: DFValTP[DFNumber, RP])(using DFC): DFValTP[DFPhysical[U], LP | RP] =
      //     DFVal.Func(lhs.dfType, FuncOp.*, List(lhs, rhs))
      // extension [LP](lhs: DFValTP[DFNumber, LP])
      //   @targetName("numberMulPhysical")
      //   def *[U <: PhysicalUnit, RP](rhs: DFValTP[DFPhysical[U], RP])(using
      //       DFC
      //   ): DFValTP[DFPhysical[U], LP | RP] =
      //     DFVal.Func(rhs.dfType, FuncOp.*, List(lhs, rhs))
      // extension (lhs: Int | Double)
      //   def *[U <: PhysicalUnit, RP](rhs: DFValTP[DFPhysical[U], RP])(using
      //       DFC
      //   ): DFValTP[DFPhysical[U], RP] =
      //     val lhsVal = DFVal.Const(DFNumber, (BigDecimal(lhs), ir.DFPhysical.Unit.Number))
      //     DFVal.Func(rhs.dfType, FuncOp.*, List(lhsVal, rhs)).asValTP[DFPhysical[U], RP]
      // extension [LP](lhs: DFValTP[DFFreq, LP])
      //   @targetName("freqMulTime")
      //   def *[RP](rhs: DFValTP[DFTime, RP])(using DFC): DFValTP[DFNumber, LP | RP] =
      //     DFVal.Func(DFNumber, FuncOp.*, List(lhs, rhs)).asValTP[DFNumber, LP | RP]
      // extension [LP](lhs: DFValTP[DFTime, LP])
      //   @targetName("timeMulFreq")
      //   def *[RP](rhs: DFValTP[DFFreq, RP])(using DFC): DFValTP[DFNumber, LP | RP] =
      //     DFVal.Func(DFNumber, FuncOp.*, List(lhs, rhs)).asValTP[DFNumber, LP | RP]
    end Ops
  end Val
end DFPhysical

type DFTime = DFPhysical[ir.DFTime.Unit]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[ir.DFFreq.Unit]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[ir.DFNumber.Unit]
val DFNumber = ir.DFNumber.asFE[DFNumber]
