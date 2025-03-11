package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.compiler.printing.{DefaultPrinter, Printer}
import dfhdl.internals.*
import scala.annotation.unchecked.uncheckedVariance
import ir.DFPhysical.Unit as PhysicalUnit
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName
import scala.annotation.implicitNotFound

extension (bd: BigDecimal.type)
  private def apply(arg: Int | Long | Double): BigDecimal = arg match
    case i: Int    => BigDecimal(i)
    case l: Long   => BigDecimal(l)
    case d: Double => BigDecimal(d)

type DFPhysical[+U <: PhysicalUnit] = DFType[ir.DFPhysical, Args1[U @uncheckedVariance]]
object DFPhysical:
  given [U <: PhysicalUnit](using ValueOf[U]): DFPhysical[U] =
    ir.DFPhysical(valueOf[U]).asFE[DFPhysical[U]]
  object Val:
    object Ops:
      protected type CYInRT = AssertGiven[
        DomainType.RT,
        "`.cy` unit is only allowed under register-transfer (RT) domains."
      ]
      extension (lhs: Int | Long)
        def cy(using DFC, CYInRT): DFConstOf[DFCycles] =
          DFVal.Const(DFCycles, (BigDecimal(lhs), ir.DFPhysical.Unit.Cycles), named = true)
      extension (lhs: Int | Double)
        def fs(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.fs), named = true)
        def ps(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.ps), named = true)
        def ns(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.ns), named = true)
        def us(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.us), named = true)
        def ms(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.ms), named = true)
        def sec(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.sec), named = true)
        def min(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.min), named = true)
        def hr(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, (BigDecimal(lhs), PhysicalUnit.Time.Scale.hr), named = true)
        def Hz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), PhysicalUnit.Freq.Scale.Hz), named = true)
        def KHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), PhysicalUnit.Freq.Scale.KHz), named = true)
        def MHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), PhysicalUnit.Freq.Scale.MHz), named = true)
        def GHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, (BigDecimal(lhs), PhysicalUnit.Freq.Scale.GHz), named = true)
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

type DFTime = DFPhysical[PhysicalUnit.Time.type]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[PhysicalUnit.Freq.type]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[PhysicalUnit.Number.type]
val DFNumber = ir.DFNumber.asFE[DFNumber]
type DFCycles = DFPhysical[PhysicalUnit.Cycles.type]
val DFCycles = ir.DFCycles.asFE[DFCycles]
type Duration = DFTime | DFCycles
