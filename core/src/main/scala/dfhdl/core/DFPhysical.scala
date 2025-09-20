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

type DFPhysical[U <: ir.PhysicalNumber] = DFType[ir.DFPhysical[U], NoArgs]
object DFPhysical:
  given DFTime = DFTime
  given DFFreq = DFFreq
  given DFNumber = DFNumber
  trait TC[-T]:
    type OutU <: ir.PhysicalNumber
    type OutP
    def apply(value: T)(using DFC): DFValTP[DFPhysical[OutU], OutP]
  object TC:
    type Aux[T, U <: ir.PhysicalNumber, P] = TC[T] { type OutU = U; type OutP = P }
    given [U <: ir.PhysicalNumber, P]: Aux[DFValTP[DFPhysical[U], P], U, P] =
      new TC[DFValTP[DFPhysical[U], P]]:
        type OutU = U
        type OutP = P
        def apply(value: DFValTP[DFPhysical[U], P])(using DFC): DFValTP[DFPhysical[U], P] = value
    given fromDFIntOrDFDouble[P]: TC[DFValTP[DFInt32 | DFDouble, P]] with
      type OutU = ir.LiteralNumber
      type OutP = P
      def apply(value: DFValTP[DFInt32 | DFDouble, P])(using DFC): DFValTP[DFNumber, OutP] =
        Val.Ops.toNumber(value)
    given TC[Int] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Int)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
    given TC[Long] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Long)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
    given TC[Double] with
      type OutU = ir.LiteralNumber
      type OutP = CONST
      def apply(value: Double)(using DFC): DFValTP[DFNumber, OutP] =
        DFVal.Const(DFNumber, ir.LiteralNumber(BigDecimal(value)), named = true)
  end TC
  object Val:
    object Ops:
      trait OpTC[Op <: FuncOp, LU <: ir.PhysicalNumber, RU <: ir.PhysicalNumber]:
        type OutU <: ir.PhysicalNumber
        def apply(
            lhsDFType: DFPhysical[LU],
            rhsDFType: DFPhysical[RU]
        ): DFPhysical[OutU]
      end OpTC
      trait OpTCLP:
        given divSameUnit[U <: ir.PhysicalNumber]: OpTC[FuncOp./.type, U, U] with
          type OutU = ir.LiteralNumber
          def apply(
              lhsDFType: DFPhysical[U],
              rhsDFType: DFPhysical[U]
          ): DFPhysical[ir.LiteralNumber] = DFNumber
      object OpTC extends OpTCLP:
        type Aux[
            Op <: FuncOp,
            LU <: ir.PhysicalNumber,
            RU <: ir.PhysicalNumber,
            OU <: ir.PhysicalNumber
        ] =
          OpTC[Op, LU, RU] { type OutU = OU }
        given add[U <: ir.PhysicalNumber]: OpTC[FuncOp.+.type, U, U] with
          type OutU = U
          def apply(
              lhsDFType: DFPhysical[U],
              rhsDFType: DFPhysical[U]
          ): DFPhysical[U] = lhsDFType
        given sub[U <: ir.PhysicalNumber]: OpTC[FuncOp.-.type, U, U] with
          type OutU = U
          def apply(
              lhsDFType: DFPhysical[U],
              rhsDFType: DFPhysical[U]
          ): DFPhysical[U] = lhsDFType
        given divNumbers[U <: ir.PhysicalNumber]: OpTC[FuncOp./.type, U, ir.LiteralNumber] with
          type OutU = U
          def apply(
              lhsDFType: DFPhysical[U],
              rhsDFType: DFNumber
          ): DFPhysical[U] = lhsDFType
        given mulNumbers[U <: ir.PhysicalNumber]: OpTC[FuncOp.`*`.type, U, ir.LiteralNumber] with
          type OutU = U
          def apply(
              lhsDFType: DFPhysical[U],
              rhsDFType: DFNumber
          ): DFPhysical[U] = lhsDFType
        given mulFreqTime: OpTC[FuncOp.`*`.type, ir.FreqNumber, ir.TimeNumber] with
          type OutU = ir.LiteralNumber
          def apply(
              lhsDFType: DFPhysical[ir.FreqNumber],
              rhsDFType: DFPhysical[ir.TimeNumber]
          ): DFPhysical[ir.LiteralNumber] = DFNumber
        given mulTimeFreq: OpTC[FuncOp.`*`.type, ir.TimeNumber, ir.FreqNumber] with
          type OutU = ir.LiteralNumber
          def apply(
              lhsDFType: DFPhysical[ir.TimeNumber],
              rhsDFType: DFPhysical[ir.FreqNumber]
          ): DFPhysical[ir.LiteralNumber] = DFNumber
        given divNumberTime: OpTC[FuncOp./.type, ir.LiteralNumber, ir.TimeNumber] with
          type OutU = ir.FreqNumber
          def apply(
              lhsDFType: DFPhysical[ir.LiteralNumber],
              rhsDFType: DFPhysical[ir.TimeNumber]
          ): DFPhysical[ir.FreqNumber] = DFFreq
        given divNumberFreq: OpTC[FuncOp./.type, ir.LiteralNumber, ir.FreqNumber] with
          type OutU = ir.TimeNumber
          def apply(
              lhsDFType: DFPhysical[ir.LiteralNumber],
              rhsDFType: DFPhysical[ir.FreqNumber]
          ): DFPhysical[ir.TimeNumber] = DFTime
      end OpTC
      extension (lhs: Int | Long | Double)
        def fs(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.fs), named = true)
        def ps(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.ps), named = true)
        def ns(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.ns), named = true)
        def us(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.us), named = true)
        def ms(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.ms), named = true)
        def sec(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.sec), named = true)
        def mn(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.mn), named = true)
        def hr(using DFC): DFConstOf[DFTime] =
          DFVal.Const(DFTime, ir.TimeNumber(BigDecimal(lhs), ir.TimeNumber.Unit.hr), named = true)
        def Hz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, ir.FreqNumber(BigDecimal(lhs), ir.FreqNumber.Unit.Hz), named = true)
        def KHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, ir.FreqNumber(BigDecimal(lhs), ir.FreqNumber.Unit.KHz), named = true)
        def MHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, ir.FreqNumber(BigDecimal(lhs), ir.FreqNumber.Unit.MHz), named = true)
        def GHz(using DFC): DFConstOf[DFFreq] =
          DFVal.Const(DFFreq, ir.FreqNumber(BigDecimal(lhs), ir.FreqNumber.Unit.GHz), named = true)
      end extension

      given evOpArithDFPhysical[
          Op <: FuncOp.+.type | FuncOp.-.type | FuncOp.*.type | FuncOp./.type,
          U <: ir.PhysicalNumber,
          LP,
          L <: DFValTP[DFPhysical[U], LP],
          R,
          RU <: ir.PhysicalNumber,
          RP,
          OU <: ir.PhysicalNumber
      ](using
          tcR: TC.Aux[R, RU, RP],
          tcOp: OpTC.Aux[Op, U, RU, OU],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFPhysical[OU], LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFPhysical[OU], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val rhsVal = tcR(rhs)
            DFVal.Func(tcOp(lhs.dfType, rhsVal.dfType), op, List(lhs, rhsVal))
          }(using dfc, CTName(op.value.toString))
      end evOpArithDFPhysical

      extension [LP](lhs: DFValTP[DFInt32 | DFDouble, LP])
        def toNumber(using DFC): DFValTP[DFNumber, LP] = trydf {
          DFVal.Alias.AsIs(DFNumber, lhs, forceNewAlias = true)
        }
      extension [LP](lhs: DFValTP[DFNumber, LP])
        @targetName("fromNumberToInt")
        def toInt(using DFC): DFValTP[DFInt32, LP] = trydf {
          DFVal.Alias.AsIs(DFInt32, lhs, forceNewAlias = true)
        }
        def toDouble(using DFC): DFValTP[DFDouble, LP] = trydf {
          DFVal.Alias.AsIs(DFDouble, lhs, forceNewAlias = true)
        }
    end Ops
  end Val
end DFPhysical

type DFTime = DFPhysical[ir.TimeNumber]
val DFTime = ir.DFTime.asFE[DFTime]
type DFFreq = DFPhysical[ir.FreqNumber]
val DFFreq = ir.DFFreq.asFE[DFFreq]
type DFNumber = DFPhysical[ir.LiteralNumber]
val DFNumber = ir.DFNumber.asFE[DFNumber]
