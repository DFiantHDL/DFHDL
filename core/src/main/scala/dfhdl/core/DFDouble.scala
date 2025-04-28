package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*
import scala.annotation.targetName
type DFDouble = TDFDouble
final val DFDouble = ir.DFDouble.asFE[DFDouble]

//using TDFDouble to have a companion object
type TDFDouble = DFType[ir.DFDouble, NoArgs]
object TDFDouble:
  given DFDouble = DFDouble
  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutT = DFDouble
      type OutP
      type Out = DFValTP[OutT, OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out

    object Candidate:
      type Exact = Exact0[DFC, Candidate]
      given fromDouble[R <: Double]: Candidate[R] with
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(DFDouble, Some(arg), named = true)
      given fromDFDoubleVal[P, R <: DFValTP[DFDouble, P]]: Candidate[R] with
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg

    object TC:
      import DFVal.TC
      given DFDoubleFromCandidate[R, IC <: Candidate[R]](using ic: IC): TC[DFDouble, R] with
        type OutP = ic.OutP
        def conv(dfType: DFDouble, arg: R)(using DFC): Out = ic(arg)

    object Compare:
      import DFVal.Compare
      given DFDoubleCompare[R, IC <: Candidate[R], Op <: FuncOp, C <: Boolean](using
          ic: IC,
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFDouble, R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFDouble, arg: R)(using DFC): Out = ic(arg)

    object Ops:
      extension [P](lhs: DFValTP[DFDouble, P])
        def toScalaDouble(using DFC, DFVal.ConstCheck[P]): Double =
          lhs.toScalaValue

        @targetName("plusDFDouble")
        def +[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp.+, List(lhs, rhs))
        }

        def -[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp.-, List(lhs, rhs))
        }

        def *[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp.*, List(lhs, rhs))
        }

        def /[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp./, List(lhs, rhs))
        }

        def unary_-(using DFC): DFValTP[DFDouble, P] = trydf {
          DFVal.Func(DFDouble, FuncOp.unary_-, List(lhs))
        }

        def max[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp.max, List(lhs, rhs))
        }

        def min[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValTP[DFDouble, P | RP] = trydf {
          DFVal.Func(DFDouble, FuncOp.min, List(lhs, rhs))
        }

        def <[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.<, List(lhs, rhs))
        }

        def <=[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.<=, List(lhs, rhs))
        }

        def >[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.>, List(lhs, rhs))
        }

        def >=[RP](rhs: DFValTP[DFDouble, RP])(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.>=, List(lhs, rhs))
        }
      end extension
    end Ops
  end Val
end TDFDouble
