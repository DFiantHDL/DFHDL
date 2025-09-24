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
      type Aux[R, P] = Candidate[R] { type OutP = P }
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
      given evOpArithDFDouble[
          Op <: FuncOp.+.type | FuncOp.-.type | FuncOp.*.type | FuncOp./.type | FuncOp.max.type | FuncOp.min.type,
          LPA,
          L <: DFValTP[DFDouble, LPA] | Double,
          LP,
          RPA,
          R <: DFValTP[DFDouble, RPA] | Double,
          RP
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFDouble, LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFDouble, LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            DFVal.Func(DFDouble, op.value, List(lhsVal, rhsVal))
          }(using dfc, CTName(op.value.toString))
      end evOpArithDFDouble

      extension [P](lhs: DFValTP[DFDouble, P])
        def toScalaDouble(using DFC, DFVal.ConstCheck[P]): Double =
          lhs.toScalaValue

        def unary_-(using DFCG): DFValTP[DFDouble, P] = trydf {
          DFVal.Func(DFDouble, FuncOp.unary_-, List(lhs))
        }

        def <[RP](rhs: DFValTP[DFDouble, RP])(using DFCG): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.<, List(lhs, rhs))
        }

        def <=[RP](rhs: DFValTP[DFDouble, RP])(using DFCG): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.<=, List(lhs, rhs))
        }

        def >[RP](rhs: DFValTP[DFDouble, RP])(using DFCG): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.>, List(lhs, rhs))
        }

        def >=[RP](rhs: DFValTP[DFDouble, RP])(using DFCG): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.>=, List(lhs, rhs))
        }
      end extension
    end Ops
  end Val
end TDFDouble
