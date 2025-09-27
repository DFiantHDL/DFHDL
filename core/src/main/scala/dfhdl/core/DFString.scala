package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp
import scala.annotation.targetName
type DFString = TDFString
final val DFString = new DFType(ir.DFString).asInstanceOf[DFString]

type TDFString = DFType[ir.DFString, NoArgs]
object TDFString:
  given DFString = DFString

  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutT = DFString
      type OutP
      type Out = DFValTP[OutT, OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out

    object Candidate:
      type Exact = Exact0[DFC, Candidate]
      type Aux[R, P] = Candidate[R] { type OutP = P }
      given fromString[R <: String]: Candidate[R] with
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(DFString, Some(arg), named = true)
      given fromDFStringVal[P, R <: DFValTP[DFString, P]]: Candidate[R] with
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg

    object TC:
      import DFVal.TC
      given DFStringFromCandidate[R, IC <: Candidate[R]](using ic: IC): TC[DFString, R] with
        type OutP = ic.OutP
        def conv(dfType: DFString, arg: R)(using DFC): Out = ic(arg)

    object Compare:
      import DFVal.Compare
      given DFStringCompare[R, IC <: Candidate[R], Op <: FuncOp, C <: Boolean](using
          ic: IC,
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFString, R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFString, arg: R)(using DFC): Out = ic(arg)

    object Ops:
      given evOpArithDFString[
          LP,
          L <: DFValOf[DFString] | String,
          RP,
          R <: DFValOf[DFString] | String
      ](using
          icL: Candidate.Aux[L, LP],
          icR: Candidate.Aux[R, RP]
      ): ExactOp2Aux[FuncOp.+.type, DFC, DFValAny, L, R, DFValTP[DFString, LP | RP]] =
        new ExactOp2[FuncOp.+.type, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFString, LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            DFVal.Func(DFString, FuncOp.++, List(lhsVal, rhsVal))
          }(using dfc, CTName("+"))
      end evOpArithDFString
      extension [P](lhs: DFValTP[DFString, P])
        def toScalaString(using DFC, DFVal.ConstCheck[P]): String =
          lhs.toScalaValue
      end extension
    end Ops
  end Val
end TDFString
