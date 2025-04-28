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
      extension [P](lhs: DFValTP[DFString, P])
        def toScalaString(using DFC, DFVal.ConstCheck[P]): String =
          lhs.toScalaValue

        @targetName("plusDFString")
        def +(rhs: Candidate.Exact)(using DFC): DFValTP[DFString, P | rhs.tc.OutP] =
          DFVal.Func(DFString, FuncOp.++, List(lhs, rhs()))
      end extension
    end Ops
  end Val
end TDFString
