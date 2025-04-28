package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}

type BitNum = 0 | 1
type BitOrBool = BitNum | Boolean
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  given DFBool = DFBool
  given DFBit = DFBit

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit DFHDL value."
    )
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutT <: DFBoolOrBit
      type OutP
      type Out = DFValTP[OutT, OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out
    object Candidate:
      type Exact = Exact0[DFC, Candidate]
      given fromBoolean[R <: Boolean]: Candidate[R] with
        type OutT = DFBool
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(DFBool, Some(arg), named = true)
      given fromBit[R <: BitNum]: Candidate[R] with
        type OutT = DFBit
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(DFBit, Some(arg > 0), named = true)
      given fromDFBoolOrBitVal[T <: DFBoolOrBit, P, R <: DFValTP[T, P]]: Candidate[R] with
        type OutT = T
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg
    end Candidate

    private def b2b[T <: DFBoolOrBit, RP](
        dfType: T,
        dfValArg: DFValTP[DFBoolOrBit, RP]
    )(using DFC): DFValTP[T, RP] =
      import Ops.{bit, bool}
      val dfValOut = (dfType, dfValArg.dfType) match
        case (DFBit, DFBool) => dfValArg.asValOf[DFBool].bit
        case (DFBool, DFBit) => dfValArg.asValOf[DFBit].bool
        case _               => dfValArg
      dfValOut.asValTP[T, RP]
    private def b2b[T <: DFBoolOrBit, R](dfType: T, arg: R)(using
        ic: Candidate[R],
        dfc: DFC
    ): DFValTP[T, ic.OutP] = b2b(dfType, ic(arg))

    object TC:
      import DFVal.TC
      given DFBoolOrBitFromCandidate[T <: DFBoolOrBit, R, IC <: Candidate[R]](using
          ic: IC
      ): TC[T, R] with
        type OutP = ic.OutP
        def conv(dfType: T, arg: R)(using DFC): Out = b2b(dfType, arg)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBoolOrBitCompare[T <: DFBoolOrBit, R, IC <: Candidate[R], Op <: FuncOp, C <: Boolean](
          using
          ic: IC,
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[T, R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: T, arg: R)(using DFC): Out =
          b2b(dfType, arg)

    object Ops:
      extension [P](lhs: DFValTP[DFBoolOrBit, P])
        def toScalaBoolean(using DFC, DFVal.ConstCheck[P]): Boolean =
          lhs.toScalaValue
        def toScalaBitNum(using DFC, DFVal.ConstCheck[P]): BitNum =
          if (lhs.toScalaBoolean) 1 else 0
      extension [P](lhs: DFValTP[DFBit, P])
        def rising(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.rising, List(lhs))
        }
        def falling(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.falling, List(lhs))
        }
        def bool(using DFC): DFValTP[DFBool, P] = trydf {
          DFVal.Alias.AsIs(DFBool, lhs)
        }
        @targetName("notOfDFBit")
        def unary_!(using DFC): DFValTP[DFBit, P] = trydf {
          DFVal.Func(DFBit, FuncOp.unary_!, List(lhs))
        }
      end extension
      extension [P](lhs: DFValTP[DFBool, P])
        def bit(using DFC): DFValTP[DFBit, P] = trydf {
          DFVal.Alias.AsIs(DFBit, lhs)
        }
        @targetName("notOfDFBool")
        def unary_!(using DFC): DFValTP[DFBool, P] = trydf {
          DFVal.Func(DFBool, FuncOp.unary_!, List(lhs))
        }

      private def logicOp[T <: DFBoolOrBit, P, RP](
          dfVal: DFValTP[T, P],
          arg: DFValTP[DFBoolOrBit, RP],
          op: FuncOp,
          castle: Boolean
      )(using DFC): DFValTP[T, P | RP] =
        val dfValArg = b2b(dfVal.dfType, arg)
        val (lhs, rhs) = if (castle) (dfValArg, dfVal) else (dfVal, dfValArg)
        DFVal.Func(lhs.dfType.asFE[T], op, List(lhs, rhs))
      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        @targetName("notOfDFBoolOrBit")
        private[core] def not(using DFC): DFValTP[T, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_!, List(lhs))
        }
        def ||(rhs: Candidate.Exact)(using DFC): DFValTP[T, P | rhs.tc.OutP] =
          trydf { logicOp(lhs, rhs(), FuncOp.|, false) }
        def &&(rhs: Candidate.Exact)(using DFC): DFValTP[T, P | rhs.tc.OutP] =
          trydf { logicOp(lhs, rhs(), FuncOp.&, false) }
        def ^(rhs: Candidate.Exact)(using DFC): DFValTP[T, P | rhs.tc.OutP] =
          trydf { logicOp(lhs, rhs(), FuncOp.^, false) }
        inline def sel[OT, OF](inline onTrue: OT, inline onFalse: OF)(using
            dfc: DFC
        ): BoolSelWrapper[P, OT, OF] = BoolSelWrapper[P, OT, OF](lhs, onTrue, onFalse)

      end extension
      extension [L](lhs: L)
        inline def ||[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using Candidate[es.Out]): Nothing =
          compiletime.error(
            "Unsupported Scala BitNum/Boolean primitive at the LHS of `||` with a DFHDL value.\nConsider switching positions of the arguments."
          )
        inline def &&[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using Candidate[es.Out]): Nothing =
          compiletime.error(
            "Unsupported Scala BitNum/Boolean primitive at the LHS of `&&` with a DFHDL value.\nConsider switching positions of the arguments."
          )
        inline def ^[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using Candidate[es.Out]): Nothing =
          compiletime.error(
            "Unsupported Scala BitNum/Boolean primitive at the LHS of `^` with a DFHDL value.\nConsider switching positions of the arguments."
          )
      end extension
    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived

type DFConstBool = DFConstOf[DFBool]
type DFConstBit = DFConstOf[DFBit]
