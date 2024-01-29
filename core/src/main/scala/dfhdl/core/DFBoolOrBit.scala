package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}

type BitNum = 0 | 1
type BitOrBool = BitNum | Boolean
type DFBoolOrBit = DFType[ir.DFBoolOrBit, NoArgs]
object DFBoolOrBit:
  type Data = Option[Boolean]
  type Token = DFToken[DFBoolOrBit]
  given DFBool = DFBool
  given DFBit = DFBit
  object Token:
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        data: Option[Boolean]
    ): DFToken[T] = ir.DFToken(dfType.asIR)(data).asTokenOf[T]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Boolean
    ): DFToken[T] = Token(dfType, Some(value))
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: BitNum
    ): DFToken[T] = Token(dfType, value > 0)
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bubble
    ): DFToken[T] = Token(dfType, None)
  end Token

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit DFHDL value."
    )
    trait Candidate[R]:
      type OutT <: DFBoolOrBit
      type OutP
      type Out = DFValTP[OutT, OutP]
      def apply(arg: R)(using DFC): Out
    object Candidate:
      given fromBoolean[R <: Boolean]: Candidate[R] with
        type OutT = DFBool
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(Token(DFBool, arg), named = true)
      given fromBit[R <: BitNum]: Candidate[R] with
        type OutT = DFBit
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(Token(DFBit, arg), named = true)
      given fromDFBoolOrBitVal[T <: DFBoolOrBit, P, R <: DFValTP[T, P]]: Candidate[R] with
        type OutT = T
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg
    end Candidate

    private def b2b[T <: DFBoolOrBit, R](dfType: T, arg: R)(using
        ic: Candidate[R],
        dfc: DFC
    ): DFValTP[T, ic.OutP] =
      import Ops.{bit, bool}
      val dfValArg = ic(arg)
      val dfValOut = (dfType, dfValArg.dfType) match
        case (DFBit, DFBool) => dfValArg.asValOf[DFBool].bit
        case (DFBool, DFBit) => dfValArg.asValOf[DFBit].bool
        case _               => dfValArg
      dfValOut.asValTP[T, ic.OutP]

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
        def toScalaBoolean(using DFVal.ConstCheck[P]): Boolean = true
        def toScalaBitNum(using DFVal.ConstCheck[P]): BitNum = ???
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

      private def logicOp[T <: DFBoolOrBit, P, R](
          dfVal: DFValTP[T, P],
          arg: R,
          op: FuncOp,
          castle: Boolean
      )(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
        val dfValArg = b2b(dfVal.dfType, arg)
        val (lhs, rhs) = if (castle) (dfValArg, dfVal) else (dfVal, dfValArg)
        DFVal.Func(lhs.dfType.asFE[T], op, List(lhs, rhs))
      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        def ||[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.|, false) }
        def &&[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.&, false) }
        def ^[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): DFValTP[T, P | ic.OutP] =
          trydf { logicOp[T, P, R](lhs, rhs, FuncOp.^, false) }
      extension [L](lhs: L)
        def ||[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.|, true).asValTP[RT, RP | ic.OutP]
        }
        def &&[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.&, true).asValTP[RT, RP | ic.OutP]
        }
        def ^[RT <: DFBoolOrBit, RP](
            rhs: DFValTP[RT, RP]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): DFValTP[RT, RP | ic.OutP] = trydf {
          logicOp(rhs, ic(es(lhs)), FuncOp.^, true).asValTP[RT, RP | ic.OutP]
        }
      end extension
    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
