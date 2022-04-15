package DFiant.core
import DFiant.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import DFiant.internals.*

import annotation.{implicitNotFound, targetName}

type Bit = 0 | 1
type BitOrBool = Bit | Boolean
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
    ): T <> TOKEN =
      ir.DFToken(dfType.asIR)(data).asTokenOf[T]
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Boolean
    ): T <> TOKEN =
      Token(dfType, Some(value))
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bit
    ): T <> TOKEN =
      Token(dfType, value > 0)
    protected[core] def apply[T <: DFBoolOrBit](
        dfType: T,
        value: Bubble
    ): T <> TOKEN =
      Token(dfType, None)

    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit token."
    )
    trait Candidate[-R]:
      type OutT <: DFBoolOrBit
      def apply(arg: R): DFToken[OutT]
    object Candidate:
      type Aux[-R, T <: DFBoolOrBit] = Candidate[R] { type OutT = T }
      transparent inline given fromBooleanSing[
          R <: Boolean
      ]: Candidate[ValueOf[R]] = new Candidate[ValueOf[R]]:
        type OutT = DFBool
        def apply(arg: ValueOf[R]): DFToken[DFBool] =
          Token(DFBool, arg.value)
      transparent inline given fromBoolean: Candidate[Boolean] =
        new Candidate[Boolean]:
          type OutT = DFBool
          def apply(arg: Boolean): DFToken[DFBool] =
            Token(DFBool, arg)
      transparent inline given fromBit[
          R <: Bit
      ]: Candidate[ValueOf[R]] = new Candidate[ValueOf[R]]:
        type OutT = DFBit
        def apply(arg: ValueOf[R]): DFToken[DFBit] =
          Token(DFBit, arg.value)
      transparent inline given fromDFBoolOrBitToken[
          T <: DFBoolOrBit
      ]: Candidate[DFToken[T]] = new Candidate[DFToken[T]]:
        type OutT = T
        def apply(arg: DFToken[T]): DFToken[T] = arg
    end Candidate

    object TC:
      import DFToken.TC
      given DFBoolTokenFromCandidate[T <: DFBoolOrBit, R](using
          ic: Candidate[R]
      ): TC[T, R] with
        def conv(dfType: T, arg: R): Out =
          val tokenArg = ic(arg)
          val tokenOut = (dfType, tokenArg.dfType) match
            case (DFBit, DFBool) => Token(DFBit, tokenArg.data)
            case (DFBool, DFBit) => Token(DFBool, tokenArg.data)
            case _               => tokenArg
          tokenOut.asIR.asTokenOf[T]
    end TC

    private def logicOp[O <: DFBoolOrBit, T <: DFBoolOrBit](
        dfType: O,
        token: DFToken[T],
        tokenArg: DFToken[DFBoolOrBit],
        op: FuncOp
    ): DFToken[O] =
      val dataOut = (token.data, tokenArg.data) match
        case (Some(l), Some(r)) =>
          op match
            case FuncOp.=== => Some(l == r)
            case FuncOp.=!= => Some(l != r)
            case FuncOp.|   => Some(l || r)
            case FuncOp.&   => Some(l && r)
            case FuncOp.^   => Some(l ^ r)
            case _          => throw new IllegalArgumentException("Unsupported Op")
        case _ => None
      Token(dfType, dataOut)
    end logicOp

    private def logicOp[T <: DFBoolOrBit](
        token: DFToken[T],
        tokenArg: DFToken[DFBoolOrBit],
        op: FuncOp
    ): DFToken[T] = logicOp[T, T](token.dfType, token, tokenArg, op)

    object Compare:
      import DFToken.Compare
      given DFBoolOrBitCompare[T <: DFBoolOrBit, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R],
          op: ValueOf[Op],
          castle: ValueOf[C]
      ): Compare[T, R, Op, C] with
        def conv(dfType: T, arg: R): DFToken[T] =
          val tokenArg = ic(arg)
          Token(dfType, tokenArg.data)
    end Compare

    object Ops:
      extension (lhs: DFBit <> TOKEN)
        def bool: DFBool <> TOKEN = Token(DFBool, lhs.data)
        @targetName("notOfDFBit")
        def unary_! : DFBit <> TOKEN = Token(DFBit, lhs.data.map(!_))
      extension (lhs: DFBool <> TOKEN)
        def bit: DFBit <> TOKEN = Token(DFBit, lhs.data)
        @targetName("notOfDFBool")
        def unary_! : DFBool <> TOKEN = Token(DFBool, lhs.data.map(!_))
      extension [T <: DFBoolOrBit](lhs: T <> TOKEN)
        def ||[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.|)
        def &&[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.&)
        def ^[R](rhs: Exact[R])(using ic: Candidate[R]): T <> TOKEN =
          logicOp(lhs, ic(rhs), FuncOp.^)
      extension [L](lhs: L)
        def ||[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.|)
        def &&[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.&)
        def ^[RT <: DFBoolOrBit](
            rhs: DFToken[RT]
        )(using es: Exact.Summon[L, lhs.type])(using
            ic: Candidate[es.Out]
        ): RT <> TOKEN = logicOp(rhs, ic(es(lhs)), FuncOp.^)
      end extension
    end Ops
  end Token

  object Val:
    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a DFBool or DFBit dataflow value."
    )
    trait Candidate[-R]:
      type OutT <: DFBoolOrBit
      def apply(arg: R)(using DFC): DFValOf[OutT]
    object Candidate:
      transparent inline given fromTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutT = ic.OutT
        def apply(arg: R)(using DFC): DFValOf[OutT] = DFVal.Const(ic(arg))
      transparent inline given fromDFBoolOrBitVal[T <: DFBoolOrBit]: Candidate[T <> VAL] =
        new Candidate[T <> VAL]:
          type OutT = T
          def apply(arg: T <> VAL)(using DFC): T <> VAL = arg

    private def b2b[T <: DFBoolOrBit, R](dfType: T, arg: R)(using
        ic: Candidate[R],
        dfc: DFC
    ): T <> VAL =
      val dfcAnon = dfc.anonymize
      import Ops.{bit, bool}
      val dfValArg = ic(arg)(using dfcAnon)
      val dfValOut = (dfType, dfValArg.dfType) match
        case (DFBit, DFBool) => dfValArg.asIR.asValOf[DFBool].bit(using dfcAnon)
        case (DFBool, DFBit) => dfValArg.asIR.asValOf[DFBit].bool(using dfcAnon)
        case _               => dfValArg
      dfValOut.asIR.asValOf[T]

    object TC:
      import DFVal.TC
      given DFBoolOrBitFromCandidate[T <: DFBoolOrBit, R](using
          dfc: DFC,
          ic: Candidate[R]
      ): TC[T, R] with
        def conv(dfType: T, arg: R): Out = b2b(dfType, arg)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBoolOrBitCompare[T <: DFBoolOrBit, R, Op <: FuncOp, C <: Boolean](using
          DFC,
          Candidate[R],
          ValueOf[Op],
          ValueOf[C]
      ): Compare[T, R, Op, C] with
        def conv(dfType: T, arg: R): T <> VAL =
          b2b(dfType, arg)

    object Ops:
      extension (lhs: DFBit <> VAL)
        def rising(using DFC): DFBool <> VAL = trydf {
          DFVal.Func(DFBool, FuncOp.rising, List(lhs))
        }
        def falling(using DFC): DFBool <> VAL = trydf {
          DFVal.Func(DFBool, FuncOp.falling, List(lhs))
        }
        def bool(using DFC): DFBool <> VAL = trydf {
          import Token.Ops.{bool => boolToken}
          DFVal.Alias.AsIs(DFBool, lhs, _.boolToken)
        }
        @targetName("notOfDFBit")
        def unary_!(using DFC): DFBit <> VAL = trydf {
          DFVal.Func(DFBit, FuncOp.unary_!, List(lhs))
        }
      end extension
      extension (lhs: DFBool <> VAL)
        def bit(using DFC): DFBit <> VAL = trydf {
          import Token.Ops.{bit => bitToken}
          DFVal.Alias.AsIs(DFBit, lhs, _.bitToken)
        }
        @targetName("notOfDFBool")
        def unary_!(using DFC): DFBool <> VAL = trydf {
          DFVal.Func(DFBool, FuncOp.unary_!, List(lhs))
        }

      private def logicOp[T <: DFBoolOrBit, R](
          dfVal: T <> VAL,
          arg: R,
          op: FuncOp,
          castle: Boolean
      )(using dfc: DFC, ic: Candidate[R]): T <> VAL =
        val dfValArg = b2b(dfVal.dfType, arg)
        val (lhs, rhs) = if (castle) (dfValArg, dfVal) else (dfVal, dfValArg)
        DFVal.Func(lhs.dfType.asIR.asFE[T], op, List(lhs, rhs))
      extension [T <: DFBoolOrBit](lhs: T <> VAL)
        def ||[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): T <> VAL =
          trydf { logicOp[T, R](lhs, rhs, FuncOp.|, false) }
        def &&[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): T <> VAL =
          trydf { logicOp[T, R](lhs, rhs, FuncOp.&, false) }
        def ^[R](rhs: Exact[R])(using dfc: DFC, ic: Candidate[R]): T <> VAL =
          trydf { logicOp[T, R](lhs, rhs, FuncOp.^, false) }
      extension [L](lhs: L)
        def ||[RT <: DFBoolOrBit](
            rhs: RT <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): RT <> VAL = trydf { logicOp(rhs, ic(es(lhs)), FuncOp.|, true) }
        def &&[RT <: DFBoolOrBit](
            rhs: RT <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): RT <> VAL = trydf { logicOp(rhs, ic(es(lhs)), FuncOp.&, true) }
        def ^[RT <: DFBoolOrBit](
            rhs: RT <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            ic: Candidate[es.Out]
        ): RT <> VAL = trydf { logicOp(rhs, ic(es(lhs)), FuncOp.^, true) }
      end extension
    end Ops
  end Val
end DFBoolOrBit

type DFBool = DFType[ir.DFBool.type, NoArgs]
final lazy val DFBool = ir.DFBool.asFE[DFBool]
type DFBit = DFType[ir.DFBit.type, NoArgs]
final lazy val DFBit = ir.DFBit.asFE[DFBit]
given CanEqual[DFBoolOrBit, DFBoolOrBit] = CanEqual.derived
