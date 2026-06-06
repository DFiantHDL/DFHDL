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
      type Types = DFValOf[DFBoolOrBit] | Boolean | BitNum | IfWrapper[?, ?, ?]
      type Aux[R, T <: DFBoolOrBit, P] = Candidate[R] { type OutT = T; type OutP = P }
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
      given fromIf[
          C <: DFValOf[DFBoolOrBit],
          T,
          F,
          TT <: DFBoolOrBit,
          TP,
          FP,
          R <: IfWrapper[C, T, F]
      ](using
          tTC: Candidate[T] { type OutT = TT; type OutP = TP },
          fTC: DFVal.TC[TT, F] { type OutP = FP }
      ): Candidate[R] with
        type OutT = TT
        type OutP = TP | FP
        def apply(value: R)(using DFC): Out = value.unwrap
      end fromIf
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
      given DFBoolOrBitFromCandidate[T <: DFBoolOrBit, R, RP, IC <: Candidate[R]](using
          ic: IC { type OutP = RP }
      ): TC[T, R] with
        type OutP = RP
        def conv(dfType: T, arg: R)(using DFC): Out = b2b(dfType, arg)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBoolOrBitCompare[
          T <: DFBoolOrBit,
          R,
          RP,
          IC <: Candidate[R],
          Op <: FuncOp.===.type | FuncOp.=!=.type,
          C <: Boolean
      ](
          using
          ic: IC { type OutP = RP },
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[T, R, Op, C] with
        type OutP = RP
        def conv(dfType: T, arg: R)(using DFC): Out =
          b2b(dfType, arg)
      end DFBoolOrBitCompare
    end Compare

    object Ops:
      import DFDecimal.Constraints
      import DFVal.Ops.BoolOnlyOp
      given evLogicOpDFBoolOrBit[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          L <: Candidate.Types,
          LT <: DFBoolOrBit,
          LP,
          R <: Candidate.Types,
          RT <: DFBoolOrBit,
          RP
      ](using
          icL: Candidate.Aux[L, LT, LP],
          icR: Candidate.Aux[R, RT, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[LT, LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[LT, LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val lhsVal = icL(lhs)
            val rhsVal = b2b(lhsVal.dfType, icR(rhs))
            DFVal.Func(lhsVal.dfType, op.value, List(lhsVal, rhsVal))
          }
      end evLogicOpDFBoolOrBit
      given evLogicOpDFBoolOrBit2[
          Op <: FuncOp.|.type | FuncOp.&.type,
          L <: Candidate.Types,
          R <: Candidate.Types,
          O <: DFValAny
      ](using
          ic: ExactOp2Aux[Op, DFC, DFValAny, L, R, O]
      ): ExactOp2Aux[BoolOnlyOp[Op], DFC, DFValAny, L, R, O] =
        new ExactOp2[BoolOnlyOp[Op], DFC, DFValAny, L, R]:
          type Out = O
          def apply(lhs: L, rhs: R)(using DFC): Out = ic(lhs, rhs)
      end evLogicOpDFBoolOrBit2

      extension [P](lhs: DFValTP[DFBoolOrBit, P])
        def toScalaBoolean(using DFC, DFVal.ConstCheck[P]): Boolean =
          lhs.toScalaValue
        def toScalaBitNum(using DFC, DFVal.ConstCheck[P]): BitNum =
          if (lhs.toScalaBoolean) 1 else 0
        def toBits[W <: IntP](width: IntParam[W])(using
            DFCG,
            Constraints.Width.CheckNUB[false, W]
        ): DFValTP[DFBits[W], P] = trydf {
          DFVal.Alias.AsIs(DFBits(width), lhs)
        }
        def toUInt[W <: IntP](width: IntParam[W])(using
            DFCG,
            Constraints.Width.CheckNUB[false, W]
        ): DFValTP[DFUInt[W], P] = trydf {
          DFVal.Alias.AsIs(DFUInt(width), lhs)
        }
        def toSInt[W <: IntP](width: IntParam[W])(using
            DFCG,
            Constraints.Width.CheckNUB[true, W]
        ): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Alias.AsIs(DFSInt(width), lhs)
        }
      end extension
      extension [P](lhs: DFValTP[DFBit, P])
        def rising(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.rising, List(lhs))
        }
        def falling(using DFC): DFValOf[DFBool] = trydf {
          DFVal.Func(DFBool, FuncOp.falling, List(lhs))
        }
        def bool(using DFCG): DFValTP[DFBool, P] = trydf {
          DFVal.Alias.AsIs(DFBool, lhs)
        }
        @targetName("notOfDFBit")
        def unary_!(using DFCG): DFValTP[DFBit, P] = trydf {
          DFVal.Func(DFBit, FuncOp.unary_!, List(lhs))
        }
        @targetName("not2OfDFBit")
        inline def unary_~(using DFCG) = lhs.unary_!
      end extension
      extension [P](lhs: DFValTP[DFBool, P])
        def bit(using DFCG): DFValTP[DFBit, P] = trydf {
          DFVal.Alias.AsIs(DFBit, lhs)
        }
        @targetName("notOfDFBool")
        def unary_!(using DFCG): DFValTP[DFBool, P] = trydf {
          DFVal.Func(DFBool, FuncOp.unary_!, List(lhs))
        }
        @targetName("not2OfDFBool")
        inline def unary_~(using DFCG) = lhs.unary_!

      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        @targetName("notOfDFBoolOrBit")
        private[core] def not(using DFC): DFValTP[T, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_!, List(lhs))
        }
        transparent inline def sel[OT, OF](inline onTrue: OT, inline onFalse: OF)(using
            dfc: DFCG
        ): Any =
          inline val onTrueIsDFVal = inline compiletime.erasedValue[OT] match
            case _: DFValAny => true
            case _           => false
          inline val onTrueIsDFConstInt32 = inline compiletime.erasedValue[OT] match
            case _: DFConstInt32 => true
            case _               => false
          inline val onFalseIsDFVal = inline compiletime.erasedValue[OF] match
            case _: DFValAny => true
            case _           => false
          inline val onFalseIsDFConstInt32 = inline compiletime.erasedValue[OF] match
            case _: DFConstInt32 => true
            case _               => false
          // onTrue type has priority, except when onTrue is a DFHDL Int parameter while onFalse is not
          inline if (onTrueIsDFVal && !(onTrueIsDFConstInt32 && !onFalseIsDFConstInt32))
            inline onTrue match
              case ___onTrueDFVal: DFValTP[tt, tp] =>
                val tc = compiletime.summonInline[DFVal.TC[tt, OF]]
                val dfType = ___onTrueDFVal.dfType
                inline if (isConstCheck[OF])
                  DFVal.Func(dfType, FuncOp.sel, List(lhs, ___onTrueDFVal, tc(dfType, onFalse)))
                    .asValTP[tt, P | tp]
                else
                  DFVal.Func(dfType, FuncOp.sel, List(lhs, ___onTrueDFVal, tc(dfType, onFalse)))
                    .asValOf[tt]
          else if (onFalseIsDFVal)
            inline onFalse match
              case ___onFalseDFVal: DFValTP[ft, fp] =>
                val tc = compiletime.summonInline[DFVal.TC[ft, OT]]
                val dfType = ___onFalseDFVal.dfType
                inline if (isConstCheck[OT])
                  DFVal.Func(dfType, FuncOp.sel, List(lhs, tc(dfType, onTrue), ___onFalseDFVal))
                    .asValTP[ft, P | fp]
                else
                  DFVal.Func(dfType, FuncOp.sel, List(lhs, tc(dfType, onTrue), ___onFalseDFVal))
                    .asValOf[ft]
          else
            BoolSelWrapper[P, OT, OF](lhs, onTrue, onFalse)
        end sel
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
