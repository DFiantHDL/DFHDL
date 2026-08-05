package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import annotation.{implicitNotFound, targetName}
import scala.util.NotGiven

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
      type Types = DFValOf[DFBoolOrBit] | Boolean | BitNum | IfWrapper[?, ?, ?] | BitNumWrapper
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
      given fromBitNumWrapper[R <: BitNumWrapper]: Candidate[R] with
        type OutT = DFBit
        type OutP = CONST
        def apply(arg: R)(using DFC): Out =
          DFVal.Const(DFBit, Some(arg.value > 0), named = true)
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
        @dfhdl.hw.annotation.pure(true, "*")
        def toScalaBoolean(using DFC, DFVal.ConstCheck[P]): Boolean =
          lhs.toScalaValue
        @dfhdl.hw.annotation.pure(true, "*")
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

      // Runtime construction for the `sel` operation givens below. The
      // candidates are by-name so their evaluation (including the TC
      // conversion of the non-DFHDL candidate and the dfType access, which
      // throws a derived error for an errored value) happens under `trydf`,
      // surfacing as a positioned elaboration error instead of an escaping
      // exception.
      def selRuntime[OT <: DFTypeAny](
          cond: DFValOf[DFBoolOrBit],
          onTrue: => DFValOf[OT],
          onFalse: => DFValOf[OT]
      )(using dfc: DFC): DFValOf[OT] =
        trydf {
          val onTrueVal = onTrue
          val onFalseVal = onFalse
          DFVal.Func(onTrueVal.dfType, FuncOp.sel, List(cond, onTrueVal, onFalseVal))
        }(using dfc, CTName("sel"))

      // ~~~ `sel` candidate resolution ~~~
      // The onTrue candidate type leads, except when onTrue is a DFHDL Int
      // parameter (DFConstInt32) while onFalse is not, and when neither
      // candidate is a DFHDL value the selection is deferred through
      // BoolSelWrapper for an outer context to type. The cases are kept
      // mutually exclusive via the NotGiven guards, so no given
      // prioritization is involved.
      given evSelOnTrueDFVal[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          TT <: DFTypeAny,
          TP,
          OT <: DFValTP[TT, TP],
          OF,
          RP
      ](using
          NotGiven[OT <:< DFConstInt32]
      )(using
          tc: DFVal.TC[TT, OF] { type OutP = RP }
      ): ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, DFValTP[TT, CP | TP | RP]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = DFValTP[TT, CP | TP | RP]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            selRuntime[TT](lhs, mhs, tc(mhs.dfType, rhs)).asValTP[TT, CP | TP | RP]
      end evSelOnTrueDFVal
      given evSelBothConstInt32[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          OT <: DFConstInt32,
          OF <: DFConstInt32
      ]: ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, DFValTP[DFInt32, CP | CONST]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = DFValTP[DFInt32, CP | CONST]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            selRuntime[DFInt32](lhs, mhs, rhs).asValTP[DFInt32, CP | CONST]
      end evSelBothConstInt32
      given evSelOnFalseDFValFlip[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          OT <: DFConstInt32,
          FT <: DFTypeAny,
          FP,
          OF <: DFValTP[FT, FP],
          RP
      ](using
          NotGiven[OF <:< DFConstInt32]
      )(using
          tc: DFVal.TC[FT, OT] { type OutP = RP }
      ): ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, DFValTP[FT, CP | FP | RP]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = DFValTP[FT, CP | FP | RP]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            selRuntime[FT](lhs, tc(rhs.dfType, mhs), rhs).asValTP[FT, CP | FP | RP]
      end evSelOnFalseDFValFlip
      given evSelOnFalseDFVal[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          OT,
          FT <: DFTypeAny,
          FP,
          OF <: DFValTP[FT, FP],
          RP
      ](using
          NotGiven[OT <:< DFValAny]
      )(using
          tc: DFVal.TC[FT, OT] { type OutP = RP }
      ): ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, DFValTP[FT, CP | FP | RP]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = DFValTP[FT, CP | FP | RP]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            selRuntime[FT](lhs, tc(rhs.dfType, mhs), rhs).asValTP[FT, CP | FP | RP]
      end evSelOnFalseDFVal
      given evSelWrapperInt32[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          OT <: DFConstInt32,
          OF
      ](using
          NotGiven[OF <:< DFValAny]
      ): ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, BoolSelWrapper[CP, OT, OF]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = BoolSelWrapper[CP, OT, OF]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            BoolSelWrapper[CP, OT, OF](lhs, mhs, rhs)
      end evSelWrapperInt32
      given evSelWrapper[
          CP,
          L <: DFValTP[DFBoolOrBit, CP],
          OT,
          OF
      ](using
          NotGiven[OT <:< DFValAny],
          NotGiven[OF <:< DFValAny]
      ): ExactOp3Aux[FuncOp.sel.type, DFC, Any, L, OT, OF, BoolSelWrapper[CP, OT, OF]] =
        new ExactOp3[FuncOp.sel.type, DFC, Any, L, OT, OF]:
          type Out = BoolSelWrapper[CP, OT, OF]
          def apply(lhs: L, mhs: OT, rhs: OF)(using DFC): Out =
            BoolSelWrapper[CP, OT, OF](lhs, mhs, rhs)
      end evSelWrapper

      extension [T <: DFBoolOrBit, P](lhs: DFValTP[T, P])
        @targetName("notOfDFBoolOrBit")
        private[core] def not(using DFC): DFValTP[T, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_!, List(lhs))
        }
        // The exactOp3 macro boundary binds all three operands at the user's
        // call site, so candidate failures are reported at the user's code
        // (an in-body summon would report inside this file), and the operand
        // typing goes through exactInfo widening, which also covers the
        // `unstableSkolemPrefix` concern noted in `DFVal.Ops.<>`.
        transparent inline def sel[OT, OF](inline onTrue: OT, inline onFalse: OF)(using
            dfc: DFCG
        ): Any =
          exactOp3[FuncOp.sel.type, DFC, Any](lhs, onTrue, onFalse)
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

//BitNumWrapper is a wrapper for BitNum to preserve 0 or 1 values in basic operations
//The type is also used as `Bit` in the DFHDL frontend, to allow using BitNum values in DFHDL code
//and constructing DFBit DFHDL valeu types such as `Bit <> CONST`.
//TODO: implemented workaround for https://github.com/scala/scala3/issues/26550
into sealed class BitNumWrapper(val value: Int) extends AnyVal derives CanEqual:
  def unary_! : BitNumWrapper = BitNumWrapper(if value == 0 then 1 else 0)
  def unary_~ : BitNumWrapper = unary_!
  def |(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value == 1 || rhs.value == 1 then 1 else 0)
  def &(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value == 1 && rhs.value == 1 then 1 else 0)
  def ^(rhs: BitNumWrapper): BitNumWrapper =
    BitNumWrapper(if value != rhs.value then 1 else 0)
  def &&(rhs: BitNumWrapper): BitNumWrapper = this & rhs
  def ||(rhs: BitNumWrapper): BitNumWrapper = this | rhs
  def ==(rhs: BitNum): Boolean = value == rhs
  def !=(rhs: BitNum): Boolean = value != rhs

object BitNumWrapper:
  def apply(value: BitNum): BitNumWrapper = new BitNumWrapper(value)
  given [T <: Int & Singleton](using T <:< BitNum): Conversion[T, BitNumWrapper] =
    x => BitNumWrapper(x.asInstanceOf[BitNum])
  given CanEqual[BitNumWrapper, BitNum] = CanEqual.derived
  // TODO: implemented workaround for https://github.com/scala/scala3/issues/26550
  implicit def toBitNum(wrapper: BitNumWrapper): BitNum = wrapper.value.asInstanceOf[BitNum]
