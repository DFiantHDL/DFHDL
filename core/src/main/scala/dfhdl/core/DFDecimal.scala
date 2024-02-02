package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: Int, F <: Int] =
  DFType[ir.DFDecimal, Args3[S, W, F]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
      signed: Inlined[S],
      width: Inlined[W],
      fractionWidth: Inlined[F]
  )(using check: Width.Check[S, W]): DFDecimal[S, W, F] =
    check(signed, width)
    ir.DFDecimal(signed, width, fractionWidth).asFE[DFDecimal[S, W, F]]
  protected[core] def forced[S <: Boolean, W <: Int, F <: Int](
      signed: Boolean,
      width: Int,
      fractionWidth: Int
  ): DFDecimal[S, W, F] =
    val check = summon[Width.Check[Boolean, Int]]
    check(signed, width)
    ir.DFDecimal(signed, width, fractionWidth).asFE[DFDecimal[S, W, F]]

  given [S <: Boolean, W <: Int, F <: Int](using
      ValueOf[S],
      ValueOf[W],
      ValueOf[F]
  )(using Width.Check[S, W]): DFDecimal[S, W, F] =
    DFDecimal(valueOf[S], valueOf[W], valueOf[F])
  object Extensions:
    extension [S <: Boolean, W <: Int, F <: Int](dfType: DFDecimal[S, W, F])
      def signed: Inlined[S] = Inlined.forced[S](dfType.asIR.signed)

  protected[core] object Constraints:
    object Width
        extends Check2[
          Boolean,
          Int,
          [s <: Boolean, w <: Int] =>> ITE[s, w > 1, w > 0],
          [s <: Boolean, w <: Int] =>> ITE[
            s,
            "Signed value width must be larger than 1, but found: " + w,
            "Unsigned value width must be positive, but found: " + w
          ]
        ]
    object Sign
        extends Check2[
          Boolean,
          Int,
          [s <: Boolean, n <: Int] =>> ITE[s, true, n >= 0],
          [s <: Boolean, n <: Int] =>> "Unsigned value must be natural, but found: " + n
        ]

    object `LW >= RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW >= RW,
          [LW <: Int, RW <: Int] =>> "The applied RHS value width (" + RW +
            ") is larger than the LHS variable width (" + LW + ")."
        ]
    object `LW == RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW == RW,
          [LW <: Int, RW <: Int] =>> "Cannot apply this operation between a value of " + LW +
            " bits width (LHS) to a value of " + RW +
            " bits width (RHS).\nAn explicit conversion must be applied."
        ]
    object `ValW >= ArgW`
        extends Check2[
          Int,
          Int,
          [ValW <: Int, ArgW <: Int] =>> ValW >= ArgW,
          [ValW <: Int, ArgW <: Int] =>> "Cannot compare a DFHDL value (width = " + ValW +
            ") with a Scala `Int` argument that is wider (width = " + ArgW +
            ").\nAn explicit conversion must be applied."
        ]
    object `LS >= RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS || ![RS],
          [LS <: Boolean, RS <: Boolean] =>> "Cannot apply a signed value to an unsigned variable."
        ]
    type SignStr[S <: Boolean] = ITE[S, "a signed", "an unsigned"]
    object `LS == RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS == RS,
          [LS <: Boolean, RS <: Boolean] =>> "Cannot apply this operation between " +
            ITE[LS, "a signed", "an unsigned"] + " value (LHS) and " +
            ITE[RS, "a signed", "an unsigned"] +
            " value (RHS).\nAn explicit conversion must be applied."
        ]
    trait TCCheck[LS <: Boolean, LW <: Int, RS <: Boolean, RW <: Int]:
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit
    given [LS <: Boolean, LW <: Int, RS <: Boolean, RW <: Int](using
        checkS: `LS >= RS`.Check[LS, RS],
        checkW: `LW >= RW`.Check[LW, ITE[LS != RS, RW + 1, RW]]
    ): TCCheck[LS, LW, RS, RW] with
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit =
        checkS(leftSigned, rightSigned)
        checkW(
          leftWidth,
          if (leftSigned != rightSigned) rightWidth + 1 else rightWidth
        )
    end given
    trait CompareCheck[
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit
    given [
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        argWFix: Id[ITE[ArgIsInt && ValS && ![ArgS], ArgW + 1, ArgW]],
        skipChecks: Id[ArgIsInt && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValW]],
        rw: Id[ITE[Castle ^ skipChecks.Out, ValW, argWFix.Out]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        checkW: `LW == RW`.Check[lw.Out, rw.Out],
        checkVAW: `ValW >= ArgW`.Check[ValW, ITE[ArgIsInt, argWFix.Out, 0]],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): CompareCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit =
        val skipChecks = argIsInt.value && (dfValSigned || !argSigned)
        val argWFix =
          if (argIsInt.value && dfValSigned && !argSigned) argWidth + 1
          else argWidth
        if (argIsInt) checkVAW(dfValWidth, argWFix)
        if (!skipChecks)
          val ls = if (castle) argSigned else dfValSigned
          val rs = if (castle) dfValSigned else argSigned
          checkS(ls, rs)
          val lw = if (castle) argWFix else dfValWidth
          val rw = if (castle) dfValWidth else argWFix
          checkW(lw, rw)
      end apply
    end given
    trait ArithCheck[
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValDFType: DFXInt[ValS, ValW],
          argDFType: DFXInt[ArgS, ArgW]
      ): Unit
    given [
        ValS <: Boolean,
        ValW <: Int,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        argWFix: Id[
          ITE[ArgIsInt && ![Castle] && ValS && ![ArgS], ArgW + 1, ArgW]
        ],
        skipSignChecks: Id[ArgIsInt && ![Castle] && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipSignChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValW]],
        rw: Id[ITE[Castle, ValW, argWFix.Out]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        checkW: `LW >= RW`.Check[lw.Out, rw.Out],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): ArithCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(
          dfValDFType: DFXInt[ValS, ValW],
          argDFType: DFXInt[ArgS, ArgW]
      ): Unit =
        val skipSignChecks: Boolean =
          argIsInt.value && !castle && (dfValDFType.signed || !argDFType.signed)
        val argWFix: Int =
          if (argIsInt.value && !castle && dfValDFType.signed && !argDFType.signed)
            argDFType.width + 1
          else argDFType.width
        if (!skipSignChecks)
          val ls: Boolean = if (castle) argDFType.signed else dfValDFType.signed
          val rs: Boolean = if (castle) dfValDFType.signed else argDFType.signed
          checkS(ls, rs)
        val lw: Int = if (castle) argWFix else dfValDFType.width
        val rw: Int = if (castle) dfValDFType.width else argWFix
        checkW(lw, rw)
      end apply
    end given
    trait SignCheck[
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          argSigned: Boolean
      ): Unit
    given [
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        skipSignChecks: Id[ArgIsInt && ![Castle] && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipSignChecks.Out, ValS, ArgS]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): SignCheck[ValS, ArgS, ArgIsInt, Castle] with
      def apply(
          dfValSigned: Boolean,
          argSigned: Boolean
      ): Unit =
        val skipSignChecks: Boolean =
          argIsInt.value && !castle && (dfValSigned || !argSigned)
        if (!skipSignChecks)
          val ls: Boolean = if (castle) argSigned else dfValSigned
          val rs: Boolean = if (castle) dfValSigned else argSigned
          checkS(ls, rs)
      end apply
    end given
  end Constraints

  object StrInterp:
    private val widthIntExp = "(\\d+)'(-?\\d+)".r
    private val widthFixedExp = "(\\d+)\\.(\\d+)'(-?\\d+)\\.?(\\d*)".r
    private val intExp = "(-?\\d+)".r
    private def fromDecString(
        dec: String,
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      def fromValidString(numStr: String): (Boolean, Int, Int, BigInt) =
        val value = BigInt(numStr)
        val signed = value < 0 | signedForced
        val actualWidth = value.bitsWidth(signed)
        (signed, actualWidth, 0, value)
      dec.replace(",", "").replace("_", "") match
        case widthFixedExp(
              magnitudeWidthStr,
              fractionWidthStr,
              magnitudeStr,
              fractionStr
            ) =>
          val explicitMagnitudeWidth = magnitudeWidthStr.toInt
          val explicitFractionWidth = fractionWidthStr.toInt
          val magnitude = BigInt(magnitudeStr)
          val fraction =
            if (fractionStr.isEmpty) BigInt(0) else BigInt(fractionStr)
          Left("Fixed-point decimal literals are not yet supported")
        case widthIntExp(widthStr, numStr) =>
          val explicitWidth = widthStr.toInt
          val (signed, width, fractionWidth, value) = fromValidString(numStr)
          if (explicitWidth < width)
            Left(
              s"Explicit given width ($explicitWidth) is smaller than the actual width ($width)"
            )
          else
            Right((signed, explicitWidth, fractionWidth, value))
        case intExp(numStr) => Right(fromValidString(numStr))
        case _ =>
          Left(s"Invalid decimal pattern found: $dec")
      end match
    end fromDecString

    class DParts[P <: Tuple](parts: P):
      transparent inline def apply(inline args: Any*): Any =
        ${ applyMacro('{ false })('parts, 'args) }
      transparent inline def unapplySeq[T <: DFTypeAny](
          inline arg: DFValOf[T]
      )(using DFC): Option[Seq[DFValOf[T]]] =
        ${ unapplySeqMacro('{ false })('parts, 'arg) }

    class SDParts[P <: Tuple](parts: P):
      transparent inline def apply(inline args: Any*): Any =
        ${ applyMacro('{ true })('parts, 'args) }
      transparent inline def unapplySeq[T <: DFTypeAny](
          inline arg: DFValOf[T]
      )(using DFC): Option[Seq[DFValOf[T]]] =
        ${ unapplySeqMacro('{ true })('parts, 'arg) }

    extension (inline sc: StringContext)
      transparent inline def d: Any = ${ SIParts.scMacro[DParts]('sc) }
      transparent inline def sd: Any = ${ SIParts.scMacro[SDParts]('sc) }

    private def applyMacro[P <: Tuple](signedForcedExpr: Expr[Boolean])(
        scParts: Expr[P],
        args: Expr[Seq[Any]]
    )(using Quotes, Type[P]): Expr[DFConstAny] =
      scParts.scPartsWithArgs(args).interpolate(signedForcedExpr)

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private def interpolate(
          signedForcedExpr: Expr[Boolean]
      ): Expr[DFConstAny] =
        import quotes.reflect.*
        val signedForced = signedForcedExpr.value.get
        val (signedTpe, widthTpe, fractionWidthTpe): (TypeRepr, TypeRepr, TypeRepr) =
          fullTerm match
            case Literal(StringConstant(t)) =>
              fromDecString(t, signedForced) match
                case Right((signed, width, fractionWidth, _)) =>
                  (
                    ConstantType(BooleanConstant(signed)),
                    ConstantType(IntConstant(width)),
                    ConstantType(IntConstant(fractionWidth))
                  )
                case Left(msg) =>
                  report.errorAndAbort(msg)
            case _ => (TypeRepr.of[Boolean], TypeRepr.of[Int], TypeRepr.of[Int])
        val signedType = signedTpe.asTypeOf[Boolean]
        val widthType = widthTpe.asTypeOf[Int]
        val fractionWidthType = fractionWidthTpe.asTypeOf[Int]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          import dfhdl.internals.Inlined
          val (signed, width, fractionWidth, value) =
            fromDecString($fullExpr, $signedForcedExpr).toOption.get
          val dfc = compiletime.summonInline[DFC]
          val dfType =
            DFDecimal.forced[
              signedType.Underlying,
              widthType.Underlying,
              fractionWidthType.Underlying
            ](signed, width, fractionWidth)
          DFVal.Const(dfType, Some(value), named = true)(using dfc)
        }
      end interpolate
    end extension

    private def unapplySeqMacro[P <: Tuple, T <: DFTypeAny](
        signedForcedExpr: Expr[Boolean]
    )(
        scParts: Expr[P],
        arg: Expr[DFValOf[T]]
    )(using Quotes, Type[P], Type[T]): Expr[Option[Seq[DFValOf[T]]]] =
      import quotes.reflect.*
      val parts = TypeRepr.of[P].getTupleArgs
      if (TypeRepr.of[P].getTupleArgs.length > 1)
        '{
          compiletime.error(
            "Extractors for decimal token string interpolation are not allowed."
          )
          Some(Seq())
        }
      else
        val dfVal =
          SIParts
            .tupleToExprs(scParts)
            .head
            .asTerm
            .interpolate(signedForcedExpr)
        val dfValType = dfVal.asTerm.tpe.asTypeOf[DFConstAny]
        '{
          val tc = compiletime.summonInline[
            DFVal.Compare[T, dfValType.Underlying, FuncOp.===.type, false]
          ]
          Some(
            Seq(
              tc.conv(${ arg }.dfType, $dfVal)(using compiletime.summonInline[DFC])
            )
          )
        }
      end if
    end unapplySeqMacro
  end StrInterp

  object Val:
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int],
          dfVal: DFValOf[DFDecimal[Boolean, Int, Int]]
      ): DFValOf[DFDecimal[Boolean, Int, Int]] =
        `LW >= RW`(dfType.width, dfVal.width)
        `LS >= RS`(dfType.signed, dfVal.dfType.signed)
        dfVal
    end TC
    object Compare:
      export DFXInt.Val.Compare.given
    object Ops:
      export DFXInt.Val.Ops.*
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: Int] = DFDecimal[S, W, 0]
object DFXInt:
  def apply[S <: Boolean, W <: Int](signed: Inlined[S], width: Inlined[W])(using
      Width.Check[S, W]
  ): DFXInt[S, W] = DFDecimal(signed, width, 0)

  object Val:
    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: Int
      type OutP
      type Out = DFValTP[DFXInt[OutS, OutW], OutP]
      type IsScalaInt <: Boolean
      def apply(arg: R)(using DFC): Out
    trait CandidateLP:
      given fromDFBitsValCandidate[R, IC <: DFBits.Val.Candidate[R]](using
          ic: IC
      ): Candidate[R] with
        type OutS = false
        type OutW = ic.OutW
        type OutP = ic.OutP
        type IsScalaInt = false
        def apply(arg: R)(using dfc: DFC): Out =
          import DFBits.Val.Ops.uint
          val dfVal = ic(arg)(using dfc.anonymize)
          if (dfVal.hasTag[DFVal.TruncateTag])
            dfVal.uint.tag(DFVal.TruncateTag).asValTP[DFXInt[OutS, OutW], OutP]
          else if (dfVal.hasTag[DFVal.ExtendTag])
            dfVal.uint.tag(DFVal.ExtendTag).asValTP[DFXInt[OutS, OutW], OutP]
          else dfVal.uint.asValTP[DFXInt[OutS, OutW], OutP]
      end fromDFBitsValCandidate
    end CandidateLP
    object Candidate extends CandidateLP:
      given fromInlinedInt[R <: Int, I <: IntInfo[R]](using
          info: I
      ): Candidate[Inlined[R]] with
        type OutS = info.OutS
        type OutW = info.OutW
        type OutP = CONST
        type IsScalaInt = true
        def apply(arg: Inlined[R])(using DFC): Out =
          val dfType = DFXInt(info.signed(arg), info.width(arg))
          DFVal.Const(dfType, Some(arg.value), named = true)
      type IntInfoAux[R <: Int, OS <: Boolean, OW <: Int] =
        IntInfo[R]:
          type OutS = OS
          type OutW = OW
      given fromInt[R <: Int, OS <: Boolean, OW <: Int](using
          info: IntInfoAux[R, OS, OW]
      ): Candidate[R] with
        type OutS = OS
        type OutW = OW
        type OutP = CONST
        type IsScalaInt = true
        def apply(arg: R)(using dfc: DFC): Out =
          val dfType = DFXInt(info.signed(arg), info.width(arg))
          DFVal.Const(dfType, Some(BigInt(arg)), named = true)
      given fromDFBitsVal[W <: Int, P, R <: DFValTP[DFBits[W], P]]: Candidate[R] with
        type OutS = false
        type OutW = W
        type OutP = P
        type IsScalaInt = false
        def apply(arg: R)(using DFC): DFValTP[DFUInt[W], P] =
          import DFBits.Val.Ops.uint
          arg.uint
      given fromDFXIntVal[S <: Boolean, W <: Int, P, R <: DFValTP[DFXInt[S, W], P]]: Candidate[R]
      with
        type OutS = S
        type OutW = W
        type OutP = P
        type IsScalaInt = false
        def apply(arg: R)(using DFC): DFValTP[DFXInt[S, W], P] = arg
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a DFHDL decimal variable."
        )
    end Candidate

    object TC:
      import DFVal.TC
      given [LS <: Boolean, LW <: Int, R, IC <: Candidate[R]](using
          ic: IC
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW],
          lsigned: ValueOf[LS]
      ): TC[DFXInt[LS, LW], R] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW], value: R)(using DFC): Out =
          import Ops.resize
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          (dfType.asIR: ir.DFType) match
            case ir.DFNothing =>
              val signCheck = summon[`LS >= RS`.Check[Boolean, Boolean]]
              signCheck(lsigned.value, rhs.dfType.signed)
              if (lsigned.value != rhs.dfType.signed.value)
                rhs.asValOf[DFUInt[Int]].signed.asValTP[DFXInt[LS, LW], ic.OutP]
              else rhs.asValTP[DFXInt[LS, LW], ic.OutP]
            case _ =>
              if (!rhs.hasTag[DFVal.TruncateTag] || dfType.signed != rhs.dfType.signed)
                check(dfType.signed, dfType.width, rhs.dfType.signed, rhs.width)
              val dfValIR =
                val rhsSignFix: DFValOf[DFSInt[Int]] =
                  if (dfType.signed != rhs.dfType.signed)
                    rhs.asValOf[DFUInt[Int]].signed.asValOf[DFSInt[Int]]
                  else rhs.asValOf[DFSInt[Int]]
                if (
                  dfType.width > rhsSignFix.width ||
                  rhs.hasTag[DFVal.TruncateTag] && dfType.width < rhsSignFix.width
                )
                  rhsSignFix.resize(dfType.width).asIR
                else rhsSignFix.asIR
              dfValIR.asValTP[DFXInt[LS, LW], ic.OutP]
          end match
        end conv
      end given
    end TC

    object Compare:
      import DFVal.Compare
      given DFXIntCompare[
          LS <: Boolean,
          LW <: Int,
          R,
          IC <: Candidate[R],
          Op <: FuncOp,
          C <: Boolean
      ](using
          ic: IC
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW], R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW], arg: R)(using dfc: DFC): Out =
          import Ops.resize
          import DFUInt.Val.Ops.signed
          given dfcAnon: DFC = dfc.anonymize
          val dfValArg = ic(arg)
          check(
            dfType.signed,
            dfType.width,
            dfValArg.dfType.signed,
            dfValArg.dfType.width
          )
          val dfValArgSigned =
            if (dfType.signed && !dfValArg.dfType.signed) dfValArg.asValOf[DFUInt[Int]].signed
            else dfValArg
          val dfValArgResized =
            if (dfValArgSigned.width < dfType.width)
              dfValArgSigned.asValOf[DFXInt[Boolean, Int]].resize(dfType.width)
            else dfValArgSigned
          dfValArgResized.asValTP[DFXInt[LS, LW], ic.OutP]
        end conv
      end DFXIntCompare
    end Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
        def <[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.<.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def <=[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.<=.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def >[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.>.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def >=[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.>=.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
      end extension
      extension [S <: Boolean, W <: Int, P](lhs: DFValTP[DFXInt[S, W], P])
        @targetName("truncateDFXInt")
        def truncate(using DFC): DFValTP[DFXInt[S, Int], P] =
          lhs.tag(DFVal.TruncateTag).asValTP[DFXInt[S, Int], P]
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using
            dfc: DFC,
            check: Width.Check[S, RW]
        ): DFValTP[DFXInt[S, RW], P] = trydf {
          val signed = lhs.dfType.signed
          check(signed, updatedWidth)
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFXInt[S, RW]]
//          else
          DFVal.Alias.AsIs(DFXInt(signed, updatedWidth), lhs)
        }
        end resize
        @targetName("shiftRightDFXInt")
        def >>[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValTP[DFXInt[S, W], P | c.OutP] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.>>, List(lhs, shiftVal))
        }
        @targetName("shiftLeftDFXInt")
        def <<[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValTP[DFXInt[S, W], P | c.OutP] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.<<, List(lhs, shiftVal))
        }
      end extension

      extension [L](lhs: L)
        def <[RS <: Boolean, RW <: Int](
            rhs: DFValOf[DFXInt[RS, RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.<.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def <=[RS <: Boolean, RW <: Int](
            rhs: DFValOf[DFXInt[RS, RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.<=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >[RS <: Boolean, RW <: Int](
            rhs: DFValOf[DFXInt[RS, RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.>.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >=[RS <: Boolean, RW <: Int](
            rhs: DFValOf[DFXInt[RS, RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.>=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
      end extension
      private def arithOp[
          OS <: Boolean,
          OW <: Int,
          LS <: Boolean,
          LW <: Int,
          LP,
          RS <: Boolean,
          RW <: Int,
          RP
      ](
          dfType: DFXInt[OS, OW],
          op: FuncOp,
          lhs: DFValTP[DFXInt[LS, LW], LP],
          rhs: DFValTP[DFXInt[RS, RW], RP]
      )(using dfc: DFC): DFValTP[DFXInt[OS, OW], LP | RP] =
        val dfcAnon = dfc.anonymize
        // TODO: maybe do fixing in a separate stage?
        val rhsFixSign =
          if (lhs.dfType.signed && !rhs.dfType.signed)
            rhs.asValTP[DFUInt[Int], RP].signed(using dfcAnon)
          else rhs
        val rhsFixSize =
          rhsFixSign.asValTP[DFSInt[Int], RP].resize(lhs.width)(using dfcAnon)
        DFVal.Func(dfType, op, List(lhs, rhsFixSize))
      end arithOp
      extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
        def +[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType, rhsVal.dfType)
          arithOp(lhsVal.dfType, FuncOp.+, lhsVal, rhsVal)
        }
        def -[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType, rhsVal.dfType)
          arithOp(lhsVal.dfType, FuncOp.-, lhsVal, rhsVal)
        }
        def *[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType, rhsVal.dfType)
          arithOp(lhsVal.dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
        def /[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType, rhsVal.dfType)
          arithOp(lhsVal.dfType, FuncOp./, lhsVal, rhsVal)
        }
        def %[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType, rhsVal.dfType)
          arithOp(lhsVal.dfType, FuncOp.%, lhsVal, rhsVal)
        }
        def +^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
            (lhsVal.dfType.width.value max rhsVal.dfType.width) + 1
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          arithOp(dfType, FuncOp.+, lhsVal, rhsVal)
        }
        def -^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
            (lhsVal.dfType.width.value max rhsVal.dfType.width) + 1
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          arithOp(dfType, FuncOp.-, lhsVal, rhsVal)
        }
        def *^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW + icR.OutW], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          val width = Inlined.forced[icL.OutW + icR.OutW](
            lhsVal.dfType.width.value + rhsVal.dfType.width
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          arithOp(dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
      end extension
      extension [L](lhs: L)
        def +[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType, lhsVal.dfType)
          DFVal.Func(lhsVal.dfType, FuncOp.+, List(lhsVal, rhs))
        }
        def -[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType, lhsVal.dfType)
          DFVal.Func(lhsVal.dfType, FuncOp.-, List(lhsVal, rhs))
        }
        def *[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType, lhsVal.dfType)
          DFVal.Func(lhsVal.dfType, FuncOp.`*`, List(lhsVal, rhs))
        }
        def /[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType, lhsVal.dfType)
          DFVal.Func(lhsVal.dfType, FuncOp./, List(lhsVal, rhs))
        }
        def %[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType, lhsVal.dfType)
          DFVal.Func(lhsVal.dfType, FuncOp.%, List(lhsVal, rhs))
        }
        def +^[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, RW] + 1], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsVal.dfType.width.value max rhs.dfType.width) + 1
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          DFVal.Func(dfType, FuncOp.+, List(lhsVal, rhs))
        }
        def -^[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, RW] + 1], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsVal.dfType.width.value max rhs.dfType.width) + 1
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          DFVal.Func(dfType, FuncOp.-, List(lhsVal, rhs))
        }
        end -^
        def *^[RS <: Boolean, RW <: Int, RP](
            rhs: DFValTP[DFXInt[RS, RW], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW + RW], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[icL.OutW + RW](
            lhsVal.dfType.width.value + rhs.dfType.width
          )
          val dfType = DFXInt(lhsVal.dfType.signed, width)
          DFVal.Func(dfType, FuncOp.`*`, List(lhsVal, rhs))
        }
        end *^
      end extension
    end Ops
  end Val
end DFXInt

type DFUInt[W <: Int] = DFXInt[false, W]
object DFUInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[false, W]
  ): DFUInt[W] = DFXInt(false, width)
  def apply[W <: Int](using dfType: DFUInt[W]): DFUInt[W] = dfType
  def until[V <: Int](sup: Inlined[V])(using
      check: Arg.LargerThan1.Check[V],
      info: IntInfo[V - 1]
  ): DFUInt[info.OutW] =
    check(sup)
    DFXInt(false, info.width(sup - 1))
  def max[V <: Int](max: Inlined[V])(using
      check: Arg.Positive.Check[V],
      info: IntInfo[V]
  ): DFUInt[info.OutW] =
    check(max)
    DFXInt(false, info.width(max))

  protected object Unsigned
      extends Check1[
        Boolean,
        [S <: Boolean] =>> ![S],
        [S <: Boolean] =>> "Argument must be unsigned"
      ]
  protected object `UB > R`
      extends Check2[
        Int,
        Int,
        [UB <: Int, R <: Int] =>> UB > R,
        [UB <: Int, R <: Int] =>> "The argument must be smaller than the upper-bound " + UB +
          " but found: " + R
      ]
  protected object `UBW == RW`
      extends Check2[
        Int,
        Int,
        [UBW <: Int, RW <: Int] =>> UBW == RW,
        [UBW <: Int, RW <: Int] =>> "Expected argument width " + UBW + " but found: " + RW
      ]

  object Val:
    trait UBArg[UB <: Int, R]:
      type OutW <: Int
      type OutP
      type Out = DFValTP[DFUInt[OutW], OutP]
      def apply(ub: Inlined[UB], arg: R)(using DFC): Out
    trait UBArgLP:
      transparent inline given errorDMZ[UB <: Int, R](using
          r: ShowType[R]
      ): UBArg[UB, R] =
        Error.call[
          (
              "Upper-bound argument cannot be constructed from the type `",
              r.Out,
              "`."
          )
        ]
    object UBArg extends UBArgLP:
      given fromInt[UB <: Int, R <: Int, I <: IntInfo[UB - 1]](using
          ubInfo: I
      )(using
          unsignedCheck: Unsigned.Check[R < 0],
          ubCheck: `UB > R`.Check[UB, R]
      ): UBArg[UB, R] with
        type OutW = ubInfo.OutW
        type OutP = CONST
        def apply(ub: Inlined[UB], arg: R)(using DFC): Out =
          unsignedCheck(arg < 0)
          // TODO: https://github.com/lampepfl/dotty/issues/15798
          val fixme = (ub - 1).asInstanceOf[Inlined[Int]].value
          ubCheck(ub, arg)
          DFVal.Const(DFUInt(ubInfo.width(fixme)), Some(BigInt(arg)))
      end fromInt
      given fromR[UB <: Int, R, IC <: DFXInt.Val.Candidate[R], I <: IntInfo[UB - 1]](using
          ic: IC,
          ubInfo: I
      )(using
          unsignedCheck: Unsigned.Check[ic.OutS],
          widthCheck: `UBW == RW`.Check[ubInfo.OutW, ic.OutW]
      ): UBArg[UB, R] with
        type OutW = ubInfo.OutW
        type OutP = ic.OutP
        def apply(ub: Inlined[UB], arg: R)(using DFC): Out =
          val argVal = ic(arg)
          unsignedCheck(argVal.dfType.signed)
          // TODO: https://github.com/lampepfl/dotty/issues/15798
          val fixme = (ub - 1).asInstanceOf[Inlined[Int]].value
          widthCheck(ubInfo.width(fixme), argVal.width)
          // for constant value we apply an explicit check for the bound
          argVal.asIR match
            case ir.DFVal.Const(dfType: ir.DFDecimal, data: Option[BigInt] @unchecked, _, _, _) =>
              data match
                case Some(value) =>
                  summon[`UB > R`.Check[UB, Int]](ub, value.toInt)
                case _ => // no check
            case _ => // no check
          argVal.asValTP[DFUInt[OutW], ic.OutP]
        end apply
      end fromR
    end UBArg
    object Ops:
      extension [W <: Int, P](lhs: DFValTP[DFUInt[W], P])
        def signed(using DFC): DFValTP[DFSInt[W + 1], P] = trydf {
          DFVal.Alias.AsIs(DFSInt(lhs.width + 1), lhs)
        }
        @targetName("negateDFUInt")
        def unary_-(using DFC): DFValTP[DFSInt[W + 1], P] = trydf {
          import DFSInt.Val.Ops.unary_- as negate
          lhs.signed.negate
        }
  end Val

end DFUInt

type DFSInt[W <: Int] = DFXInt[true, W]
object DFSInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[true, W]
  ): DFSInt[W] = DFXInt(true, width)
  def apply[W <: Int](using dfType: DFSInt[W]): DFSInt[W] = dfType

  object Val:
    object Ops:
      extension [W <: Int, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("negateDFSInt")
        def unary_-(using DFC): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
end DFSInt
