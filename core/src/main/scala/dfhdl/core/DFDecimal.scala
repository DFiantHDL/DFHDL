package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.Op as FuncOp

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

  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken[DFDecimal[S, W, F]]
  object Token:
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        dfType: DFDecimal[S, W, F],
        data: Option[BigInt]
    ): Token[S, W, F] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFDecimal[S, W, F]]
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F],
        value: BigInt
    ): Token[S, W, F] =
      require(
        value.bitsWidth(signed) <= width,
        s"\nThe init value $value width must be smaller or equal to $width"
      )
      Token(DFDecimal(signed, width, fractionWidth), Some(value))
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F],
        value: Int
    ): Token[S, W, F] = Token(signed, width, fractionWidth, BigInt(value))

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

    object TC:
      export DFXInt.Token.TC.given

    object Compare:
      export DFXInt.Token.Compare.given

    object StrInterp:
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
      )(using Quotes, Type[P]): Expr[DFTokenAny] =
        scParts.scPartsWithArgs(args).interpolate(signedForcedExpr)

      extension (using Quotes)(fullTerm: quotes.reflect.Term)
        private def interpolate(
            signedForcedExpr: Expr[Boolean]
        ): Expr[DFTokenAny] =
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
            val signedInlined =
              Inlined.forced[signedType.Underlying](signed)
            val widthInlined =
              Inlined.forced[widthType.Underlying](width)
            val fractionWidthInlined =
              Inlined.forced[fractionWidthType.Underlying](fractionWidth)
            Token[
              signedType.Underlying,
              widthType.Underlying,
              fractionWidthType.Underlying
            ](signedInlined, widthInlined, fractionWidthInlined, value)
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
          val token =
            SIParts
              .tupleToExprs(scParts)
              .head
              .asTerm
              .interpolate(signedForcedExpr)
          val tokenType = token.asTerm.tpe.asTypeOf[DFTokenAny]
          '{
            val tc = compiletime
              .summonInline[
                DFVal.Compare[
                  T,
                  tokenType.Underlying,
                  FuncOp.===.type,
                  false
                ]
              ]
            Some(
              Seq(
                tc.conv(${ arg }.dfType, $token)(using compiletime.summonInline[tc.Ctx])
              )
            )
          }
        end if
      end unapplySeqMacro
    end StrInterp
    object Ops:
      export DFXInt.Token.Ops.*
  end Token

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

  type Token[S <: Boolean, W <: Int] = DFDecimal.Token[S, W, 0]
  object Token:
    protected[core] def apply[S <: Boolean, W <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        data: Option[BigInt]
    ): Token[S, W] = DFDecimal.Token(DFXInt(signed, width), data)

    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: Int
      type IsScalaInt <: Boolean
      def apply(arg: R): Token[OutS, OutW]
    object Candidate:
      // change to `given fromInt[R <: Int, I <: IntInfo[R]](using info: I): Candidate[R] with` after
      // https://github.com/lampepfl/dotty/issues/18509 is resolved
      transparent inline given fromInt[R <: Int](using
          info: IntInfo[R]
      ): Candidate[R] = new Candidate[R]:
        type OutS = info.OutS
        type OutW = info.OutW
        type IsScalaInt = true
        def apply(arg: R): Token[OutS, OutW] =
          Token(info.signed(arg), info.width(arg), Some(arg))
      transparent inline given fromInlinedInt[R <: Int](using
          info: IntInfo[R]
      ): Candidate[Inlined[R]] = new Candidate[Inlined[R]]:
        type OutS = info.OutS
        type OutW = info.OutW
        type IsScalaInt = true
        def apply(arg: Inlined[R]): Token[OutS, OutW] =
          Token(info.signed(arg), info.width(arg), Some(arg.value))
      given fromDFXIntToken[W <: Int, S <: Boolean, R <: Token[S, W]]: Candidate[R] with
        type OutS = S
        type OutW = W
        type IsScalaInt = false
        def apply(arg: R): Token[S, W] = arg
      given fromDFBitsTokenCandidate[R, IC <: DFBits.Token.Candidate[R]](using ic: IC): Candidate[R]
      with
        type OutS = false
        type OutW = ic.OutW
        type IsScalaInt = false
        def apply(arg: R): Token[false, ic.OutW] =
          import DFBits.Token.Ops.uint
          ic(arg).uint
    end Candidate

    object TC:
      import DFToken.TC
      given [LS <: Boolean, LW <: Int, R, IC <: Candidate[R]](using
          ic: IC
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW]
      ): TC[DFXInt[LS, LW], R] with
        def conv(dfType: DFXInt[LS, LW], value: R)(using Ctx): Out =
          import DFUInt.Token.Ops.signed
          val token = ic(value)
          check(dfType.signed, dfType.width, token.dfType.signed, token.width)
          // We either need to widen the token we got from a value int candidate
          // or it remains the same. In either case, there is not need to touch
          // the data itself, but just the dfType of the token.
          val resizedToken: ir.DFTokenAny =
            val tokenIR =
              if (dfType.signed != token.dfType.signed)
                token.asTokenOf[DFUInt[LW]].signed.asIR
              else token.asIR
            if (dfType.width > token.width)
              ir.DFToken(dfType.asIR)(token.data)
            else tokenIR
          resizedToken.asTokenOf[DFXInt[LS, LW]]
        end conv
      end given
    end TC

    object Compare:
      import DFToken.Compare
      given [LS <: Boolean, LW <: Int, R, Op <: FuncOp, C <: Boolean, IC <: Candidate[R]](using
          ic: IC
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW], R, Op, C] with
        override def apply(token: Token[LS, LW], arg: R)(using
            op: ValueOf[Op],
            castling: ValueOf[C]
        ): DFBool <> TOKEN =
          val tokenArg = conv(token.dfType, arg)
          val (lhsData, rhsData) =
            if (castling) (tokenArg.data, token.data)
            else (token.data, tokenArg.data)
          val outData = (lhsData, rhsData) match
            case (Some(l), Some(r)) =>
              import DFVal.Func.Op
              op.value match
                case Op.=== => Some(l == r)
                case Op.=!= => Some(l != r)
                case Op.<   => Some(l < r)
                case Op.>   => Some(l > r)
                case Op.<=  => Some(l <= r)
                case Op.>=  => Some(l >= r)
                case _      => throw new IllegalArgumentException("Unsupported Op")
            case _ => None
          DFBoolOrBit.Token(DFBool, outData)
        end apply
        def conv(dfType: DFXInt[LS, LW], arg: R)(using Ctx): DFXInt[LS, LW] <> TOKEN =
          val tokenArg = ic(arg)
          check(
            dfType.signed,
            dfType.width,
            tokenArg.dfType.signed,
            tokenArg.dfType.width
          )
          tokenArg.asTokenOf[DFXInt[LS, LW]]
      end given
    end Compare

    object Ops:
      export DFUInt.Token.Ops.*
      export DFSInt.Token.Ops.*
      private def arithOp[
          OS <: Boolean,
          OW <: Int,
          LS <: Boolean,
          LW <: Int,
          RS <: Boolean,
          RW <: Int
      ](
          dfType: DFXInt[OS, OW],
          op: FuncOp,
          lhs: Token[LS, LW],
          rhs: Token[RS, RW]
      ): Token[OS, OW] =
        val dataOut = (lhs.data, rhs.data) match
          case (Some(l), Some(r)) =>
            val dataNoTrunc = op match
              case FuncOp.+ => l + r
              case FuncOp.- => l - r
              case FuncOp.* => l * r
              case FuncOp./ => l / r
              case FuncOp.% => l % r
              case _        => ???
            val widthNoTrunc = dataNoTrunc.bitsWidth(dfType.signed)
            val dataTrunc =
              if (widthNoTrunc > dfType.width)
                dataNoTrunc.toBitVector(dfType.width).toBigInt(dfType.signed)
              else dataNoTrunc
            val dataFixSign =
              if (dataTrunc < 0 && !dfType.signed)
                dataTrunc.asUnsigned(dfType.width)
              else dataTrunc
            Some(dataFixSign)
          case _ => None
        Token(dfType.signed, dfType.width, dataOut)
      end arithOp
      extension [L <: DFTokenAny](lhs: L)(using icL: Candidate[L])
        def <[R](rhs: Exact[R])(using
            op: DFToken.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.<.type, false]
        ): DFBool <> TOKEN = op(icL(lhs), rhs)
        def >[R](rhs: Exact[R])(using
            op: DFToken.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.>.type, false]
        ): DFBool <> TOKEN = op(icL(lhs), rhs)
        def <=[R](rhs: Exact[R])(using
            op: DFToken.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.<=.type, false]
        ): DFBool <> TOKEN = op(icL(lhs), rhs)
        def >=[R](rhs: Exact[R])(using
            op: DFToken.Compare[DFXInt[icL.OutS, icL.OutW], R, FuncOp.>=.type, false]
        ): DFBool <> TOKEN = op(icL(lhs), rhs)
        def +[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType, rhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.+, lhsToken, rhsToken)
        def -[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType, rhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.-, lhsToken, rhsToken)
        def *[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType, rhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.`*`, lhsToken, rhsToken)
        def /[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType, rhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp./, lhsToken, rhsToken)
        def %[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: ArithCheck[icL.OutS, icL.OutW, icR.OutS, icR.OutW, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType, rhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.%, lhsToken, rhsToken)
        def +^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType.signed, rhsToken.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
            (lhsToken.dfType.width.value max rhsToken.dfType.width) + 1
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.+, lhsToken, rhsToken)
        def -^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType.signed, rhsToken.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
            (lhsToken.dfType.width.value max rhsToken.dfType.width) + 1
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.-, lhsToken, rhsToken)
        def *^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFXInt[icL.OutS, icL.OutW + icR.OutW] <> TOKEN =
          val lhsToken = icL(lhs)
          val rhsToken = icR(rhs)
          check(lhsToken.dfType.signed, rhsToken.dfType.signed)
          val width = Inlined.forced[icL.OutW + icR.OutW](
            lhsToken.dfType.width.value + rhsToken.dfType.width
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.`*`, lhsToken, rhsToken)
      end extension

      extension [S <: Boolean, W <: Int](lhs: Token[S, W])
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using check: Width.Check[S, RW]): Token[S, RW] =
          val updatedTokenIR =
            // no change in width
            if (updatedWidth == lhs.width) lhs.asIR
            else
              val signed = lhs.dfType.signed
              check(signed, updatedWidth)
              // updated width is larger or the data is bubble
              // TODO:Wrong run error workaround by changing to `updatedWidth.value` and `lhs.width.value`
              if (updatedWidth.value > lhs.width.value || lhs.asIR.isBubble)
                DFXInt.Token(signed, updatedWidth, lhs.data).asIR
              else // updated width is smaller
                import DFToken.Ops.bits
                import DFBits.Token.Ops.{resize => resizeDFBits, *}
                if (signed)
                  val tokenBits = lhs.bits
                  (tokenBits.msbit.bits ++
                    tokenBits(updatedWidth.value - 2, 0)).sint.asIR
                else // unsigned
                  lhs.bits
                    .resizeDFBits(updatedWidth)
                    .uint
                    .asIR
              end if
          updatedTokenIR.asTokenOf[DFXInt[S, RW]]
        end resize
      end extension
      extension [L](lhs: L)
        def <[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.<.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        def >[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.>.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        def <=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.<=.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        def >=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.>=.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
      end extension
      extension [L](lhs: L)
        def +[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType, lhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.+, lhsToken, rhs)
        def -[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType, lhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.-, lhsToken, rhs)
        def *[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType, lhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.`*`, lhsToken, rhs)
        def /[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType, lhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp./, lhsToken, rhs)
        def %[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: ArithCheck[RS, RW, icL.OutS, icL.OutW, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType, lhsToken.dfType)
          arithOp(lhsToken.dfType, FuncOp.%, lhsToken, rhs)
        def +^[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, Max[icL.OutW, RW] + 1] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType.signed, lhsToken.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsToken.dfType.width.value max rhs.dfType.width.value) + 1
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.+, lhsToken, rhs)
        def -^[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, Max[icL.OutW, RW] + 1] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType.signed, lhsToken.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsToken.dfType.width.value max rhs.dfType.width.value) + 1
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.-, lhsToken, rhs)
        def *^[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFXInt[icL.OutS, icL.OutW + RW] <> TOKEN =
          val lhsToken = icL(sL(lhs))
          check(rhs.dfType.signed, lhsToken.dfType.signed)
          val width = Inlined.forced[icL.OutW + RW](
            lhsToken.dfType.width.value + rhs.dfType.width.value
          )
          val dfType = DFXInt(lhsToken.dfType.signed, width)
          arithOp(dfType, FuncOp.`*`, lhsToken, rhs)
      end extension
    end Ops
  end Token

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
      given fromTokenCandidate[R, IC <: Token.Candidate[R]](using
          ic: IC
      ): Candidate[R] with
        type OutS = ic.OutS
        type OutW = ic.OutW
        type OutP = CONST
        type IsScalaInt = ic.IsScalaInt
        def apply(arg: R)(using dfc: DFC): Out =
          DFVal.Const(ic(arg), named = true)
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
        def conv(dfType: DFXInt[LS, LW], value: R)(using dfc: Ctx): Out =
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
          Op <: FuncOp,
          C <: Boolean
      ](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW], R, Op, C] with
        def conv(dfType: DFXInt[LS, LW], arg: R)(using dfc: Ctx): DFValOf[DFXInt[LS, LW]] =
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
          dfValArgResized.asValOf[DFXInt[LS, LW]]
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
          import Token.Ops.{resize => resizeToken}
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFXInt[S, RW]]
//          else
          DFVal.Alias.AsIs(
            DFXInt(signed, updatedWidth),
            lhs,
            _.resizeToken(updatedWidth)
          )
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

  type Token[W <: Int] = DFDecimal.Token[false, W, 0]
  object Token:
    object Ops:
      extension [W <: Int](lhs: Token[W])
        def signed: DFSInt.Token[W + 1] =
          import DFToken.Ops.bits
          import DFXInt.Token.Ops.resize
          import DFBits.Token.Ops.sint
          lhs.resize(lhs.width + 1).bits.sint
        @targetName("negateDFUIntToken")
        def unary_- : DFSInt.Token[W + 1] =
          import DFSInt.Token.Ops.unary_- as negate
          lhs.signed.negate

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
          val token =
            DFXInt.Token(false, ubInfo.width(fixme), Some(BigInt(arg)))
          DFVal.Const(token)
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
            case ir.DFVal.Const(ir.DFDecimal.Token(dfType, data), _, _, _, _) =>
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
          import Token.Ops.{signed => signedToken}
          DFVal.Alias.AsIs(DFSInt(lhs.width + 1), lhs, _.signedToken)
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

  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
  object Token:
    object Ops:
      extension [W <: Int](lhs: Token[W])
        @targetName("negateDFSIntToken")
        def unary_- : Token[W] =
          DFXInt.Token(true, lhs.width, lhs.data.map(-_))
  object Val:
    object Ops:
      extension [W <: Int, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("negateDFSInt")
        def unary_-(using DFC): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
end DFSInt
