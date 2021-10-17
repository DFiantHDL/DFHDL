package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import ir.DFVal.Func.{Op => FuncOp}

import scala.quoted.*
import scala.annotation.targetName
import CompanionsDFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: Int, F <: Int] =
  OpaqueDFDecimal.DFDecimal[S, W, F]
val DFDecimal = OpaqueDFDecimal.DFDecimal

private object OpaqueDFDecimal:
  opaque type DFDecimal[S <: Boolean, W <: Int, F <: Int] <: DFType.Of[
    ir.DFDecimal
  ] = DFType.Of[ir.DFDecimal]
  object DFDecimal:
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F]
    )(using check: Width.Check[S, W]): DFDecimal[S, W, F] =
      check(signed, width)
      ir.DFDecimal(signed, width, fractionWidth).asFE[DFDecimal[S, W, F]]
    export CompanionsDFDecimal.Extensions.*
    export CompanionsDFDecimal.DFTypeGiven.given
    type Token[S <: Boolean, W <: Int, F <: Int] =
      CompanionsDFDecimal.Token[S, W, F]
    val Token = CompanionsDFDecimal.Token
    val Val = CompanionsDFDecimal.Val
  end DFDecimal
end OpaqueDFDecimal

private object CompanionsDFDecimal:
  object DFTypeGiven:
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
          [s <: Boolean,
          n <: Int] =>> "Unsigned value must be natural, but found: " + n
        ]

    object `LW >= RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW >= RW,
          [LW <: Int, RW <: Int] =>> "The applied value width (" + RW +
            ") is larger than the variable width (" + LW + ")."
        ]
    object `LW == RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW == RW,
          [LW <: Int,
          RW <: Int] =>> "Cannot compare a value of " + LW + " bits width (LHS) to a value of " +
            RW + " bits width (RHS).\nAn explicit conversion must be applied."
        ]
    object `ValW >= ArgW`
        extends Check2[
          Int,
          Int,
          [ValW <: Int, ArgW <: Int] =>> ValW >= ArgW,
          [ValW <: Int,
          ArgW <: Int] =>> "Cannot compare a dataflow value (width = " + ValW +
            ") with a Scala `Int` argument that is wider (width = " + ArgW + ").\nAn explicit conversion must be applied."
        ]
    object `LS >= RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS || ![RS],
          [LS <: Boolean,
          RS <: Boolean] =>> "Cannot apply a signed value to an unsigned variable."
        ]
    type SignStr[S <: Boolean] = ITE[S, "a signed", "an unsigned"]
    object `LS == RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS == RS,
          [LS <: Boolean, RS <: Boolean] =>> "Cannot compare " +
            ITE[LS, "a signed", "an unsigned"] +
            " value (LHS) to " +
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
        ArgIsInt <: Boolean, //argument is from a Scala Int
        Castle <: Boolean //castling of dfVal and arg
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
  end Constraints

  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken[DFDecimal[S, W, F]]
  object Token:
    extension [S <: Boolean, W <: Int, F <: Int](token: Token[S, W, F])
      def data: Option[BigInt] =
        token.asIR.data.asInstanceOf[Option[BigInt]]

    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        dfType: DFDecimal[S, W, F],
        data: Option[BigInt]
    ): Token[S, W, F] =
      ir.DFToken(dfType.asIR, data).asTokenOf[DFDecimal[S, W, F]]
    protected[core] def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F],
        value: BigInt
    ): Token[S, W, F] =
      assert(
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
      dec match
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
      extension (inline sc: StringContext)
        transparent inline def d(inline args: Any*): DFTokenAny =
          ${
            interpMacro('{ false })('sc, 'args)
          }
        transparent inline def sd(inline args: Any*): DFTokenAny =
          ${
            interpMacro('{ true })('sc, 'args)
          }

      private def interpMacro(signedForcedExpr: Expr[Boolean])(
          sc: Expr[StringContext],
          args: Expr[Seq[Any]]
      )(using Quotes): Expr[DFTokenAny] =
        import quotes.reflect.*
        val signedForced = signedForcedExpr.value.get
        val fullTerm = sc.termWithArgs(args)
        val (signedTpe, widthTpe, fractionWidthTpe)
            : (TypeRepr, TypeRepr, TypeRepr) = fullTerm match
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
          import DFiant.internals.Inlined
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
      end interpMacro
    end StrInterp
    object Ops:
      export DFXInt.Token.Ops.*
  end Token

  object Val:
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int],
          dfVal: DFDecimal[Boolean, Int, Int] <> VAL
      ): DFDecimal[Boolean, Int, Int] <> VAL =
        `LW >= RW`(dfType.width, dfVal.width)
        `LS >= RS`(dfType.signed, dfVal.dfType.signed)
        dfVal
    end TC
    object Compare:
      export DFXInt.Val.Compare.given
    object Ops:
      export DFXInt.Val.Ops.*
    object Conversions:
      export DFXInt.Val.Conversions.*
  end Val

end CompanionsDFDecimal

type DFXInt[S <: Boolean, W <: Int] = DFDecimal[S, W, 0]
object DFXInt:
  def apply[S <: Boolean, W <: Int](signed: Inlined[S], width: Inlined[W])(using
      Width.Check[S, W]
  ): DFXInt[S, W] = DFDecimal(signed, width, 0)

  type Token[S <: Boolean, W <: Int] = DFDecimal.Token[S, W, 0]
  object Token:
    import DFDecimal.Token.data
    protected[core] def apply[S <: Boolean, W <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        data: Option[BigInt]
    ): Token[S, W] = DFDecimal.Token(DFXInt(signed, width), data)

    trait Candidate[-R]:
      type OutS <: Boolean
      type OutW <: Int
      type IsScalaInt <: Boolean
      def apply(arg: R): Token[OutS, OutW]
    object Candidate:
      //change to given...with after
      //https://github.com/lampepfl/dotty/issues/13580 is resolved
      transparent inline given fromIntLiteral[R <: Int](using
          info: IntInfo[R]
      ): Candidate[ValueOf[R]] = new Candidate[ValueOf[R]]:
        type OutS = info.OutS
        type OutW = info.OutW
        type IsScalaInt = true
        def apply(arg: ValueOf[R]): Token[OutS, OutW] =
          Token(
            info.signed(arg.value),
            info.width(arg.value),
            Some(arg.value)
          )
      transparent inline given fromInt(using
          info: IntInfo[Int]
      ): Candidate[Int] = new Candidate[Int]:
        type OutS = info.OutS
        type OutW = info.OutW
        type IsScalaInt = true
        def apply(arg: Int): Token[OutS, OutW] =
          Token(info.signed(arg), info.width(arg), Some(arg))
      transparent inline given fromDFXIntToken[W <: Int, S <: Boolean]
          : Candidate[Token[S, W]] =
        new Candidate[Token[S, W]]:
          type OutS = S
          type OutW = W
          type IsScalaInt = false
          def apply(arg: Token[S, W]): Token[S, W] = arg
      transparent inline given fromDFBitsToken[W <: Int]
          : Candidate[DFBits.Token[W]] =
        new Candidate[DFBits.Token[W]]:
          type OutS = false
          type OutW = W
          type IsScalaInt = false
          def apply(arg: DFBits.Token[W]): Token[false, W] =
            import DFBits.Token.Ops.uint
            arg.uint
    end Candidate

    object TC:
      import DFToken.TC
      given [LS <: Boolean, LW <: Int, R](using
          ic: Candidate[R]
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW]
      ): TC[DFXInt[LS, LW], R] with
        def apply(dfType: DFXInt[LS, LW], value: R): Out =
          import DFUInt.Token.Ops.signed
          val token = ic(value)
          check(dfType.signed, dfType.width, token.dfType.signed, token.width)
          //We either need to widen the token we got from a value int candidate
          //or it remains the same. In either case, there is not need to touch
          //the data itself, but just the dfType of the token.
          val resizedToken =
            val tokenIR =
              if (dfType.signed != token.dfType.signed)
                token.asIR.asTokenOf[DFUInt[LW]].signed.asIR
              else token.asIR
            if (dfType.width > token.width)
              tokenIR.copy(dfType = dfType.asIR)
            else tokenIR
          resizedToken.asTokenOf[DFXInt[LS, LW]]
        end apply
      end given
    end TC

    object Compare:
      import DFToken.Compare
      given [LS <: Boolean, LW <: Int, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW], R, Op, C] with
        def apply(token: Token[LS, LW], arg: R): DFBool <> TOKEN =
          val tokenArg = ic(arg)
          check(
            token.dfType.signed,
            token.dfType.width,
            tokenArg.dfType.signed,
            tokenArg.dfType.width
          )
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
                case _ => throw new IllegalArgumentException("Unsupported Op")
            case _ => None
          DFBoolOrBit.Token(DFBool, outData)
        end apply
      end given
    end Compare

    object Ops:
      export DFUInt.Token.Ops.*
      export DFSInt.Token.Ops.*
      extension [S <: Boolean, W <: Int](
          lhs: Token[S, W]
      )
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using check: Width.Check[S, RW]): Token[S, RW] =
          val updatedTokenIR =
            //no change in width
            if (updatedWidth == lhs.width) lhs.asIR
            else
              val signed = lhs.dfType.signed
              check(signed, updatedWidth)
              //updated width is larger or the data is bubble
              if (updatedWidth.value > lhs.width.value || lhs.asIR.isBubble)
                DFXInt.Token(signed, updatedWidth, lhs.data).asIR
              else //updated width is smaller
                import DFToken.Ops.bits
                import DFBits.Token.Ops.{resize => resizeDFBits, *}
                if (signed)
                  val tokenBits = lhs.bits
                  (tokenBits.msbit.bits ++
                    tokenBits(updatedWidth - 2, 0)).sint.asIR
                else //unsigned
                  lhs.bits
                    .resizeDFBits(updatedWidth)
                    .uint
                    .asIR
              end if
          updatedTokenIR.asTokenOf[DFXInt[S, RW]]
        end resize
        def <[R](rhs: Inlined[R])(using
            op: DFToken.Compare[DFXInt[S, W], R, FuncOp.<.type, false]
        ): DFBool <> TOKEN = op(lhs, rhs)
        def >[R](rhs: Inlined[R])(using
            op: DFToken.Compare[DFXInt[S, W], R, FuncOp.>.type, false]
        ): DFBool <> TOKEN = op(lhs, rhs)
        def <=[R](rhs: Inlined[R])(using
            op: DFToken.Compare[DFXInt[S, W], R, FuncOp.<=.type, false]
        ): DFBool <> TOKEN = op(lhs, rhs)
        def >=[R](rhs: Inlined[R])(using
            op: DFToken.Compare[DFXInt[S, W], R, FuncOp.>=.type, false]
        ): DFBool <> TOKEN = op(lhs, rhs)
      end extension
      extension [L](inline lhs: L)
        inline def <[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.<.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        inline def >[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.>.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        inline def <=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.<=.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
        inline def >=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using es: Exact.Summon[L, lhs.type])(using
            c: DFToken.Compare[DFXInt[RS, RW], es.Out, FuncOp.>=.type, true]
        ): DFBool <> TOKEN = c(rhs, es(lhs))
      end extension
      extension [L](inline lhs: L)
        inline def +[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> TOKEN
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            check: TCCheck[icL.OutS, icL.OutW, RS, RW]
        ): Unit = {}
      end extension
    end Ops
  end Token

  object Val:
    trait Candidate[-R]:
      type OutS <: Boolean
      type OutW <: Int
      type IsScalaInt <: Boolean
      def apply(arg: R)(using DFC): DFValOf[DFXInt[OutS, OutW]]
    object Candidate:
      transparent inline given fromTokenCandidate[R](using
          ic: Token.Candidate[R]
      ): Candidate[R] = new Candidate[R]:
        type OutS = ic.OutS
        type OutW = ic.OutW
        type IsScalaInt = ic.IsScalaInt
        def apply(arg: R)(using DFC): DFValOf[DFXInt[OutS, OutW]] =
          DFVal.Const(ic(arg))
      given fromDFXIntVal[S <: Boolean, W <: Int]
          : Candidate[DFValOf[DFXInt[S, W]]] with
        type OutS = S
        type OutW = W
        type IsScalaInt = false
        def apply(arg: DFValOf[DFXInt[S, W]])(using
            DFC
        ): DFValOf[DFXInt[S, W]] =
          arg
      given fromDFBitsVal[W <: Int]: Candidate[DFValOf[DFBits[W]]] with
        type OutS = false
        type OutW = W
        type IsScalaInt = false
        def apply(arg: DFValOf[DFBits[W]])(using
            DFC
        ): DFValOf[DFXInt[false, W]] =
          import DFBits.Val.Ops.uint
          arg.uint
    end Candidate

    object TC:
      import DFVal.TC
      given [LS <: Boolean, LW <: Int, R](using
          ic: Candidate[R],
          dfc: DFC
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW]
      ): TC[DFXInt[LS, LW], R] with
        def apply(dfType: DFXInt[LS, LW], value: R): Out =
          import Ops.resize
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          check(dfType.signed, dfType.width, rhs.dfType.signed, rhs.width)
          val dfValIR =
            val rhsSignFix: DFValOf[DFSInt[Int]] =
              if (dfType.signed != rhs.dfType.signed)
                rhs.asIR.asValOf[DFUInt[Int]].signed.asIR.asValOf[DFSInt[Int]]
              else rhs.asIR.asValOf[DFSInt[Int]]
            if (rhsSignFix.width < dfType.width)
              rhsSignFix.resize(dfType.width).asIR
            else rhsSignFix.asIR
          dfValIR.asValOf[DFXInt[LS, LW]]
      end given
    end TC

    object Compare:
      import DFVal.Compare
      given [LS <: Boolean, LW <: Int, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW], R, Op, C] with
        def apply(dfVal: DFValOf[DFXInt[LS, LW]], arg: R)(using
            dfc: DFC
        ): DFValOf[DFBool] =
          import Ops.resize
          import DFUInt.Val.Ops.signed
          val dfValArg = ic(arg)(using dfc.anonymize)
          check(
            dfVal.dfType.signed,
            dfVal.dfType.width,
            dfValArg.dfType.signed,
            dfValArg.dfType.width
          )
          val maxWidth = dfVal.width max dfValArg.width
          val dfValResized =
            if (dfVal.width < dfValArg.width) dfVal.resize(maxWidth)
            else dfVal
          val dfValArgResized =
            if (dfValArg.width < dfVal.width) dfValArg.resize(maxWidth)
            else dfValArg
          func(dfValResized, dfValArgResized)
        end apply
      end given
    end Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      extension [S <: Boolean, W <: Int](lhs: DFValOf[DFXInt[S, W]])
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using
            check: Width.Check[S, RW],
            dfc: DFC
        ): DFValOf[DFXInt[S, RW]] =
          val signed = lhs.dfType.signed
          check(signed, updatedWidth)
          import Token.Ops.{resize => resizeToken}
          DFVal.Alias.AsIs(
            DFXInt(signed, updatedWidth),
            lhs,
            _.resizeToken(updatedWidth)
          )
        end resize
        def <[R](rhs: Inlined[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[S, W], R, FuncOp.<.type, false]
        ): DFBool <> VAL = op(lhs, rhs)
        def <=[R](rhs: Inlined[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[S, W], R, FuncOp.<=.type, false]
        ): DFBool <> VAL = op(lhs, rhs)
        def >[R](rhs: Inlined[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[S, W], R, FuncOp.>.type, false]
        ): DFBool <> VAL = op(lhs, rhs)
        def >=[R](rhs: Inlined[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[S, W], R, FuncOp.>=.type, false]
        ): DFBool <> VAL = op(lhs, rhs)
      end extension

      extension [L](inline lhs: L)
        inline def ===[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            eq: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.===.type, true]
        ): DFBool <> VAL = eq(rhs, es(lhs))
        inline def =!=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            eq: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.=!=.type, true]
        ): DFBool <> VAL = eq(rhs, es(lhs))
        inline def <[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.<.type, true]
        ): DFBool <> VAL = op(rhs, es(lhs))
        inline def <=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.<=.type, true]
        ): DFBool <> VAL = op(rhs, es(lhs))
        inline def >[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.>.type, true]
        ): DFBool <> VAL = op(rhs, es(lhs))
        inline def >=[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW], es.Out, FuncOp.>=.type, true]
        ): DFBool <> VAL = op(rhs, es(lhs))
      end extension
      extension [L](inline lhs: L)
        inline def +[RS <: Boolean, RW <: Int](
            rhs: DFXInt[RS, RW] <> VAL
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: TCCheck[icL.OutS, icL.OutW, RS, RW]
        ): Unit = {}
      end extension
      extension [LS <: Boolean, LW <: Int](lhs: DFXInt[LS, LW] <> VAL)
        def +[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: TCCheck[LS, LW, icR.OutS, icR.OutW]
        ): Unit = {}
    end Ops

    object Conversions
//      implicit inline def DFUIntValConversion[R](inline from: R)(using
//          ic: Candidate[from.type]
//      ): DFValOf[DFUInt[Int]] =
//        val rhs = ic(from)
////        check(false, rhs.dfType.signed)
//        rhs.asIR.asValOf[DFUInt[Int]]
  end Val
end DFXInt

type DFUInt[W <: Int] = DFXInt[false, W]
object DFUInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[false, W]
  ): DFUInt[W] = DFXInt(false, width)

  type Token[W <: Int] = DFDecimal.Token[false, W, 0]
  object Token:
    object Ops:
      extension [W <: Int](lhs: Token[W])
        def signed: DFSInt.Token[W + 1] =
          import DFToken.Ops.bits
          import DFXInt.Token.Ops.resize
          import DFBits.Token.Ops.sint
          lhs.resize(lhs.width + 1).bits.sint

  object Val:
    object Ops:
      extension [W <: Int](lhs: DFValOf[DFUInt[W]])
        def signed(using DFC): DFValOf[DFSInt[W + 1]] =
          import Token.Ops.{signed => signedToken}
          DFVal.Alias.AsIs(DFSInt(lhs.width + 1), lhs, _.signedToken)
end DFUInt

type DFSInt[W <: Int] = DFXInt[true, W]
object DFSInt:
  def apply[W <: Int](width: Inlined[W])(using
      Width.Check[true, W]
  ): DFSInt[W] = DFXInt(true, width)
  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
  object Token:
    object Ops
  object Val:
    object Ops
//      extension [W <: Int](lhs: DFValOf[DFSInt[W]])
end DFSInt
