package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.quoted.*
import scala.annotation.targetName

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
    export CompanionsDFDecimal.DFTypeGiven.given
    val Width = CompanionsDFDecimal.Width
    type Token[S <: Boolean, W <: Int, F <: Int] =
      CompanionsDFDecimal.Token[S, W, F]
    val Token = CompanionsDFDecimal.Token
    val Val = CompanionsDFDecimal.Val
  end DFDecimal
//    export Companions.Extensions.*
end OpaqueDFDecimal

private object CompanionsDFDecimal:
  object DFTypeGiven:
    given [S <: Boolean, W <: Int, F <: Int](using
        ValueOf[S],
        ValueOf[W],
        ValueOf[F]
    )(using Width.Check[S, W]): DFDecimal[S, W, F] =
      DFDecimal(valueOf[S], valueOf[W], valueOf[F])

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
  protected object Sign
      extends Check2[
        Boolean,
        Int,
        [s <: Boolean, n <: Int] =>> ITE[s, true, n >= 0],
        [s <: Boolean,
        n <: Int] =>> "Unsigned value must be natural, but found: " + n
      ]

  protected[core] object `LW >= RW`
      extends Check2[
        Int,
        Int,
        [LW <: Int, RW <: Int] =>> LW >= RW,
        [LW <: Int, RW <: Int] =>> "The token value width (" + RW +
          ") is larger than the dataflow value width (" + LW + ")."
      ]
  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken.Of[DFDecimal[S, W, F]]
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

    object StrInterp:
      extension (inline sc: StringContext)
        transparent inline def d(inline args: Any*): DFToken =
          ${
            interpMacro('{ false })('sc, 'args)
          }
        transparent inline def sd(inline args: Any*): DFToken =
          ${
            interpMacro('{ true })('sc, 'args)
          }

      private def interpMacro(signedForcedExpr: Expr[Boolean])(
          sc: Expr[StringContext],
          args: Expr[Seq[Any]]
      )(using Quotes): Expr[DFToken] =
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
        `LW >= RW`(dfType.asIR.width, dfVal.asIR.dfType.width)
        dfVal
    end TC
    object Ops:
      export DFXInt.Val.Ops.*
    object Conversions
  //TODO: add checks for LW according to signed
//      given DFXIntValConversion[S <: Boolean, R](using
//          candidate: IntCandidate[R, S]
//      ): Conversion[R, DFValOf[DFXInt[S, Int]]] = from =>
//        candidate(from).asIR.asValOf[DFXInt[S, Int]]
  end Val

end CompanionsDFDecimal

type DFXInt[S <: Boolean, W <: Int] = DFDecimal[S, W, 0]
object DFXInt:
  def apply[S <: Boolean, W <: Int](signed: Inlined[S], width: Inlined[W])(using
      DFDecimal.Width.Check[S, W]
  ): DFXInt[S, W] = DFDecimal(signed, width, 0)

  type Token[S <: Boolean, W <: Int] = DFDecimal.Token[S, W, 0]
  object Token:
    import DFDecimal.Token.data
    protected[core] def apply[S <: Boolean, W <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        data: Option[BigInt]
    ): Token[S, W] = DFDecimal.Token(DFXInt(signed, width), data)

    trait Candidate[-R, Signed <: Boolean]:
      type OutW <: Int
      def apply(arg: R): Token[Signed, OutW]
    object Candidate:
      //change to given...with after
      //https://github.com/lampepfl/dotty/issues/13580 is resolved
      transparent inline given fromIntLiteral[R <: Int, Signed <: Boolean](using
          v: ValueOf[Signed],
          w: IntWidth[R, Signed]
      ): Candidate[ValueOf[R], Signed] =
        new Candidate[ValueOf[R], Signed]:
          type OutW = w.Out
          def apply(arg: ValueOf[R]): Token[Signed, OutW] =
            Token(valueOf[Signed], w(arg.value), Some(arg.value))
      transparent inline given fromInt[Signed <: Boolean](using
          v: ValueOf[Signed],
          w: IntWidth[Int, Signed]
      ): Candidate[Int, Signed] =
        new Candidate[Int, Signed]:
          type OutW = w.Out
          def apply(arg: Int): Token[Signed, OutW] =
            Token(valueOf[Signed], w(arg), Some(arg))
      transparent inline given fromDFXIntToken[W <: Int, S <: Boolean]
          : Candidate[Token[S, W], S] =
        new Candidate[Token[S, W], S]:
          type OutW = W
          def apply(arg: Token[S, W]): Token[S, W] = arg
      transparent inline given fromDFBitsToken[W <: Int]
          : Candidate[DFBits.Token[W], false] =
        new Candidate[DFBits.Token[W], false]:
          type OutW = W
          def apply(arg: DFBits.Token[W]): Token[false, W] =
            import DFBits.Token.Ops.uint
            arg.uint
      inline given errorForSignedToUnsigned[W <: Int]
          : Candidate[DFSInt.Token[W], false] =
        compiletime.error(
          "Cannot apply a signed value to an unsigned variable."
        )
      transparent inline given fromUnsignedToSigned[W <: Int]
          : Candidate[DFUInt.Token[W], true] =
        new Candidate[DFUInt.Token[W], true]:
          type OutW = W + 1
          def apply(arg: DFUInt.Token[W]): Token[true, W + 1] =
            import DFUInt.Token.Ops.signed
            arg.signed
    end Candidate

    object TC:
      import DFToken.TC
      given [S <: Boolean, LW <: Int, R](using
          ic: Candidate[R, S]
      )(using
          check: CompanionsDFDecimal.`LW >= RW`.Check[LW, ic.OutW]
      ): TC[DFXInt[S, LW], R] with
        def apply(dfType: DFXInt[S, LW], value: R): Out =
          val dfTypeIR = dfType.asIR
          val token = ic(value).asIR
          check(dfTypeIR.width, token.width)
          //We either need to widen the token we got from a value int candidate
          //or it remains the same. In either case, there is not need to touch
          //the data itself, but just the dfType of the token.
          val resizedToken =
            if (dfTypeIR.width > token.width)
              token.copy(dfType = dfTypeIR)
            else token
          resizedToken.asTokenOf[DFXInt[S, LW]]
      end given
    end TC

    object Ops:
      export DFUInt.Token.Ops.*
      export DFSInt.Token.Ops.*
      extension [S <: Boolean, W <: Int](
          lhs: Token[S, W]
      )(using ValueOf[S])
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using check: DFDecimal.Width.Check[S, RW]): Token[S, RW] =
          val updatedTokenIR =
            //no change in width
            if (updatedWidth == lhs.width) lhs.asIR
            else
              val signed = valueOf[S]
              check(signed, updatedWidth)
              //updated width is larger or the data is bubble
              if (updatedWidth > lhs.width || lhs.asIR.isBubble)
                DFXInt.Token(signed, updatedWidth, lhs.data).asIR
              else //updated width is smaller
                import DFToken.Ops.bits
                import DFBits.Token.Ops.{resize => resizeDFBits, *}
                val value = lhs.data.get
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
      end extension
    end Ops
  end Token

  object Val:
    trait Candidate[-R, Signed <: Boolean]:
      type OutW <: Int
      def apply(arg: R): DFValOf[DFXInt[Signed, OutW]]
    object Candidate:
      transparent inline given fromTokenCandidate[R, Signed <: Boolean](using
          ic: Token.Candidate[R, Signed],
          dfc: DFC
      ): Candidate[R, Signed] = new Candidate[R, Signed]:
        type OutW = ic.OutW
        def apply(arg: R): DFValOf[DFXInt[Signed, OutW]] =
          val token = ic(arg)
          DFVal.Const(token)
      given fromDFBitsVal[W <: Int](using
          DFC
      ): Candidate[DFValOf[DFBits[W]], false] with
        type OutW = W
        def apply(arg: DFValOf[DFBits[W]]): DFValOf[DFXInt[false, W]] =
          import DFBits.Val.Ops.uint
          arg.uint
      inline given errorForSignedToUnsigned[W <: Int]
          : Candidate[DFValOf[DFSInt[W]], false] =
        compiletime.error(
          "Cannot apply a signed value to an unsigned variable."
        )
    end Candidate
    object TC:
      import DFVal.TC
      given [S <: Boolean, LW <: Int, R](using
          ic: Candidate[R, S],
          s: ValueOf[S],
          dfc: DFC
      )(using
          check: CompanionsDFDecimal.`LW >= RW`.Check[LW, ic.OutW]
      ): TC[DFXInt[S, LW], R] with
        def apply(dfType: DFXInt[S, LW], value: R): Out =
          import Ops.resize
          val rhs = ic(value)
          check(dfType.width, rhs.width)
          if (rhs.width < dfType.width)
            rhs.resize(dfType.width)
          else rhs.asIR.asValOf[DFXInt[S, LW]]
      end given
    end TC

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      extension [S <: Boolean, W <: Int](lhs: DFValOf[DFXInt[S, W]])(using
          ValueOf[S]
      )
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using
            check: DFDecimal.Width.Check[S, RW],
            dfc: DFC
        ): DFValOf[DFXInt[S, RW]] =
          val signed: S = valueOf[S]
          check(signed, updatedWidth)
          import Token.Ops.{resize => resizeToken}
          DFVal.Alias.AsIs(
            DFXInt(signed, updatedWidth),
            lhs,
            _.resizeToken(updatedWidth)
          )
    end Ops
  end Val
end DFXInt

type DFUInt[W <: Int] = DFXInt[false, W]
object DFUInt:
  def apply[W <: Int](width: Inlined[W])(using
      DFDecimal.Width.Check[false, W]
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
        def signed(using DFC): DFValOf[DFUInt[W + 1]] =
          import DFVal.Ops.bits
          import DFBits.Val.Ops.sint
          import DFXInt.Val.Ops.resize
          lhs.resize(lhs.width + 1).bits.sint.asIR.asValOf[DFUInt[W + 1]]
end DFUInt

type DFSInt[W <: Int] = DFXInt[true, W]
object DFSInt:
  def apply[W <: Int](width: Inlined[W])(using
      DFDecimal.Width.Check[true, W]
  ): DFSInt[W] = DFXInt(true, width)
  type Token[W <: Int] = DFDecimal.Token[true, W, 0]
  object Token:
    object Ops
  object Val:
    object Ops
//      extension [W <: Int](lhs: DFValOf[DFSInt[W]])
end DFSInt
