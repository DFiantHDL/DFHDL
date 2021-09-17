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
    def apply[S <: Boolean, W <: Int, F <: Int](
        signed: Inlined[S],
        width: Inlined[W],
        fractionWidth: Inlined[F]
    ): DFDecimal[S, W, F] =
      ir.DFDecimal(signed, width, fractionWidth).asFE[DFDecimal[S, W, F]]

    type Token[S <: Boolean, W <: Int, F <: Int] =
      CompanionsDFDecimal.Token[S, W, F]
    val Token = CompanionsDFDecimal.Token
    val Val = CompanionsDFDecimal.Val
//    export Companions.Extensions.*
end OpaqueDFDecimal

private object CompanionsDFDecimal:
  protected object `LW >= RW`
      extends Check2[
        Int,
        Int,
        [LW <: Int, RW <: Int] =>> LW >= RW,
        [LW <: Int, RW <: Int] =>> "The token value width (" +
          ToString[RW] +
          ") is larger than the dataflow value width (" +
          ToString[LW] +
          ")."
      ]
  type Token[S <: Boolean, W <: Int, F <: Int] = DFToken.Of[DFDecimal[S, W, F]]
  object Token:
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
        dec: String
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      def fromValidString(numStr: String): (Boolean, Int, Int, BigInt) =
        val value = BigInt(numStr)
        val signed = value < 0
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

    trait IntCandidate[-R, Signed <: Boolean]:
      type OutW <: Int
      def apply(arg: R): Token[Signed, OutW, 0]
    object IntCandidate:
      given [R <: Int, Signed <: Boolean](using
          w: IntWidth[R, Signed],
          v: ValueOf[Signed]
      ): IntCandidate[ValueOf[R], Signed] with
        type OutW = w.Out
        def apply(arg: ValueOf[R]): Token[Signed, OutW, 0] =
          val width = Inlined.forced[OutW](w(arg.value))
          Token(valueOf[Signed], width, 0, arg.value)
      given [W <: Int, S <: Boolean]: IntCandidate[Token[S, W, 0], S] with
        type OutW = W
        def apply(arg: Token[S, W, 0]): Token[S, W, 0] = arg
      given [W <: Int]: IntCandidate[DFUInt.Token[W], true] with
        type OutW = W + 1
        def apply(arg: DFUInt.Token[W]): Token[true, W + 1, 0] =
//          import
//          arg.signed
          ???
    end IntCandidate

    object TC:
      import DFToken.TC
      given [S <: Boolean, LW <: Int, R](using
          ic: IntCandidate[R, S]
      )(using
          check: `LW >= RW`.Check[LW, ic.OutW]
      ): TC[DFDecimal[S, LW, 0], R] with
        def apply(dfType: DFDecimal[S, LW, 0], value: R): Out =
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
          resizedToken.asTokenOf[DFDecimal[S, LW, 0]]
      end given
    end TC

    object StrInterp:
      extension (inline sc: StringContext)
        transparent inline def d(inline args: Any*): DFToken =
          ${
            interpMacro('sc, 'args)
          }

      private def interpMacro(
          sc: Expr[StringContext],
          args: Expr[Seq[Any]]
      )(using Quotes): Expr[DFToken] =
        import quotes.reflect.*
        val argsExprs = args match
          case Varargs(argsExprs) => argsExprs
        val '{ StringContext.apply($parts*) } = sc
        val partsExprs = parts match
          case Varargs(argsExprs) => argsExprs
        val fullTermParts =
          Seq(partsExprs, argsExprs)
            .flatMap(_.zipWithIndex)
            .sortBy(_._2)
            .map(_._1.asTerm)
        val fullTerm = fullTermParts.reduce[Term] {
          case (Literal(StringConstant(l)), Literal(StringConstant(r))) =>
            Literal(StringConstant(l + r))
          case (l, r) =>
            '{ ${ l.asExpr }.toString + ${ r.asExpr }.toString }.asTerm
        }
        val (signedTpe, widthTpe, fractionWidthTpe)
            : (TypeRepr, TypeRepr, TypeRepr) = fullTerm match
          case Literal(StringConstant(t)) =>
            fromDecString(t) match
              case Right((signed, width, fractionWidth, _)) =>
                (
                  ConstantType(BooleanConstant(signed)),
                  ConstantType(IntConstant(width)),
                  ConstantType(IntConstant(fractionWidth))
                )
              case Left(msg) =>
                report.error(msg)
                ???
          case _ => (TypeRepr.of[Boolean], TypeRepr.of[Int], TypeRepr.of[Int])
        val signedType = signedTpe.asTypeOf[Boolean]
        val widthType = widthTpe.asTypeOf[Int]
        val fractionWidthType = fractionWidthTpe.asTypeOf[Int]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          import DFiant.internals.Inlined
          val (signed, width, fractionWidth, value) =
            fromDecString($fullExpr).toOption.get
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
  end Token

  object Val:
    trait IntCandidate[-R, Signed <: Boolean]:
      type OutW <: Int
      def apply(arg: R): DFValOf[DFDecimal[Signed, OutW, 0]]
    object IntCandidate:
      given [R, Signed <: Boolean](using
          ic: Token.IntCandidate[R, Signed],
          dfc: DFC
      ): IntCandidate[R, Signed] with
        type OutW = ic.OutW
        def apply(arg: R): DFValOf[DFDecimal[Signed, OutW, 0]] =
          val token = ic(arg)
          DFVal.Const(token)
//      given [W <: Int]: IntCandidate[DFBits.Token[W], false] with
//        type OutW = W
//        def apply(arg: DFBits.Token[W]): Token[false, W, 0] =
//          import DFBits.Token.Ops.as
//          arg.as(DFUInt(arg.widthHack))
    end IntCandidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFDecimal[Boolean, Int, Int],
          dfVal: DFDecimal[Boolean, Int, Int] <> VAL
      ): DFDecimal[Boolean, Int, Int] <> VAL =
        `LW >= RW`(dfType.asIR.width, dfVal.asIR.dfType.width)
        dfVal
      given [S <: Boolean, LW <: Int, R](using
          ic: IntCandidate[R, S],
          p: PrintType[R]
      )(using
          check: `LW >= RW`.Check[LW, ic.OutW]
      ): TC[DFDecimal[S, LW, 0], R] with
        def apply(dfType: DFDecimal[S, LW, 0], value: R): Out =
//          val dfTypeIR = dfType.asIR
//          val token = ic(value).asIR
//          check(dfTypeIR.width, token.width)
          ???
    end TC
  end Val

end CompanionsDFDecimal

type DFUInt[W <: Int] = DFDecimal[false, W, 0]
object DFUInt:
  def apply[W <: Int](width: Inlined[W])(using
      check: Arg.Width.Check[W]
  ): DFUInt[W] =
    check(width)
    DFDecimal(false, width, 0)
  type Token[W <: Int] = DFDecimal.Token[false, W, 0]
  object Token:
    object Ops:
      extension [W <: Int](lhs: Token[W])
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using check: Arg.Width.Check[RW]): Token[RW] =
          check(updatedWidth)
          ???

  object Val:
    object Ops:
      extension [W <: Int](lhs: DFValOf[DFUInt[W]])
        @targetName("resizeDFUInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using Arg.Width.Check[RW], DFC): DFValOf[DFUInt[RW]] =
          DFVal.Alias.AsIs(DFUInt(updatedWidth), lhs)
end DFUInt

type DFSInt[W <: Int] = DFDecimal[true, W, 0]
object DFSInt:
  def apply[W <: Int](width: Inlined[W])(using
      check: Arg.SignedWidth.Check[W]
  ): DFSInt[W] =
    check(width)
    DFDecimal(true, width, 0)
  type Token[W <: Int] = DFDecimal.Token[true, W, 0]

  object Val:
    object Ops:
      extension [W <: Int](lhs: DFValOf[DFSInt[W]])
        @targetName("resizeDFSInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using Arg.SignedWidth.Check[RW], DFC): DFValOf[DFSInt[RW]] =
          DFVal.Alias.AsIs(DFSInt(updatedWidth), lhs)
end DFSInt
