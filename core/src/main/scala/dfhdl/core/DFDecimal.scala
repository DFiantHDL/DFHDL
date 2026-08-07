package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.*

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

// `M` is the magnitude (integer-part) width, including the sign bit for signed values. The
// total bit width is `M + F`; for integer types (F == 0) the magnitude width is the total
// width. See `DecimalWidth` for the derived total width used by the `Width` type class.
type DFDecimal[S <: Boolean, M <: IntP, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, M, F, N]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, M <: IntP, F <: Int, N <: NativeType](
      signed: Inlined[S],
      magnitudeWidth: IntParam[M],
      fractionWidth: Inlined[F],
      nativeType: N
  )(using dfc: DFC, check: Width.CheckNUB[S, DecimalWidth[M, F]]): DFDecimal[S, M, F, N] = trydf:
    // the width constraints apply to the total bit width (magnitude + fraction)
    magnitudeWidth.toScalaIntOpt.foreach(m => check(signed, m + fractionWidth))
    ir.DFDecimal(signed, magnitudeWidth.ref, fractionWidth, nativeType).asFE[DFDecimal[S, M, F, N]]
  protected[core] def forced[S <: Boolean, M <: IntP, F <: Int, N <: NativeType](
      signed: Boolean,
      magnitudeWidth: Int,
      fractionWidth: Int,
      nativeType: NativeType
  )(using DFC): DFDecimal[S, M, F, N] =
    val check = summon[Width.Check[Boolean, Int]]
    check(signed, magnitudeWidth + fractionWidth)
    ir.DFDecimal(signed, ir.IntParamRef(magnitudeWidth), fractionWidth, nativeType)
      .asFE[DFDecimal[S, M, F, N]]

  given DFInt32 = DFInt32
  given [S <: Boolean, M <: IntP & Singleton, F <: Int, N <: NativeType](using
      ValueOf[S],
      ValueOf[M],
      ValueOf[F],
      ValueOf[N]
  )(using DFCG, Width.CheckNUB[S, DecimalWidth[M, F]]): DFDecimal[S, M, F, N] = trydf:
    DFDecimal(valueOf[S], IntParam[M](valueOf[M]), valueOf[F], valueOf[N])
  object Extensions:
    extension [S <: Boolean, M <: IntP, F <: Int, N <: NativeType](dfType: DFDecimal[S, M, F, N])
      def signed: Inlined[S] = Inlined.forced[S](dfType.asIR.signed)
      def nativeType: N = dfType.asIR.nativeType.asInstanceOf[N]
      def fractionWidth: Inlined[F] = Inlined.forced[F](dfType.asIR.fractionWidth)

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
    object FractionWidth
        extends Check1[
          Int,
          [f <: Int] =>> f >= 0,
          [f <: Int] =>> "Fraction width must be non-negative, but found: " + f
        ]
    object MagnitudeWidth
        extends Check2[
          Boolean,
          Int,
          [s <: Boolean, m <: Int] =>> ITE[s, m > 0, m >= 0],
          [s <: Boolean, m <: Int] =>> ITE[
            s,
            "Signed magnitude width must include the sign bit (at least 1), but found: " + m,
            "Unsigned magnitude width must be non-negative, but found: " + m
          ]
        ]

    object `LW >= RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW >= RW,
          [LW <: Int, RW <: Int] =>> "The applied RHS value width (" + RW +
            ") is larger than the LHS variable width (" + LW + ")."
        ]
    object `W <= 32`
        extends Check1[
          Int,
          [W <: Int] =>> W <= 32,
          [W <: Int] =>> "Width must be no larger than 32, but found: " + W
        ]
    object `W <= 31`
        extends Check1[
          Int,
          [W <: Int] =>> W <= 31,
          [W <: Int] =>> "Width must be no larger than 31, but found: " + W
        ]
    object `LW == RW`
        extends Check2[
          Int,
          Int,
          [LW <: Int, RW <: Int] =>> LW == RW,
          [LW <: Int, RW <: Int] =>> "Cannot apply this operation between a value of " + LW +
            " bits width (LHS) and a value of " + RW +
            " bits width (RHS).\nAn explicit conversion must be applied."
        ]
    object `LS >= RS`
        extends Check2[
          Boolean,
          Boolean,
          [LS <: Boolean, RS <: Boolean] =>> LS || ![RS],
          [LS <: Boolean, RS <: Boolean] =>> "Cannot apply this operation between " +
            ITE[LS, "a signed", "an unsigned"] + " value (LHS) and " +
            ITE[RS, "a signed", "an unsigned"] +
            " value (RHS).\nAn explicit conversion must be applied."
        ]
    object `BaS >= WcS`
        extends Check2[
          Boolean,
          Boolean,
          [BaS <: Boolean, WcS <: Boolean] =>> BaS || ![WcS],
          [BaS <: Boolean, WcS <: Boolean] =>> "Cannot apply a signed wildcard `Int` value to " +
            ITE[BaS, "a signed", "an unsigned"] +
            " bit-accurate value.\nUse an explicit conversion or `sd\"\"` interpolation."
        ]
    object `BaW >= WcW`
        extends Check2[
          Int,
          Int,
          [BaW <: Int, WcW <: Int] =>> BaW >= WcW,
          [BaW <: Int, WcW <: Int] =>> "The wildcard `Int` value width (" + WcW +
            ") is larger than the bit-accurate value width (" + BaW + ")."
        ]
    object `!(LN && RN)`
        extends Check2[
          Boolean,
          Boolean,
          [LN <: Boolean, RN <: Boolean] =>> ![LN && RN],
          [LN <: Boolean,
          RN <: Boolean] =>> "Carry operations require at least one bit-accurate operand (`UInt`/`SInt`), but both operands are `Int` values."
        ]
    // A carry operation widens relative to a bit-accurate operand, so at least one operand
    // must be bit-accurate (native `Int` operands are wildcards with no width of their own).
    type CarryCheck[LN <: NativeType, RN <: NativeType] = `!(LN && RN)`.Check[LN, RN]
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
    trait TCCheck[LS <: Boolean, LW <: IntP, RS <: Boolean, RW <: IntP]:
      def apply(
          leftSigned: Boolean,
          leftWidth: Int,
          rightSigned: Boolean,
          rightWidth: Int
      ): Unit
    given [LS <: Boolean, LW <: IntP, LWI <: Int, RS <: Boolean, RW <: IntP, RWI <: Int](using
        ubLW: UBound.Aux[Int, LW, LWI],
        ubRW: UBound.Aux[Int, RW, RWI],
        checkS: `LS >= RS`.Check[LS, RS],
        checkW: `LW >= RW`.Check[LWI, ITE[LS != RS, RWI + 1, RWI]]
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
        ValW <: IntP,
        ArgS <: Boolean,
        ArgW <: IntP,
        ArgIsInt <: Boolean, // argument is a wildcard (Int32 NativeType)
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit
    end CompareCheck
    given [
        ValS <: Boolean,
        ValW <: IntP,
        ValWI <: Int,
        ArgS <: Boolean,
        ArgW <: IntP,
        ArgWI <: Int,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        ubv: UBound.Aux[Int, ValW, ValWI],
        uba: UBound.Aux[Int, ArgW, ArgWI],
        argWFix: Id[ITE[ArgIsInt && ValS && ![ArgS], ArgWI + 1, ArgWI]],
        skipChecks: Id[ArgIsInt && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValWI]],
        rw: Id[ITE[Castle ^ skipChecks.Out, ValWI, argWFix.Out]]
    )(using
        checkS: `LS == RS`.Check[ls.Out, rs.Out],
        checkW: `LW == RW`.Check[lw.Out, rw.Out],
        checkVAW: `BaW >= WcW`.Check[ValWI, ITE[ArgIsInt, argWFix.Out, 0]],
        argIsInt: ValueOf[ArgIsInt],
        castle: ValueOf[Castle]
    ): CompareCheck[ValS, ValW, ArgS, ArgW, ArgIsInt, Castle] with
      def apply(
          dfValSigned: Boolean,
          dfValWidth: Int,
          argSigned: Boolean,
          argWidth: Int
      ): Unit =
        val isInt = argIsInt.value
        val skipChecks = isInt && (dfValSigned || !argSigned)
        val argWFix =
          if (isInt && dfValSigned && !argSigned) argWidth + 1
          else argWidth
        if (isInt) checkVAW(dfValWidth, argWFix)
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
        LS <: Boolean,
        LW <: IntP,
        LN <: NativeType,
        RS <: Boolean,
        RW <: IntP,
        RN <: NativeType
    ]:
      def apply(
          lhs: DFValOf[DFXInt[LS, LW, LN]],
          rhs: DFValOf[DFXInt[RS, RW, RN]]
      )(using DFC): Unit
    end ArithCheck
    given [
        LS <: Boolean,
        LW <: IntP,
        LN <: NativeType,
        LWI <: Int,
        RS <: Boolean,
        RW <: IntP,
        RN <: NativeType,
        RWI <: Int
    ](using
        // forcing Int upper-bound
        ubL: UBound.Aux[Int, LW, LWI],
        // forcing Int upper-bound
        ubR: UBound.Aux[Int, RW, RWI],
        // the RHS width is increased by 1 if the LHS is signed and the RHS is unsigned,
        // because the RHS will be converted to signed for the arithmetic operation
        signedRW: Id[ITE[LS && ![RS], RWI + 1, RWI]]
    )(using
        // When LHS is a wildcard (LN=Int32), bypass sign/width checks by comparing
        // the value against itself (always passes). Wildcards adapt at runtime.
        checkS: `LS >= RS`.Check[ITE[LN, LS, LS], ITE[LN, LS, RS]],
        checkW: `LW >= RW`.Check[ITE[LN, LWI, LWI], ITE[LN, LWI, signedRW.Out]],
        isWildcardL: ValueOf[LN]
    ): ArithCheck[LS, LW, LN, RS, RW, RN] with
      def apply(
          lhs: DFValOf[DFXInt[LS, LW, LN]],
          rhs: DFValOf[DFXInt[RS, RW, RN]]
      )(using dfc: DFC): Unit =
        if (!isWildcardL.value)
          import dfc.getSet
          import DFXInt.Val.getActualSignedWidthOpt
          (lhs.getActualSignedWidthOpt, rhs.getActualSignedWidthOpt) match
            case (Some(lhsSigned, lhsWidthIntOpt), Some(rhsSigned, rhsWidthIntOpt)) =>
              checkS(lhsSigned, rhsSigned)
              (lhsWidthIntOpt, rhsWidthIntOpt) match
                case (Some(lhsWidth), Some(rhsWidth)) =>
                  val rhsSignedWidth: Int =
                    if (lhsSigned && !rhsSigned) rhsWidth + 1
                    else rhsWidth
                  checkW(lhsWidth, rhsSignedWidth)
                case _ =>
            case _ =>
      end apply
    end given

    trait SignCheck[
        ValS <: Boolean,
        ArgS <: Boolean,
        ArgIsInt <: Boolean, // argument is a wildcard (Int32 NativeType)
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

    type NativeCheck[LN <: NativeType, RN <: NativeType] =
      AssertGiven[
        (RN =:= Int32) | ((LN =:= RN) | (LN =:= BitAccurate)),
        "Cannot implicitly convert to DFHDL Int type."
      ]
  end Constraints

  object StrInterp:
    private[DFDecimal] val widthNoValuePattern = "([\\d_,]+)'".r
    private[DFDecimal] val valueNoWidthPattern = "'(-?\\d+)".r
    private[DFDecimal] val widthValuePattern = "(\\d+)'(-?[\\d_,]+)".r
    private[DFDecimal] val widthFixedPattern = "(\\d+)\\.(\\d+)'(-?\\d+)(?:\\.(\\d+))?".r
    private[DFDecimal] val valueFixedPattern = "(-?\\d+)\\.(\\d+)".r
    // reserved grammar for scaled formats (binary point outside the stored bits)
    private[DFDecimal] val scaledFormatPattern = "\\d+(?:\\.\\d+)?[pP][+-]?\\d+'.*".r
    private[DFDecimal] val numPattern = "(-?\\d+)".r
    // all `from*DecString` helpers and `fromDecString` return the tuple
    // (signed, magnitudeWidth, fractionWidth, value); for integers the magnitude width is the
    // total width (fraction width == 0)
    private[DFDecimal] def fromIntDecString(
        numStr: String,
        signedForced: Boolean
    ): (Boolean, Int, Int, BigInt) =
      val value = BigInt(numStr)
      val signed = value < 0 | signedForced
      val actualWidth = value.bitsWidth(signed)
      (signed, actualWidth, 0, value)
    // explicit `M.F'value` fixed-point literal: the value is rounded to the closest
    // representable value (round-half-up, ties away from zero) and must fit the magnitude
    private def fromFixedDecString(
        magnitudeWidth: Int,
        fractionWidth: Int,
        wholeStr: String,
        fractionStrOpt: Option[String],
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      val decValue = BigDecimal(wholeStr + fractionStrOpt.map("." + _).getOrElse(""))
      val signed = decValue < 0 | signedForced
      val raw = (decValue * BigDecimal(2).pow(fractionWidth))
        .setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      val totalWidth = magnitudeWidth + fractionWidth
      val actualWidth = raw.bitsWidth(signed)
      if (actualWidth > totalWidth)
        Left(
          s"The value $decValue requires a magnitude width of at least ${actualWidth - fractionWidth}, but found: $magnitudeWidth"
        )
      else Right((signed, magnitudeWidth, fractionWidth, raw))
    end fromFixedDecString
    // `value.fraction` fixed-point literal without an explicit format: allowed only when
    // the value is exactly representable in binary, inferring the minimal `M.F` format
    private def fromFixedValueDecString(
        wholeStr: String,
        fractionStr: String,
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      val decValue = BigDecimal(wholeStr + "." + fractionStr)
      val signed = decValue < 0 | signedForced
      // a finite decimal fraction is exactly representable in binary iff its reduced
      // denominator is a power of two, in which case the minimal fraction width never
      // exceeds the decimal fraction digit count
      var scaled = decValue
      var fractionWidth = 0
      while (!scaled.isWhole && fractionWidth < fractionStr.length)
        scaled *= 2
        fractionWidth += 1
      if (!scaled.isWhole)
        Left(
          s"""|The value $decValue is not exactly representable in binary.
              |To Fix: use an explicit `M.F'` width format to opt into rounding.""".stripMargin
        )
      else
        val raw = scaled.toBigInt
        // minimal magnitude: the raw value's bits beyond the fraction, but at least the sign
        // bit for signed values (and never negative for unsigned)
        val magnitudeWidth = (raw.bitsWidth(signed) - fractionWidth) max (if (signed) 1 else 0)
        Right((signed, magnitudeWidth, fractionWidth, raw))
    end fromFixedValueDecString
    private def fromDecString(
        dec: String,
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      dec.replace(",", "").replace("_", "") match
        case numPattern(numStr)    => Right(fromIntDecString(numStr, signedForced))
        case scaledFormatPattern() =>
          Left("Scaled formats (`p` binary-exponent notation) are not yet supported.")
        case widthFixedPattern(magnitudeWidthStr, fractionWidthStr, wholeStr, fractionStr) =>
          fromFixedDecString(
            magnitudeWidthStr.toInt,
            fractionWidthStr.toInt,
            wholeStr,
            Option(fractionStr),
            signedForced
          )
        case valueFixedPattern(wholeStr, fractionStr) =>
          fromFixedValueDecString(wholeStr, fractionStr, signedForced)
        case _ =>
          Left(s"Invalid decimal pattern found: $dec")
      end match
    end fromDecString

    extension (fullTerm: String)
      // the `M` type parameter is the magnitude width (== total width for integer literals,
      // where the explicit-width forms below only ever apply)
      private[DFDecimal] def interpolate[S <: Boolean, M <: IntP, F <: Int](
          op: String,
          explicitWidthOption: Option[IntP]
      )(using DFC): DFConstOf[DFDecimal[S, M, F, BitAccurate]] =
        val (interpSigned, interpMagnitudeWidth, interpFractionWidth, interpValue) =
          fromDecString(fullTerm, op == "sd").toOption.get
        val signed = Inlined.forced[S](interpSigned)
        val fractionWidth = Inlined.forced[F](interpFractionWidth)
        explicitWidthOption match
          // explicit integer width (integer literals only, so magnitude == total width)
          case Some(int: Int) =>
            val magnitudeWidth = IntParam.forced[M](int)
            DFVal.Const(
              DFDecimal(signed, magnitudeWidth, fractionWidth, BitAccurate),
              Some(interpValue),
              named = true
            )
          // no explicit width, use the inferred magnitude width from the value
          case None =>
            val magnitudeWidth = IntParam.forced[M](interpMagnitudeWidth)
            DFVal.Const(
              DFDecimal(signed, magnitudeWidth, fractionWidth, BitAccurate),
              Some(interpValue),
              named = true
            )
          // explicit parametric width, so use the inferred constant and resize it with the parameter
          case Some(ref) =>
            val magnitudeWidth = IntParam.forced[M](ref)
            import DFXInt.Val.Ops.resize
            DFVal.Const(
              DFDecimal(signed, interpMagnitudeWidth, fractionWidth, BitAccurate),
              Some(interpValue)
            )
              .asConstOf[DFXInt[S, M, BitAccurate]].resize(magnitudeWidth)
              .asConstOf[DFDecimal[S, M, F, BitAccurate]]
        end match
    end extension

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private[DFDecimal] def interpolate(
          opExpr: Expr[String],
          explicitWidthOptionExpr: Expr[Option[IntP]]
      )(dfc: Expr[DFC]): Expr[DFConstAny] =
        import quotes.reflect.*
        val explicitWidthTpeOption: Option[TypeRepr] = explicitWidthOptionExpr match
          case '{ Some($expr) } => Some(expr.asTerm.tpe)
          case _                => None
        val signedForced = opExpr.value.get == "sd"
        val (signedTpe, interpMagnitudeWidthTpe, fractionWidthTpe) =
          fullTerm match
            case Literal(StringConstant(t)) =>
              fromDecString(t, signedForced) match
                case Right((signed, magnitudeWidth, fractionWidth, value)) =>
                  if (!signedForced && value < 0)
                    report.errorAndAbort(
                      s"Negative value in unsigned `d\"\"` interpolation. Use `sd\"\"` for signed values."
                    )
                  explicitWidthTpeOption match
                    case Some(ConstantType(IntConstant(explicitWidth))) =>
                      val actualWidth = fromIntDecString(t, signedForced)._2
                      if (explicitWidth < actualWidth)
                        report.errorAndAbort(
                          s"Explicit given width ($explicitWidth) is smaller than the actual width ($actualWidth)."
                        )
                    case _ =>
                  (
                    ConstantType(BooleanConstant(signed)),
                    ConstantType(IntConstant(magnitudeWidth)),
                    ConstantType(IntConstant(fractionWidth))
                  )
                case Left(msg) =>
                  report.errorAndAbort(msg)
            case _ => (TypeRepr.of[Boolean], TypeRepr.of[Int], TypeRepr.of[Int])
        // for explicit integer widths the magnitude width is the given total width (fraction
        // width is zero on those paths)
        val magnitudeWidthTpe: TypeRepr = explicitWidthTpeOption.getOrElse(interpMagnitudeWidthTpe)
        val signedType = signedTpe.asTypeOf[Boolean]
        val magnitudeWidthType = magnitudeWidthTpe.asTypeOf[IntP]
        val fractionWidthType = fractionWidthTpe.asTypeOf[Int]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          $fullExpr.interpolate[
            signedType.Underlying,
            magnitudeWidthType.Underlying,
            fractionWidthType.Underlying
          ](
            $opExpr,
            $explicitWidthOptionExpr
          )(using $dfc)
        }
      end interpolate
    end extension
  end StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.*
    opaque type DecStrCtx <: StringContext = StringContext
    object DecStrCtx:
      extension (inline sc: DecStrCtx)
        transparent inline def apply(inline args: Any*)(using dfc: DFCG): Any =
          ${ applyMacro('sc, 'args)('dfc) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using dfc: DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('sc, 'arg)('dfc) }

    extension (sc: StringContext)
      /** Decimal Integer String Interpolator
        *
        * Syntax: {{{d"width'dec"}}}
        *   - `dec` is a sequence of decimal characters ('0'-'9') with an optional prefix `-` for
        *     negative values. `dec` can also be a single interpolated expression of type `String`,
        *     `Int`, or `BigInt`.
        *   - Separators `_` (underscore) and `,` (comma) within `dec` are ignored.
        *   - `width`, followed by a `'`, is optional and specifies the exact width of the integer's
        *     bit representation. If omitted, the width is inferred from the value's size. If
        *     specified, the output is padded with zeros or extended for signed numbers using two's
        *     complement representation to match the `width`. `width` can also be an interpolated
        *     single expression of type `Int` or a DFHDL `Int` parameter.
        *   - The output type is unsigned `UInt[W]` for natural numbers and signed `SInt[W]` for
        *     negative numbers, where `W` is the width in bits.
        *   - If the specified `width` is less than the required number of bits to represent the
        *     value, an error occurs.
        *
        * @example
        *   {{{
        *   d"0"             // UInt[1], value = 0
        *   d"-1"            // SInt[2], value = -1
        *   d"8'-1"          // SInt[8], value = -1
        *   d"255"           // UInt[8], value = 255
        *   d"1,023"         // UInt[10], value = 1023
        *   val str42 = "42"
        *   d"${str42}"      // UInt[6], value = 42
        *   val w = 8
        *   d"${w}'${str42}" // UInt[8], value = 42
        *   val p: Int <> CONST = 8
        *   d"${p}'${str42}" // UInt[p.type], value = 42
        *   }}}
        *
        * Fixed-point syntax: {{{d"M.F'dec"}}}
        *   - `M.F`, followed by a `'`, specifies the format: `M` integer (magnitude) bits and `F`
        *     fraction bits, with a total width of `M + F` bits. `dec` may then contain a decimal
        *     point (e.g., `11.223`) and is rounded to the closest representable value
        *     (round-half-up). If the rounded value's integer part does not fit `M`, an error
        *     occurs. `d"M.0'dec"` is equivalent to `d"M'dec"`.
        *   - Without an explicit format, a `dec` containing a decimal point is allowed only when it
        *     is exactly representable in binary, inferring the minimal `UFix[M, F]` format.
        *   - The output type is `UFix[M, F]` (`DFDecimal` with a non-zero fraction width).
        *
        * @example
        *   {{{
        *   d"8.10'11.223"   // UFix[8, 10], value = 11.22265625 (rounded)
        *   d"1.5"           // UFix[1, 1], value = 1.5 (exact)
        *   d"0.25"          // UFix[0, 2], value = 0.25 (exact)
        *   d"11.223"        // Error: not exactly representable in binary
        *   }}}
        *
        * @note
        *   This interpolator does not accept external arguments through `${arg}` and currently
        *   supports only integer values.
        * @return
        *   A decimal type representing an unsigned (`UInt`) or signed (`SInt`) integer, encoded in
        *   two's complement.
        */
      def d: DecStrCtx = sc

      /** Signed Decimal Integer String Interpolator
        *
        * Syntax: {{{sd"width'dec"}}}
        *   - `dec` is a sequence of decimal characters ('0'-'9') with an optional prefix `-` for
        *     negative values. `dec` can also be a single interpolated expression of type `String`,
        *     `Int`, or `BigInt`.
        *   - Separators `_` (underscore) and `,` (comma) within `dec` are ignored.
        *   - `width`, followed by a `'`, is optional and specifies the exact width of the integer's
        *     bit representation, which is always at least 2 bits to accommodate the sign bit.
        *     `width` can also be a single interpolated expression of type `Int` or a DFHDL `Int`
        *     parameter.
        *   - The output is always a signed integer type `SInt[W]`, regardless of whether the `dec`
        *     value is negative or natural, where `W` is the width in bits.
        *   - If the specified `width` is less than the required number of bits to represent the
        *     value including the sign bit, an error occurs.
        *
        * @example
        *   {{{
        *   sd"0"             // SInt[2], value = 0 (natural number represented as a signed type)
        *   sd"-1"            // SInt[2], value = -1
        *   sd"255"           // SInt[9], value = 255 (natural number represented as a signed type)
        *   sd"8'255"         // Error: width is too small to represent the value including the sign bit
        *   val str42 = "42"
        *   sd"${str42}"      // SInt[7], value = 42
        *   val w = 8
        *   sd"${w}'${str42}" // SInt[8], value = 42
        *   val p: Int <> CONST = 8
        *   sd"${p}'${str42}" // SInt[p.type], value = 42
        *   }}}
        *
        * Fixed-point syntax: {{{sd"M.F'dec"}}}
        *   - Same as the `d` interpolator's fixed-point form, but the output is always signed:
        *     `SFix[M, F]`, where `M` includes the sign bit (consistent with `SInt[W]`).
        *
        * @example
        *   {{{
        *   sd"4.4'-1.5"     // SFix[4, 4], value = -1.5
        *   sd"1.5"          // SFix[2, 1], value = 1.5 (minimal exact format)
        *   }}}
        *
        * @note
        *   This interpolator does not accept external arguments through `${arg}` and currently
        *   supports only integer values. It ensures that the output is always treated as a signed
        *   integer, providing an explicit way to work with signed numbers.
        * @return
        *   A decimal type representing a signed integer (`SInt`) value, encoded in two's
        *   complement.
        */
      def sd: DecStrCtx = sc
    end extension

    private def uintConst(value: BigInt)(using DFC): DFConstOf[DFUInt[Int]] =
      if (value < 0) throw new IllegalArgumentException(
        sn"""|Unexpected negative value found for unsigned decimal string interpolation: $value
             |To Fix: Use the signed decimal string interpolator `sd` instead."""
      )
      DFVal.Const(DFUInt.forced[Int](value.bitsWidth(false)), Some(value), named = true)
    end uintConst
    private def sintConst(value: BigInt)(using DFC): DFConstOf[DFSInt[Int]] =
      DFVal.Const(DFSInt.forced[Int](value.bitsWidth(true)), Some(value), named = true)
    end sintConst

    private def applyMacro(
        sc: Expr[DecStrCtx],
        args: Expr[Seq[Any]]
    )(dfc: Expr[DFC])(using Quotes): Expr[DFConstAny] =
      import quotes.reflect.*
      val Varargs(argsExprs) = args.runtimeChecked
      val parts = sc.parts.map(_.value.get).toList
      object WidthExpr:
        def unapply(arg: Expr[Any]): Option[Expr[IntP]] =
          val tpe = arg.asTerm.tpe
          tpe.asTypeOf[Any] match
            case '[IntP] => Some(arg.asExprOf[IntP])
            case _       =>
              report.errorAndAbort(
                s"Expecting a constant DFHDL Int value but found: `${tpe.showType}`",
                arg.asTerm.pos
              )
      object ValueExpr:
        def unapply(arg: Expr[Any]): Option[Expr[DFConstAny]] =
          val tpe = arg.asTerm.tpe
          tpe.asTypeOf[Any] match
            case '[DFConstInt32] => Some(arg.asExprOf[DFConstInt32])
            case '[Int]          => Some(ConstIntExpr(arg.asExprOf[Int]))
            case '[Long]         => Some(ConstLongExpr(arg.asExprOf[Long]))
            case '[BigInt]       => Some(ConstBigIntExpr(arg.asExprOf[BigInt]))
            case '[String]       => Some(ConstStringExpr(arg.asExprOf[String]))
            case _               =>
              report.errorAndAbort(
                s"Expecting a constant DFHDL Int value but found: `${tpe.showType}`",
                arg.asTerm.pos
              )
      def ConstIntExpr(valueExpr: Expr[Int]): Expr[DFConstAny] =
        ConstBigIntExpr('{ BigInt($valueExpr) })
      def ConstLongExpr(valueExpr: Expr[Long]): Expr[DFConstAny] =
        ConstBigIntExpr('{ BigInt($valueExpr) })
      def ConstStringExpr(valueExpr: Expr[String]): Expr[DFConstAny] =
        ConstBigIntExpr('{ BigInt($valueExpr) })
      def ConstBigIntExpr(valueExpr: Expr[BigInt]): Expr[DFConstAny] =
        if (sc.funcName == "sd") '{ sintConst($valueExpr)(using $dfc) }
        else '{ uintConst($valueExpr)(using $dfc) }
      end ConstBigIntExpr
      def AsIsExpr(widthExpr: Expr[IntP], valueExpr: Expr[DFConstAny]): Expr[DFConstAny] =
        val widthType = widthExpr.asTerm.tpe.asTypeOf[IntP]
        sc.funcName match
          case "d" =>
            '{
              DFVal.Alias.AsIs(
                DFUInt.forced[widthType.Underlying]($widthExpr)(using $dfc),
                $valueExpr
              )(using $dfc)
            }
          case "sd" =>
            '{
              DFVal.Alias.AsIs(
                DFSInt.forced[widthType.Underlying]($widthExpr)(using $dfc),
                $valueExpr
              )(using $dfc)
            }
        end match
      end AsIsExpr
      val result = parts match
        // $value
        case "" :: "" :: Nil =>
          val (ValueExpr(valueExpr) :: Nil) = argsExprs.toList.runtimeChecked
          valueExpr
        // $width'$value
        case "" :: "'" :: "" :: Nil =>
          val (WidthExpr(widthExpr) :: ValueExpr(valueExpr) :: Nil) =
            argsExprs.toList.runtimeChecked
          AsIsExpr(widthExpr, valueExpr)
        // 16'$value
        case widthNoValuePattern(widthStr) :: "" :: Nil =>
          val (ValueExpr(valueExpr) :: Nil) = argsExprs.toList.runtimeChecked
          val widthExpr = Expr(widthStr.toInt)
          AsIsExpr(widthExpr, valueExpr)
        // $width'1234
        case "" :: valueNoWidthPattern(valueStr) :: Nil =>
          val (WidthExpr(widthExpr) :: Nil) = argsExprs.toList.runtimeChecked
          Expr(valueStr).asTerm.interpolate(Expr(sc.funcName), '{ Some($widthExpr) })(dfc)
        // 16'1234
        case widthValuePattern(widthStr, valueStr) :: Nil =>
          val widthExpr = Expr(widthStr.toInt)
          Expr(valueStr).asTerm.interpolate(Expr(sc.funcName), '{ Some($widthExpr) })(dfc)
        // any single-part decimal literal, e.g. 1234 / 1.5 / 8.10'11.223
        // (validity is determined by `fromDecString` within the interpolation macro)
        case singlePart :: Nil =>
          Expr(singlePart).asTerm.interpolate(Expr(sc.funcName), '{ None })(dfc)
        case _ =>
          report.errorAndAbort(
            s"Unsupported decimal string interpolation pattern"
          )
      end result
      val ctName = '{ CTName(${ Expr(sc.funcName + " decimal string interpolation") }) }
      val resultType = result.asTerm.tpe.asTypeOf[DFConstAny]
      '{ trydf[resultType.Underlying]($result)(using $dfc, $ctName) }
    end applyMacro

    private def unapplySeqMacro[T <: DFTypeAny](
        sc: Expr[DecStrCtx],
        arg: Expr[DFValOf[T]]
    )(dfc: Expr[DFC])(using Quotes, Type[T]): Expr[Option[Seq[DFValOf[T]]]] =
      import quotes.reflect.*
      val parts = sc.parts
      val partsStr = parts.map(_.value.get).toList
      val op = sc.funcName
      val opExpr = Expr(op)
      if (parts.length > 1)
        '{
          compiletime.error(
            "Extractors for decimal string interpolation are not allowed."
          )
          Some(Seq())
        }
      else
        val dfVal = partsStr.head match
          case widthValuePattern(widthStr, wordStr) =>
            Literal(StringConstant(wordStr)).interpolate(
              opExpr,
              '{ Some(${ Expr(widthStr.toInt) }) }
            )(dfc)
          case _ => parts.head.asTerm.interpolate(opExpr, '{ None })(dfc)
        val dfValType = dfVal.asTerm.tpe.asTypeOf[DFConstAny]
        '{
          val tc = compiletime.summonInline[
            DFVal.Compare[T, dfValType.Underlying, FuncOp.===.type, false]
          ]
          Some(
            Seq(
              trydf(
                tc.conv(${ arg }.dfType, $dfVal)(using $dfc)
              )(using $dfc, CTName($opExpr))
            )
          )
        }
      end if
    end unapplySeqMacro
  end StrInterpOps

  object Val:
    // runtime checks for applying a value to a fixed-point receiver: implicit application
    // is never lossy — fraction/magnitude widths may only grow
    private def checkFixApply(target: ir.DFDecimal, rhs: ir.DFDecimal)(using dfc: DFC): Unit =
      import dfc.getSet
      if (!target.signed && rhs.signed)
        throw new IllegalArgumentException(
          "Cannot apply a signed value to an unsigned fixed-point receiver.\nAn explicit conversion must be applied."
        )
      if (target.fractionWidth < rhs.fractionWidth)
        throw new IllegalArgumentException(
          s"The applied value's fraction width (${rhs.fractionWidth}) is larger than the fixed-point receiver's fraction width (${target.fractionWidth}) and would lose precision.\nAn explicit conversion must be applied."
        )
      (target.widthIntOpt, rhs.widthIntOpt) match
        case (Some(lw), Some(rw)) =>
          val lm = lw - target.fractionWidth
          val rm = rw - rhs.fractionWidth + (if (target.signed && !rhs.signed) 1 else 0)
          if (lm < rm)
            throw new IllegalArgumentException(
              s"The applied value's magnitude width ($rm) is larger than the fixed-point receiver's magnitude width ($lm)."
            )
        case _ =>
    end checkFixApply
    // exact (lossless) conversion of a decimal value to a fixed-point receiver format,
    // composed from the fixed-point primitives that mirror the integer ones: a `.signed`/
    // `.unsigned` sign cast (adjusts the magnitude by the sign bit) followed by a `resize` to
    // the target magnitude and fraction (which realizes the binary-point alignment).
    private[core] def fixConv[LS <: Boolean, LW <: IntP, LF <: Int, P](
        dfType: DFDecimal[LS, LW, LF, BitAccurate],
        rhs: DFValAny
    )(using dfc: DFC): DFValTP[DFDecimal[LS, LW, LF, BitAccurate], P] =
      import dfc.getSet
      import DFUInt.Val.Ops.signed
      import DFSInt.Val.Ops.unsigned
      val targetIR = dfType.asIR
      val rhsIR = rhs.dfType.asIR.asInstanceOf[ir.DFDecimal]
      if (targetIR =~ rhsIR) rhs.asValTP[DFDecimal[LS, LW, LF, BitAccurate], P]
      else
        given dfcAnon: DFC = dfc.anonymize
        // sign cast: `.signed` (UFix[M,F] -> SFix[M+1,F]) / `.unsigned` (SFix[M,F] -> UFix[M-1,F])
        val signFix: DFValAny =
          if (targetIR.signed && !rhsIR.signed) rhs.asValOf[DFUFix[Int, Int]].signed
          else if (!targetIR.signed && rhsIR.signed) rhs.asValOf[DFSFix[Int, Int]].unsigned
          else rhs
        // resize to the target format (magnitude + fraction), unless already there
        val signFixIR = signFix.dfType.asIR.asInstanceOf[ir.DFDecimal]
        val resized: DFValAny =
          if (
            signFixIR.magnitudeWidthParamRef =~ targetIR.magnitudeWidthParamRef &&
            signFixIR.fractionWidth == targetIR.fractionWidth
          ) signFix
          else DFVal.Alias.AsIs(dfType, signFix)
        resized.asValTP[DFDecimal[LS, LW, LF, BitAccurate], P]
      end if
    end fixConv
    // exact minimal fixed-point representation of a Double, aligned to the target format.
    // Every finite Double is a binary rational, so the conversion either fits exactly or
    // errors — it never rounds.
    private def doubleToFixRaw(value: Double, target: ir.DFDecimal)(using dfc: DFC): BigInt =
      import dfc.getSet
      if (value.isNaN || value.isInfinity)
        throw new IllegalArgumentException(
          s"Cannot apply a non-finite Double value ($value) to a fixed-point receiver."
        )
      if (!target.signed && value < 0)
        throw new IllegalArgumentException(
          "Cannot apply a negative value to an unsigned fixed-point receiver."
        )
      var scaled = BigDecimal.exact(value)
      var fractionWidth = 0
      while (!scaled.isWhole && fractionWidth < target.fractionWidth)
        scaled *= 2
        fractionWidth += 1
      if (!scaled.isWhole)
        throw new IllegalArgumentException(
          s"The Double value $value requires a fraction width larger than the fixed-point receiver's fraction width (${target.fractionWidth}).\nUse an explicit `M.F'` formatted literal to opt into rounding."
        )
      val raw = scaled.toBigInt << (target.fractionWidth - fractionWidth)
      target.widthIntOpt.foreach { w =>
        if (raw.bitsWidth(target.signed) > w)
          throw new IllegalArgumentException(
            s"The Double value $value requires a magnitude width of at least ${raw.bitsWidth(target.signed) -
                target.fractionWidth}, but the fixed-point receiver's magnitude width is ${w -
                target.fractionWidth}."
          )
      }
      raw
    end doubleToFixRaw
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int, NativeType],
          dfVal: DFValOf[DFDecimal[Boolean, Int, Int, NativeType]]
      )(using DFC): DFValOf[DFDecimal[Boolean, Int, Int, NativeType]] =
        (dfType.widthIntOpt, dfVal.widthIntOpt) match
          case (Some(lw), Some(rw)) => `LW >= RW`(lw, rw)
          case _                    =>
        `LS >= RS`(dfType.signed, dfVal.dfType.signed)
        dfVal
      // fixed-point receiver (LF != 0) accepting any bit-accurate decimal value —
      // including DFXInt values (RF == 0) — as long as the conversion is lossless
      given DFFixTC[
          LS <: Boolean,
          LW <: IntP,
          LF <: Int,
          RS <: Boolean,
          RW <: IntP,
          RF <: Int,
          RP,
          R <: DFValTP[DFDecimal[RS, RW, RF, BitAccurate], RP]
      ](using
          scala.util.NotGiven[LF =:= 0]
      ): DFVal.TC[DFDecimal[LS, LW, LF, BitAccurate], R] with
        type OutP = RP
        def conv(dfType: DFDecimal[LS, LW, LF, BitAccurate], value: R)(using dfc: DFC): Out =
          checkFixApply(dfType.asIR, value.dfType.asIR)
          fixConv[LS, LW, LF, RP](dfType, value)
      end DFFixTC
      // Double is the fixed-point wildcard literal: it adapts exactly to the receiver's
      // format or errors (it never rounds)
      given DFFixTCFromDouble[
          LS <: Boolean,
          LW <: IntP,
          LF <: Int,
          R <: Double
      ](using
          scala.util.NotGiven[LF =:= 0]
      ): DFVal.TC[DFDecimal[LS, LW, LF, BitAccurate], R] with
        type OutP = CONST
        def conv(dfType: DFDecimal[LS, LW, LF, BitAccurate], value: R)(using dfc: DFC): Out =
          val raw = doubleToFixRaw(value, dfType.asIR)
          DFVal.Const(dfType, Some(raw), named = true)
      end DFFixTCFromDouble
    end TC
    object TCConv:
      export DFXInt.Val.TCConv.given
    object Compare:
      export DFXInt.Val.Compare.given
    object Ops:
      export DFXInt.Val.Ops.*
  end Val
end DFDecimal

type DFXInt[S <: Boolean, W <: IntP, N <: NativeType] = DFDecimal[S, W, 0, N]
object DFXInt:
  def apply[S <: Boolean, W <: IntP, N <: NativeType & Singleton](
      signed: Inlined[S],
      width: IntParam[W],
      nativeType: N
  )(using DFC, Width.CheckNUB[S, W]): DFXInt[S, W, N] = DFDecimal(signed, width, 0, nativeType)

  object Val:
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutS <: Boolean
      type OutW <: IntP
      type OutN <: NativeType
      type OutP
      type Out = DFValTP[DFXInt[OutS, OutW, OutN], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(arg: R)(using DFC): Out
    trait CandidateLP:
      given fromDFBitsValCandidate[R, W <: IntP, P](using
          ic: DFBits.Val.Candidate.Aux[R, W, P]
      ): Candidate[R] with
        type OutS = false
        type OutW = W
        type OutN = BitAccurate
        type OutP = P
        def apply(arg: R)(using dfc: DFC): Out =
          import DFBits.Val.Ops.uint
          val dfVal = ic(arg)(using dfc.anonymize)
          val ret =
            if (dfVal.hasTag[ir.ResizeTag])
              dfVal.uint.tag(ir.ResizeTag)
            else dfVal.uint
          ret.asValTP[DFXInt[OutS, OutW, OutN], OutP]
      end fromDFBitsValCandidate
    end CandidateLP
    object Candidate extends CandidateLP:
      type Exact = Exact0[DFC, Candidate]
      type ExactAux[R] = Exact0[DFC, Candidate] {
        type ExactFrom = R
      }
      type Aux[R, S <: Boolean, W <: IntP, N <: NativeType, P] =
        Candidate[R] {
          type OutS = S
          type OutW = W
          type OutN = N
          type OutP = P
        }
      given fromInt[R <: Int, OS <: Boolean, OW <: Int](using
          info: IntInfo.Aux[R, OS, OW]
      ): Candidate[R] with
        type OutS = OS
        type OutW = OW
        type OutN = Int32
        type OutP = CONST
        def apply(arg: R)(using dfc: DFC): Out =
          val dfType = DFXInt(info.signed(arg), info.width(arg), BitAccurate)
          DFVal.Const(dfType, Some(BigInt(arg)), named = true)(using
            dfc.tag(ir.ImplicitlyFromIntTag)
          ).asInstanceOf[Out]
      // DFInt32 acts as a wildcard in operations: it adapts to the
      // bit-accurate value's sign and width. OutN = Int32 (true) signals wildcard status.
      given fromDFConstInt32[P, R <: DFValTP[DFInt32, P]]: Candidate[R] with
        type OutS = Boolean
        type OutW = Int
        type OutN = Int32
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg
      given fromDFXIntVal[S <: Boolean, W <: IntP, N <: NativeType, P, R <: DFValTP[
        DFXInt[S, W, N],
        P
      ]]: Candidate[R] with
        type OutS = S
        type OutW = W
        type OutN = N
        type OutP = P
        def apply(arg: R)(using DFC): Out = arg
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a DFHDL decimal variable."
        )
      given fromIf[
          C <: DFValOf[DFBoolOrBit],
          T,
          F,
          TS <: Boolean,
          TW <: IntP,
          TN <: NativeType,
          TP,
          FP,
          R <: IfWrapper[C, T, F]
      ](using
          tTC: Candidate[T] { type OutS = TS; type OutW = TW; type OutN = TN; type OutP = TP },
          fTC: DFVal.TC[DFXInt[TS, TW, TN], F] { type OutP = FP }
      ): Candidate[R] with
        type OutS = TS
        type OutW = TW
        type OutN = TN
        type OutP = TP | FP
        def apply(value: R)(using DFC): Out = value.unwrap
      end fromIf
    end Candidate

    extension [S <: Boolean, W <: IntP, N <: NativeType](dfVal: DFValOf[DFXInt[S, W, N]])
      private[core] def getActualSignedWidthOpt(using
          dfc: DFC
      ): Option[(signed: Boolean, widthIntOpt: Option[Int])] =
        if (dfVal.dfType.asIR.isDFInt32)
          import dfc.getSet
          dfVal.asIR.injectGlobalCtx()
          dfVal.asIR.getConstData[Option[BigInt]] match
            case ir.ConstData.KnownConst(Some(n: BigInt)) =>
              val int = n.toInt
              Some(int < 0, Some(IntInfo.calcWidth(int)))
            case _ => None
        else
          Some(dfVal.dfType.signed.value, dfVal.widthIntOpt)
      end getActualSignedWidthOpt
    end extension

    object TC:
      def apply(
          dfType: DFXInt[Boolean, Int, NativeType],
          dfVal: DFValOf[DFXInt[Boolean, Int, NativeType]]
      )(using DFC): DFValOf[DFXInt[Boolean, Int, NativeType]] =
        val check = summon[TCCheck[Boolean, Int, Boolean, Int]]
        (dfType.widthIntOpt, dfVal.widthIntOpt) match
          case (Some(dfTypeW), Some(dfValW)) =>
            check(dfType.signed, dfTypeW, dfVal.dfType.signed, dfValW)
          case _ =>
        dfVal
      end apply
      import DFVal.TC
      given [LS <: Boolean, LW <: IntP, LN <: NativeType, R, RP, IC <: Candidate[R]](using
          ic: IC { type OutP = RP }
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW],
          nativeCheck: NativeCheck[LN, ic.OutN]
      ): DFVal.TC[DFXInt[LS, LW, LN], R] with
        type OutP = RP
        def conv(dfType: DFXInt[LS, LW, LN], value: R)(using dfc: DFC): Out =
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          rhs.getActualSignedWidthOpt match
            case Some(rhsSigned, rhsWidthOpt) =>
              if (!rhs.hasTag[ir.ResizeTag] || dfType.signed != rhsSigned)
                (dfType.widthIntOpt, rhsWidthOpt) match
                  case (Some(dfTypeW), Some(rhsW)) => check(dfType.signed, dfTypeW, rhsSigned, rhsW)
                  case _                           =>
                    import dfc.getSet
                    if (
                      !dfType.asIR.isDFInt32 && !rhs.dfType.asIR.isDFInt32 &&
                      !DFXInt.Val.Ops.hasImplicitlyFromIntTag(rhs.asIR)
                    )
                      // integer operands (fraction 0): the magnitude ref is the total-width
                      // ref and may be parametric
                      val dfTypeWidthRef = dfType.asIR.magnitudeWidthParamRef
                      val rhsWidthRef = rhs.dfType.asIR.magnitudeWidthParamRef
                      def dfTypeWidthStr = dfTypeWidthRef.refErrorString
                      def rhsWidthStr = rhsWidthRef.refErrorString
                      // width-fit acceptance rule: LHS >= RHS after symbolic elimination, so a
                      // mixed max/min drops its symbolic operands (`16 >= WIDTH max 16` decides
                      // as `16 >= 16`); a residual plain-symbol comparison stays undecidable
                      // and is conservatively rejected below
                      dfTypeWidthRef.compare(rhsWidthRef, elimSymbolicMaxMin = true)(_ >= _) match
                        case Some(false) =>
                          throw new IllegalArgumentException(
                            s"""The applied RHS value width ($rhsWidthStr) is larger than the LHS variable width ($dfTypeWidthStr)."""
                          )
                        case None =>
                          throw new IllegalArgumentException(
                            s"""The applied RHS value width ($rhsWidthStr) is undefined compared to the LHS variable width ($dfTypeWidthStr)."""
                          )
                        case _ => // ok
                    end if
            case None =>
          end match
          // a widened cone lands exactly at the target type with the original func's
          // (anonymous) meta, so a named-val binding must be applied here, like the
          // DFBits TC does (with an anonymous or positionally-foreign DFC this no-ops)
          DFXInt.Val.Ops.toDFXIntOf(rhs)(dfType).nameInDFCPosition.asValTP[DFXInt[LS, LW, LN], RP]
        end conv
      end given
    end TC

    object TCConv:
      given DFXIntFromCandidateConv[LS <: Boolean, R, RP, IC <: Candidate[R]](using
          ic: IC { type OutP = RP }
      )(using
          checkS: `LS >= RS`.Check[LS, ic.OutS],
          lsigned: OptionalGiven[ValueOf[LS]]
      ): DFVal.TCConv[DFXInt[LS, Int, BitAccurate], R] with
        type OutP = RP
        def apply(value: R)(using dfc: DFC): Out =
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          checkS(lsigned.get.value, rhs.dfType.signed)
          if (lsigned.get.value != rhs.dfType.signed.value)
            rhs.asValOf[DFUInt[Int]].signed.asValTP[DFXInt[LS, Int, BitAccurate], RP]
          else rhs.asValTP[DFXInt[LS, Int, BitAccurate], RP]
    end TCConv

    object Compare:
      import DFVal.Compare
      given DFXIntCompare[
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          R,
          RP,
          IC <: Candidate[R],
          Op <: FuncOp,
          C <: Boolean
      ](using
          ic: IC { type OutP = RP }
      )(using
          check: CompareCheck[LS, LW, ic.OutS, ic.OutW, ic.OutN, C],
          nativeCheck: NativeCheck[LN, ic.OutN]
      ): Compare[DFXInt[LS, LW, LN], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFXInt[LS, LW, LN], arg: R)(using dfc: DFC): Out =
          given dfcAnon: DFC = dfc.anonymize
          val dfValArg = ic(arg)
          dfValArg.getActualSignedWidthOpt match
            case Some(rhsSigned, rhsWidthOpt) =>
              (dfType.widthIntOpt, rhsWidthOpt) match
                case (Some(dfTypeW), Some(rhsW)) => check(dfType.signed, dfTypeW, rhsSigned, rhsW)
                case _                           =>
            case None =>
          DFXInt.Val.Ops.toDFXIntOf(dfValArg)(dfType).asValTP[DFXInt[LS, LW, LN], RP]
        end conv
        // Check Verilog-semantics mismatch for comparisons: same trigger as
        // `/`, `%` -- a narrow non-carry chain mixed with an implicit Int on
        // either side widens to 32-bit in Verilog but not in DFHDL.
        override def apply[P](dfVal: DFValTP[DFXInt[LS, LW, LN], P], arg: R)(using
            dfc: DFC,
            opv: ValueOf[Op],
            cv: ValueOf[C]
        ): DFValTP[DFBool, P | RP] = trydf:
          val dfValArg = conv(dfVal.dfType, arg)(using dfc.anonymize)
          import dfc.getSet
          val op = opv.value
          op match
            case FuncOp.=== | FuncOp.=!= | FuncOp.< | FuncOp.> | FuncOp.<= | FuncOp.>= =>
              if DFXInt.Val.Ops.shouldWarnVerilogSemantics(dfVal.asIR, dfValArg.asIR)
              then
                dfc.logEvent(
                  DFWarning(op.toString, DFXInt.Val.Ops.verilogSemanticsWarnMsg)
                )
            case _ =>
          func(dfVal, dfValArg)
        end apply
      end DFXIntCompare
    end Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      import DFBits.{BitIndex, BitsHiLo}
      import IntP.{-, +}
      given evOpApplyDFXInt[
          S <: Boolean,
          W <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFXInt[S, W, BitAccurate], Modifier[A, C, I, P]],
          R
      ](using
          ub: DFUInt.Val.UBArg[W, R]
      ): ExactOp2Aux["apply", DFC, DFValAny, L, R, DFVal[DFBit, Modifier[A, C, Any, P]]] =
        new ExactOp2["apply", DFC, DFValAny, L, R]:
          type Out = DFVal[DFBit, Modifier[A, C, Any, P]]
          def apply(lhs: L, idx: R)(using DFC): Out = trydf {
            DFVal.Alias.ApplyIdx(DFBit, lhs, ub(lhs.widthIntParam, idx)(using dfc.anonymize))
          }(using dfc, CTName("bit selection (apply)"))
      end evOpApplyDFXInt
      given evOpApplyRangeDFXInt[
          S <: Boolean,
          W <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFXInt[S, W, BitAccurate], Modifier[A, C, I, P]],
          HI <: IntP,
          LO <: IntP
      ](using
          checkHigh: BitIndex.CheckNUB[HI, W],
          checkLow: BitIndex.CheckNUB[LO, W],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, HI, LO, DFVal[
        DFUInt[IntP.RangeWidth[HI, LO]],
        Modifier[A, C, Any, P]
      ]] =
        new ExactOp3["apply", DFC, DFValAny, L, HI, LO]:
          type Out = DFVal[DFUInt[IntP.RangeWidth[HI, LO]], Modifier[A, C, Any, P]]
          def apply(lhs: L, idxHigh: HI, idxLow: LO)(using DFC): Out = trydf {
            val idxHighParam = IntParam(idxHigh)
            val idxLowParam = IntParam(idxLow)
            val idxHighIntOpt = idxHighParam.toScalaIntOpt
            val idxLowIntOpt = idxLowParam.toScalaIntOpt
            val widthIntOpt = lhs.widthIntOpt
            (idxHighIntOpt, widthIntOpt) match
              case (Some(idxHighInt), Some(widthInt)) => checkHigh(idxHighInt, widthInt)
              case _                                  =>
            (idxLowIntOpt, widthIntOpt) match
              case (Some(idxLowInt), Some(widthInt)) => checkLow(idxLowInt, widthInt)
              case _                                 =>
            (idxHighIntOpt, idxLowIntOpt) match
              case (Some(idxHighInt), Some(idxLowInt)) => checkHiLo(idxHighInt, idxLowInt)
              case _                                   =>
            DFVal.Alias.ApplyRange.applyDFXInt(lhs, idxHighParam, idxLowParam)
          }(using dfc, CTName("bit range selection (apply)"))
      end evOpApplyRangeDFXInt
      given evOpShiftOrPowerInt[
          Op <: FuncOp.>>.type | FuncOp.<<.type | FuncOp.**.type,
          L <: Int,
          RP,
          R <: DFValTP[DFInt32, RP]
      ](using
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFInt32, RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFInt32, RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            DFVal.Func(DFInt32, op.value, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, RP]
          }
      end evOpShiftOrPowerInt
      given evOpLogicUInt[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          LW <: IntP,
          LP,
          RW <: IntP,
          RP,
          L <: DFValTP[DFUInt[LW], LP],
          R <: DFValTP[DFUInt[RW], RP]
      ](using
          op: ValueOf[Op]
      )(using
          check: `LW == RW`.CheckNUB[LW, RW]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFUInt[LW], LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFUInt[LW], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            (lhs.widthIntOpt, rhs.widthIntOpt) match
              case (Some(lw), Some(rw)) => check(lw, rw)
              case _                    =>
            DFVal.Func(lhs.dfType, op.value, List(lhs, rhs))
          }
      end evOpLogicUInt

      export dfhdl.internals.clog2
      def clog2[P, S <: Boolean, W <: IntP, N <: NativeType](
          dfVal: DFValTP[DFXInt[S, W, N], P]
      )(using
          DFCG,
          DFVal.ConstCheck[P]
      ): DFValTP[DFXInt[S, W, N], P] =
        DFVal.Func(dfVal.dfType, FuncOp.clog2, List(dfVal))
      // TODO: generate error for unsigned values
      def abs[P, S <: Boolean, W <: IntP, N <: NativeType](
          dfVal: DFValTP[DFXInt[S, W, N], P]
      )(using
          DFCG
      ): DFValTP[DFXInt[S, W, N], P] =
        DFVal.Func(dfVal.dfType, FuncOp.abs, List(dfVal))
      extension [P, S <: Boolean, W <: IntP, N <: NativeType](lhs: DFValTP[DFXInt[S, W, N], P])
        protected[core] def toDFXIntOf[RS <: Boolean, RW <: IntP, RN <: NativeType](
            dfType: DFXInt[RS, RW, RN]
        )(using dfc: DFC): DFValTP[DFXInt[RS, RW, RN], P] =
          import dfc.getSet
          val dfValIR =
            if (dfType.asIR.isDFInt32 && lhs.dfType.asIR.isDFInt32) lhs.asIR
            else
              // Deep target-context widening, matching Verilog's assignment-context width
              // propagation: an anonymous non-carry +/-/* cone converted to a WIDER type
              // is re-evaluated at the target's width and sign. Every func in the cone is
              // retyped to the target and every leaf is converted to it (recursively, via
              // toDFXIntOf on each argument), so all intermediates evaluate at the target
              // width. Truncation to the target width commutes with +/-/*, so this is the
              // unique evaluation that agrees with Verilog for every input; in particular
              // a sign conversion is applied to the OPERANDS, never to a narrower result
              // (zero-extending a wrapped subtraction result flips its sign).
              // The candidate is taken BEFORE any sign conversion: an upstream anonymous
              // sign-conversion alias (the commutative-arith sign alignment creates one)
              // is unwrapped, or it would hide the func and pin the chain at its narrow
              // width. A carry func (result wider than its operands) keeps its documented
              // exact semantics and converts as a leaf; so do all non-arithmetic ops
              // (shifts, selections), whose evaluation this rule does not context-widen.
              val signFixNeeded =
                !lhs.dfType.asIR.isDFInt32 && dfType.signed && !lhs.dfType.signed
              val candidateIR = signConversionRelVal(lhs.asIR).getOrElse(lhs.asIR)

              // symbolic elimination keeps this consistent with the width-fit acceptance
              // rule of the TC conversion: `16 > WIDTH max 16` decides as `16 > 16` (no
              // widening), so the anonymous form resolves exactly like a named
              // intermediate value; if still undecidable, optimistically assume the
              // target is wider.
              def contextWidenCheck(funcWidth: IntParam[Int]): Boolean =
                dfType.asFE[DFSInt[Int]]
                  .compareWidths(DFXInt(true, funcWidth, BitAccurate), elimSymbolicMaxMin = true)(
                    _ > _
                  )
                  .getOrElse(true)

              val lhsConverted: DFValOf[DFSInt[Int]] = candidateIR match
                case func @ ir.DFVal.Func(
                      dfType = ir.DFUInt(_) | ir.DFSInt(_),
                      op = FuncOp.+ | FuncOp.- | FuncOp.*
                    )
                    if func.isAnonymous && {
                      // non-carry (modular) func: its type equals its aligned operands'
                      func.dfType =~ func.args.head.get.dfType &&
                      contextWidenCheck(func.asValOf[DFSInt[Int]].widthIntParam)
                    } =>
                  // The widened Func is BUILT FRESH rather than revised in place (an
                  // anonymous member is never revised; issue #449); the original cone
                  // becomes debris for the end-of-design sweep. The spelling of the result
                  // (a carry op or explicit operand widenings) is purely a PRINTING
                  // decision, reconstructed from this shape by the CarryFunc/Eby
                  // extractors. The widened evaluation type is the target itself as a
                  // bit-accurate type; an Int target widens the cone at its native 32-bit
                  // width (Verilog's `integer` context) and converts below.
                  val newDT = dfType.asIR.asInstanceOf[ir.DFDecimal].copy(
                    magnitudeWidthParamRef = dfType.widthIntParam.ref,
                    nativeType = BitAccurate
                  )
                  if (dfc.inMetaProgramming)
                    // no MutableDB revision under meta-programming (matching `setMember`'s
                    // behavior there): the retyped value is returned unregistered and the
                    // argument conversions are skipped, since no member is registered
                    func.updateDFType(newDT).asValOf[DFSInt[Int]]
                  else
                    val widenedArgs = func.args.map { argRef =>
                      DFXInt.Val.Ops.toDFXIntOf(
                        argRef.get.asValOf[DFXInt[Boolean, Int, NativeType]]
                      )(DFXInt(dfType.signed, dfType.widthIntParam, BitAccurate))(using
                        dfc.anonymize
                      )
                    }
                    ir.DFVal.Func(
                      newDT,
                      func.op,
                      widenedArgs.map(_.asIR.refTW[ir.DFVal](knownReachable = true)),
                      dfc.ownerOrEmptyRef,
                      func.meta,
                      func.tags
                    ).addMember.asValOf[DFSInt[Int]]
                  end if
                case _ =>
                  // Fold stacked widenings: an anonymous same-kind widening resize alias
                  // is transparent to a further conversion (both are value-preserving
                  // extensions), so when the width fix below would resize anyway, it
                  // applies to the alias's base directly instead of stacking.
                  def unstack(v: ir.DFVal): ir.DFVal = v match
                    case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
                      val relVal = alias.relValRef.get
                      val widening = (alias.dfType, relVal.dfType) match
                        case (ir.DFUInt(toW), ir.DFUInt(fromW)) =>
                          toW.compare(fromW)(_ > _).getOrElse(false)
                        case (ir.DFSInt(toW), ir.DFSInt(fromW)) =>
                          toW.compare(fromW)(_ > _).getOrElse(false)
                        case _ => false
                      if (widening) unstack(relVal) else v
                    case _ => v
                  val widthChanges = !dfType.asIR.magnitudeWidthParamRef
                    .isSimilarTo(lhs.dfType.asIR.magnitudeWidthParamRef)
                  val base =
                    if (widthChanges) unstack(lhs.asIR).asValOf[DFSInt[Int]]
                    else lhs.asValOf[DFSInt[Int]]
                  // no widening: apply the plain sign fix when the target requires it
                  if (signFixNeeded) base.asValOf[DFUInt[Int]].signed.asValOf[DFSInt[Int]]
                  else base
              end lhsConverted
              val nativeTypeChanged = dfType.nativeType != lhsConverted.dfType.nativeType
              if (nativeTypeChanged) dfType.asIR.nativeType match
                case Int32 =>
                  lhsConverted.toInt.asIR
                case BitAccurate =>
                  DFVal.Alias.AsIs(dfType, lhsConverted)(using
                    dfc.tag(ir.ImplicitlyFromIntTag)
                  ).asIR
              else if (
                // integer operands (fraction 0): the magnitude ref is the total-width ref
                !dfType.asIR.magnitudeWidthParamRef
                  .isSimilarTo(lhsConverted.dfType.asIR.magnitudeWidthParamRef)
              )
                lhsConverted.resize(dfType.widthIntParam).asIR
              else lhsConverted.asIR
              end if
            end if
          end dfValIR
          dfValIR.asValTP[DFXInt[RS, RW, RN], P]
        end toDFXIntOf
        @dfhdl.hw.annotation.pure(true, "*")
        def toScalaInt(using DFC, DFVal.ConstCheck[P]): Int =
          lhs.toScalaValue.toInt
        @dfhdl.hw.annotation.pure(true, "*")
        def toScalaBigInt(using DFC, DFVal.ConstCheck[P]): BigInt =
          lhs.toScalaValue
      end extension
      extension [S <: Boolean, W <: IntP, N <: NativeType, P](lhs: DFValTP[DFXInt[S, W, N], P])
        @targetName("resizeDFXIntAuto")
        def resize(using DFCG): DFValTP[DFXInt[S, Int, N], P] =
          lhs.tag(ir.ResizeTag).asValTP[DFXInt[S, Int, N], P]
        @targetName("resizeDFXInt")
        def resize[RW <: IntP](
            updatedWidth: IntParam[RW]
        )(using
            dfc: DFCG,
            check: Width.CheckNUB[S, RW]
        ): DFValTP[DFXInt[S, RW, BitAccurate], P] = trydf {
          val signed = lhs.dfType.signed
          updatedWidth.toScalaIntOpt.foreach(check(signed, _))
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFXInt[S, RW, BitAccurate]]
//          else
          DFVal.Alias.AsIs(DFXInt(signed, updatedWidth, BitAccurate), lhs)
        }
        end resize
        // extend-by: a RELATIVE widening by `delta` bits (zero-extension for unsigned,
        // sign-extension for signed), sugar over `.resize(width + delta)`; printed back in
        // this relative form whenever the width delta folds to a literal
        @targetName("ebyDFXInt")
        def eby[RK <: IntP](
            delta: IntParam[RK]
        )(using
            dfc: DFCG,
            check: Arg.Positive.CheckNUB[RK]
        ): DFValTP[DFXInt[S, IntP.ExtendByWidth[W, RK], BitAccurate], P] = trydf {
          delta.toScalaIntOpt.foreach(check(_))
          import IntParam.+
          DFVal.Alias.AsIs(
            DFXInt(lhs.dfType.signed, lhs.dfType.widthIntParam + delta, BitAccurate),
            lhs
          ).asValTP[DFXInt[S, IntP.ExtendByWidth[W, RK], BitAccurate], P]
        }
        end eby
      end extension

      private[core] val verilogSemanticsWarnMsg =
        """|Implicit Scala/DFHDL Int conversion may produce different results than Verilog.
           |In Verilog, integer literals are 32-bit, which can widen intermediate arithmetic.
           |In DFHDL, Int literals are converted to minimum bit-accurate width.
           |Use carry operations (+^, -^, *^) or explicit bit-accurate literals (d"W'V").""".stripMargin

      // Check if a value is tagged with ImplicitlyFromIntTag. An implicit `Int` operand
      // adapted to a parametric width keeps its tagged const under a resize alias (the
      // fold into a single const happens only for literal widths), so the check follows
      // alias chains down to the underlying value.
      private[core] def hasImplicitlyFromIntTag(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean =
        dfVal.tags.hasTagOf[ir.ImplicitlyFromIntTag] ||
          (dfVal match
            case alias: ir.DFVal.Alias => hasImplicitlyFromIntTag(alias.relValRef.get)
            case _                     => false)

      // A width reference resolved through design parameters: this runs during
      // elaboration, where a parameter's applied (or default) value is known, so a
      // parametric width like `CORDW + 1` resolves to its actual value.
      private def resolvedWidthOf(ref: ir.IntParamRef)(using
          getSet: ir.MemberGetSet
      ): Option[Int] =
        ref.getIntConstData(using
          getSet,
          ir.ConstData.CachePolicy.GoThroughDesignParams
        ) match
          case ir.ConstData.KnownConst(w) => Some(w)
          case _                          => None

      // A value's width classified as narrow (< 32 bits). A width that cannot be
      // resolved counts as narrow: a false-positive warning costs one carry op, while a
      // false negative is silently wrong hardware.
      private def resolvedWidthIsNarrow(dfVal: ir.DFVal)(using ir.MemberGetSet): Boolean =
        dfVal.dfType match
          case dec: ir.DFDecimal =>
            resolvedWidthOf(dec.magnitudeWidthParamRef) match
              case Some(m) => m + dec.fractionWidth < 32
              case None    => true
          case _ =>
            dfVal.dfType.widthIntOpt.map(_ < 32).getOrElse(true)

      // An anonymous sign-conversion alias: an unsigned value reinterpreted as signed
      // with exactly one extra bit (`.signed`). The Verilog backend emits it as
      // `$signed({1'b0, ...})`, whose concatenation operand is self-determined, so a
      // narrow chain stays narrow through it and the promotion/warning machinery must
      // look through it. An equal-width alias is a reinterpret cast and never matches.
      private def signConversionRelVal(dfVal: ir.DFVal)(using
          ir.MemberGetSet
      ): Option[ir.DFVal] =
        dfVal match
          case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
            alias.dfType match
              case ir.DFSInt(aliasWidthRef) =>
                val relVal = alias.relValRef.get
                relVal.dfType match
                  case ir.DFUInt(relWidthRef) =>
                    (resolvedWidthOf(aliasWidthRef), resolvedWidthOf(relWidthRef)) match
                      case (Some(aw), Some(rw)) if aw == rw + 1 => Some(relVal)
                      case _                                    => None
                  case _ => None
              case _ => None
          case _ => None

      // Check if an anonymous sub-tree contains non-carry +/-/* with width < 32.
      private[core] def containsNarrowNonCarryArith(
          dfVal: ir.DFVal
      )(using ir.MemberGetSet): Boolean =
        dfVal match
          case func: ir.DFVal.Func if func.isAnonymous =>
            func.op match
              case FuncOp.+ | FuncOp.- | FuncOp.* =>
                // carry-ness is a SHAPE property now (operand-widened funcs, see CarryFunc)
                val isNonCarry =
                  dfhdl.compiler.analysis.CarryFunc.unapply(func).isEmpty
                val isNarrowNonCarry = isNonCarry && resolvedWidthIsNarrow(func)
                isNarrowNonCarry ||
                func.args.exists(ref => containsNarrowNonCarryArith(ref.get))
              case _ =>
                func.args.exists(ref => containsNarrowNonCarryArith(ref.get))
          case _ =>
            signConversionRelVal(dfVal) match
              case Some(relVal) => containsNarrowNonCarryArith(relVal)
              case None         =>
                dfVal match
                  case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
                    dfhdl.compiler.analysis.Eby.unapply(alias) match
                      case Some(relVal, _) => containsNarrowNonCarryArith(relVal)
                      case None            => false
                  case _ => false

      // Check if an anonymous sub-tree contains narrow non-carry arith that
      // also has an ImplicitlyFromIntTag operand (Verilog "Forcing Larger
      // Evaluation" pattern).
      private[core] def containsNarrowNonCarryArithWithTaggedOperand(
          dfVal: ir.DFVal
      )(using ir.MemberGetSet): Boolean =
        dfVal match
          case func: ir.DFVal.Func if func.isAnonymous =>
            func.op match
              case FuncOp.+ | FuncOp.- | FuncOp.* =>
                val isNonCarry =
                  dfhdl.compiler.analysis.CarryFunc.unapply(func).isEmpty
                val isNarrowNonCarry = isNonCarry && resolvedWidthIsNarrow(func)
                (isNarrowNonCarry && func.args.exists(ref => hasImplicitlyFromIntTag(ref.get))) ||
                func.args.exists(ref =>
                  containsNarrowNonCarryArithWithTaggedOperand(ref.get)
                )
              case _ =>
                func.args.exists(ref =>
                  containsNarrowNonCarryArithWithTaggedOperand(ref.get)
                )
          case _ =>
            signConversionRelVal(dfVal) match
              case Some(relVal) => containsNarrowNonCarryArithWithTaggedOperand(relVal)
              case None         =>
                dfVal match
                  case alias: ir.DFVal.Alias.AsIs if alias.isAnonymous =>
                    dfhdl.compiler.analysis.Eby.unapply(alias) match
                      case Some(relVal, _) =>
                        containsNarrowNonCarryArithWithTaggedOperand(relVal)
                      case None => false
                  case _ => false

      // Unified Verilog-semantics warning trigger shared by `/`, `%` (arithOp)
      // and comparison operations (DFXIntCompare). Warns when a narrow non-carry
      // chain mixes with a tagged-from-Int operand on either side - directly OR
      // nested inside the chain.
      private[core] def shouldWarnVerilogSemantics(
          lhs: ir.DFVal,
          rhs: ir.DFVal
      )(using ir.MemberGetSet): Boolean =
        (hasImplicitlyFromIntTag(rhs) && containsNarrowNonCarryArith(lhs)) ||
          (hasImplicitlyFromIntTag(lhs) && containsNarrowNonCarryArith(rhs)) ||
          containsNarrowNonCarryArithWithTaggedOperand(lhs) ||
          containsNarrowNonCarryArithWithTaggedOperand(rhs)

      // Check that a wildcard `Int` value fits in the bit-accurate value's type.
      // Produces an elaboration error if it doesn't.
      private def checkWildcardFit(
          wildcard: DFValOf[DFInt32],
          bitAccurateType: DFTypeAny
      )(using dfc: DFC): Unit =
        val baType = bitAccurateType.asIR.asInstanceOf[ir.DFDecimal]
        import dfc.getSet
        import DFXInt.Val.getActualSignedWidthOpt
        wildcard.getActualSignedWidthOpt match
          case Some(wcSigned, wcWidthIntOpt) =>
            if (!baType.signed && wcSigned)
              throw new IllegalArgumentException(
                s"Wildcard `Int` value is negative and cannot adapt to an unsigned bit-accurate value."
              )
            (baType.widthIntOpt, wcWidthIntOpt) match
              case (Some(baWidth), Some(wcWidth)) =>
                // Unsigned wildcard adapting to signed bit-accurate value needs an extra bit
                val effectiveWidth =
                  if (baType.signed && !wcSigned) wcWidth + 1 else wcWidth
                if (effectiveWidth > baWidth)
                  throw new IllegalArgumentException(
                    s"Wildcard `Int` value width ($effectiveWidth) is larger than the bit-accurate value width ($baWidth)."
                  )
              case _ =>
          case _ =>
        end match
      end checkWildcardFit

      private def arithOp[
          OS <: Boolean,
          OW <: IntP,
          ON <: NativeType,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](
          dfType: DFXInt[OS, OW, ON],
          op: FuncOp,
          lhs: DFValTP[DFXInt[LS, LW, LN], LP],
          rhs: DFValTP[DFXInt[RS, RW, RN], RP]
      )(using dfc: DFC): DFValTP[DFXInt[OS, OW, ON], LP | RP] =
        val rhsFix = rhs.toDFXIntOf(lhs.dfType)(using dfc.anonymize)
        import dfc.getSet
        // Check A: / and % — both operands are context-determined in Verilog,
        // so any narrow non-carry chain mixed with an implicit Int diverges.
        val shouldWarn = op match
          case FuncOp./ | FuncOp.% =>
            shouldWarnVerilogSemantics(lhs.asIR, rhsFix.asIR)
          case _ => false
        if shouldWarn then
          dfc.logEvent(DFWarning(op.toString, verilogSemanticsWarnMsg))
        DFVal.Func(dfType, op, List(lhs, rhsFix))
      end arithOp

      type CommutativeArithOp =
        FuncOp.+.type | FuncOp.*.type | FuncOp.max.type | FuncOp.min.type
      type NonCommutativeArithOp =
        FuncOp.-.type | FuncOp./.type | FuncOp.%.type
      type ArithOp = CommutativeArithOp | NonCommutativeArithOp
      given evOpArithIntDFInt32[
          Op <: ArithOp,
          L <: Int,
          RP,
          R <: DFValTP[DFInt32, RP]
      ](using
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFInt32, RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFInt32, RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val lhsVal = DFVal.Const(DFInt32, Some(BigInt(lhs)))
            DFVal.Func(DFInt32, op, List(lhsVal, rhs)).asValTP[DFInt32, RP]
          }(using dfc, CTName(op.value.toString))
      end evOpArithIntDFInt32
      given evOpCommutativeArithDFXInt[
          Op <: CommutativeArithOp,
          L,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          R,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP,
          LWUB <: Int,
          RWUB <: Int
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP],
          op: ValueOf[Op],
          isWildcardL: ValueOf[LN],
          isWildcardR: ValueOf[RN],
          // Type-level wildcard detection: when exactly one operand is a wildcard
          // (Int32 NativeType), adapt to the bit-accurate value's sign and width.
          // When both are wildcards, use LS || RS and Max (both-wildcard = DFInt32-like).
          resultSign: Id[ITE[LN && ![RN], RS, ITE[RN && ![LN], LS, ITE[LN && RN, LS, LS || RS]]]],
          resultWidth: Id[ITE[LN && ![RN], RW, ITE[
            RN && ![LN],
            LW,
            ITE[LN && RN, LW, IntP.ArithMaxWidth[LS, LW, RS, RW]]
          ]]],
          resultNative: Id[ITE[LN && ![RN], RN, LN]],
          // Compile-time wildcard fit: when one operand is a literal wildcard,
          // verify its sign and width fit in the bit-accurate value's type.
          // the UBound outputs are bound to plain type parameters rather than read off the
          // instances: `IsConst` answers `false` for a path-dependent type just as it does for an
          // unreduced match type, which would collapse these widths (see `IntP.IsConstInt2`)
          ubLW: UBound.Aux[Int, LW, LWUB],
          ubRW: UBound.Aux[Int, RW, RWUB],
          checkWS: `BaS >= WcS`.Check[
            ITE[RN && ![LN], LS, ITE[LN && ![RN], RS, LS]],
            ITE[RN && ![LN], RS, ITE[LN && ![RN], LS, LS]]
          ],
          checkWW: `BaW >= WcW`.Check[
            ITE[RN && ![LN], LWUB, ITE[LN && ![RN], RWUB, LWUB]],
            ITE[
              RN && ![LN],
              ITE[LS && ![RS], IntP.Inc[RWUB], RWUB],
              ITE[
                LN && ![RN],
                ITE[RS && ![LS], IntP.Inc[LWUB], LWUB],
                LWUB
              ]
            ]
          ]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[
        DFXInt[resultSign.Out, resultWidth.Out, resultNative.Out],
        LP | RP
      ]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFXInt[resultSign.Out, resultWidth.Out, resultNative.Out], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            import IntParam.{+, max}
            val lhsIsWildcard = isWildcardL.value
            val rhsIsWildcard = isWildcardR.value
            val retVal =
              if (lhsIsWildcard && !rhsIsWildcard)
                // LHS is wildcard: adapt to RHS type, keeping the written operand order
                checkWildcardFit(lhsVal.asValOf[DFInt32], rhsVal.dfType)
                val lhsFix = lhsVal.toDFXIntOf(rhsVal.dfType)(using dfcAnon)
                DFVal.Func(rhsVal.dfType, op.value, List(lhsFix, rhsVal))
              else if (rhsIsWildcard) // LHS may be wildcard or concrete
                // RHS is wildcard: adapt to LHS type
                checkWildcardFit(rhsVal.asValOf[DFInt32], lhsVal.dfType)
                arithOp(lhsVal.dfType, op.value, lhsVal, rhsVal)
              else
                // Both concrete: use max width, max signed
                val lhsSFix =
                  if (!lhsVal.dfType.signed && rhsVal.dfType.signed)
                    lhsVal.asValOf[DFUInt[Int]].signed(using dfcAnon).asValOf[DFSInt[Int]]
                  else lhsVal.asValOf[DFSInt[Int]]
                val rhsSFix =
                  if (!rhsVal.dfType.signed && lhsVal.dfType.signed)
                    rhsVal.asValOf[DFUInt[Int]].signed(using dfcAnon).asValOf[DFSInt[Int]]
                  else rhsVal.asValOf[DFSInt[Int]]
                lhsSFix.compareWidths(rhsSFix)(_ >= _) match
                  case Some(true)  => arithOp(lhsSFix.dfType, op.value, lhsSFix, rhsSFix)
                  case Some(false) =>
                    // RHS is wider: the result takes its type, but the written operand
                    // order is kept, so the narrower LHS converts in place
                    val lhsFix = lhsSFix.toDFXIntOf(rhsSFix.dfType)(using dfcAnon)
                    DFVal.Func(rhsSFix.dfType, op.value, List(lhsFix, rhsSFix))
                  case None =>
                    val lhsEffWidth: IntParam[Int] = lhsSFix.widthIntParam
                    val rhsEffWidth: IntParam[Int] = rhsSFix.widthIntParam
                    val maxWidth = lhsEffWidth.max(rhsEffWidth)
                    val lhsWFix = lhsSFix.resize(maxWidth)
                    val rhsWFix = rhsSFix.resize(maxWidth)
                    arithOp(lhsWFix.dfType, op.value, lhsWFix, rhsWFix)
              end if
            end retVal
            retVal.asInstanceOf[Out]
          }(using dfc, CTName(op.value.toString))
      end evOpCommutativeArithDFXInt

      given evOpNonCommutativeArithDFXInt[
          Op <: NonCommutativeArithOp,
          L,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          R,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP,
          LWUB <: Int,
          RWUB <: Int
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP],
          op: ValueOf[Op],
          isWildcardL: ValueOf[LN],
          isWildcardR: ValueOf[RN]
      )(using
          check: ArithCheck[LS, LW, LN, RS, RW, RN],
          // Wildcard LHS adapts to RHS type; otherwise LHS-dominant
          resultSign: Id[ITE[LN && ![RN], RS, LS]],
          resultWidth: Id[ITE[LN && ![RN], RW, LW]],
          resultNative: Id[ITE[LN && ![RN], RN, LN]],
          // Compile-time wildcard fit: when LHS is a literal wildcard,
          // verify its sign and width fit in the RHS (bit-accurate value) type.
          // the UBound outputs are bound to plain type parameters rather than read off the
          // instances: `IsConst` answers `false` for a path-dependent type just as it does for an
          // unreduced match type, which would collapse these widths (see `IntP.IsConstInt2`)
          ubLW: UBound.Aux[Int, LW, LWUB],
          ubRW: UBound.Aux[Int, RW, RWUB],
          checkWS: `BaS >= WcS`.Check[
            ITE[LN && ![RN], RS, LS],
            ITE[LN && ![RN], LS, LS]
          ],
          checkWW: `BaW >= WcW`.Check[
            ITE[LN && ![RN], RWUB, LWUB],
            ITE[
              LN && ![RN],
              ITE[RS && ![LS], IntP.Inc[LWUB], LWUB],
              LWUB
            ]
          ]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[
        DFXInt[resultSign.Out, resultWidth.Out, resultNative.Out],
        LP | RP
      ]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFXInt[resultSign.Out, resultWidth.Out, resultNative.Out], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            val lhsIsWildcard = isWildcardL.value
            val rhsIsWildcard = isWildcardR.value
            if (lhsIsWildcard && !rhsIsWildcard)
              // LHS is wildcard, RHS is concrete: adapt LHS to RHS type, keep operand order
              checkWildcardFit(lhsVal.asValOf[DFInt32], rhsVal.dfType)
              val lhsAdj = lhsVal.toDFXIntOf(rhsVal.dfType)(using dfcAnon)
              DFVal.Func(rhsVal.dfType, op.value, List(lhsAdj, rhsVal)).asInstanceOf[Out]
            else
              // Both concrete, both wildcards, or only RHS is wildcard: LHS-dominant
              check(lhsVal, rhsVal)
              arithOp(lhsVal.dfType, op.value, lhsVal, rhsVal).asInstanceOf[Out]
          }(using dfc, CTName(op.value.toString))
      end evOpNonCommutativeArithDFXInt

      import DFVal.Ops.CarryOp
      given evOpCarryAddSubDFXInt[
          Op <: FuncOp.+.type | FuncOp.-.type,
          L,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          R,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP],
          op: ValueOf[Op]
      )(using
          carryCheck: CarryCheck[LN, RN]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[
        DFXInt[LS || RS, IntP.ArithCarryWidth[LW, RW], BitAccurate],
        LP | RP
      ]] = new ExactOp2[CarryOp[Op], DFC, DFValAny, L, R]:
        type Out = DFValTP[DFXInt[LS || RS, IntP.ArithCarryWidth[LW, RW], BitAccurate], LP | RP]
        def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          import IntParam.{+, max}
          // A wildcard `Int` operand (a DFHDL `Int` parameter or an expression over one)
          // adapts to the bit-accurate operand's sign and width before the carry widening,
          // instead of contributing its 32-bit signed representation. `carryCheck` rules
          // out two wildcard operands. Scala `Int` operands are already bit-accurate here
          // (the candidate converts them at their value's minimal width), so they keep
          // contributing that width to the common-width calculation.
          val lhsIsWildcard = lhsVal.dfType.asIR.isDFInt32
          val rhsIsWildcard = rhsVal.dfType.asIR.isDFInt32
          carryCheck(lhsIsWildcard, rhsIsWildcard)
          if (rhsIsWildcard) checkWildcardFit(rhsVal.asValOf[DFInt32], lhsVal.dfType)
          else if (lhsIsWildcard) checkWildcardFit(lhsVal.asValOf[DFInt32], rhsVal.dfType)
          val resultSigned: Boolean =
            if (rhsIsWildcard) lhsVal.dfType.signed
            else if (lhsIsWildcard) rhsVal.dfType.signed
            else lhsVal.dfType.signed || rhsVal.dfType.signed
          val commonWidth: IntParam[Int] =
            if (rhsIsWildcard) lhsVal.widthIntParam
            else if (lhsIsWildcard) rhsVal.widthIntParam
            else lhsVal.widthIntParam.max(rhsVal.widthIntParam)
          val width = commonWidth + 1
          // Align both operands to the common width (converting sign if needed), then widen
          // them BY THE CARRY BIT as explicit aliases: a carry operation IS the modular
          // operation over carry-widened operands, with no special-cased Func type; the
          // printers reconstruct the `op^` spelling from this shape (see `CarryFunc`). The
          // widening is an explicit alias, never a re-evaluation, so a nested anonymous
          // chain operand keeps its own width semantics.
          val commonType = DFXInt(resultSigned, commonWidth, BitAccurate)
          val lhsFix = lhsVal.toDFXIntOf(commonType)(using dfcAnon)
          val rhsFix = rhsVal.toDFXIntOf(commonType)(using dfcAnon)
          def wideType = DFXInt(resultSigned, width, BitAccurate)
          val lhsWide = DFVal.Alias.AsIs(wideType, lhsFix)(using dfcAnon)
          val rhsWide = DFVal.Alias.AsIs(wideType, rhsFix)(using dfcAnon)
          DFVal.Func(wideType, op.value, List(lhsWide, rhsWide))
            .asInstanceOf[Out]
        }(using dfc, CTName(op.value.toString + "^"))
      end evOpCarryAddSubDFXInt

      given evOpCarryMulDFXInt[
          Op <: FuncOp.`*`.type,
          L,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          R,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](using
          icL: Candidate.Aux[L, LS, LW, LN, LP],
          icR: Candidate.Aux[R, RS, RW, RN, RP]
      )(using
          carryCheck: CarryCheck[LN, RN]
      ): ExactOp2Aux[CarryOp[Op], DFC, DFValAny, L, R, DFValTP[
        DFXInt[LS || RS, IntP.+[LW, RW], BitAccurate],
        LP | RP
      ]] = new ExactOp2[CarryOp[Op], DFC, DFValAny, L, R]:
        type Out = DFValTP[DFXInt[LS || RS, IntP.+[LW, RW], BitAccurate], LP | RP]
        def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          import IntParam.+
          // Same wildcard adaptation as carry add/sub: the wildcard `Int` operand takes
          // the bit-accurate operand's sign and width, so the product doubles that width
          // and keeps that sign.
          val lhsIsWildcard = lhsVal.dfType.asIR.isDFInt32
          val rhsIsWildcard = rhsVal.dfType.asIR.isDFInt32
          carryCheck(lhsIsWildcard, rhsIsWildcard)
          if (rhsIsWildcard) checkWildcardFit(rhsVal.asValOf[DFInt32], lhsVal.dfType)
          else if (lhsIsWildcard) checkWildcardFit(lhsVal.asValOf[DFInt32], rhsVal.dfType)
          if (lhsIsWildcard || rhsIsWildcard)
            val baSigned: Boolean =
              if (rhsIsWildcard) lhsVal.dfType.signed else rhsVal.dfType.signed
            val baWidth: IntParam[Int] =
              if (rhsIsWildcard) lhsVal.widthIntParam else rhsVal.widthIntParam
            val commonType = DFXInt(baSigned, baWidth, BitAccurate)
            def wideType = DFXInt(baSigned, baWidth + baWidth, BitAccurate)
            val lhsFix = lhsVal.toDFXIntOf(commonType)(using dfcAnon)
            val rhsFix = rhsVal.toDFXIntOf(commonType)(using dfcAnon)
            val lhsWide = DFVal.Alias.AsIs(wideType, lhsFix)(using dfcAnon)
            val rhsWide = DFVal.Alias.AsIs(wideType, rhsFix)(using dfcAnon)
            DFVal.Func(wideType, FuncOp.`*`, List(lhsWide, rhsWide))
              .asInstanceOf[Out]
          else
            val resultSigned = lhsVal.dfType.signed || rhsVal.dfType.signed
            val width = lhsVal.widthIntParam + rhsVal.widthIntParam
            def wideType = DFXInt(resultSigned, width, BitAccurate)
            // Convert unsigned operand to signed if needed
            val lhsFix =
              if (resultSigned && !lhsVal.dfType.signed)
                lhsVal.toDFXIntOf(DFXInt(true, lhsVal.widthIntParam + 1, BitAccurate))(using
                  dfcAnon
                )
              else lhsVal
            val rhsFix =
              if (resultSigned && !rhsVal.dfType.signed)
                rhsVal.toDFXIntOf(DFXInt(true, rhsVal.widthIntParam + 1, BitAccurate))(using
                  dfcAnon
                )
              else rhsVal
            val lhsWide = DFVal.Alias.AsIs(wideType, lhsFix)(using dfcAnon)
            val rhsWide = DFVal.Alias.AsIs(wideType, rhsFix)(using dfcAnon)
            DFVal.Func(wideType, FuncOp.`*`, List(lhsWide, rhsWide))
              .asInstanceOf[Out]
          end if
        }(using dfc, CTName("*^"))
      end evOpCarryMulDFXInt

      // TODO: this takes the RHS's width as the result type width. This is how VHDL behaves.
      // But verilog requires the result type width to be the same as the LHS's width.
      // The general rule that we apply in evOpArithDFXInt is to take the LHS's width and the RHS is also resized to the LHS's width.
      // This approach always works, but then requires resizing if we require the actual (smaller) width of the result.
      // However when compiling to verilog this creates linting warnings.
      // given evOpModDFXInt[
      //     Op <: FuncOp.%.type,
      //     LS <: Boolean,
      //     LW <: IntP,
      //     LP,
      //     L <: DFValTP[DFXInt[LS, LW, BitAccurate], LP],
      //     R,
      //     RS <: Boolean,
      //     RW <: IntP,
      //     RN <: NativeType,
      //     RP,
      //     RSM <: Boolean,
      //     RWM <: IntP,
      //     RI <: Boolean
      // ](using
      //     icR: Candidate.AuxM[R, RS, RW, RN, RP, RSM, RWM],
      //     op: ValueOf[Op]
      // )(using
      //     check: AssertGiven[
      //       (RP =:= CONST) | (RN =:= BitAccurate),
      //       "The RHS argument of the modulo operation must be a constant DFHDL Int value or any SInt value."
      //     ]
      // ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFXInt[LS, RW, BitAccurate], LP | RP]] =
      //   new ExactOp2[Op, DFC, DFValAny, L, R]:
      //     type Out = DFValTP[DFXInt[LS, RW, BitAccurate], LP | RP]
      //     def apply(lhs: L, rhs: R)(using dfc: DFC): Out = trydf {
      //       given DFC = dfc.anonymize
      //       val rhsVal = icR(rhs)
      //       val rhsFix =
      //         if (lhs.dfType.signed)
      //           if (rhsVal.dfType.signed) rhsVal.toDFXIntOf(DFSInt(rhsVal.widthIntParam))
      //           else rhsVal.toDFXIntOf(DFSInt(rhsVal.widthIntParam + 1))
      //         else
      //           if (rhsVal.dfType.signed) rhsVal.toDFXIntOf(DFSInt(rhsVal.widthIntParam))
      //           else rhsVal.toDFXIntOf(DFUInt(rhsVal.widthIntParam))
      //       DFVal.Func(
      //         rhsFix.dfType,
      //         op,
      //         List(lhs, rhsFix)
      //       )(using dfc).asValTP[DFXInt[LS, RW, BitAccurate], LP | RP]
      //     }(using dfc, CTName(op.value.toString))
      // end evOpModDFXInt
    end Ops
  end Val
end DFXInt

type DFUInt[W <: IntP] = DFXInt[false, W, BitAccurate]
object DFUInt:
  def apply[W <: IntP](width: IntParam[W])(using DFCG, Width.CheckNUB[false, W]): DFUInt[W] =
    trydf {
      DFXInt(false, width, BitAccurate)
    }
  def forced[W <: IntP](width: IntP)(using DFC): DFUInt[W] =
    DFUInt(IntParam[W](width.asInstanceOf[W]))
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFUInt[W]): DFUInt[W] = trydf { dfType }
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFUInt[IntP.CLog2[V]] = trydf {
    sup.toScalaIntOpt.foreach(check(_))
    DFXInt(false, sup.clog2, BitAccurate)
  }
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFCG,
      check: Arg.Positive.CheckNUB[V]
  ): DFUInt[IntP.CLog2P1[V]] = trydf {
    max.toScalaIntOpt.foreach(check(_))
    // the width value is `clog2(max + 1)`; the declared type says the same thing under a single
    // guard on `V`, which the composed spelling cannot (see `IntP.IsConstInt2`)
    DFXInt(false, (max + 1).clog2, BitAccurate).asInstanceOf[DFUInt[IntP.CLog2P1[V]]]
  }

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
        [UBW <: Int, RW <: Int] =>> "Expected argument width " + UBW + " but found: " + RW +
          "\nTo Fix:\nUse `.resize` to match the width automatically."
      ]

  object Val:
    trait UBArg[UB <: IntP, R] extends Exact1.TC[IntP, UB, [ub <: IntP] =>> IntParam[ub], R, DFC]:
      type OutP
      type Out = DFValTP[DFInt32, OutP]
      def conv(arg1: IntParam[UB], from: R)(using DFC): Out = apply(arg1, from)
      def apply(ub: IntParam[UB], arg: R)(using DFC): Out
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
      type Aux[UB <: IntP, R, P] = UBArg[UB, R] { type OutP = P }
      type Exact[UB <: IntP] = Exact1[IntP, UB, [ub <: IntP] =>> IntParam[ub], DFC, UBArg]
      given fromInt[UB <: Int, R <: Int](using
          unsignedCheck: Unsigned.Check[R < 0],
          ubCheck: `UB > R`.CheckNUB[UB, R]
      ): UBArg[UB, R] with
        type OutP = CONST
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out =
          unsignedCheck(arg < 0)
          ub.toScalaIntOpt.foreach(ubCheck(_, arg))
          DFConstInt32(arg)
      end fromInt
      given fromR[
          UB <: IntP,
          R,
          S <: Boolean,
          W <: IntP,
          N <: NativeType,
          P
      ](using
          ic: DFXInt.Val.Candidate.Aux[R, S, W, N, P]
      )(using
          unsignedCheck: Unsigned.Check[S],
          widthCheck: `UBW == RW`.CheckNUB[IntP.CLog2[UB], W]
      ): UBArg[UB, R] with
        type OutP = P
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out =
          import dfc.getSet
          val argVal = ic(arg)
          val argValIR = argVal.asIR
          // if the argument is a constant, we can check its value and width
          val fixedArgValIR = argValIR.getConstData[Option[BigInt]].toOption match
            case Some(Some(arg: BigInt)) if arg.isValidInt =>
              unsignedCheck(arg < 0)
              ub.toScalaIntOpt.foreach(ub => summon[`UB > R`.CheckNUB[UB, Int]](ub, arg.toInt))
              argValIR
            case _ =>
              import DFXInt.Val.Ops.resize
              // skip checks if the argument is an Int32.
              // TODO: in the future, it's worth considering adding assertions
              if (argValIR.dfType != ir.DFInt32)
                unsignedCheck(argVal.dfType.signed)
                if (argValIR.hasTagOf[ir.ResizeTag])
                  argVal.resize(ub.clog2).asIR
                else
                  (ub.toScalaIntOpt, argVal.widthIntOpt) match
                    case (Some(ubInt), Some(argWidth)) =>
                      widthCheck(clog2(ubInt), argWidth)
                      argValIR
                    case _ =>
                  argValIR
              else argValIR
          DFVal.Alias.AsIs(DFInt32, fixedArgValIR.asValTP[DFUInt[Int], P])
        end apply
      end fromR
    end UBArg
    object Ops:
      // `.signed` adds a sign bit: UFix[M, F] -> SFix[M+1, F] (UInt[W] -> SInt[W+1] is the
      // fraction-zero case, so this is the single unified sign cast). Parametric-safe: the
      // new magnitude is derived via `IntParam`.
      extension [M <: IntP, F <: Int, P](lhs: DFValTP[DFUFix[M, F], P])
        def signed(using dfc: DFCG): DFValTP[DFSFix[IntP.+[M, 1], F], P] = trydf {
          import dfc.getSet
          val fractionWidth = lhs.dfType.asIR.fractionWidth
          val newMagnitude = (lhs.widthIntParam - fractionWidth + 1).ref
          DFVal.Alias.AsIs(
            ir.DFDecimal(true, newMagnitude, fractionWidth, BitAccurate)
              .asFE[DFSFix[IntP.+[M, 1], F]],
            lhs
          ).asValTP[DFSFix[IntP.+[M, 1], F], P]
        }
      extension [W <: IntP, P](lhs: DFValTP[DFUInt[W], P])
        @targetName("negateDFUInt")
        def unary_-(using DFCG): DFValTP[DFSInt[IntP.+[W, 1]], P] = trydf {
          import DFSInt.Val.Ops.unary_- as negate
          lhs.signed.negate.asValTP[DFSInt[IntP.+[W, 1]], P]
        }
        @targetName("toIntDFUInt")
        def toInt(using
            dfc: DFCG,
            check: `W <= 31`.CheckNUB[W]
        ): DFValTP[DFInt32, P] = trydf {
          lhs.widthIntOpt.foreach(check(_))
          DFVal.Alias.AsIs(DFInt32, lhs.signed)
        }
        @targetName("msbitsDFUInt")
        def msbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFUInt[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, lhs.widthIntParam - 1, lhs.widthIntParam - updatedWidth)
            .asValTP[DFUInt[RW], P]
        }
        @targetName("lsbitsDFUInt")
        def lsbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFUInt[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, updatedWidth - 1, 0)
            .asValTP[DFUInt[RW], P]
        }
      end extension
      extension [W <: IntP, A, C, I, P](lhs: DFVal[DFUInt[W], Modifier[A, C, I, P]])
        // ascending part-select (Verilog `lhs[baseIdx +: selWidth]`):
        // selWidth bits whose LSB is anchored at baseIdx
        @targetName("lsbitsAtDFUInt")
        def lsbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkLow: DFBits.BitIndex.CheckNUB[BI, W],
            checkHigh: DFBits.BitIndex.CheckNUB[IntP.PartSelectHigh[BI, SW], W]
        ): DFVal[DFUInt[SW], Modifier[A, C, Any, P]] = trydf {
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxHigh = baseIdx + selWidth - 1
          (baseIdx.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(baseIdxInt), Some(widthInt)) => checkLow(baseIdxInt, widthInt)
            case _                                  =>
          (idxHigh.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(idxHighInt), Some(widthInt)) => checkHigh(idxHighInt, widthInt)
            case _                                  =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, idxHigh, baseIdx)
            .asVal[DFUInt[SW], Modifier[A, C, Any, P]]
        }
        // descending part-select (Verilog `lhs[baseIdx -: selWidth]`):
        // selWidth bits whose MSB is anchored at baseIdx
        @targetName("msbitsAtDFUInt")
        def msbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkHigh: DFBits.BitIndex.CheckNUB[BI, W],
            checkLow: DFBits.BitIndex.CheckNUB[IntP.PartSelectLow[BI, SW], W]
        ): DFVal[DFUInt[SW], Modifier[A, C, Any, P]] = trydf {
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxLow = baseIdx - selWidth + 1
          (baseIdx.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(baseIdxInt), Some(widthInt)) => checkHigh(baseIdxInt, widthInt)
            case _                                  =>
          (idxLow.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(idxLowInt), Some(widthInt)) => checkLow(idxLowInt, widthInt)
            case _                                 =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, baseIdx, idxLow)
            .asVal[DFUInt[SW], Modifier[A, C, Any, P]]
        }
      end extension
    end Ops
  end Val

end DFUInt

type DFSInt[W <: IntP] = DFXInt[true, W, BitAccurate]
object DFSInt:
  def apply[W <: IntP](width: IntParam[W])(using DFCG, Width.CheckNUB[true, W]): DFSInt[W] =
    DFXInt(true, width, BitAccurate)
  def forced[W <: IntP](width: IntP)(using DFC): DFSInt[W] =
    DFSInt(IntParam[W](width.asInstanceOf[W]))
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFSInt[W]): DFSInt[W] = trydf { dfType }
  def untilAbs[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFSInt[IntP.CLog2Signed[V]] = trydf {
    sup.toScalaIntOpt.foreach(check(_))
    DFXInt(true, sup.clog2 + 1, BitAccurate).asInstanceOf[DFSInt[IntP.CLog2Signed[V]]]
  }
  def toAbs[V <: IntP](max: IntParam[V])(using
      dfc: DFCG,
      check: Arg.Positive.CheckNUB[V]
  ): DFSInt[IntP.CLog2P1Signed[V]] = trydf {
    max.toScalaIntOpt.foreach(check(_))
    DFXInt(true, (max + 1).clog2 + 1, BitAccurate).asInstanceOf[DFSInt[IntP.CLog2P1Signed[V]]]
  }

  object Val:
    object Ops:
      // `.unsigned` drops the sign bit: SFix[M, F] -> UFix[M-1, F] (SInt[W] -> UInt[W-1] is
      // the fraction-zero case, so this is the single unified unsign cast).
      extension [M <: IntP, F <: Int, P](lhs: DFValTP[DFSFix[M, F], P])
        def unsigned(using dfc: DFCG): DFValTP[DFUFix[IntP.-[M, 1], F], P] = trydf {
          import dfc.getSet
          val fractionWidth = lhs.dfType.asIR.fractionWidth
          val newMagnitude = (lhs.widthIntParam - fractionWidth - 1).ref
          DFVal.Alias.AsIs(
            ir.DFDecimal(false, newMagnitude, fractionWidth, BitAccurate)
              .asFE[DFUFix[IntP.-[M, 1], F]],
            lhs
          ).asValTP[DFUFix[IntP.-[M, 1], F], P]
        }
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("negateDFSInt")
        def unary_-(using DFCG): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
        def signbit(using dfc: DFCG): DFValTP[DFBit, P] =
          val idx = locally {
            given DFCG = dfc.anonymize
            (lhs.widthIntParam - 1).toDFConst
          }
          DFVal.Alias.ApplyIdx(DFBit, lhs, idx).asValTP[DFBit, P]
        @targetName("msbitsDFSInt")
        def msbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFUInt[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, lhs.widthIntParam - 1, lhs.widthIntParam - updatedWidth)
            .asValTP[DFUInt[RW], P]
        }
        @targetName("lsbitsDFSInt")
        def lsbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFUInt[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, updatedWidth - 1, 0)
            .asValTP[DFUInt[RW], P]
        }
      end extension
      extension [W <: IntP, A, C, I, P](lhs: DFVal[DFSInt[W], Modifier[A, C, I, P]])
        // ascending part-select (Verilog `lhs[baseIdx +: selWidth]`):
        // selWidth bits whose LSB is anchored at baseIdx
        @targetName("lsbitsAtDFSInt")
        def lsbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkLow: DFBits.BitIndex.CheckNUB[BI, W],
            checkHigh: DFBits.BitIndex.CheckNUB[IntP.PartSelectHigh[BI, SW], W]
        ): DFVal[DFUInt[SW], Modifier[A, C, Any, P]] = trydf {
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxHigh = baseIdx + selWidth - 1
          (baseIdx.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(baseIdxInt), Some(widthInt)) => checkLow(baseIdxInt, widthInt)
            case _                                  =>
          (idxHigh.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(idxHighInt), Some(widthInt)) => checkHigh(idxHighInt, widthInt)
            case _                                  =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, idxHigh, baseIdx)
            .asVal[DFUInt[SW], Modifier[A, C, Any, P]]
        }
        // descending part-select (Verilog `lhs[baseIdx -: selWidth]`):
        // selWidth bits whose MSB is anchored at baseIdx
        @targetName("msbitsAtDFSInt")
        def msbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkHigh: DFBits.BitIndex.CheckNUB[BI, W],
            checkLow: DFBits.BitIndex.CheckNUB[IntP.PartSelectLow[BI, SW], W]
        ): DFVal[DFUInt[SW], Modifier[A, C, Any, P]] = trydf {
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxLow = baseIdx - selWidth + 1
          (baseIdx.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(baseIdxInt), Some(widthInt)) => checkHigh(baseIdxInt, widthInt)
            case _                                  =>
          (idxLow.toScalaIntOpt, lhs.widthIntOpt) match
            case (Some(idxLowInt), Some(widthInt)) => checkLow(idxLowInt, widthInt)
            case _                                 =>
          DFVal.Alias.ApplyRange
            .applyDFXInt(lhs, baseIdx, idxLow)
            .asVal[DFUInt[SW], Modifier[A, C, Any, P]]
        }
      end extension
      extension [P](lhs: DFValTP[DFInt32, P])
        @targetName("negateDFInt32")
        def unary_-(using DFCG): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("toIntDFSInt")
        def toInt(using
            dfc: DFCG,
            check: `W <= 32`.CheckNUB[W]
        ): DFValTP[DFInt32, P] = trydf {
          lhs.widthIntOpt.foreach(check(_))
          DFVal.Alias.AsIs(DFInt32, lhs)
        }
    end Ops
  end Val
end DFSInt

type DFUFix[M <: IntP, F <: Int] = DFDecimal[false, M, F, BitAccurate]
object DFUFix:
  def apply[M <: IntP, F <: Int](magnitudeWidth: IntParam[M], fractionWidth: Inlined[F])(using
      dfc: DFCG,
      checkM: MagnitudeWidth.CheckNUB[false, M],
      checkF: FractionWidth.Check[F],
      checkW: Width.CheckNUB[false, DecimalWidth[M, F]]
  ): DFUFix[M, F] = trydf {
    checkF(fractionWidth)
    magnitudeWidth.toScalaIntOpt.foreach(checkM(false, _))
    DFDecimal(false, magnitudeWidth, fractionWidth, BitAccurate)
  }
  def apply[M <: IntP, F <: Int](using dfc: DFCG, dfType: => DFUFix[M, F]): DFUFix[M, F] =
    trydf { dfType }
end DFUFix

type DFSFix[M <: IntP, F <: Int] = DFDecimal[true, M, F, BitAccurate]
object DFSFix:
  def apply[M <: IntP, F <: Int](magnitudeWidth: IntParam[M], fractionWidth: Inlined[F])(using
      dfc: DFCG,
      checkM: MagnitudeWidth.CheckNUB[true, M],
      checkF: FractionWidth.Check[F],
      checkW: Width.CheckNUB[true, DecimalWidth[M, F]]
  ): DFSFix[M, F] = trydf {
    checkF(fractionWidth)
    magnitudeWidth.toScalaIntOpt.foreach(checkM(true, _))
    DFDecimal(true, magnitudeWidth, fractionWidth, BitAccurate)
  }
  def apply[M <: IntP, F <: Int](using dfc: DFCG, dfType: => DFSFix[M, F]): DFSFix[M, F] =
    trydf { dfType }
end DFSFix

//a native Int32 decimal has no explicit Scala compile-time width, since the
//actual value determines its width.
type DFInt32 =
  DFType[ir.DFDecimal, Args4[Boolean, Int, 0, Int32]] // This means: DFDecimal[Boolean, Int, 0, Int32] (could not be defined this way because of type recursion)
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
object DFConstInt32:
  def apply(int: Int, named: Boolean = false)(using DFC): DFConstInt32 =
    DFVal.Const(DFInt32, Some(BigInt(int)), named)
