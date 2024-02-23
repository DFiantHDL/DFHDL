package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.{valueOf => _, *}

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: Int, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: Int, F <: Int, N <: NativeType](
      signed: Inlined[S],
      width: IntParam[W],
      fractionWidth: Inlined[F],
      nativeType: N
  )(using dfc: DFC, check: Width.Check[S, W]): DFDecimal[S, W, F, N] = trydf:
    check(signed, width)
    ir.DFDecimal(signed, width.ref, fractionWidth, nativeType).asFE[DFDecimal[S, W, F, N]]
  protected[core] def forced[S <: Boolean, W <: Int, F <: Int, N <: NativeType](
      signed: Boolean,
      width: Int,
      fractionWidth: Int,
      nativeType: NativeType
  )(using DFC): DFDecimal[S, W, F, N] =
    val check = summon[Width.Check[Boolean, Int]]
    check(signed, width)
    ir.DFDecimal(signed, ir.IntParamRef(width), fractionWidth, nativeType)
      .asFE[DFDecimal[S, W, F, N]]

  given DFInt32 = DFInt32
  given [S <: Boolean, W <: Int, F <: Int, N <: NativeType](using
      ValueOf[S],
      ValueOf[W],
      ValueOf[F],
      ValueOf[N]
  )(using DFC, Width.Check[S, W]): DFDecimal[S, W, F, N] =
    DFDecimal(valueOf[S], IntParam.fromValue[W](valueOf[W]), valueOf[F], valueOf[N])
  object Extensions:
    extension [S <: Boolean, W <: Int, F <: Int, N <: NativeType](dfType: DFDecimal[S, W, F, N])
      def signed: Inlined[S] = Inlined.forced[S](dfType.asIR.signed)
      def nativeType: N = dfType.asIR.nativeType.asInstanceOf[N]

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
        ValN <: NativeType,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgN <: NativeType,
        ArgIsInt <: Boolean, // argument is from a Scala Int
        Castle <: Boolean // castling of dfVal and arg
    ]:
      def apply(
          dfVal: DFValOf[DFXInt[ValS, ValW, ValN]],
          arg: DFValOf[DFXInt[ArgS, ArgW, ArgN]]
      )(using DFC): Unit
    given [
        ValS <: Boolean,
        ValW <: Int,
        ValN <: NativeType,
        ArgS <: Boolean,
        ArgW <: Int,
        ArgN <: NativeType,
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
    ): ArithCheck[ValS, ValW, ValN, ArgS, ArgW, ArgN, ArgIsInt, Castle] with
      def apply(
          dfVal: DFValOf[DFXInt[ValS, ValW, ValN]],
          arg: DFValOf[DFXInt[ArgS, ArgW, ArgN]]
      )(using dfc: DFC): Unit =
        import dfc.getSet
        import DFXInt.Val.getActualSignedWidth
        val dfValSigned = dfVal.dfType.signed
        val dfValWidth = dfVal.dfType.width
        val (argSigned, argWidth) = arg.getActualSignedWidth
        val skipSignChecks: Boolean =
          argIsInt.value && !castle && (dfValSigned || !argSigned)
        val argWFix: Int =
          if (argIsInt.value && !castle && dfValSigned && !argSigned)
            argWidth + 1
          else argWidth
        if (!skipSignChecks)
          val ls: Boolean = if (castle) argSigned else dfValSigned
          val rs: Boolean = if (castle) dfValSigned else argSigned
          checkS(ls, rs)
        val lw: Int = if (castle) argWFix else dfValWidth
        val rw: Int = if (castle) dfValWidth else argWFix
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

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private[DFDecimal] def interpolate(
          opExpr: Expr[String]
      ): Expr[DFConstAny] =
        import quotes.reflect.*
        val signedForced = opExpr.value.get == "sd"
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
            fromDecString($fullExpr, ${ Expr(signedForced) }).toOption.get
          val dfc = compiletime.summonInline[DFC]
          val dfType =
            DFDecimal.forced[
              signedType.Underlying,
              widthType.Underlying,
              fractionWidthType.Underlying,
              BitAccurate
            ](signed, width, fractionWidth, BitAccurate)(using dfc)
          DFVal.Const(dfType, Some(value), named = true)(using dfc)
        }
      end interpolate
    end extension
  end StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.{fromDecString, interpolate}
    opaque type DecStrCtx <: StringContext = StringContext
    object DecStrCtx:
      extension (inline sc: DecStrCtx)
        transparent inline def apply(inline args: Any*): Any =
          ${ applyMacro('sc, 'args) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('sc, 'arg) }

    extension (sc: StringContext)
      /** Decimal Integer String Interpolator
        *
        * Syntax: {{{d"width'dec"}}}
        *   - `dec` is a sequence of decimal characters ('0'-'9') with an optional prefix `-` for
        *     negative values.
        *   - Separators `_` (underscore) and `,` (comma) within `dec` are ignored.
        *   - `width`, followed by a `'`, is optional and specifies the minimum width of the
        *     integer's bit representation. If omitted, the width is inferred from the value's size.
        *     If specified, the output is padded with zeros or extended for signed numbers using
        *     two's complement representation to match the `width`.
        *   - The output type is `UInt[W]` for natural numbers and `SInt[W]` for negative numbers,
        *     where `W` is the width in bits.
        *   - If the specified `width` is less than the required number of bits to represent the
        *     value, an error occurs.
        *
        * @example
        *   {{{
        *   d"0"      // UInt[1], value = 0
        *   d"-1"     // SInt[2], value = -1
        *   d"8'-1"   // SInt[8], value = -1
        *   d"255"    // UInt[8], value = 255
        *   d"1,023"  // UInt[10], value = 1023
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
        *     negative values.
        *   - Separators `_` (underscore) and `,` (comma) within `dec` are ignored.
        *   - `width`, followed by a `'`, is optional and specifies the minimum width of the
        *     integer's bit representation, which is always at least 2 bits to accommodate the sign
        *     bit.
        *   - The output is always a signed integer type `SInt[W]`, regardless of whether the `dec`
        *     value is negative or natural, where `W` is the width in bits.
        *   - If the specified `width` is less than the required number of bits to represent the
        *     value including the sign bit, an error occurs.
        *
        * @example
        *   {{{
        *   sd"0"     // SInt[2], value = 0 (unsigned number represented as a signed type)
        *   sd"-1"    // SInt[2], value = -1
        *   sd"255"   // SInt[9], value = 255 (unsigned number represented as a signed type)
        *   sd"8'255" // Error: width is too small to represent the value including the sign bit
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

    private def applyMacro(
        sc: Expr[DecStrCtx],
        args: Expr[Seq[Any]]
    )(using Quotes): Expr[DFConstAny] =
      sc.scPartsWithArgs(args).interpolate(Expr(sc.funcName))

    private def unapplySeqMacro[T <: DFTypeAny](
        sc: Expr[DecStrCtx],
        arg: Expr[DFValOf[T]]
    )(using Quotes, Type[T]): Expr[Option[Seq[DFValOf[T]]]] =
      import quotes.reflect.*
      val parts = sc.parts.map(_.asTerm).toList
      val op = sc.funcName
      if (parts.length > 1)
        '{
          compiletime.error(
            "Extractors for decimal string interpolation are not allowed."
          )
          Some(Seq())
        }
      else
        val dfVal = parts.head.interpolate(Expr(op))
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
  end StrInterpOps

  object Val:
    object TC:
      export DFXInt.Val.TC.given
      def apply(
          dfType: DFDecimal[Boolean, Int, Int, NativeType],
          dfVal: DFValOf[DFDecimal[Boolean, Int, Int, NativeType]]
      )(using DFC): DFValOf[DFDecimal[Boolean, Int, Int, NativeType]] =
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

type DFXInt[S <: Boolean, W <: Int, N <: NativeType] = DFDecimal[S, W, 0, N]
object DFXInt:
  def apply[S <: Boolean, W <: Int, N <: NativeType & Singleton](
      signed: Inlined[S],
      width: IntParam[W],
      nativeType: N
  )(using DFC, Width.Check[S, W]): DFXInt[S, W, N] = DFDecimal(signed, width, 0, nativeType)
  def fromInlined[S <: Boolean, W <: Int, N <: NativeType & Singleton](
      signed: Inlined[S],
      width: Inlined[W],
      nativeType: N
  )(using DFC, Width.Check[S, W]): DFXInt[S, W, N] =
    DFDecimal(signed, IntParam(width), 0, nativeType)

  object Val:
    trait Candidate[R]:
      type OutS <: Boolean
      type OutW <: Int
      type OutN <: NativeType
      type OutP
      type Out = DFValTP[DFXInt[OutS, OutW, OutN], OutP]
      type IsScalaInt <: Boolean
      extension (dfVal: ir.DFVal) def asOut: Out = dfVal.asValTP[DFXInt[OutS, OutW, OutN], OutP]
      extension (dfVal: DFValAny) def asOut: Out = dfVal.asValTP[DFXInt[OutS, OutW, OutN], OutP]
      def apply(arg: R)(using DFC): Out
    trait CandidateLP:
      given fromDFBitsValCandidate[R, W <: Int, P](using
          ic: DFBits.Val.Candidate.Aux[R, W, P]
      ): Candidate[R] with
        type OutS = false
        type OutW = W
        type OutN = BitAccurate
        type OutP = P
        type IsScalaInt = false
        def apply(arg: R)(using dfc: DFC): Out =
          import DFBits.Val.Ops.uint
          val dfVal = ic(arg)(using dfc.anonymize)
          val ret =
            if (dfVal.hasTag[DFVal.TruncateTag])
              dfVal.uint.tag(DFVal.TruncateTag)
            else if (dfVal.hasTag[DFVal.ExtendTag])
              dfVal.uint.tag(DFVal.ExtendTag)
            else dfVal.uint
          ret.asValTP[DFXInt[OutS, OutW, OutN], OutP]
      end fromDFBitsValCandidate
    end CandidateLP
    object Candidate extends CandidateLP:
      type IntInfoAux[R <: Int, OS <: Boolean, OW <: Int] =
        IntInfo[R]:
          type OutS = OS
          type OutW = OW
      given fromInt[R <: Int, OS <: Boolean, OW <: Int](using
          info: IntInfoAux[R, OS, OW]
      ): Candidate[R] with
        type OutS = OS
        type OutW = OW
        type OutN = BitAccurate
        type OutP = CONST
        type IsScalaInt = true
        def apply(arg: R)(using dfc: DFC): Out =
          val dfType = DFXInt.fromInlined(info.signed(arg), info.width(arg), BitAccurate)
          DFVal.Const(dfType, Some(BigInt(arg)), named = true)
      // when the candidate is a DFInt32 constant, then we remove the signed and width tags
      // from the type to allow for elaboration (runtime) check that considers the actual
      // signed and width properties of the constant value.
      given fromDFInt32Const[R <: DFValTP[DFInt32, CONST]]: Candidate[R] with
        type OutS = Boolean
        type OutW = Int
        type OutN = Int32
        type OutP = CONST
        type IsScalaInt = false
        def apply(arg: R)(using DFC): Out = arg.asValTP[DFXInt[Boolean, Int, Int32], CONST]
      given fromDFXIntVal[S <: Boolean, W <: Int, N <: NativeType, P, R <: DFValTP[
        DFXInt[S, W, N],
        P
      ]]: Candidate[R] with
        type OutS = S
        type OutW = W
        type OutN = N
        type OutP = P
        type IsScalaInt = false
        def apply(arg: R)(using DFC): Out = arg
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a DFHDL decimal variable."
        )
    end Candidate

    extension [S <: Boolean, W <: Int, N <: NativeType](dfVal: DFValOf[DFXInt[S, W, N]])
      private[core] def getActualSignedWidth(using dfc: DFC): (Boolean, Int) =
        val int32Data: Option[Int] =
          if (dfVal.dfType.asIR.isDFInt32)
            import dfc.getSet
            dfVal.asIR.getParamData match
              case Some(Some(n: BigInt)) => Some(n.toInt)
              case _                     => None
          else None
        int32Data match
          case Some(int) => (int < 0, IntInfo.calcWidth(int))
          case None      => (dfVal.dfType.signed.value, dfVal.dfType.width.value)

    object TC:
      def apply(
          dfType: DFXInt[Boolean, Int, NativeType],
          dfVal: DFValOf[DFXInt[Boolean, Int, NativeType]]
      )(using DFC): DFValOf[DFXInt[Boolean, Int, NativeType]] =
        val check = summon[TCCheck[Boolean, Int, Boolean, Int]]
        check(dfType.signed, dfType.width, dfVal.dfType.signed, dfVal.dfType.width)
        dfVal
      import DFVal.TC
      given [LS <: Boolean, LW <: Int, LN <: NativeType, R, IC <: Candidate[R]](using
          ic: IC
      )(using
          check: TCCheck[LS, LW, ic.OutS, ic.OutW],
          lsigned: OptionalGiven[ValueOf[LS]]
      ): TC[DFXInt[LS, LW, LN], R] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW, LN], value: R)(using dfc: DFC): Out =
          import Ops.resize
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          (dfType.asIR: ir.DFType) match
            case ir.DFNothing =>
              val signCheck = summon[`LS >= RS`.Check[Boolean, Boolean]]
              signCheck(lsigned.get.value, rhs.dfType.signed)
              if (lsigned.get.value != rhs.dfType.signed.value)
                rhs.asValOf[DFUInt[Int]].signed.asValTP[DFXInt[LS, LW, LN], ic.OutP]
              else rhs.asValTP[DFXInt[LS, LW, LN], ic.OutP]
            case _ =>
              val (rhsSigned, rhsWidth) = rhs.getActualSignedWidth
              if (!rhs.hasTag[DFVal.TruncateTag] || dfType.signed != rhsSigned)
                check(dfType.signed, dfType.width, rhsSigned, rhsWidth)
              val dfValIR =
                if (dfType.asIR.isDFInt32 && rhs.dfType.asIR.isDFInt32) rhs.asIR
                else
                  val rhsSignFix: DFValOf[DFSInt[Int]] =
                    if (dfType.signed != rhsSigned)
                      rhs.asValOf[DFUInt[Int]].signed.asValOf[DFSInt[Int]]
                    else rhs.asValOf[DFSInt[Int]]
                  val nativeTypeChanged = dfType.nativeType != rhs.dfType.nativeType
                  if (nativeTypeChanged)
                    dfType.asIR.nativeType match
                      case Int32 =>
                        import DFSInt.Val.Ops.toInt
                        rhsSignFix.toInt.asIR
                      case BitAccurate =>
                        rhsSignFix.resize(dfType.width).asIR
                  else if (
                    dfType.width > rhsSignFix.width ||
                    rhs.hasTag[DFVal.TruncateTag] && dfType.width < rhsSignFix.width
                  )
                    rhsSignFix.resize(dfType.width).asIR
                  else rhsSignFix.asIR
              end dfValIR
              dfValIR.asValTP[DFXInt[LS, LW, LN], ic.OutP]
          end match
        end conv
      end given
    end TC

    object Compare:
      import DFVal.Compare
      given DFXIntCompare[
          LS <: Boolean,
          LW <: Int,
          LN <: NativeType,
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
      ): Compare[DFXInt[LS, LW, LN], R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW, LN], arg: R)(using dfc: DFC): Out =
          import dfc.getSet
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
              dfValArgSigned.asValOf[DFXInt[Boolean, Int, NativeType]].resize(dfType.width)
            else dfValArgSigned
          dfValArgResized.asValTP[DFXInt[LS, LW, LN], ic.OutP]
        end conv
      end DFXIntCompare
    end Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      def clog2[P, S <: Boolean, W <: Int, N <: NativeType](dfVal: DFValTP[DFXInt[S, W, N], P])(
          using
          DFC,
          DFVal.ConstCheck[P]
      ): DFValTP[DFXInt[S, W, N], P] =
        DFVal.Func(dfVal.dfType, FuncOp.clog2, List(dfVal))
      extension [P, S <: Boolean, W <: Int, N <: NativeType](lhs: DFValTP[DFXInt[S, W, N], P])
        def toScalaInt(using DFC, DFVal.ConstCheck[P]): Int =
          lhs.toScalaValue.toInt
        def toScalaBigInt(using DFC, DFVal.ConstCheck[P]): BigInt =
          lhs.toScalaValue
      extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
        def <[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW, icL.OutN], R, FuncOp.<.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def <=[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW, icL.OutN], R, FuncOp.<=.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def >[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW, icL.OutN], R, FuncOp.>.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
        def >=[R](rhs: Exact[R])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[icL.OutS, icL.OutW, icL.OutN], R, FuncOp.>=.type, false]
        ): DFValOf[DFBool] = trydf { op(icL(lhs), rhs) }
      end extension
      extension [S <: Boolean, W <: Int, N <: NativeType, P](lhs: DFValTP[DFXInt[S, W, N], P])
        @targetName("truncateDFXInt")
        def truncate(using DFC): DFValTP[DFXInt[S, Int, N], P] =
          lhs.tag(DFVal.TruncateTag).asValTP[DFXInt[S, Int, N], P]
        @targetName("resizeDFXInt")
        def resize[RW <: Int](
            updatedWidth: Inlined[RW]
        )(using
            dfc: DFC,
            check: Width.Check[S, RW]
        ): DFValTP[DFXInt[S, RW, BitAccurate], P] = trydf {
          val signed = lhs.dfType.signed
          check(signed, updatedWidth)
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFXInt[S, RW, BitAccurate]]
//          else
          DFVal.Alias.AsIs(DFXInt.fromInlined(signed, updatedWidth, BitAccurate), lhs)
        }
        end resize
        @targetName("shiftRightDFXInt")
        def >>[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValTP[DFXInt[S, W, N], P | c.OutP] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.>>, List(lhs, shiftVal))
        }
        @targetName("shiftLeftDFXInt")
        def <<[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValTP[DFXInt[S, W, N], P | c.OutP] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.<<, List(lhs, shiftVal))
        }
      end extension

      extension [L](lhs: L)
        def <[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.<.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def <=[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.<=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.>.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >=[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.>=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
      end extension
      private def arithOp[
          OS <: Boolean,
          OW <: Int,
          ON <: NativeType,
          LS <: Boolean,
          LW <: Int,
          LN <: NativeType,
          LP,
          RS <: Boolean,
          RW <: Int,
          RN <: NativeType,
          RP
      ](
          dfType: DFXInt[OS, OW, ON],
          op: FuncOp,
          lhs: DFValTP[DFXInt[LS, LW, LN], LP],
          rhs: DFValTP[DFXInt[RS, RW, RN], RP]
      )(using dfc: DFC): DFValTP[DFXInt[OS, OW, ON], LP | RP] =
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
            check: ArithCheck[
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icR.IsScalaInt,
              false
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.+, lhsVal, rhsVal)
        }
        def -[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icR.IsScalaInt,
              false
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.-, lhsVal, rhsVal)
        }
        def *[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icR.IsScalaInt,
              false
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
        def /[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icR.IsScalaInt,
              false
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp./, lhsVal, rhsVal)
        }
        def %[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: ArithCheck[
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icR.IsScalaInt,
              false
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.%, lhsVal, rhsVal)
        }
        def +^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[
          DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1, BitAccurate],
          icL.OutP | icR.OutP
        ] =
          trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            check(lhsVal.dfType.signed, rhsVal.dfType.signed)
            val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
              (lhsVal.dfType.width.value max rhsVal.dfType.width) + 1
            )
            val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
            arithOp(dfType, FuncOp.+, lhsVal, rhsVal)
          }
        def -^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[
          DFXInt[icL.OutS, Max[icL.OutW, icR.OutW] + 1, BitAccurate],
          icL.OutP | icR.OutP
        ] =
          trydf {
            val dfcAnon = dfc.anonymize
            val lhsVal = icL(lhs)(using dfcAnon)
            val rhsVal = icR(rhs)(using dfcAnon)
            check(lhsVal.dfType.signed, rhsVal.dfType.signed)
            val width = Inlined.forced[Max[icL.OutW, icR.OutW] + 1](
              (lhsVal.dfType.width.value max rhsVal.dfType.width) + 1
            )
            val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
            arithOp(dfType, FuncOp.-, lhsVal, rhsVal)
          }
        def *^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: SignCheck[icL.OutS, icR.OutS, icR.IsScalaInt, false]
        ): DFValTP[
          DFXInt[icL.OutS, icL.OutW + icR.OutW, BitAccurate],
          icL.OutP | icR.OutP
        ] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          val width = Inlined.forced[icL.OutW + icR.OutW](
            lhsVal.dfType.width.value + rhsVal.dfType.width
          )
          val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
          arithOp(dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
      end extension
      extension [L](lhs: L)
        def +[RS <: Boolean, RW <: Int, RN <: NativeType, RP, R <: DFValTP[DFXInt[RS, RW, RN], RP]](
            rhs: R
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out],
            icR: Candidate[R]
        )(using
            dfc: DFC,
            check: ArithCheck[
              icR.OutS,
              icR.OutW,
              icR.OutN,
              icL.OutS,
              icL.OutW,
              icL.OutN,
              icL.IsScalaInt,
              true
            ]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | icR.OutP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          val rhsVal = icR(rhs)(using dfc.anonymize)
          check(rhsVal, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.+, List(lhsVal, rhsVal))
        }
        def -[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, RN, icL.OutS, icL.OutW, icL.OutN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.-, List(lhsVal, rhs))
        }
        def *[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, RN, icL.OutS, icL.OutW, icL.OutN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.`*`, List(lhsVal, rhs))
        }
        def /[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, RN, icL.OutS, icL.OutW, icL.OutN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp./, List(lhsVal, rhs))
        }
        def %[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: ArithCheck[RS, RW, RN, icL.OutS, icL.OutW, icL.OutN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW, icL.OutN], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.%, List(lhsVal, rhs))
        }
        def +^[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, RW] + 1, BitAccurate], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsVal.dfType.width.value max rhs.dfType.width) + 1
          )
          val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
          DFVal.Func(dfType, FuncOp.+, List(lhsVal, rhs))
        }
        def -^[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, Max[icL.OutW, RW] + 1, BitAccurate], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[Max[icL.OutW, RW] + 1](
            (lhsVal.dfType.width.value max rhs.dfType.width) + 1
          )
          val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
          DFVal.Func(dfType, FuncOp.-, List(lhsVal, rhs))
        }
        end -^
        def *^[RS <: Boolean, RW <: Int, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using sL: Exact.Summon[L, lhs.type])(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, icL.OutW + RW, BitAccurate], icL.OutP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          val width = Inlined.forced[icL.OutW + RW](
            lhsVal.dfType.width.value + rhs.dfType.width
          )
          val dfType = DFXInt.fromInlined(lhsVal.dfType.signed, width, BitAccurate)
          DFVal.Func(dfType, FuncOp.`*`, List(lhsVal, rhs))
        }
        end *^
      end extension
    end Ops
  end Val
end DFXInt

type DFUInt[W <: Int] = DFXInt[false, W, BitAccurate]
object DFUInt:
  def apply[W <: Int](width: IntParam[W])(using DFC, Width.Check[false, W]): DFUInt[W] =
    DFXInt(false, width, BitAccurate)
  def fromInlined[W <: Int](width: Inlined[W])(using DFC, Width.Check[false, W]): DFUInt[W] =
    DFXInt.fromInlined(false, width, BitAccurate)
  def apply[W <: Int](using dfType: DFUInt[W]): DFUInt[W] = dfType
  def until[V <: Int](sup: Inlined[V])(using
      dfc: DFC,
      check: Arg.LargerThan1.Check[V],
      info: IntInfo[V - 1]
  ): DFUInt[info.OutW] =
    check(sup)
    DFXInt.fromInlined(false, info.width(sup - 1), BitAccurate)
  def max[V <: Int](max: Inlined[V])(using
      dfc: DFC,
      check: Arg.Positive.Check[V],
      info: IntInfo[V]
  ): DFUInt[info.OutW] =
    check(max)
    DFXInt.fromInlined(false, info.width(max), BitAccurate)

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
          DFVal.Const(DFUInt.fromInlined(ubInfo.width(fixme)), Some(BigInt(arg)))
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
          DFVal.Alias.AsIs(DFSInt.fromInlined(lhs.width + 1), lhs)
        }
        @targetName("negateDFUInt")
        def unary_-(using DFC): DFValTP[DFSInt[W + 1], P] = trydf {
          import DFSInt.Val.Ops.unary_- as negate
          lhs.signed.negate
        }
        @targetName("toIntDFUInt")
        def toInt(using
            dfc: DFC,
            check: `W <= 31`.Check[W]
        ): DFValTP[DFInt32, P] = trydf {
          check(lhs.width)
          DFVal.Alias.AsIs(DFInt32, lhs.signed)
        }
      end extension
    end Ops
  end Val

end DFUInt

type DFSInt[W <: Int] = DFXInt[true, W, BitAccurate]
object DFSInt:
  def apply[W <: Int](width: IntParam[W])(using DFC, Width.Check[true, W]): DFSInt[W] =
    DFXInt(true, width, BitAccurate)
  def fromInlined[W <: Int](width: Inlined[W])(using DFC, Width.Check[true, W]): DFSInt[W] =
    DFXInt.fromInlined(true, width, BitAccurate)
  def apply[W <: Int](using dfc: DFC, dfType: DFSInt[W]): DFSInt[W] = dfType

  object Val:
    object Ops:
      extension [W <: Int, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("negateDFSInt")
        def unary_-(using DFC): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
      extension [P](lhs: DFValTP[DFInt32, P])
        @targetName("negateDFInt32")
        def unary_-(using DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
      extension [W <: Int, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("toIntDFSInt")
        def toInt(using
            dfc: DFC,
            check: `W <= 32`.Check[W]
        ): DFValTP[DFInt32, P] = trydf {
          check(lhs.width)
          DFVal.Alias.AsIs(DFInt32, lhs)
        }
    end Ops
  end Val
end DFSInt

//a native Int32 decimal has no explicit Scala compile-time width, since the
//actual value determines its width
type DFInt32 = DFDecimal[true, 32, 0, Int32]
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
