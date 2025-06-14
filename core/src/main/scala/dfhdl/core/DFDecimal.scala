package dfhdl.core
import dfhdl.compiler.ir
import dfhdl.internals.*
import ir.DFVal.Func.{Op => FuncOp}
import ir.DFDecimal.NativeType
import NativeType.{valueOf => _, *}
import scala.runtime.RichInt

import scala.quoted.*
import scala.annotation.targetName
import DFDecimal.Constraints.*

type DFDecimal[S <: Boolean, W <: IntP, F <: Int, N <: NativeType] =
  DFType[ir.DFDecimal, Args4[S, W, F, N]]
object DFDecimal:
  protected[core] def apply[S <: Boolean, W <: IntP, F <: Int, N <: NativeType](
      signed: Inlined[S],
      width: IntParam[W],
      fractionWidth: Inlined[F],
      nativeType: N
  )(using dfc: DFC, check: Width.CheckNUB[S, W]): DFDecimal[S, W, F, N] = trydf:
    check(signed, width)
    ir.DFDecimal(signed, width.ref, fractionWidth, nativeType).asFE[DFDecimal[S, W, F, N]]
  protected[core] def forced[S <: Boolean, W <: IntP, F <: Int, N <: NativeType](
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
  given [S <: Boolean, W <: IntP & Singleton, F <: Int, N <: NativeType](using
      ValueOf[S],
      ValueOf[W],
      ValueOf[F],
      ValueOf[N]
  )(using DFC, Width.CheckNUB[S, W]): DFDecimal[S, W, F, N] = trydf:
    DFDecimal(valueOf[S], IntParam[W](valueOf[W]), valueOf[F], valueOf[N])
  object Extensions:
    extension [S <: Boolean, W <: IntP, F <: Int, N <: NativeType](dfType: DFDecimal[S, W, F, N])
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
        checkVAW: `ValW >= ArgW`.Check[ValWI, ITE[ArgIsInt, argWFix.Out, 0]],
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
        ValW <: IntP,
        ValN <: NativeType,
        ArgS <: Boolean,
        ArgW <: IntP,
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
        ValW <: IntP,
        ValWI <: Int,
        ValN <: NativeType,
        ArgS <: Boolean,
        ArgW <: IntP,
        ArgWI <: Int,
        ArgN <: NativeType,
        ArgIsInt <: Boolean,
        Castle <: Boolean
    ](using
        ubv: UBound.Aux[Int, ValW, ValWI],
        uba: UBound.Aux[Int, ArgW, ArgWI],
        argWFix: Id[
          ITE[ArgIsInt && ![Castle] && ValS && ![ArgS], ArgWI + 1, ArgWI]
        ],
        skipSignChecks: Id[ArgIsInt && ![Castle] && (ValS || ![ArgS])]
    )(using
        ls: Id[ITE[Castle, ArgS, ValS]],
        rs: Id[ITE[Castle ^ skipSignChecks.Out, ValS, ArgS]],
        lw: Id[ITE[Castle, argWFix.Out, ValWI]],
        rw: Id[ITE[Castle, ValWI, argWFix.Out]]
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
        val dfValWidth = dfVal.dfType.widthInt
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
    private[DFDecimal] val widthIntExp = "(\\d+)'(-?\\d+)".r
    private[DFDecimal] val widthFixedExp = "(\\d+)\\.(\\d+)'(-?\\d+)\\.?(\\d*)".r
    private val intExp = "(-?\\d+)".r
    private[DFDecimal] def fromIntDecString(
        numStr: String,
        signedForced: Boolean
    ): (Boolean, Int, Int, BigInt) =
      val value = BigInt(numStr)
      val signed = value < 0 | signedForced
      val actualWidth = value.bitsWidth(signed)
      (signed, actualWidth, 0, value)
    private def fromDecString(
        dec: String,
        signedForced: Boolean
    ): Either[String, (Boolean, Int, Int, BigInt)] =
      dec.replace(",", "").replace("_", "") match
        case intExp(numStr) => Right(fromIntDecString(numStr, signedForced))
        case _              =>
          Left(s"Invalid decimal pattern found: $dec")
      end match
    end fromDecString

    extension (fullTerm: String)
      private[DFDecimal] def interpolate[S <: Boolean, W <: IntP, F <: Int](
          op: String,
          explicitWidthOption: Option[IntP]
      )(using DFC): DFConstOf[DFDecimal[S, W, F, BitAccurate]] =
        val (interpSigned, interpWidth, interpFractionWidth, interpValue) =
          fromDecString(fullTerm, op == "sd").toOption.get
        val signed = Inlined.forced[S](interpSigned)
        val width = IntParam.forced[W](explicitWidthOption.getOrElse(interpWidth))
        val fractionWidth = Inlined.forced[F](interpFractionWidth)
        DFVal.Const(
          DFDecimal(signed, width, fractionWidth, BitAccurate),
          Some(interpValue),
          named = true
        )
    end extension

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private[DFDecimal] def interpolate(
          opExpr: Expr[String],
          explicitWidthOptionExpr: Expr[Option[IntP]]
      ): Expr[DFConstAny] =
        import quotes.reflect.*
        val explicitWidthTpeOption: Option[TypeRepr] = explicitWidthOptionExpr match
          case '{ Some($expr) } => Some(expr.asTerm.tpe)
          case _                => None
        val signedForced = opExpr.value.get == "sd"
        val (signedTpe, interpWidthTpe, fractionWidthTpe): (TypeRepr, TypeRepr, TypeRepr) =
          fullTerm match
            case Literal(StringConstant(t)) =>
              fromDecString(t, signedForced) match
                case Right((signed, width, fractionWidth, _)) =>
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
                    ConstantType(IntConstant(width)),
                    ConstantType(IntConstant(fractionWidth))
                  )
                case Left(msg) =>
                  report.errorAndAbort(msg)
            case _ => (TypeRepr.of[Boolean], TypeRepr.of[Int], TypeRepr.of[Int])
        val widthTpe: TypeRepr = explicitWidthTpeOption.getOrElse(interpWidthTpe)
        val signedType = signedTpe.asTypeOf[Boolean]
        val widthType = widthTpe.asTypeOf[IntP]
        val fractionWidthType = fractionWidthTpe.asTypeOf[Int]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          val dfc = compiletime.summonInline[DFC]
          $fullExpr.interpolate[
            signedType.Underlying,
            widthType.Underlying,
            fractionWidthType.Underlying
          ](
            $opExpr,
            $explicitWidthOptionExpr
          )(using dfc)
        }
      end interpolate
    end extension
  end StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.{fromDecString, interpolate, widthIntExp}
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
        *   - `width`, followed by a `'`, is optional and specifies the exact width of the integer's
        *     bit representation. If omitted, the width is inferred from the value's size. If
        *     specified, the output is padded with zeros or extended for signed numbers using two's
        *     complement representation to match the `width`.
        *   - The output type is unsigned `UInt[W]` for natural numbers and signed `SInt[W]` for
        *     negative numbers, where `W` is the width in bits.
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
        *   - `width`, followed by a `'`, is optional and specifies the exact width of the integer's
        *     bit representation, which is always at least 2 bits to accommodate the sign bit.
        *   - The output is always a signed integer type `SInt[W]`, regardless of whether the `dec`
        *     value is negative or natural, where `W` is the width in bits.
        *   - If the specified `width` is less than the required number of bits to represent the
        *     value including the sign bit, an error occurs.
        *
        * @example
        *   {{{
        *   sd"0"     // SInt[2], value = 0 (natural number represented as a signed type)
        *   sd"-1"    // SInt[2], value = -1
        *   sd"255"   // SInt[9], value = 255 (natural number represented as a signed type)
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
      import quotes.reflect.*
      var Varargs(argsExprs) = args: @unchecked

      var parts = sc.parts.map(_.value.get).toList
      var explicitWidthOption: Expr[Option[IntP]] = '{ None }
      parts match
        case "" :: p :: _ if p.startsWith("'") =>
          argsExprs.headOption.map(_.asTerm) match
            case Some(t) =>
              t.tpe.asType match
                case '[IntP] =>
                  argsExprs = argsExprs.drop(1)
                  parts = p.drop(1) :: parts.drop(2)
                  explicitWidthOption = '{ Some(${ t.asExprOf[IntP] }) }
                case '[DFValAny] =>
                  report.errorAndAbort(
                    s"Expecting a constant DFHDL Int value but found: `${t.tpe.showType}`",
                    t.pos
                  )
                case _ =>
                  report.errorAndAbort(
                    s"Unsupported type as the width interpolation argument. Found: `${t.tpe.showType}`",
                    t.pos
                  )

            case _ =>
        case widthIntExp(widthStr, wordStr) :: rest =>
          parts = wordStr :: rest
          explicitWidthOption = '{ Some(${ Expr(widthStr.toInt) }) }
        case _ =>
      end match
      // println(widthParamOption.map(_.show))
      parts.map(Expr(_)).scPartsWithArgs(argsExprs).interpolate(
        Expr(sc.funcName),
        explicitWidthOption
      )
    end applyMacro

    private def unapplySeqMacro[T <: DFTypeAny](
        sc: Expr[DecStrCtx],
        arg: Expr[DFValOf[T]]
    )(using Quotes, Type[T]): Expr[Option[Seq[DFValOf[T]]]] =
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
          case widthIntExp(widthStr, wordStr) =>
            Literal(StringConstant(wordStr)).interpolate(
              opExpr,
              '{ Some(${ Expr(widthStr.toInt) }) }
            )
          case _ => parts.head.asTerm.interpolate(opExpr, '{ None })
        val dfValType = dfVal.asTerm.tpe.asTypeOf[DFConstAny]
        '{
          val dfc = compiletime.summonInline[DFC]
          val tc = compiletime.summonInline[
            DFVal.Compare[T, dfValType.Underlying, FuncOp.===.type, false]
          ]
          Some(
            Seq(
              trydf(
                tc.conv(${ arg }.dfType, $dfVal)(using dfc)
              )(using dfc, CTName($opExpr))
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
        `LW >= RW`(dfType.widthInt, dfVal.widthInt)
        `LS >= RS`(dfType.signed, dfVal.dfType.signed)
        dfVal
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
      type OutSMask <: Boolean
      type OutWMask <: IntP
      type IsScalaInt <: Boolean
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
        type OutSMask = false
        type OutWMask = W
        type IsScalaInt = false
        def apply(arg: R)(using dfc: DFC): Out =
          import DFBits.Val.Ops.uint
          val dfVal = ic(arg)(using dfc.anonymize)
          val ret =
            if (dfVal.hasTag[ir.TruncateTag])
              dfVal.uint.tag(ir.TruncateTag)
            else if (dfVal.hasTag[ir.ExtendTag])
              dfVal.uint.tag(ir.ExtendTag)
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
      type AuxM[R, S <: Boolean, W <: IntP, N <: NativeType, P, SMask <: Boolean, WMask <: IntP] =
        Aux[R, S, W, N, P] {
          type OutSMask = SMask
          type OutWMask = WMask
        }
      given fromInt[R <: Int, OS <: Boolean, OW <: Int](using
          info: IntInfo.Aux[R, OS, OW]
      ): Candidate[R] with
        type OutS = OS
        type OutW = OW
        type OutN = BitAccurate
        type OutP = CONST
        type OutSMask = OS
        type OutWMask = OW
        type IsScalaInt = true
        def apply(arg: R)(using dfc: DFC): Out =
          val dfType = DFXInt(info.signed(arg), info.width(arg), BitAccurate)
          DFVal.Const(dfType, Some(BigInt(arg)), named = true)
      // when the candidate is a DFInt32 we remove the signed and width tags
      // from the type to allow for elaboration (runtime) check where values
      // are constant and known.
      given fromDFConstInt32[P, R <: DFValTP[DFInt32, P]]: Candidate[R] with
        type OutS = true
        type OutW = 32
        type OutN = Int32
        type OutP = P
        type OutSMask = Boolean
        type OutWMask = Int
        type IsScalaInt = false
        def apply(arg: R)(using DFC): Out = arg
      given fromDFXIntVal[S <: Boolean, W <: IntP, N <: NativeType, P, R <: DFValTP[
        DFXInt[S, W, N],
        P
      ]]: Candidate[R] with
        type OutS = S
        type OutW = W
        type OutN = N
        type OutP = P
        type OutSMask = S
        type OutWMask = W
        type IsScalaInt = false
        def apply(arg: R)(using DFC): Out = arg
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a DFHDL decimal variable."
        )
    end Candidate

    extension [S <: Boolean, W <: IntP, N <: NativeType](dfVal: DFValOf[DFXInt[S, W, N]])
      private[core] def getActualSignedWidth(using dfc: DFC): (Boolean, Int) =
        val int32Data: Option[Int] =
          if (dfVal.dfType.asIR.isDFInt32)
            import dfc.getSet
            dfVal.asIR.getConstData match
              case Some(Some(n: BigInt)) => Some(n.toInt)
              case _                     => None
          else None
        int32Data match
          case Some(int) => (int < 0, IntInfo.calcWidth(int))
          case None      => (dfVal.dfType.signed.value, dfVal.dfType.widthInt.value)

    object TC:
      def apply(
          dfType: DFXInt[Boolean, Int, NativeType],
          dfVal: DFValOf[DFXInt[Boolean, Int, NativeType]]
      )(using DFC): DFValOf[DFXInt[Boolean, Int, NativeType]] =
        val check = summon[TCCheck[Boolean, Int, Boolean, Int]]
        check(dfType.signed, dfType.widthInt, dfVal.dfType.signed, dfVal.dfType.widthInt)
        dfVal
      import DFVal.TC
      given [LS <: Boolean, LW <: IntP, LN <: NativeType, R, IC <: Candidate[R]](using
          ic: IC
      )(using
          check: TCCheck[LS, LW, ic.OutSMask, ic.OutWMask]
      ): TC[DFXInt[LS, LW, LN], R] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW, LN], value: R)(using dfc: DFC): Out =
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          val (rhsSigned, rhsWidth) = rhs.getActualSignedWidth
          if (!rhs.hasTag[ir.TruncateTag] || dfType.signed != rhsSigned)
            check(dfType.signed, dfType.widthInt, rhsSigned, rhsWidth)
          DFXInt.Val.Ops.toDFXIntOf(rhs)(dfType).asValTP[DFXInt[LS, LW, LN], ic.OutP]
        end conv
      end given
    end TC

    object TCConv:
      import DFVal.TCConv
      given [LS <: Boolean, R, IC <: Candidate[R]](using
          ic: IC
      )(using
          checkS: `LS >= RS`.Check[LS, ic.OutSMask],
          lsigned: OptionalGiven[ValueOf[LS]]
      ): TCConv[DFXInt[LS, Int, BitAccurate], R] with
        type OutP = ic.OutP
        def apply(value: R)(using dfc: DFC): Out =
          import DFUInt.Val.Ops.signed
          val rhs = ic(value)
          checkS(lsigned.get.value, rhs.dfType.signed)
          if (lsigned.get.value != rhs.dfType.signed.value)
            rhs.asValOf[DFUInt[Int]].signed.asValTP[DFXInt[LS, Int, BitAccurate], ic.OutP]
          else rhs.asValTP[DFXInt[LS, Int, BitAccurate], ic.OutP]
      end given
    end TCConv

    object Compare:
      import DFVal.Compare
      given DFXIntCompare[
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          R,
          IC <: Candidate[R],
          Op <: FuncOp,
          C <: Boolean
      ](using
          ic: IC
      )(using
          check: CompareCheck[LS, LW, ic.OutSMask, ic.OutWMask, ic.IsScalaInt, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFXInt[LS, LW, LN], R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFXInt[LS, LW, LN], arg: R)(using dfc: DFC): Out =
          given dfcAnon: DFC = dfc.anonymize
          val dfValArg = ic(arg)
          val (rhsSigned, rhsWidth) = dfValArg.getActualSignedWidth
          check(
            dfType.signed,
            dfType.widthInt,
            rhsSigned,
            rhsWidth
          )
          DFXInt.Val.Ops.toDFXIntOf(dfValArg)(dfType).asValTP[DFXInt[LS, LW, LN], ic.OutP]
        end conv
      end DFXIntCompare
    end Compare

    object Ops:
      export DFUInt.Val.Ops.*
      export DFSInt.Val.Ops.*
      export dfhdl.internals.clog2
      def clog2[P, S <: Boolean, W <: IntP, N <: NativeType](
          dfVal: DFValTP[DFXInt[S, W, N], P]
      )(using
          DFC,
          DFVal.ConstCheck[P]
      ): DFValTP[DFXInt[S, W, N], P] =
        DFVal.Func(dfVal.dfType, FuncOp.clog2, List(dfVal))
      extension [P, S <: Boolean, W <: IntP, N <: NativeType](lhs: DFValTP[DFXInt[S, W, N], P])
        protected[core] def toDFXIntOf[RS <: Boolean, RW <: IntP, RN <: NativeType](
            dfType: DFXInt[RS, RW, RN]
        )(using dfc: DFC): DFValTP[DFXInt[RS, RW, RN], P] =
          import dfc.getSet
          val dfValIR =
            val (lhsSigned, lhsWidth) = lhs.getActualSignedWidth
            if (dfType.asIR.isDFInt32 && lhs.dfType.asIR.isDFInt32) lhs.asIR
            else
              val lhsSignFix: DFValOf[DFSInt[Int]] =
                if (dfType.signed != lhsSigned)
                  lhs.asValOf[DFUInt[Int]].signed.asValOf[DFSInt[Int]]
                else lhs.asValOf[DFSInt[Int]]
              val nativeTypeChanged = dfType.nativeType != lhs.dfType.nativeType
              if (nativeTypeChanged)
                dfType.asIR.nativeType match
                  case Int32 =>
                    lhsSignFix.toInt.asIR
                  case BitAccurate =>
                    DFVal.Alias.AsIs(dfType, lhsSignFix).asIR
              else if (!dfType.asIR.widthParamRef.isSimilarTo(lhsSignFix.dfType.asIR.widthParamRef))
                lhsSignFix.resize(dfType.widthIntParam).asIR
              else lhsSignFix.asIR
              end if
            end if
          end dfValIR
          dfValIR.asValTP[DFXInt[RS, RW, RN], P]
        end toDFXIntOf
        def toScalaInt(using DFC, DFVal.ConstCheck[P]): Int =
          lhs.toScalaValue.toInt
        def toScalaBigInt(using DFC, DFVal.ConstCheck[P]): BigInt =
          lhs.toScalaValue
      end extension
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
      extension [S <: Boolean, W <: IntP, N <: NativeType, P](lhs: DFValTP[DFXInt[S, W, N], P])
        @targetName("truncateDFXInt")
        def truncate(using DFC): DFValTP[DFXInt[S, Int, N], P] =
          lhs.tag(ir.TruncateTag).asValTP[DFXInt[S, Int, N], P]
        @targetName("resizeDFXInt")
        def resize[RW <: IntP](
            updatedWidth: IntParam[RW]
        )(using
            dfc: DFC,
            check: Width.CheckNUB[S, RW]
        ): DFValTP[DFXInt[S, RW, BitAccurate], P] = trydf {
          val signed = lhs.dfType.signed
          check(signed, updatedWidth)
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFXInt[S, RW, BitAccurate]]
//          else
          DFVal.Alias.AsIs(DFXInt(signed, updatedWidth, BitAccurate), lhs)
        }
        end resize
        @targetName("shiftRightDFXInt")
        def >>(shift: DFUInt.Val.UBArg.Exact[W])(using
            dfc: DFC
        ): DFValTP[DFXInt[S, W, N], P | shift.tc.OutP] = trydf {
          val shiftVal = shift(lhs.widthIntParam)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.>>, List(lhs, shiftVal))
        }
        @targetName("shiftLeftDFXInt")
        def <<(shift: DFUInt.Val.UBArg.Exact[W])(using
            dfc: DFC
        ): DFValTP[DFXInt[S, W, N], P | shift.tc.OutP] = trydf {
          val shiftVal = shift(lhs.widthIntParam)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.<<, List(lhs, shiftVal))
        }
      end extension

      extension [L <: Int](lhs: L)
        def <[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.<.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def <=[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.<=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.>.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def >=[RS <: Boolean, RW <: Int, RN <: NativeType](
            rhs: DFValOf[DFXInt[RS, RW, RN]]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            op: DFVal.Compare[DFXInt[RS, RW, RN], es.Out, FuncOp.>=.type, true]
        ): DFValOf[DFBool] = trydf { op(rhs, es(lhs)) }
        def <<[P](shift: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.<<, List(DFConstInt32(lhs), shift)).asValTP[DFInt32, P]
        }
        def >>[P](shift: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, CONST | P] = trydf {
          DFVal.Func(DFInt32, FuncOp.>>, List(DFConstInt32(lhs), shift)).asValTP[DFInt32, P]
        }
        def +[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.+, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def -[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.-, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def *[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.`*`, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def /[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp./, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def %[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.%, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def max[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.max, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def min[P](rhs: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.min, List(DFConstInt32(lhs), rhs)).asValTP[DFInt32, P]
        }
        def **[P](shift: DFValTP[DFInt32, P])(using dfc: DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(DFInt32, FuncOp.**, List(DFConstInt32(lhs), shift)).asValTP[DFInt32, P]
        }
      end extension
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
        DFVal.Func(dfType, op, List(lhs, rhsFix))
      end arithOp
      extension [
          L <: DFValAny,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          R,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](lhs: L)(using icL: Candidate.Aux[L, LS, LW, LN, LP])
        def +(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.+, lhsVal, rhsVal)
        }
        def -(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.-, lhsVal, rhsVal)
        }
        def *(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
        def /(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp./, lhsVal, rhsVal)
        }
        def %(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.%, lhsVal, rhsVal)
        }
        def max(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.max, lhsVal, rhsVal)
        }
        def min(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: ArithCheck[LS, LW, LN, RS, RW, RN, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal, rhsVal)
          arithOp(lhsVal.dfType, FuncOp.min, lhsVal, rhsVal)
        }
        def +^(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: SignCheck[LS, RS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, IntP.+[IntP.Max[LW, RW], 1], BitAccurate], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          import IntParam.{+, max}
          val width = lhsVal.widthIntParam.max(rhsVal.widthIntParam) + 1
          val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
          arithOp(dfType, FuncOp.+, lhsVal, rhsVal)
        }
        def -^(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: SignCheck[LS, RS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, IntP.+[IntP.Max[LW, RW], 1], BitAccurate], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          import IntParam.{+, max}
          val width = lhsVal.widthIntParam.max(rhsVal.widthIntParam) + 1
          val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
          arithOp(dfType, FuncOp.-, lhsVal, rhsVal)
        }
        def *^(rhs: Candidate.ExactAux[R])(using
            dfc: DFC,
            icR: Candidate.Aux[R, RS, RW, RN, RP]
        )(using
            check: SignCheck[LS, RS, icR.IsScalaInt, false]
        ): DFValTP[DFXInt[LS, IntP.+[LW, RW], BitAccurate], LP | RP] = trydf {
          val dfcAnon = dfc.anonymize
          val lhsVal = icL(lhs)(using dfcAnon)
          val rhsVal = icR(rhs)(using dfcAnon)
          check(lhsVal.dfType.signed, rhsVal.dfType.signed)
          import IntParam.+
          val width = lhsVal.widthIntParam + rhsVal.widthIntParam
          val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
          arithOp(dfType, FuncOp.`*`, lhsVal, rhsVal)
        }
      end extension
      extension [
          L <: Int,
          LE,
          LS <: Boolean,
          LW <: IntP,
          LN <: NativeType,
          LP,
          RS <: Boolean,
          RW <: IntP,
          RN <: NativeType,
          RP
      ](lhs: L)
        def +(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.+, List(lhsVal, rhs))
        }
        def -(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.-, List(lhsVal, rhs))
        }
        def *(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.`*`, List(lhsVal, rhs))
        }
        def /(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp./, List(lhsVal, rhs))
        }
        def %(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.%, List(lhsVal, rhs))
        }
        def max(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.max, List(lhsVal, rhs))
        }
        // to restore default max functionality that is overridden
        def max(rhs: Int): Int = RichInt(lhs) max rhs
        def min(rhs: DFValTP[DFXInt[RS, RW, RN], RP])(using
            dfc: DFC,
            sL: Exact.Summon.Aux[L, lhs.type, LE]
        )(using
            icL: Candidate.Aux[LE, LS, LW, LN, LP]
        )(using
            check: ArithCheck[RS, RW, RN, LS, LW, LN, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[LS, LW, LN], LP | RP] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs, lhsVal)
          DFVal.Func(lhsVal.dfType, FuncOp.min, List(lhsVal, rhs))
        }
        // to restore default min functionality that is overridden
        def min(rhs: Int): Int = RichInt(lhs) min rhs
      end extension
      extension [L <: Int](lhs: L)
        def +^[RS <: Boolean, RW <: IntP, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using
            sL: Exact.Summon[L, lhs.type]
        )(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[
          DFXInt[icL.OutS, IntP.+[IntP.Max[icL.OutW, RW], 1], BitAccurate],
          icL.OutP | RP
        ] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          import IntParam.{+, max}
          val width = lhsVal.widthIntParam.max(rhs.widthIntParam) + 1
          val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
          DFVal.Func(dfType, FuncOp.+, List(lhsVal, rhs))
        }
        def -^[RS <: Boolean, RW <: IntP, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using
            sL: Exact.Summon[L, lhs.type]
        )(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[
          DFXInt[icL.OutS, IntP.+[IntP.Max[icL.OutW, RW], 1], BitAccurate],
          icL.OutP | RP
        ] = trydf {
          val lhsVal = icL(sL(lhs))(using dfc.anonymize)
          check(rhs.dfType.signed, lhsVal.dfType.signed)
          import IntParam.{+, max}
          val width = lhsVal.widthIntParam.max(rhs.widthIntParam) + 1
          val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
          DFVal.Func(dfType, FuncOp.-, List(lhsVal, rhs))
        }
        end -^
        def *^[RS <: Boolean, RW <: IntP, RN <: NativeType, RP](
            rhs: DFValTP[DFXInt[RS, RW, RN], RP]
        )(using
            sL: Exact.Summon[L, lhs.type]
        )(using
            icL: Candidate[sL.Out]
        )(using
            dfc: DFC,
            check: SignCheck[RS, icL.OutS, icL.IsScalaInt, true]
        ): DFValTP[DFXInt[icL.OutS, IntP.+[icL.OutW, RW], BitAccurate], icL.OutP | RP] =
          trydf {
            val lhsVal = icL(sL(lhs))(using dfc.anonymize)
            check(rhs.dfType.signed, lhsVal.dfType.signed)
            import IntParam.+
            val width = lhsVal.widthIntParam + rhs.widthIntParam
            val dfType = DFXInt(lhsVal.dfType.signed, width, BitAccurate)
            DFVal.Func(dfType, FuncOp.`*`, List(lhsVal, rhs))
          }
        end *^
      end extension
    end Ops
  end Val
end DFXInt

type DFUInt[W <: IntP] = DFXInt[false, W, BitAccurate]
object DFUInt:
  def apply[W <: IntP](width: IntParam[W])(using DFC, Width.CheckNUB[false, W]): DFUInt[W] =
    DFXInt(false, width, BitAccurate)
  def apply[W <: IntP](using dfc: DFC, dfType: => DFUInt[W]): DFUInt[W] = trydf { dfType }
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFC,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFUInt[IntP.CLog2[V]] =
    DFXInt(false, sup.clog2, BitAccurate)
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFC,
      check: Arg.Positive.CheckNUB[V]
  ): DFUInt[IntP.CLog2[IntP.+[V, 1]]] =
    check(max)
    DFXInt(false, (max + 1).clog2, BitAccurate)

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
      type Exact[UB <: IntP] = Exact1[IntP, UB, [ub <: IntP] =>> IntParam[ub], DFC, UBArg]
      given fromInt[UB <: Int, R <: Int](using
          unsignedCheck: Unsigned.Check[R < 0],
          ubCheck: `UB > R`.CheckNUB[UB, R]
      ): UBArg[UB, R] with
        type OutP = CONST
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out =
          unsignedCheck(arg < 0)
          ubCheck(ub, arg)
          DFConstInt32(arg)
      end fromInt
      given fromR[
          UB <: IntP,
          R,
          S <: Boolean,
          W <: IntP,
          N <: NativeType,
          P,
          SMask <: Boolean,
          WMask <: IntP
      ](using
          ic: DFXInt.Val.Candidate.AuxM[R, S, W, N, P, SMask, WMask]
      )(using
          unsignedCheck: Unsigned.Check[SMask],
          widthCheck: `UBW == RW`.CheckNUB[IntP.CLog2[UB], WMask]
      ): UBArg[UB, R] with
        type OutP = ic.OutP
        def apply(ub: IntParam[UB], arg: R)(using DFC): Out =
          import dfc.getSet
          val argVal = ic(arg)
          // if the argument is a constant, we can check its value and width
          argVal.asIR.getConstData match
            case Some(Some(arg: BigInt)) =>
              unsignedCheck(arg < 0)
              summon[`UB > R`.CheckNUB[UB, Int]](ub, arg.toInt)
            case _ =>
              // skip checks if the argument is an Int32.
              // TODO: in the future, it's worth considering adding assertions
              if (argVal.asIR.dfType != ir.DFInt32)
                unsignedCheck(argVal.dfType.signed)
                widthCheck(ub.clog2, argVal.widthInt)
          DFVal.Alias.AsIs(DFInt32, argVal)
        end apply
      end fromR
    end UBArg
    object Ops:
      extension [W <: IntP, P](lhs: DFValTP[DFUInt[W], P])
        def signed(using DFC): DFValTP[DFSInt[IntP.+[W, 1]], P] = trydf {
          DFVal.Alias.AsIs(DFSInt(lhs.widthIntParam + 1), lhs)
        }
        @targetName("negateDFUInt")
        def unary_-(using DFC): DFValTP[DFSInt[IntP.+[W, 1]], P] = trydf {
          import DFSInt.Val.Ops.unary_- as negate
          lhs.signed.negate
        }
        @targetName("toIntDFUInt")
        def toInt(using
            dfc: DFC,
            check: `W <= 31`.CheckNUB[W]
        ): DFValTP[DFInt32, P] = trydf {
          check(lhs.widthInt)
          DFVal.Alias.AsIs(DFInt32, lhs.signed)
        }
      end extension
    end Ops
  end Val

end DFUInt

type DFSInt[W <: IntP] = DFXInt[true, W, BitAccurate]
object DFSInt:
  def apply[W <: IntP](width: IntParam[W])(using DFC, Width.CheckNUB[true, W]): DFSInt[W] =
    DFXInt(true, width, BitAccurate)
  def apply[W <: IntP](using dfc: DFC, dfType: => DFSInt[W]): DFSInt[W] = trydf { dfType }

  object Val:
    object Ops:
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("negateDFSInt")
        def unary_-(using DFC): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
      extension [P](lhs: DFValTP[DFInt32, P])
        @targetName("negateDFInt32")
        def unary_-(using DFC): DFValTP[DFInt32, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_-, List(lhs))
        }
      extension [W <: IntP, P](lhs: DFValTP[DFSInt[W], P])
        @targetName("toIntDFSInt")
        def toInt(using
            dfc: DFC,
            check: `W <= 32`.CheckNUB[W]
        ): DFValTP[DFInt32, P] = trydf {
          check(lhs.widthInt)
          DFVal.Alias.AsIs(DFInt32, lhs)
        }
    end Ops
  end Val
end DFSInt

//a native Int32 decimal has no explicit Scala compile-time width, since the
//actual value determines its width.
type DFInt32 =
  DFType[ir.DFDecimal, Args4[true, 32, 0, Int32]] // This means: DFDecimal[true, 32, 0, Int32] (could not be defined this way because of type recursion)
final val DFInt32 = ir.DFInt32.asFE[DFInt32]
type DFConstInt32 = DFConstOf[DFInt32]
object DFConstInt32:
  def apply(int: Int, named: Boolean = false)(using DFC): DFConstInt32 =
    DFVal.Const(DFInt32, Some(BigInt(int)), named)
