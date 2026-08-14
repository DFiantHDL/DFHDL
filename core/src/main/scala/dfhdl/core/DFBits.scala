package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName, nowarn}
import scala.quoted.*
import scala.util.boundary, boundary.break
import DFDecimal.Constraints.{`LW == RW`, equalWidthCheck}

type DFBitsWL[W <: IntP, L <: IntP] = DFType[ir.DFBitsWL, Args2[W, L]]
type DFBits[W <: IntP] = DFBitsWL[W, 0]

// internal width+low constructor; the user-facing spelling is the high/low-indexed DFBitsHL
object DFBitsWL:
  def apply[W <: IntP, L <: IntP](width: IntParam[W], lowIdx: IntParam[L])(using
      dfc: DFCG,
      widthCheck: Arg.Width.CheckNUB[W],
      lowCheck: Arg.Natural.CheckNUB[L]
  ): DFBitsWL[W, L] = trydf {
    width.toScalaIntOpt.foreach(widthCheck(_))
    lowIdx.toScalaIntOpt.foreach(lowCheck(_))
    ir.DFBitsWL(width.ref, lowIdx.ref).asFE[DFBitsWL[W, L]]
  }(using dfc, CTName("BitsWL constructor"))
  def forced[W <: IntP, L <: IntP](width: Int, lowIdx: Int): DFBitsWL[W, L] =
    summon[Arg.Width.Check[Int]](width)
    summon[Arg.Natural.Check[Int]](lowIdx)
    ir.DFBitsWL(ir.IntParamRef(width), ir.IntParamRef(lowIdx)).asFE[DFBitsWL[W, L]]
  // the type-only spelling (e.g. a `BitsHL[9, 2] <> VAL` struct field)
  given [W <: IntP & Singleton, L <: IntP & Singleton](using
      dfc: DFCG,
      w: ValueOf[W],
      l: ValueOf[L],
      widthCheck: Arg.Width.CheckNUB[W],
      lowCheck: Arg.Natural.CheckNUB[L]
  ): DFBitsWL[W, L] = trydf {
    val width = IntParam.forced(w)
    val lowIdx = IntParam.forced(l)
    width.toScalaIntOpt.foreach(widthCheck(_))
    lowIdx.toScalaIntOpt.foreach(lowCheck(_))
    ir.DFBitsWL(width.ref, lowIdx.ref).asFE[DFBitsWL[W, L]]
  }(using dfc, CTName("BitsWL constructor"))
end DFBitsWL

type DFBitsHL[H <: IntP, L <: IntP] = DFBitsWL[IntP.RangeWidth[H, L], L]
object DFBitsHL:
  def apply[H <: IntP, L <: IntP](idxHigh: IntParam[H], idxLow: IntParam[L])(using
      dfc: DFCG,
      hiloCheck: DFBits.BitsHiLo.CheckNUB[H, L],
      lowCheck: Arg.Natural.CheckNUB[L]
  ): DFBitsHL[H, L] = trydf {
    (idxHigh.toScalaIntOpt, idxLow.toScalaIntOpt) match
      case (Some(idxHighInt), Some(idxLowInt)) => hiloCheck(idxHighInt, idxLowInt)
      case _                                   =>
    idxLow.toScalaIntOpt.foreach(lowCheck(_))
    ir.DFBitsWL((idxHigh - idxLow + 1).ref, idxLow.ref).asFE[DFBitsHL[H, L]]
  }(using dfc, CTName("BitsHL constructor"))
  // the type-only spelling in term position (e.g. `BitsHL[HI.type, LO.type] <> IN`). The
  // bounds are taken from the EXPLICIT type arguments: for non-literal bounds the spelled
  // TYPE immediately collapses its width to `Int` (the guarded-fold collapse), erasing `H`,
  // so this apply site is the one place the high bound is still recoverable
  def apply[H <: IntP & Singleton, L <: IntP & Singleton](using
      dfc: DFCG,
      h: ValueOf[H],
      l: ValueOf[L],
      hiloCheck: DFBits.BitsHiLo.CheckNUB[H, L],
      lowCheck: Arg.Natural.CheckNUB[L]
  ): DFBitsHL[H, L] = apply(IntParam.forced[H](h.value), IntParam.forced[L](l.value))
end DFBitsHL

object DFBits:
  def apply[W <: IntP](width: IntParam[W])(using
      dfc: DFCG,
      check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = trydf {
    width.toScalaIntOpt.foreach(check(_))
    ir.DFBits(width.ref).asFE[DFBits[W]]
  }(using dfc, CTName("Bits constructor"))
  def forced[W <: IntP](width: Int): DFBits[W] =
    val check = summon[Arg.Width.Check[Int]]
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  def apply[W <: IntP](using dfc: DFCG, dfType: => DFBits[W]): DFBits[W] =
    trydf { dfType }(using dfc, CTName("Bits constructor"))
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFCG,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFBits[IntP.CLog2[V]] = trydf {
    sup.toScalaIntOpt.foreach(check(_))
    ir.DFBits(sup.clog2.ref).asFE[DFBits[IntP.CLog2[V]]]
  }(using dfc, CTName("Bits.until constructor"))
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFCG,
      check: Arg.Positive.CheckNUB[V]
  ): DFBits[IntP.CLog2P1[V]] = trydf {
    max.toScalaIntOpt.foreach(check(_))
    ir.DFBits((max + 1).clog2.ref).asFE[DFBits[IntP.CLog2P1[V]]]
  }(using dfc, CTName("Bits.to constructor"))

  protected object `AW == TW`
      extends Check2[
        Int,
        Int,
        [AW <: Int, TW <: Int] =>> AW == TW,
        [AW <: Int, TW <: Int] =>> "The alias width (" + AW +
          ") is different than the DFHDL value width (" + TW + ")."
      ]
  protected object `LW >= RW`
      extends Check2[
        Int,
        Int,
        [LW <: Int, RW <: Int] =>> LW >= RW,
        [LW <: Int, RW <: Int] =>> "The new width (" + RW +
          ") is larger than the original width (" + LW + ")."
      ]
  protected[core] object BitIndex
      extends Check2[
        Int,
        Int,
        [I <: Int, W <: Int] =>> (I < W) && (I >= 0),
        [I <: Int, W <: Int] =>> "Index " + I + " is out of range of width/length " + W
      ]
  protected[core] object BitsHiLo
      extends Check2[
        Int,
        Int,
        [H <: Int, L <: Int] =>> H >= L,
        [H <: Int, L <: Int] =>> "Low index " + L + " is bigger than High bit index " + H
      ]
  // selection on a low-indexed bit vector uses ABSOLUTE indices, so the valid index
  // range is [L, L+W-1] rather than [0, W-1] (the latter stays with BitIndex above)
  protected[core] object BitIndexLow
      extends Check2[
        Int,
        Int,
        [I <: Int, L <: Int] =>> I >= L,
        [I <: Int, L <: Int] =>> "Index " + I + " is below the low index " + L +
          " of the selected value"
      ]
  protected[core] object BitIndexHigh
      extends Check2[
        Int,
        Int,
        [I <: Int, H <: Int] =>> I <= H,
        [I <: Int, H <: Int] =>> "Index " + I + " is above the high index " + H +
          " of the selected value"
      ]
  trait CompareCheck[
      ValW <: IntP,
      ArgW <: IntP,
      Castle <: Boolean // castling of dfVal and arg
  ]:
    def apply(dfValWidth: Int, argWidth: Int): Unit
  object CompareCheck:
    given [
        ValW <: IntP,
        ValWI <: Int,
        ArgW <: IntP,
        ArgWI <: Int,
        Castle <: Boolean
    ](using
        ubv: UBound.Aux[Int, ValW, ValWI],
        uba: UBound.Aux[Int, ArgW, ArgWI],
        lw: Id[ITE[Castle, ArgWI, ValWI]],
        rw: Id[ITE[Castle, ValWI, ArgWI]]
    )(using
        checkW: `LW == RW`.Check[lw.Out, rw.Out],
        castle: ValueOf[Castle]
    ): CompareCheck[ValW, ArgW, Castle] with
      def apply(dfValWidth: Int, argWidth: Int): Unit =
        val lw = if (castle) argWidth else dfValWidth
        val rw = if (castle) dfValWidth else argWidth
        checkW(lw, rw)
    end given
  end CompareCheck

  object StrInterp:
    private[DFBits] val widthExp = "([0-9]+)'(.*)".r
    private[DFBits] val isHex = "[0-9a-fA-F]".r
    extension (fullTerm: String)
      private[DFBits] def interpolate[W <: IntP](
          op: String,
          explicitWidthOption: Option[IntP]
      )(using DFC): DFConstOf[DFBits[W]] =
        val fromString = op match
          case "b" => ir.DFBits.dataFromBinString(fullTerm)
          case "h" => ir.DFBits.dataFromHexString(fullTerm)
        val (valueBits, bubbleBits) = fromString.toOption.get
        val valueWidth = IntParam.forced[W](valueBits.length.toInt)
        explicitWidthOption.map(IntParam.forced[W]) match
          // has explicit width parameter
          case Some(explicitWidthParam) => explicitWidthParam match
              // the parameter width that is a Scala Int and we can use it directly in defining the constant
              case explicitWidth: Int =>
                DFVal.Const(
                  DFBits(explicitWidthParam),
                  (valueBits.resize(explicitWidth), bubbleBits.resize(explicitWidth)),
                  named = true
                )
              // the parameter width is not a Scala Int, so we keep the original value width and add a resize operation
              case _ =>
                import DFBits.Val.Ops.resize
                DFVal.Const(DFBits(valueWidth), (valueBits, bubbleBits))
                  .resize(explicitWidthParam)
          // has no parameter, so we can directly use the inferred width
          case None =>
            DFVal.Const(DFBits(valueWidth), (valueBits, bubbleBits), named = true)
        end match
    end extension

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private[DFBits] def interpolate(
          opExpr: Expr[String],
          explicitWidthOptionExpr: Expr[Option[IntP]]
      )(dfc: Expr[DFC]): Expr[DFConstAny] =
        import quotes.reflect.*
        val explicitWidthTpeOption: Option[TypeRepr] = explicitWidthOptionExpr match
          case '{ Some($expr) } => Some(expr.asTerm.tpe)
          case _                => None
        val interpWidthTpe: TypeRepr = fullTerm match
          case Literal(StringConstant(t)) =>
            val opStr = opExpr.value.get
            val res = opStr match
              case "b" => ir.DFBits.dataFromBinString(t)
              case "h" => ir.DFBits.dataFromHexString(t)
            res match
              case Right((valueBits, bubbleBits)) =>
                explicitWidthTpeOption match
                  case Some(ConstantType(IntConstant(explicitWidth))) =>
                    val actualWidth = valueBits.lengthOfValue.toInt
                    if (explicitWidth < actualWidth)
                      report.errorAndAbort(
                        s"Explicit given width ($explicitWidth) is smaller than the actual width ($actualWidth)."
                      )
                  case _ =>
                ConstantType(IntConstant(valueBits.length.toInt))
              case Left(msg) =>
                report.errorAndAbort(msg)
          case _ => TypeRepr.of[Int]
        val widthTpe: TypeRepr = explicitWidthTpeOption.getOrElse(interpWidthTpe)
        val widthType = widthTpe.asTypeOf[IntP]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          $fullExpr.interpolate[widthType.Underlying](
            $opExpr,
            $explicitWidthOptionExpr
          )(using $dfc)
        }
    end extension

  end StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.{interpolate, isHex, widthExp}
    opaque type BinStrCtx <: StringContext = StringContext
    object BinStrCtx:
      extension (inline sc: BinStrCtx)
        transparent inline def apply(inline args: Any*)(using dfc: DFCG): Any =
          ${ applyMacro('sc, 'args)('dfc) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using dfc: DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('sc, 'arg)('dfc) }

    extension (sc: StringContext)
      /** Binary Bits Vector String Interpolator
        *
        * Syntax: {{{b"width'bin"}}}
        *   - `bin` is a sequence of '0', '1', and '?' characters, indicating a bit bubble.
        *   - Separators ' ' (space) or '_' (underscore) within `bin` are ignored.
        *   - `width`, followed by a `'`, is optional and specifies the bit vector's width. If
        *     omitted, the width is inferred from the sequence length. If specified, leading zeros
        *     are added or the sequence is truncated based on the `width`. Truncation only occurs if
        *     the most significant bits being removed are zeros; otherwise, it triggers a
        *     compilation error.
        *
        * @example
        *   {{{
        *   b"1"        // Value = 1
        *   b"1000"     // Value = 1000
        *   b"8'1000"   // Value = 00001000
        *   b"3'0100"   // Value = 100
        *   b"3'1100"   // Compilation error
        *   b"1?11"     // Value = 1?11 (? indicates a bit bubble)
        *   b"11_00"    // Value = 1100
        *   }}}
        *
        * @note
        *   This interpolator does not accept external arguments through `${arg}`.
        * @return
        *   A DFHDL Bits vector.
        */
      def b: BinStrCtx = sc

      /** Hexadecimal Bits Vector String Interpolator
        *
        * Syntax: {{{h"width'hex"}}}
        *   - `hex` is a sequence of hexadecimal characters ('0'-'9', 'A'-'F', 'a'-'f', and '?')
        *     where '?' indicates a 4-bit bubble. Each character represents a 4-bit nibble.
        *   - Separators ' ' (space) or '_' (underscore) within `hex` are ignored.
        *   - Binary sequences can be embedded within `{bin}` tags, allowing integration of binary
        *     bit sequences of any length, not necessarily divisible by 4, between hex nibbles.
        *   - `width`, followed by a `'`, is optional and specifies the bit vector's width. If
        *     omitted, the width is inferred from the sequence length. If specified, leading zeros
        *     are added or the sequence is truncated based on the `width`. Truncation only occurs if
        *     the most significant bits being removed are zeros; otherwise, it triggers a
        *     compilation error.
        *
        * @example
        *   {{{
        *   h"1"        // Value = 0001
        *   h"27"       // Value = 00100111
        *   h"6'27"     // Value = 100111
        *   h"5'27"     // Compilation error
        *   h"2?"       // Value = 0010????
        *   h"F{00}F"   // Value = 1111001111
        *   h"3_3"      // Value = 00110011
        *   }}}
        *
        * @note
        *   This interpolator does not accept external arguments through `${arg}`.
        * @return
        *   A DFHDL Bits vector.
        */
      def h: BinStrCtx = sc
    end extension

    private def applyMacro(
        sc: Expr[BinStrCtx],
        args: Expr[Seq[Any]]
    )(dfc: Expr[DFC])(using Quotes): Expr[DFConstAny] =
      import quotes.reflect.*
      var Varargs(argsExprs) = args.runtimeChecked

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
        case widthExp(widthStr, wordStr) :: rest =>
          parts = wordStr :: rest
          explicitWidthOption = '{ Some(${ Expr(widthStr.toInt) }) }
        case _ =>
      end match
      // println(widthParamOption.map(_.show))
      parts.map(Expr(_)).scPartsWithArgs(argsExprs).interpolate(
        Expr(sc.funcName),
        explicitWidthOption
      )(dfc)
    end applyMacro

    private def unapplySeqMacro[T <: DFTypeAny](
        sc: Expr[BinStrCtx],
        arg: Expr[DFValOf[T]]
    )(dfc: Expr[DFC])(using Quotes, Type[T]): Expr[Option[Seq[Any]]] =
      import quotes.reflect.*
      val parts = sc.parts
      val partsStr = parts.map(_.value.get).toList
      val op = sc.funcName
      val opExpr = Expr(op)
      if (partsStr.length > 1)
        val vArgs = Varargs(opExpr :: partsStr.map { part =>
          val partFiltered = part.filter {
            case '_' | ' ' | '?'        => false
            case isHex() if op == "h"   => true
            case '0' | '1' if op == "b" => true
            case x                      =>
              report.errorAndAbort(
                s"""|Found invalid character: ${x}.
                    |Note: string interpolation with value extraction does not support the `[w']` width extension syntax.""".stripMargin
              )
          }
          Expr(partFiltered)
        })
        // We cast to Seq[Nothing] to avoid an `& String` type constraint on the extractor.
        // This casting is removed (ignored) later in `CustomControlPhase`.
        '{ Some(Seq(${ vArgs }*).asInstanceOf[Seq[Nothing]]) }
      else
        val dfVal = partsStr.head match
          case widthExp(widthStr, wordStr) =>
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
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutW <: IntP
      type OutP
      type Out = DFValTP[DFBits[OutW], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(value: R)(using DFC): Out
    trait CandidateLP:
      given fromIf[
          C <: DFValOf[DFBoolOrBit],
          T,
          F,
          TW <: IntP,
          TP,
          FP,
          R <: IfWrapper[C, T, F]
      ](using
          tTC: Candidate[T] { type OutW = TW; type OutP = TP },
          fTC: DFVal.TC[DFBits[TW], F] { type OutP = FP }
      ): Candidate[R] with
        type OutW = TW
        type OutP = TP | FP
        def apply(value: R)(using DFC): Out = value.unwrap
      end fromIf
    end CandidateLP
    object Candidate extends CandidateLP:
      type Exact = Exact0[DFC, Candidate]
      type Aux[R, W <: IntP, P] = Candidate[R] { type OutW = W; type OutP = P }
      type Dud[V] = Candidate[V]:
        type OutW = Int
        type OutP = NOTCONST
      transparent inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(
          "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal constant via the `d\"<width>'<number>\"` string interpolation."
        ).asInstanceOf[Dud[V]]
      given fromDFBits[W <: IntP, L <: IntP, P, R <: DFValTP[DFBitsWL[W, L], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out = value.asValTP[DFBits[W], P]
      given fromDFBoolOrBit[P, R <: DFValTP[DFBoolOrBit, P]]: Candidate[R] with
        type OutW = 1
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          value.bits
      given fromDFUInt[W <: IntP, P, R <: DFValTP[DFUInt[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          AutoConstraint.carryWidthAdjustPermission(value, value.bits)
      transparent inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        ).asInstanceOf[Dud[E]]
      transparent inline given errDFSInt[W <: IntP, R <: DFValOf[DFSInt[W]]]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        ).asInstanceOf[Dud[R]]

      private[Val] def valueToBits(value: Any)(using dfc: DFC): DFValOf[DFBits[Int]] =
        import DFBits.Val.Ops.concatBits
        val dfcAnon = dfc.anonymize
        value match
          case x: NonEmptyTuple =>
            x.toList.map(x => valueToBits(x)(using dfcAnon)).concatBits
          case i: Int =>
            DFVal.Const(DFBits(1), (BitVector.bit(i > 0), BitVector.zero), named = true)
          case dfVal: DFVal[?, ?] =>
            import DFVal.Ops.bits
            val dfValIR = dfVal.asIR
            dfValIR.dfType match
              case _: ir.DFBitsWL => dfValIR.asValOf[DFBits[Int]]
              case _              =>
                dfValIR.asValAny.bits(using dfc)(using Width.wide).asValOf[DFBits[Int]]
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple]: Candidate[R] = ${ DFBitsMacro[R] }
      object TupleCandidate extends Candidate[Any]:
        def apply(value: Any)(using DFC): Out =
          valueToBits(value).asInstanceOf[Out]

      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        '{
          TupleCandidate.asInstanceOf[
            Candidate[R] {
              type OutW = wType.Underlying
              type OutP = pType.Underlying
            }
          ]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFBits[Int],
          dfVal: DFValOf[DFBits[Int]]
      )(using DFC): DFValOf[DFBits[Int]] =
        (dfType.widthIntOpt, dfVal.widthIntOpt) match
          case (Some(lw), Some(rw)) => `LW == RW`(lw, rw)
          case _                    =>
        dfVal
      protected object `LW == RW`
          extends Check2[
            Int,
            Int,
            [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "The argument width (" + ToString[RW] +
              ") is different than the receiver width (" + ToString[LW] + ").\n" +
              ITE[
                RW > LW,
                "Consider `.truncate` to narrow it to the receiver width, or `.resize(" +
                  ToString[LW] + ")` to state the width explicitly.",
                "Consider `.extend` to widen it to the receiver width, or `.resize(" +
                  ToString[LW] + ")` to state the width explicitly."
              ]
          ]
      given DFBitsFromCandidate[LW <: IntP, LL <: IntP, V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      )(using
          check: `LW == RW`.CheckNUB[LW, ic.OutW]
      ): TC[DFBitsWL[LW, LL], V] with
        type OutP = RP
        def conv(dfType: DFBitsWL[LW, LL], value: V)(using dfc: DFC): Out =
          import Ops.resizeBits
          val dfVal = ic(value)
          if (AutoConstraint.permitsWidthAdjust(dfVal, dfType.widthIntParam))
            dfVal.resizeBits(dfType.widthIntParam).asValTP[DFBitsWL[LW, LL], RP]
          else
            (dfType.widthIntOpt, dfVal.widthIntOpt) match
              case (Some(lw), Some(rw)) => check(lw, rw)
              case _                    =>
                if (dfType.compareWidths(dfVal.dfType)(_ != _).getOrElse(true))
                  throw new IllegalArgumentException(
                    s"""|The argument width (${dfVal.dfType.widthErrorString}) is different than the receiver width (${dfType.widthErrorString}).
                        |Consider `.extend` or `.truncate` to adjust it to the receiver width, or `.resize(width)` to state the width explicitly.""".stripMargin
                  )
            dfVal.nameInDFCPosition.asValTP[DFBitsWL[LW, LL], RP]
          end if
        end conv
      end DFBitsFromCandidate
      given DFBitsFromSEV[LW <: IntP, LL <: IntP, T <: BitOrBool, V <: SameElementsVector[T]]
          : TC[DFBitsWL[LW, LL], V]
      with
        type OutP = CONST
        def conv(dfType: DFBitsWL[LW, LL], value: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, value, named = true)
            .asConstOf[DFBitsWL[LW, LL]]
    end TC

    object TCConv:
      import DFVal.TCConv
      // the target width is fixed at `Int` (statically unknown), so this relabel-only
      // conversion claims exactly the targets no width check can serve; a literal-width
      // target falls to the lower-priority `TCConv.fromTC` derivation, which runs the
      // width-checked TC. The low index is free: a nonzero-low target arises from a
      // `BitsHL` parameter spelling whose non-literal bounds collapsed the width to `Int`
      given DFBitsFromCandidateConv[L <: IntP, V, RP, IC <: Candidate[V]](using
          ic: IC { type OutP = RP }
      ): TCConv[DFBitsWL[Int, L], V] with
        type OutP = RP
        def apply(value: V)(using DFC): Out =
          val dfVal = ic(value)
          dfVal.nameInDFCPosition.asValTP[DFBitsWL[Int, L], RP]

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[
          LW <: IntP,
          LL <: IntP,
          R,
          RP,
          IC <: Candidate[R],
          Op <: FuncOp.===.type | FuncOp.=!=.type,
          C <: Boolean
      ](
          using ic: IC { type OutP = RP }
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBitsWL[LW, LL], R, Op, C] with
        type OutP = RP
        def conv(dfType: DFBitsWL[LW, LL], arg: R)(using DFC): Out =
          val dfValArg = ic(arg)
          (dfType.widthIntOpt, dfValArg.dfType.widthIntOpt) match
            case (Some(lw), Some(rw)) => check(lw, rw)
            case _                    =>
              if (dfType.compareWidths(dfValArg.dfType)(_ != _).getOrElse(true))
                val lhsStr =
                  if (castling) dfValArg.dfType.widthErrorString else dfType.widthErrorString
                val rhsStr =
                  if (castling) dfType.widthErrorString else dfValArg.dfType.widthErrorString
                throw new IllegalArgumentException(
                  s"""|Cannot apply this operation between a value of $lhsStr bits width (LHS) and a value of $rhsStr bits width (RHS).
                      |An explicit conversion must be applied.""".stripMargin
                )
          dfValArg.asValTP[DFBitsWL[LW, LL], RP]
        end conv
      end DFBitsCompareCandidate
      given DFBitsCompareSEV[
          LW <: IntP,
          LL <: IntP,
          Op <: FuncOp.===.type | FuncOp.=!=.type,
          C <: Boolean,
          T <: BitOrBool,
          V <: SameElementsVector[T]
      ](using
          ValueOf[Op],
          ValueOf[C]
      ): Compare[DFBitsWL[LW, LL], V, Op, C] with
        type OutP = CONST
        def conv(dfType: DFBitsWL[LW, LL], arg: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, arg, named = true)
            .asConstOf[DFBitsWL[LW, LL]]
      end DFBitsCompareSEV
    end Compare

    // this was defined separately from `Ops` to avoid collision with `.bits` used in `Ops`
    object TupleOps:
      // explicit conversion of a tuple to bits (concatenation)
      extension (inline tpl: NonEmptyTuple)
        transparent inline def toBits(using dfc: DFCG): Any = ${ bitsMacro('tpl)('dfc) }
      private def bitsMacro(tpl: Expr[NonEmptyTuple])(dfc: Expr[DFCG])(using Quotes): Expr[Any] =
        import quotes.reflect.*
        val exactInfo = tpl.exactInfo
        import Width.*
        val rTpe = exactInfo.exactTpe
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        '{
          Val.Candidate
            .valueToBits($tpl)(using $dfc)
            .asValTP[DFBits[wType.Underlying], pType.Underlying]
        }
    end TupleOps

    // The generalized (any low index) selection givens, at a LOWER priority than the
    // zero-based ones in `Ops` (the codebase's LP-trait idiom, like CandidateLP/WidthLP).
    // These check at elaboration time only: their type-level bounds compose over the
    // receiver's width (`W+L-1`), which does not survive the IntP const guards when the
    // width is itself an unreduced fold, e.g. a `BitsHL[9, 2] <> VAL` struct field whose
    // width slot is the unreduced `RangeWidth[9, 2]` (see the doc comment in IntParam.scala).
    trait OpsLP:
      given evOpApplyDFBitsWL[
          W <: IntP,
          L2 <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBitsWL[W, L2], Modifier[A, C, I, P]],
          R
      ](using
          ub: DFUInt.Val.UBArg[Int, R]
      ): ExactOp2Aux["apply", DFC, DFValAny, L, R, DFVal[DFBit, Modifier[A, C, Any, P]]] =
        new ExactOp2["apply", DFC, DFValAny, L, R]:
          type Out = DFVal[DFBit, Modifier[A, C, Any, P]]
          def apply(lhs: L, idx: R)(using DFC): Out = trydf {
            import dfc.getSet
            val lowRef = lhs.asIR.dfType.asInstanceOf[ir.DFBitsWL].lowIdxRef
            val bound = (lhs.widthIntParam + lowRef.get).asInstanceOf[IntParam[Int]]
            val idxVal = ub(bound, idx)(using dfc.anonymize)
            // a constant index must also respect the lower bound
            val idxIntOpt = idxVal.asIR match
              case c: ir.DFVal.Const => c.data.asInstanceOf[Option[BigInt]].map(_.toInt)
              case _                 => None
            (idxIntOpt, lowRef.getIntOpt) match
              case (Some(idxInt), Some(lowInt)) => BitIndexLow(idxInt, lowInt)
              case _                            =>
            DFVal.Alias.ApplyIdx(DFBit, lhs, idxVal)
          }(using dfc, CTName("bit selection (apply)"))
      end evOpApplyDFBitsWL
      given evOpApplyRangeDFBitsWL[
          W <: IntP,
          L2 <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBitsWL[W, L2], Modifier[A, C, I, P]],
          HI <: IntP,
          LO <: IntP
      ](using
          checkHigh: BitIndexHigh.CheckNUB[HI, IntP.HighIdx[W, L2]],
          checkLow: BitIndexLow.CheckNUB[LO, L2],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, HI, LO, DFVal[
        DFBits[IntP.RangeWidth[HI, LO]],
        Modifier[A, C, Any, P]
      ]] =
        new ExactOp3["apply", DFC, DFValAny, L, HI, LO]:
          type Out = DFVal[DFBits[IntP.RangeWidth[HI, LO]], Modifier[A, C, Any, P]]
          def apply(lhs: L, idxHigh: HI, idxLow: LO)(using DFC): Out = trydf {
            import dfc.getSet
            val idxHighParam = IntParam(idxHigh)
            val idxLowParam = IntParam(idxLow)
            val idxHighIntOpt = idxHighParam.toScalaIntOpt
            val idxLowIntOpt = idxLowParam.toScalaIntOpt
            val dfTypeIR = lhs.asIR.dfType.asInstanceOf[ir.DFBitsWL]
            val lowIntOpt = dfTypeIR.lowIdxIntOpt
            val highIntOpt = (dfTypeIR.widthIntOpt, lowIntOpt) match
              case (Some(widthInt), Some(lowInt)) => Some(lowInt + widthInt - 1)
              case _                              => None
            (idxHighIntOpt, highIntOpt) match
              case (Some(idxHighInt), Some(highInt)) => checkHigh(idxHighInt, highInt)
              case _                                 =>
            (idxLowIntOpt, lowIntOpt) match
              case (Some(idxLowInt), Some(lowInt)) => checkLow(idxLowInt, lowInt)
              case _                               =>
            (idxHighIntOpt, idxLowIntOpt) match
              case (Some(idxHighInt), Some(idxLowInt)) => checkHiLo(idxHighInt, idxLowInt)
              case _                                   =>
            DFVal.Alias.ApplyRange(lhs, idxHighParam, idxLowParam)
          }(using dfc, CTName("bit range selection (apply)"))
      end evOpApplyRangeDFBitsWL
    end OpsLP
    object Ops extends OpsLP:
      import IntP.{-, +}
      given evOpApplyDFBits[
          W <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBits[W], Modifier[A, C, I, P]],
          R
      ](using
          ub: DFUInt.Val.UBArg[W, R]
      ): ExactOp2Aux["apply", DFC, DFValAny, L, R, DFVal[DFBit, Modifier[A, C, Any, P]]] =
        new ExactOp2["apply", DFC, DFValAny, L, R]:
          type Out = DFVal[DFBit, Modifier[A, C, Any, P]]
          def apply(lhs: L, idx: R)(using DFC): Out = trydf {
            DFVal.Alias.ApplyIdx(DFBit, lhs, ub(lhs.widthIntParam, idx)(using dfc.anonymize))
          }(using dfc, CTName("bit selection (apply)"))
      end evOpApplyDFBits
      given evOpApplyRangeDFBits[
          W <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBits[W], Modifier[A, C, I, P]],
          HI <: IntP,
          LO <: IntP
      ](using
          checkHigh: BitIndex.CheckNUB[HI, W],
          checkLow: BitIndex.CheckNUB[LO, W],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, HI, LO, DFVal[
        DFBits[IntP.RangeWidth[HI, LO]],
        Modifier[A, C, Any, P]
      ]] =
        new ExactOp3["apply", DFC, DFValAny, L, HI, LO]:
          type Out = DFVal[DFBits[IntP.RangeWidth[HI, LO]], Modifier[A, C, Any, P]]
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
            DFVal.Alias.ApplyRange(lhs, idxHighParam, idxLowParam)
          }(using dfc, CTName("bit range selection (apply)"))
      end evOpApplyRangeDFBits
      // the annotation path (a `BitsHL[9, 2] <> VAL` field or parameter) carries the width
      // as the UNREDUCED `RangeWidth[H, L]` application, where the W-form's `HighIdx[W, L]`
      // bound gets stuck (fold over a fold); binding `H` structurally checks on `H` directly.
      // The term-construction path reduces the width to a literal, misses this pattern, and
      // resolves to the W-form above instead.
      given evOpApplyRangeDFBitsHL[
          H <: IntP,
          L2 <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBitsHL[H, L2], Modifier[A, C, I, P]],
          HI <: IntP,
          LO <: IntP
      ](using
          checkHigh: BitIndexHigh.CheckNUB[HI, H],
          checkLow: BitIndexLow.CheckNUB[LO, L2],
          checkHiLo: BitsHiLo.CheckNUB[HI, LO]
      ): ExactOp3Aux["apply", DFC, DFValAny, L, HI, LO, DFVal[
        DFBits[IntP.RangeWidth[HI, LO]],
        Modifier[A, C, Any, P]
      ]] =
        new ExactOp3["apply", DFC, DFValAny, L, HI, LO]:
          type Out = DFVal[DFBits[IntP.RangeWidth[HI, LO]], Modifier[A, C, Any, P]]
          def apply(lhs: L, idxHigh: HI, idxLow: LO)(using DFC): Out = trydf {
            import dfc.getSet
            val idxHighParam = IntParam(idxHigh)
            val idxLowParam = IntParam(idxLow)
            val idxHighIntOpt = idxHighParam.toScalaIntOpt
            val idxLowIntOpt = idxLowParam.toScalaIntOpt
            val dfTypeIR = lhs.asIR.dfType.asInstanceOf[ir.DFBitsWL]
            val lowIntOpt = dfTypeIR.lowIdxIntOpt
            val highIntOpt = (dfTypeIR.widthIntOpt, lowIntOpt) match
              case (Some(widthInt), Some(lowInt)) => Some(lowInt + widthInt - 1)
              case _                              => None
            (idxHighIntOpt, highIntOpt) match
              case (Some(idxHighInt), Some(highInt)) => checkHigh(idxHighInt, highInt)
              case _                                 =>
            (idxLowIntOpt, lowIntOpt) match
              case (Some(idxLowInt), Some(lowInt)) => checkLow(idxLowInt, lowInt)
              case _                               =>
            (idxHighIntOpt, idxLowIntOpt) match
              case (Some(idxHighInt), Some(idxLowInt)) => checkHiLo(idxHighInt, idxLowInt)
              case _                                   =>
            DFVal.Alias.ApplyRange(lhs, idxHighParam, idxLowParam)
          }(using dfc, CTName("bit range selection (apply)"))
      end evOpApplyRangeDFBitsHL
      given evOpLogicDFBits[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          L,
          LW <: IntP,
          LP,
          R,
          RW <: IntP,
          RP
      ](using
          icL: Candidate.Aux[L, LW, LP],
          icR: Candidate.Aux[R, RW, RP],
          op: ValueOf[Op]
      )(using
          check: `LW == RW`.CheckNUB[LW, RW]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFBits[LW], LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFBits[LW], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val lhsVal = icL(lhs)
            val rhsVal = icR(rhs)
            (lhsVal.widthIntOpt, rhsVal.widthIntOpt) match
              case (Some(lw), Some(rw)) => check(lw, rw)
              case _                    => equalWidthCheck(lhsVal.dfType, rhsVal.dfType)
            DFVal.Func(lhsVal.dfType, op.value, List(lhsVal, rhsVal))
          }(using dfc, CTName(op.value.toString))
      end evOpLogicDFBits
      given evOpLogicReduceDFBits[
          Op <: FuncOp.|.type | FuncOp.&.type | FuncOp.^.type,
          LW <: IntP,
          LL <: IntP,
          LP,
          L <: DFValTP[DFBitsWL[LW, LL], LP] | DFValTP[DFUInt[LW], LP]
      ](using
          op: ValueOf[Op]
      ): ExactOp1Aux[Op, DFC, DFValAny, L, DFValTP[DFBit, LP]] =
        new ExactOp1[Op, DFC, DFValAny, L]:
          type Out = DFValTP[DFBit, LP]
          def apply(lhs: L)(using DFC): Out = trydf {
            DFVal.Func(DFBit, op.value, List(lhs)).asValTP[DFBit, LP]
          }(using dfc, CTName(op.value.toString))
      end evOpLogicReduceDFBits
      given evConcatOpDFBits[
          Op <: FuncOp.++.type,
          L,
          LW <: IntP,
          LP,
          R,
          RW <: IntP,
          RP
      ](using
          icL: Candidate.Aux[L, LW, LP],
          icR: Candidate.Aux[R, RW, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[DFBits[IntP.+[LW, RW]], LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[DFBits[IntP.+[LW, RW]], LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            val lhsVal = icL(lhs)
            val rhsVal = icR(rhs)
            val width = lhsVal.widthIntParam + rhsVal.widthIntParam
            DFVal.Func(DFBits(width), FuncOp.++, List(lhsVal, rhsVal))
          }(using dfc, CTName(op.value.toString))
      end evConcatOpDFBits
      given evOpShift[
          Op <: FuncOp.>>.type | FuncOp.<<.type,
          LW <: IntP,
          LL <: IntP,
          LP,
          // a shift keeps its receiver's type, including a nonzero low index
          LT <: DFBitsWL[LW, LL] | DFSInt[LW] | DFUInt[LW] | DFInt32,
          L <: DFValTP[LT, LP],
          R,
          RP
      ](using
          ub: DFUInt.Val.UBArg.Aux[LW, R, RP],
          op: ValueOf[Op]
      ): ExactOp2Aux[Op, DFC, DFValAny, L, R, DFValTP[LT, LP | RP]] =
        new ExactOp2[Op, DFC, DFValAny, L, R]:
          type Out = DFValTP[LT, LP | RP]
          def apply(lhs: L, rhs: R)(using DFC): Out = trydf {
            import dfc.getSet
            // Check B: shift amount is self-determined in Verilog,
            // so only warn if the LHS chain itself contains a tagged operand
            if CarryPromote.containsNarrowNonCarryArithWithTaggedOperand(
                lhs.asIR
              )
            then
              dfc.logEvent(
                DFWarning(
                  op.value.toString,
                  CarryPromote.verilogSemanticsWarnMsg
                )
              )
            val shiftVal = ub(lhs.widthIntParam.asInstanceOf[IntParam[LW]], rhs)
            DFVal.Func(lhs.dfType, op.value, List(lhs, shiftVal))
          }(using dfc, CTName(op.value.toString))
      end evOpShift

      extension [W <: IntP, LX <: IntP, P](lhs: DFValTP[DFBitsWL[W, LX], P])
        // TODO: IntP
        private[DFBits] def resizeBits[RW <: IntP](updatedWidth: IntParam[RW])(using
            DFC
        ): DFValTP[DFBits[RW], P] =
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFBits[RW]]
//          else
          DFVal.Alias.AsIs(DFBits(updatedWidth), lhs)
        @deprecated(
          "Permits both widening and truncation, so it does not say which was meant. Use `.extend` or `.truncate` for the direction you intend, or `.resize(width)` to state the width.",
          "0.23.0"
        )
        def resize(using DFCG): DFValTP[DFBits[Int], P] =
          lhs.tag(ir.ResizeTag).asValTP[DFBits[Int], P]
        // permission to adjust the width in ONE direction, taken up by the context that decides
        // the width; in the other direction it contributes nothing (see `ir.ExtendTag`)
        @targetName("extendDFBits")
        def extend(using DFCG): DFValTP[DFBits[Int], P] =
          lhs.tag(ir.ExtendTag).asValTP[DFBits[Int], P]
        @targetName("truncateDFBits")
        def truncate(using DFCG): DFValTP[DFBits[Int], P] =
          lhs.tag(ir.TruncateTag).asValTP[DFBits[Int], P]
        def resize[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: Arg.Width.CheckNUB[RW],
            dfc: DFCG
        ): DFValTP[DFBits[RW], P] = trydf {
          updatedWidth.toScalaIntOpt.foreach(check(_))
          lhs.resizeBits(updatedWidth)
        }
        // extend-by: a RELATIVE zero-extension by `delta` bits, sugar over
        // `.resize(width + delta)`; printed back in this relative form whenever the
        // width delta folds to a literal
        def eby[RK <: IntP](delta: IntParam[RK])(using
            check: Arg.Positive.CheckNUB[RK],
            dfc: DFCG
        ): DFValTP[DFBits[IntP.ExtendByWidth[W, RK]], P] = trydf {
          delta.toScalaIntOpt.foreach(check(_))
          import IntParam.+
          lhs.resizeBits(lhs.dfType.widthIntParam + delta)
            .asValTP[DFBits[IntP.ExtendByWidth[W, RK]], P]
        }
      end extension
      extension [T <: Int, P](iter: Iterable[DFValTP[DFBits[T], P]])
        protected[core] def concatBits(using DFC): DFValTP[DFBits[Int], P] =
          val width =
            iter.map(_.widthIntParam.asInstanceOf[IntParam[Int]]).reduce(_ + _)
          DFVal.Func(DFBits(width), FuncOp.++, iter.toList)
      extension [L <: DFValAny, LW <: IntP, LP](lhs: L)(using icL: Candidate.Aux[L, LW, LP])
        @deprecated(
          "Permits both widening and truncation, so it does not say which was meant. Use `.extend` or `.truncate` for the direction you intend, or `.resize(width)` to state the width.",
          "0.23.0"
        )
        def resize(using DFCG): DFValTP[DFBits[Int], icL.OutP] =
          icL(lhs).tag(ir.ResizeTag).asValTP[DFBits[Int], icL.OutP]
        @targetName("extendDFBitsCandidate")
        def extend(using DFCG): DFValTP[DFBits[Int], icL.OutP] =
          icL(lhs).tag(ir.ExtendTag).asValTP[DFBits[Int], icL.OutP]
        @targetName("truncateDFBitsCandidate")
        def truncate(using DFCG): DFValTP[DFBits[Int], icL.OutP] =
          icL(lhs).tag(ir.TruncateTag).asValTP[DFBits[Int], icL.OutP]
        def repeat[N <: IntP](num: IntParam[N])(using
            dfc: DFCG,
            check: Arg.Positive.CheckNUB[N]
            // `LW`, not the equivalent `icL.OutW`: a path-dependent type reads as non-constant to
            // the `IsConst` guard and would collapse the width (see `IntP.IsConstInt2`)
        ): DFValTP[DFBits[IntP.*[LW, N]], icL.OutP | CONST] = trydf {
          val lhsVal = icL(lhs)
          num.toScalaIntOpt.foreach(check(_))
          val lhsWidth = lhsVal.widthIntParam
          val width = lhsWidth * num
          DFVal.Func(DFBits(width), FuncOp.repeat, List(lhsVal, num.toDFConst))
        }
      end extension

      given evOpAsDFBits[
          W <: IntP,
          LX <: IntP,
          A,
          C,
          I,
          P,
          L <: DFVal[DFBitsWL[W, LX], Modifier[A, C, I, P]],
          AT <: DFType.Supported,
          OT <: DFTypeAny,
          OW <: IntP
      ](using
          tc: DFType.TC.Aux[AT, OT],
          aW: Width.Aux[OT, OW]
      )(using
          check: `AW == TW`.CheckNUB[OW, W]
      ): ExactOp2Aux["as", DFC, DFValAny, L, AT, DFValTP[OT, P]] =
        new ExactOp2["as", DFC, DFValAny, L, AT]:
          type Out = DFValTP[OT, P]
          def apply(lhs: L, aliasType: AT)(using DFC): Out = trydf {
            import dfc.getSet
            val aliasDFType = tc(aliasType)
            (aliasDFType.asIR.widthIntOpt, lhs.widthIntOpt) match
              case (Some(aw), Some(lw)) => check(aw, lw)
              case _                    =>
            DFVal.Alias.AsIs(aliasDFType, lhs)
          }(using dfc, CTName("cast from bits"))
      end evOpAsDFBits

      extension [W <: IntP, LX <: IntP, T <: DFBitsWL[W, LX] | DFUInt[W], P](
          lhs: DFValTP[T, P]
      )
        def unary_~(using DFCG): DFValTP[T, P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_~, List(lhs))
        }
      extension [W <: IntP, LX <: IntP, A, C, I, P](
          lhs: DFVal[DFBitsWL[W, LX], Modifier[A, C, I, P]]
      )
        // the receiver's low-index ref; selections use absolute indices, so a nonzero
        // low offsets the computed bounds (the literal-0 path keeps the exact spelling
        // zero-based code has always printed)
        private def lowIdxRefIR: ir.IntParamRef =
          lhs.asIR.dfType.asInstanceOf[ir.DFBitsWL].lowIdxRef
        def uint(using DFCG): DFValTP[DFUInt[W], P] = trydf {
          DFVal.Alias.AsIs(DFUInt(lhs.widthIntParam), lhs)
        }
        def sint(using DFCG): DFValTP[DFSInt[W], P] = trydf {
          DFVal.Alias.AsIs(DFSInt(lhs.widthIntParam), lhs)
        }
        @targetName("negateDFBits")
        def unary_-(using DFCG): DFValTP[DFSInt[IntP.+[W, 1]], P] = trydf {
          import DFUInt.Val.Ops.unary_- as negate
          lhs.uint.negate
        }
        def msbit(using DFCG): DFVal[DFBit, Modifier[A, C, Any, P]] =
          import DFVal.Ops.apply as applyBits
          val lowRef = lowIdxRefIR
          val msbIdx =
            (if (lowRef.equals(0)) lhs.widthIntParam - 1
             else lhs.widthIntParam + lowRef.get - 1).asInstanceOf[IntParam[Int]]
          lhs.applyBits(msbIdx.toDFConst).asVal[DFBit, Modifier[A, C, Any, P]]
        def lsbit(using DFCG): DFVal[DFBit, Modifier[A, C, Any, P]] =
          import DFVal.Ops.apply as applyBits
          val lowRef = lowIdxRefIR
          if (lowRef.equals(0)) lhs.applyBits(0).asVal[DFBit, Modifier[A, C, Any, P]]
          else lhs.applyBits(lowRef.get.toDFConst).asVal[DFBit, Modifier[A, C, Any, P]]
        def msbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFBits[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          val lowRef = lowIdxRefIR
          val (idxHigh, idxLow) =
            (if (lowRef.equals(0)) (lhs.widthIntParam - 1, lhs.widthIntParam - updatedWidth)
             else
               val low = lowRef.get
               (lhs.widthIntParam + low - 1, lhs.widthIntParam + low - updatedWidth)
            ) .asInstanceOf[(IntParam[Int], IntParam[Int])]
          DFVal.Alias.ApplyRange(lhs, idxHigh, idxLow).asValTP[DFBits[RW], P]
        }
        def lsbits[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFCG
        ): DFValTP[DFBits[RW], P] = trydf {
          (lhs.widthIntOpt, updatedWidth.toScalaIntOpt) match
            case (Some(lhsWidthInt), Some(updatedWidthInt)) => check(lhsWidthInt, updatedWidthInt)
            case _                                          =>
          val lowRef = lowIdxRefIR
          val (idxHigh, idxLow) =
            (if (lowRef.equals(0)) (updatedWidth - 1, IntParam.forced[Int](0))
             else
               val low = lowRef.get
               (updatedWidth + low - 1, low)
            ) .asInstanceOf[(IntParam[Int], IntParam[Int])]
          DFVal.Alias.ApplyRange(lhs, idxHigh, idxLow).asValTP[DFBits[RW], P]
        }
        // ascending part-select (Verilog `lhs[baseIdx +: selWidth]`):
        // selWidth bits whose LSB is anchored at baseIdx
        def lsbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkLow: BitIndexLow.CheckNUB[BI, LX],
            checkHigh: BitIndexHigh.CheckNUB[IntP.PartSelectHigh[BI, SW], IntP.HighIdx[W, LX]]
        ): DFVal[DFBits[SW], Modifier[A, C, Any, P]] = trydf {
          import dfc.getSet
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxHigh = baseIdx + selWidth - 1
          val lowIntOpt = lowIdxRefIR.getIntOpt
          val highIntOpt = (lhs.widthIntOpt, lowIntOpt) match
            case (Some(widthInt), Some(lowInt)) => Some(lowInt + widthInt - 1)
            case _                              => None
          (baseIdx.toScalaIntOpt, lowIntOpt) match
            case (Some(baseIdxInt), Some(lowInt)) => checkLow(baseIdxInt, lowInt)
            case _                                =>
          (idxHigh.toScalaIntOpt, highIntOpt) match
            case (Some(idxHighInt), Some(highInt)) => checkHigh(idxHighInt, highInt)
            case _                                 =>
          DFVal.Alias.ApplyRange(lhs, idxHigh, baseIdx).asVal[DFBits[SW], Modifier[A, C, Any, P]]
        }
        // descending part-select (Verilog `lhs[baseIdx -: selWidth]`):
        // selWidth bits whose MSB is anchored at baseIdx
        def msbitsAt[BI <: IntP, SW <: IntP](baseIdx: IntParam[BI], selWidth: IntParam[SW])(using
            dfc: DFCG,
            checkWidth: Arg.Width.CheckNUB[SW],
            checkHigh: BitIndexHigh.CheckNUB[BI, IntP.HighIdx[W, LX]],
            checkLow: BitIndexLow.CheckNUB[IntP.PartSelectLow[BI, SW], LX]
        ): DFVal[DFBits[SW], Modifier[A, C, Any, P]] = trydf {
          import dfc.getSet
          selWidth.toScalaIntOpt.foreach(checkWidth(_))
          val idxLow = baseIdx - selWidth + 1
          val lowIntOpt = lowIdxRefIR.getIntOpt
          val highIntOpt = (lhs.widthIntOpt, lowIntOpt) match
            case (Some(widthInt), Some(lowInt)) => Some(lowInt + widthInt - 1)
            case _                              => None
          (baseIdx.toScalaIntOpt, highIntOpt) match
            case (Some(baseIdxInt), Some(highInt)) => checkHigh(baseIdxInt, highInt)
            case _                                 =>
          (idxLow.toScalaIntOpt, lowIntOpt) match
            case (Some(idxLowInt), Some(lowInt)) => checkLow(idxLowInt, lowInt)
            case _                               =>
          DFVal.Alias.ApplyRange(lhs, baseIdx, idxLow).asVal[DFBits[SW], Modifier[A, C, Any, P]]
        }
      end extension
    end Ops
  end Val
end DFBits
