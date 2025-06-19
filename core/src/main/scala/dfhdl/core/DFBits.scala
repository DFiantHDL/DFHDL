package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName, nowarn}
import scala.quoted.*
import scala.util.boundary, boundary.break
import DFDecimal.Constraints.`LW == RW`

type DFBits[W <: IntP] = DFType[ir.DFBits, Args1[W]]
object DFBits:
  def apply[W <: IntP](width: IntParam[W])(using
      dfc: DFC,
      check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = trydf:
    check(width)
    ir.DFBits(width.ref).asFE[DFBits[W]]
  def forced[W <: IntP](width: Int): DFBits[W] =
    val check = summon[Arg.Width.Check[Int]]
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  def apply[W <: IntP](using dfc: DFC, dfType: => DFBits[W]): DFBits[W] = trydf { dfType }
  def until[V <: IntP](sup: IntParam[V])(using
      dfc: DFC,
      check: Arg.LargerThan1.CheckNUB[V]
  ): DFBits[IntP.CLog2[V]] = trydf:
    check(sup)
    ir.DFBits(sup.clog2.ref).asFE[DFBits[IntP.CLog2[V]]]
  def to[V <: IntP](max: IntParam[V])(using
      dfc: DFC,
      check: Arg.Positive.CheckNUB[V]
  ): DFBits[IntP.CLog2[IntP.+[V, 1]]] = trydf:
    check(max)
    ir.DFBits((max + 1).clog2.ref).asFE[DFBits[IntP.CLog2[IntP.+[V, 1]]]]

  given [W <: IntP & Singleton](using
      dfc: DFC,
      v: ValueOf[W],
      check: Arg.Width.CheckNUB[W]
  ): DFBits[W] = trydf:
    val width = IntParam.forced(v)
    check(width.toScalaInt)
    ir.DFBits(width.ref).asFE[DFBits[W]]

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
        var (valueBits, bubbleBits) = fromString.toOption.get
        explicitWidthOption.foreach(ew =>
          val updatedWidth = IntParam.forced(ew).toScalaInt
          valueBits = valueBits.resize(updatedWidth)
          bubbleBits = bubbleBits.resize(updatedWidth)
        )
        val width = IntParam.forced[W](explicitWidthOption.getOrElse(valueBits.length.toInt))
        DFVal.Const(DFBits(width), (valueBits, bubbleBits), named = true)
    end extension

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private[DFBits] def interpolate(
          opExpr: Expr[String],
          explicitWidthOptionExpr: Expr[Option[IntP]]
      ): Expr[DFConstAny] =
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
          val dfc = compiletime.summonInline[DFC]
          $fullExpr.interpolate[widthType.Underlying](
            $opExpr,
            $explicitWidthOptionExpr
          )(using dfc)
        }
    end extension

  end StrInterp

  // Unclear why, but the compiler crashes if we do not separate these definitions from StrInterp
  object StrInterpOps:
    import StrInterp.{interpolate, isHex, widthExp}
    opaque type BinStrCtx <: StringContext = StringContext
    object BinStrCtx:
      extension (inline sc: BinStrCtx)
        transparent inline def apply(inline args: Any*): Any =
          ${ applyMacro('sc, 'args) }
        transparent inline def unapplySeq[T <: DFTypeAny](
            inline arg: DFValOf[T]
        )(using DFC): Option[Seq[Any]] =
          ${ unapplySeqMacro('sc, 'arg) }

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
        case widthExp(widthStr, wordStr) :: rest =>
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
        sc: Expr[BinStrCtx],
        arg: Expr[DFValOf[T]]
    )(using Quotes, Type[T]): Expr[Option[Seq[Any]]] =
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
        '{
          Some(Seq(${ vArgs }*))
        }
      else
        val dfVal = partsStr.head match
          case widthExp(widthStr, wordStr) =>
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
    trait Candidate[R] extends Exact0.TC[R, DFC]:
      type OutW <: IntP
      type OutP
      type Out = DFValTP[DFBits[OutW], OutP]
      def conv(from: R)(using DFC): Out = apply(from)
      def apply(value: R)(using DFC): Out
    object Candidate:
      type Exact = Exact0[DFC, Candidate]
      type Aux[R, W <: IntP, P] = Candidate[R] { type OutW = W; type OutP = P }
      type Dud[V] = Candidate[V]:
        type OutW = Int
        type OutP = NOTCONST
      transparent inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(
          "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal constant via the `d\"<width>'<number>\"` string interpolation."
        ).asInstanceOf[Dud[V]]
      given fromDFBits[W <: IntP, P, R <: DFValTP[DFBits[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out = value
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
          if (value.hasTag[ir.TruncateTag]) value.bits.tag(ir.TruncateTag)
          else if (value.hasTag[ir.ExtendTag]) value.bits.tag(ir.ExtendTag)
          else value.bits
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
              case _: ir.DFBits => dfValIR.asValOf[DFBits[Int]]
              case _            =>
                dfValIR.asValAny.bits(using Width.wide).asValOf[DFBits[Int]]
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
        `LW == RW`(dfType.widthInt, dfVal.widthInt)
        dfVal
      protected object `LW == RW`
          extends Check2[
            Int,
            Int,
            [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "The argument width (" + ToString[RW] +
              ") is different than the receiver width (" + ToString[LW] +
              ").\nConsider applying `.resize` to resolve this issue."
          ]
      given DFBitsFromCandidate[LW <: IntP, V, IC <: Candidate[V]](using
          ic: IC
      )(using
          check: `LW == RW`.CheckNUB[LW, ic.OutW]
      ): TC[DFBits[LW], V] with
        type OutP = ic.OutP
        def conv(dfType: DFBits[LW], value: V)(using dfc: DFC): Out =
          import Ops.resizeBits
          val dfVal = ic(value)
          if (dfVal.hasTag[ir.TruncateTag] && dfType.widthInt < dfVal.widthInt)
            dfVal.resizeBits(dfType.widthIntParam).asValTP[DFBits[LW], ic.OutP]
          else if (dfVal.hasTag[ir.ExtendTag] && dfType.widthInt > dfVal.widthInt)
            dfVal.resizeBits(dfType.widthIntParam).asValTP[DFBits[LW], ic.OutP]
          else
            check(dfType.widthInt, dfVal.widthInt)
            dfVal.nameInDFCPosition.asValTP[DFBits[LW], ic.OutP]
        end conv
      end DFBitsFromCandidate
      given DFBitsFromSEV[LW <: IntP, T <: BitOrBool, V <: SameElementsVector[T]]: TC[DFBits[LW], V]
      with
        type OutP = CONST
        def conv(dfType: DFBits[LW], value: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, value, named = true)
            .asConstOf[DFBits[LW]]
    end TC

    object TCConv:
      import DFVal.TCConv
      given DFBitsFromCandidateConv[V, IC <: Candidate[V]](using
          ic: IC
      ): TCConv[DFBits[Int], V] with
        type OutP = ic.OutP
        def apply(value: V)(using DFC): Out =
          val dfVal = ic(value)
          dfVal.nameInDFCPosition.asValTP[DFBits[Int], ic.OutP]

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[LW <: IntP, R, IC <: Candidate[R], Op <: FuncOp, C <: Boolean](
          using ic: IC
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFBits[LW], arg: R)(using DFC): Out =
          val dfValArg = ic(arg)
          check(dfType.widthInt, dfValArg.dfType.widthInt)
          dfValArg.asValTP[DFBits[LW], ic.OutP]
      given DFBitsCompareSEV[
          LW <: IntP,
          Op <: FuncOp,
          C <: Boolean,
          T <: BitOrBool,
          V <: SameElementsVector[T]
      ](using
          ValueOf[Op],
          ValueOf[C]
      ): Compare[DFBits[LW], V, Op, C] with
        type OutP = CONST
        def conv(dfType: DFBits[LW], arg: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.widthIntParam, arg, named = true)
            .asConstOf[DFBits[LW]]
      end DFBitsCompareSEV
    end Compare

    // this was defined separately from `Ops` to avoid collision with `.bits` used in `Ops`
    object TupleOps:
      // explicit conversion of a tuple to bits (concatenation)
      extension (inline tpl: NonEmptyTuple)
        transparent inline def toBits: Any = ${ bitsMacro('tpl) }
      private def bitsMacro(tpl: Expr[NonEmptyTuple])(using Quotes): Expr[Any] =
        import quotes.reflect.*
        val exactInfo = tpl.exactInfo
        import Width.*
        val rTpe = exactInfo.exactTpe
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        '{
          Val.Candidate
            .valueToBits($tpl)(using compiletime.summonInline[DFC])
            .asValTP[DFBits[wType.Underlying], pType.Underlying]
        }
    end TupleOps

    object Ops:
      extension [W <: IntP, P](lhs: DFValTP[DFBits[W], P])
        def truncate(using DFC): DFValTP[DFBits[Int], P] =
          lhs.tag(ir.TruncateTag).asValTP[DFBits[Int], P]
        // TODO: IntP
        private[DFBits] def resizeBits[RW <: IntP](updatedWidth: IntParam[RW])(using
            DFC
        ): DFValTP[DFBits[RW], P] =
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFBits[RW]]
//          else
          DFVal.Alias.AsIs(DFBits(updatedWidth), lhs)
      end extension
      extension [T <: Int, P](iter: Iterable[DFValTP[DFBits[T], P]])
        protected[core] def concatBits(using DFC): DFValTP[DFBits[Int], P] =
          val width =
            iter.map(_.widthIntParam.asInstanceOf[IntParam[Int]]).reduce(_ + _)
          DFVal.Func(DFBits(width), FuncOp.++, iter.toList)
      // only Bits and UInt as expected candidates
      extension [W0 <: IntP, L <: DFValOf[DFBits[W0]] | DFValOf[DFUInt[W0]], LW <: IntP, LP](
          lhs: L
      )(using
          icL: Candidate[L]
      )
        def &[R](rhs: Candidate.Exact)(using
            dfc: DFC,
            check: `LW == RW`.CheckNUB[icL.OutW, rhs.tc.OutW]
        ): DFValTP[DFBits[icL.OutW], icL.OutP | rhs.tc.OutP] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = rhs()
          check(lhsVal.widthInt, rhsVal.widthInt)
          DFVal.Func(lhsVal.dfType, FuncOp.&, List(lhsVal, rhsVal))
        }
        def |[R](rhs: Candidate.Exact)(using
            dfc: DFC,
            check: `LW == RW`.CheckNUB[icL.OutW, rhs.tc.OutW]
        ): DFValTP[DFBits[icL.OutW], icL.OutP | rhs.tc.OutP] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = rhs()
          check(lhsVal.widthInt, rhsVal.widthInt)
          DFVal.Func(lhsVal.dfType, FuncOp.|, List(lhsVal, rhsVal))
        }
        def ^[R](rhs: Candidate.Exact)(using
            dfc: DFC,
            check: `LW == RW`.CheckNUB[icL.OutW, rhs.tc.OutW]
        ): DFValTP[DFBits[icL.OutW], icL.OutP | rhs.tc.OutP] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = rhs()
          check(lhsVal.widthInt, rhsVal.widthInt)
          DFVal.Func(lhsVal.dfType, FuncOp.^, List(lhsVal, rhsVal))
        }
        ///////////////////////////////////////////////////////////////////////////////
        // The `reduce?` is a workaround https://github.com/scala/scala3/issues/20053
        // See PreTyperPhase of compiler plugin to see replacements `.?` with `.reduce?`
        ///////////////////////////////////////////////////////////////////////////////
        def `reduce&`(using dfc: DFC): DFValTP[DFBit, icL.OutP] = trydf {
          DFVal.Func(DFBit, FuncOp.&, List(icL(lhs)))
        }
        def `reduce|`(using dfc: DFC): DFValTP[DFBit, icL.OutP] = trydf {
          DFVal.Func(DFBit, FuncOp.|, List(icL(lhs)))
        }
        def `reduce^`(using dfc: DFC): DFValTP[DFBit, icL.OutP] = trydf {
          DFVal.Func(DFBit, FuncOp.^, List(icL(lhs)))
        }
        // reduction AND of all bits
        private def &(using dfc: DFC): DFValTP[DFBit, icL.OutP] = `reduce&`
        // reduction OR of all bits
        private def |(using dfc: DFC): DFValTP[DFBit, icL.OutP] = `reduce|`
        // reduction XOR of all bits
        private def ^(using dfc: DFC): DFValTP[DFBit, icL.OutP] = `reduce^`
      end extension
      extension [L <: DFValAny, LW <: IntP, LP](lhs: L)(using icL: Candidate.Aux[L, LW, LP])
        def extend(using DFC): DFValTP[DFBits[Int], icL.OutP] =
          icL(lhs).tag(ir.ExtendTag).asValTP[DFBits[Int], icL.OutP]
        def resize[RW <: IntP](updatedWidth: IntParam[RW])(using
            check: Arg.Width.CheckNUB[RW],
            dfc: DFC
        ): DFValTP[DFBits[RW], icL.OutP] = trydf {
          check(updatedWidth)
          icL(lhs).resizeBits(updatedWidth)
        }
        def repeat[N <: Int](num: IntParam[N])(using
            dfc: DFC,
            check: Arg.Positive.Check[N]
        ): DFValTP[DFBits[IntP.*[icL.OutW, N]], icL.OutP | CONST] = trydf {
          val lhsVal = icL(lhs)
          check(num)
          val lhsWidth = lhsVal.widthIntParam
          val width =
            // simplifying the representation if the argument is a single bit
            if (lhsWidth.toScalaInt == 1) num.asInstanceOf[IntParam[IntP.*[icL.OutW, N]]]
            else lhsWidth * num
          DFVal.Func(DFBits(width), FuncOp.repeat, List(lhsVal, num.toDFConst))
        }
        def ++[R](rhs: Candidate.Exact)(using
            dfc: DFC
        ): DFValTP[DFBits[IntP.+[icL.OutW, rhs.tc.OutW]], icL.OutP | rhs.tc.OutP] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = rhs()
          val width = lhsVal.widthIntParam + rhsVal.widthIntParam
          DFVal.Func(DFBits(width), FuncOp.++, List(lhsVal, rhsVal))
        }
      end extension

      extension [W <: IntP, A, C, I, P](
          lhs: DFVal[DFBits[W], Modifier[A, C, I, P]]
      )
        def as[AT <: DFType.Supported](
            aliasType: AT
        )(using
            tc: DFType.TC[AT]
        )(using
            aW: Width[tc.Type],
            dfc: DFC
        )(using check: `AW == TW`.CheckNUB[aW.Out, W]): DFValTP[tc.Type, P] = trydf {
          import dfc.getSet
          val aliasDFType = tc(aliasType)
          check(aliasDFType.asIR.width, lhs.widthInt)
          DFVal.Alias.AsIs(aliasDFType, lhs)
        }
        def uint(using DFC): DFValTP[DFUInt[W], P] = trydf { as(DFUInt(lhs.widthIntParam)) }
        def sint(using DFC): DFValTP[DFSInt[W], P] = trydf { as(DFSInt(lhs.widthIntParam)) }
        def apply(
            relIdx: DFUInt.Val.UBArg.Exact[W]
        )(using
            dfc: DFC
        ): DFVal[DFBit, Modifier[A, Any, Any, P]] = trydf {
          DFVal.Alias.ApplyIdx(DFBit, lhs, relIdx(lhs.widthIntParam)(using dfc.anonymize))
        }
        def apply[H <: Int, L <: Int](
            idxHigh: Inlined[H],
            idxLow: Inlined[L]
        )(using
            checkHigh: BitIndex.CheckNUB[H, W],
            checkLow: BitIndex.CheckNUB[L, W],
            checkHiLo: BitsHiLo.Check[H, L],
            dfc: DFC
        ): DFVal[DFBits[H - L + 1], Modifier[A, Any, Any, P]] = trydf {
          checkHigh(idxHigh, lhs.widthInt)
          checkLow(idxLow, lhs.widthInt)
          checkHiLo(idxHigh, idxLow)
          DFVal.Alias.ApplyRange(lhs, idxHigh, idxLow)
        }
        def unary_~(using DFC): DFValTP[DFBits[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_~, List(lhs))
        }
        def msbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any, P]] =
          lhs.apply(lhs.widthInt.value - 1)
        def lsbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any, P]] =
          lhs.apply(0)
        // TODO: IntP
        def msbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFC
        ): DFValTP[DFBits[RW], P] = trydf {
          check(lhs.widthInt, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, lhs.widthInt - 1, lhs.widthInt - updatedWidth)
            .asValTP[DFBits[RW], P]
        }
        // TODO: IntP
        def lsbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.CheckNUB[W, RW],
            dfc: DFC
        ): DFValTP[DFBits[RW], P] = trydf {
          check(lhs.widthInt, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, updatedWidth - 1, 0)
            .asValTP[DFBits[RW], P]
        }
        @targetName("shiftRightDFBits")
        def >>(shift: DFUInt.Val.UBArg.Exact[W])(using
            dfc: DFC
        ): DFValTP[DFBits[W], P | shift.tc.OutP] = trydf {
          val shiftVal = shift(lhs.widthIntParam)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.>>, List(lhs, shiftVal))
        }
        @targetName("shiftLeftDFBits")
        def <<(shift: DFUInt.Val.UBArg.Exact[W])(using
            dfc: DFC
        ): DFValTP[DFBits[W], P | shift.tc.OutP] = trydf {
          val shiftVal = shift(lhs.widthIntParam)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.<<, List(lhs, shiftVal))
        }
      end extension
      extension [L](lhs: L)
        def ++[RW <: IntP, RP](
            rhs: DFValTP[DFBits[RW], RP]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            c: Candidate[es.Out]
        ): DFValTP[DFBits[IntP.+[c.OutW, RW]], c.OutP | RP] = trydf {
          val lhsVal = c(es(lhs))
          val width = lhsVal.widthIntParam + rhs.widthIntParam
          DFVal.Func(DFBits(width), FuncOp.++, List(lhsVal, rhs))
        }
        def &[RW <: IntP, RP](
            rhs: DFValTP[DFBits[RW], RP]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.CheckNUB[c.OutW, RW]): DFValTP[DFBits[RW], c.OutP | RP] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.widthInt, rhs.widthInt)
          DFVal.Func(rhs.dfType, FuncOp.&, List(lhsVal, rhs))
        }
        def |[RW <: IntP, RP](
            rhs: DFValTP[DFBits[RW], RP]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.CheckNUB[c.OutW, RW]): DFValTP[DFBits[RW], c.OutP | RP] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.widthInt, rhs.widthInt)
          DFVal.Func(rhs.dfType, FuncOp.|, List(lhsVal, rhs))
        }
        def ^[RW <: IntP, RP](
            rhs: DFValTP[DFBits[RW], RP]
        )(using
            es: Exact.Summon[L, lhs.type]
        )(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.CheckNUB[c.OutW, RW]): DFValTP[DFBits[RW], c.OutP | RP] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.widthInt, rhs.widthInt)
          DFVal.Func(rhs.dfType, FuncOp.^, List(lhsVal, rhs))
        }
      end extension

    end Ops
  end Val
end DFBits
