package dfhdl.core
import dfhdl.compiler.ir
import ir.DFVal.Func.Op as FuncOp
import dfhdl.internals.*

import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import scala.util.boundary, boundary.break
type DFBits[W <: Int] = DFType[ir.DFBits, Args1[W]]
import DFDecimal.Constraints.`LW == RW`

object DFBits:
  def apply[W <: Int](width: Inlined[W])(using
      check: Arg.Width.Check[W]
  ): DFBits[W] =
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  def forced[W <: Int](width: Int): DFBits[W] =
    val check = summon[Arg.Width.Check[Int]]
    check(width)
    ir.DFBits(width).asFE[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int & Singleton](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] =
    DFBits[W](Inlined.forced[W](valueOf[W]))

  given bitsDFType[W <: Int](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] = DFBits[W](Inlined.forced[W](valueOf[W]))

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
  protected object BitsHiLo
      extends Check2[
        Int,
        Int,
        [H <: Int, L <: Int] =>> H >= L,
        [H <: Int, L <: Int] =>> "Low index " + L + " is bigger than High bit index " + H
      ]
  trait CompareCheck[
      ValW <: Int,
      ArgW <: Int,
      Castle <: Boolean // castling of dfVal and arg
  ]:
    def apply(dfValWidth: Int, argWidth: Int): Unit
  object CompareCheck:
    given [
        ValW <: Int,
        ArgW <: Int,
        Castle <: Boolean
    ](using
        lw: Id[ITE[Castle, ArgW, ValW]],
        rw: Id[ITE[Castle, ValW, ArgW]]
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
    private val widthExp = "([0-9]+)'(.*)".r
    def fromBinString(
        bin: String
    ): Either[String, (BitVector, BitVector)] = boundary {
      val (explicitWidth, word) = bin match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, bin)
      val (valueBits, bubbleBits) =
        word.foldLeft((BitVector.empty, BitVector.empty)) {
          case (t, '_' | ' ') => t // ignoring underscore or space
          case ((v, b), c) =>
            c match // bin mode
              case '?' => (v :+ false, b :+ true)
              case '0' => (v :+ false, b :+ false)
              case '1' => (v :+ true, b :+ false)
              case x =>
                break(Left(s"Found invalid binary character: $x"))
        }
      val actualWidth = valueBits.lengthOfValue.toInt
      explicitWidth match
        case Some(width) if width < actualWidth =>
          Left(
            s"Explicit given width ($width) is smaller than the actual width ($actualWidth)"
          )
        case Some(width) =>
          Right((valueBits.resize(width), bubbleBits.resize(width)))
        case None => Right((valueBits, bubbleBits))
    }
    private val isHex = "[0-9a-fA-F]".r
    def fromHexString(
        hex: String
    ): Either[String, (BitVector, BitVector)] = boundary {
      val (explicitWidth, word) = hex match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, hex)
      val (valueBits, bubbleBits, binMode) =
        word.foldLeft((BitVector.empty, BitVector.empty, false)) {
          case (t, '_' | ' ') => t // ignoring underscore or space
          case ((v, b, false), c) =>
            c match // hex mode
              case '{' => (v, b, true)
              case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
              case isHex() =>
                (
                  v ++ BitVector.fromHex(c.toString).get,
                  b ++ BitVector.low(4),
                  false
                )
              case x =>
                break(Left(s"Found invalid hex character: $x"))
          case ((v, b, true), c) =>
            c match // bin mode
              case '}' => (v, b, false)
              case '?' => (v :+ false, b :+ true, true)
              case '0' => (v :+ false, b :+ false, true)
              case '1' => (v :+ true, b :+ false, true)
              case x =>
                break(Left(s"Found invalid binary character in binary mode: $x"))
        }
      if (binMode) Left(s"Missing closing braces of binary mode")
      else
        val actualWidth = valueBits.lengthOfValue.toInt
        explicitWidth match
          case Some(width) if width < actualWidth =>
            Left(
              s"Explicit given width ($width) is smaller than the actual width ($actualWidth)"
            )
          case Some(width) =>
            Right((valueBits.resize(width), bubbleBits.resize(width)))
          case None => Right((valueBits, bubbleBits))
    }
    class BParts[P <: Tuple](parts: P):
      transparent inline def apply(inline args: Any*): Any =
        ${ applyMacro('{ "b" })('parts, 'args) }
      transparent inline def unapplySeq[T <: DFTypeAny](
          inline arg: DFValOf[T]
      )(using DFC): Option[Seq[Any]] =
        ${ unapplySeqMacro('{ "b" })('parts, 'arg) }

    class HParts[P <: Tuple](parts: P):
      transparent inline def apply(inline args: Any*): Any =
        ${ applyMacro('{ "h" })('parts, 'args) }
      transparent inline def unapplySeq[T <: DFTypeAny](
          inline arg: DFValOf[T]
      )(using DFC): Option[Seq[Any]] =
        ${ unapplySeqMacro('{ "h" })('parts, 'arg) }

    extension (inline sc: StringContext)
      /** Binary Bits Vector Token String Interpolator
        *
        * Interpolator Syntax: {{{b"width'bin"}}}
        *   - `bin` is a char sequence of '0', '1', and '?' (to indicate a bit bubble).
        *   - `bin` also allows separators of space ' ' or underscore '_' that are ignored.
        *   - `width` (with the following tag char `'`) is optional. If it's not specified, the
        *     width is determined by the length of the char sequence. Otherwise, the width is set as
        *     required. If the required width is longer than the char sequence length, then zeros
        *     are added as the MSBits. If the required width is shorter then the char sequence, it
        *     is accepted only if the MSBits it is truncating are zeros. Otherwise, a compilation
        *     error is generated.
        * @example
        *   {{{
        *   b"1"        //value = 1
        *   b"1000"     //value = 1000
        *   b"8'1000"   //value = 00001000
        *   b"3'0100"   //value = 100
        *   b"3'1100"   //error
        *   b"1?11"     //value = 1?11 (? is a bubble bit)
        *   b"11_00"    //value = 1100
        *   }}}
        * @note
        *   The string interpolator currently does not accept external arguments with `\${arg}`
        * @return
        *   Bits vector token.
        */
      transparent inline def b: Any = ${ SIParts.scMacro[BParts]('sc) }

      /** Hexadecimal Bits Vector Token String Interpolator
        *
        * Interpolator Syntax: {{{b"width'hex"}}}
        *   - `hex` is a char sequence of '0'-'9','A'-'F','a'-'f','?' (to indicate a 4-bit bubble).
        *     Each character is equivalent to a 4-bits nibble.
        *   - `hex` also allows separators of space ' ' or underscore '_' that are ignored.
        *   - `hex` also supports a binary mode within `{bin}`, where bin is equivalent to the char
        *     sequence of the binary string interpolator (see [[b]]). So between 4-bit hex nibbles,
        *     it is possible to insert a binary bit sequence of any length that is not necessarily
        *     dividable by 4.
        *   - `width` (with the following tag char `'`) is optional. If it's not specified, the
        *     width is determined by the length of the char sequence. Otherwise, the width is set as
        *     required. If the required width is longer than the char sequence length, then zeros
        *     are added as the MSBits. If the required width is shorter then the char sequence, it
        *     is accepted only if the MSBits it is truncating are zeros. Otherwise, a compilation
        *     error is generated.
        * @example
        *   {{{
        *   h"1"        //value = 0001
        *   h"27"       //value = 00100111
        *   h"6'27"     //value = 100111
        *   h"5'27"     //error
        *   h"2?"       //value = 0010????
        *   h"F{00}F"   //value = 1111001111
        *   h"3_3"      //value = 00110011
        *   }}}
        * @note
        *   The string interpolator currently does not accept external arguments with `\${arg}`
        * @return
        *   Bits vector token.
        */
      transparent inline def h: Any = ${ SIParts.scMacro[HParts]('sc) }
    end extension

    private def applyMacro[P <: Tuple](opExpr: Expr[String])(
        scParts: Expr[P],
        args: Expr[Seq[Any]]
    )(using Quotes, Type[P]): Expr[DFConstAny] =
      scParts.scPartsWithArgs(args).interpolate(opExpr)

    extension (using Quotes)(fullTerm: quotes.reflect.Term)
      private def interpolate(
          opExpr: Expr[String]
      ): Expr[DFConstAny] =
        import quotes.reflect.*
        val opStr = opExpr.value.get
        val widthTpe: TypeRepr = fullTerm match
          case Literal(StringConstant(t)) =>
            val res = opStr match
              case "b" => fromBinString(t)
              case "h" => fromHexString(t)
            res match
              case Right((valueBits, bubbleBits)) =>
                ConstantType(IntConstant(valueBits.length.toInt))
              case Left(msg) =>
                report.errorAndAbort(msg)
          case _ => TypeRepr.of[Int]
        val widthType = widthTpe.asType.asInstanceOf[Type[Int]]
        val fullExpr = opStr match
          case "b" => '{ fromBinString(${ fullTerm.asExprOf[String] }) }
          case "h" => '{ fromHexString(${ fullTerm.asExprOf[String] }) }
        '{
          val (valueBits, bubbleBits) = ${ fullExpr }.toOption.get
          val width = valueBits.length.toInt
          val dfc = compiletime.summonInline[DFC]
          DFVal.Const(
            DFBits.forced[widthType.Underlying](width),
            (valueBits, bubbleBits),
            named = true
          )(using dfc)
        }
    private def unapplySeqMacro[P <: Tuple, T <: DFTypeAny](
        opForcedExpr: Expr[String]
    )(
        scParts: Expr[P],
        arg: Expr[DFValOf[T]]
    )(using Quotes, Type[P], Type[T]): Expr[Option[Seq[Any]]] =
      import quotes.reflect.*
      val parts = TypeRepr.of[P].getTupleArgs
      val op = opForcedExpr.value.get
      if (TypeRepr.of[P].getTupleArgs.length > 1)
        val vArgs = Varargs(opForcedExpr :: parts.map {
          case ConstantType(StringConstant(part: String)) =>
            val partFiltered = part.filter {
              case '_' | ' ' | '?'        => false
              case isHex() if op == "h"   => true
              case '0' | '1' if op == "b" => true
              case x =>
                report.errorAndAbort(
                  s"""Found invalid character: ${x}. 
                      |Note: string interpolation with value extraction does not support the `[w']` width extension syntax.""".stripMargin
                )
            }
            Literal(StringConstant(partFiltered)).asExprOf[String]
        })
        '{
          Some(Seq(${ vArgs }*))
        }
      else
        val dfVal =
          SIParts
            .tupleToExprs(scParts)
            .head
            .asTerm
            .interpolate(opForcedExpr)
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
    trait Candidate[R]:
      type OutW <: Int
      type OutP
      type Out = DFValTP[DFBits[OutW], OutP]
      def apply(value: R)(using DFC): Out
    object Candidate:
      inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(
          "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal constant via the `d\"<width>'<number>\"` string interpolation."
        )
      given fromDFBits[W <: Int, P, R <: DFValTP[DFBits[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out = value
      given fromDFBoolOrBit[P, R <: DFValTP[DFBoolOrBit, P]]: Candidate[R] with
        type OutW = 1
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          value.bits
      given fromDFUInt[W <: Int, P, R <: DFValTP[DFUInt[W], P]]: Candidate[R] with
        type OutW = W
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          if (value.hasTag[DFVal.TruncateTag]) value.bits.tag(DFVal.TruncateTag)
          else if (value.hasTag[DFVal.ExtendTag]) value.bits.tag(DFVal.ExtendTag)
          else value.bits
      given fromDFTuple[T <: NonEmptyTuple, P, R <: DFValTP[DFTuple[T], P], W <: Width[DFTuple[T]]](
          using w: W
      ): Candidate[R] with
        type OutW = w.Out
        type OutP = P
        def apply(value: R)(using DFC): Out =
          import DFVal.Ops.bits
          value.bits
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        )
      inline given errDFSInt[W <: Int, R <: DFValOf[DFSInt[W]]]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        )

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
              case _ =>
                dfValIR.asValAny.bits(using Width.wide).asValOf[DFBits[Int]]
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple]: Candidate[R] = ${ DFBitsMacro[R] }
      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        val wType = rTpe.calcValWidth(false).asTypeOf[Int]
        val pType = rTpe.isConstTpe.asTypeOf[Any]
        '{
          new Candidate[R]:
            type OutW = wType.Underlying
            type OutP = pType.Underlying
            def apply(value: R)(using DFC): Out =
              valueToBits(value).asValTP[DFBits[OutW], pType.Underlying]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFBits[Int],
          dfVal: DFValOf[DFBits[Int]]
      ): DFValOf[DFBits[Int]] =
        `LW == RW`(dfType.width, dfVal.width)
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
      given DFBitsFromCandidate[LW <: Int, V, IC <: Candidate[V]](using ic: IC)(using
          check: `LW == RW`.Check[LW, ic.OutW]
      ): TC[DFBits[LW], V] with
        type OutP = ic.OutP
        def conv(dfType: DFBits[LW], value: V)(using dfc: DFC): Out =
          import Ops.resizeBits
          val dfVal = ic(value)
          (dfType.asIR: ir.DFType) match
            case ir.DFNothing =>
              dfVal.nameInDFCPosition.asValTP[DFBits[LW], ic.OutP]
            case _ =>
              if (dfVal.hasTag[DFVal.TruncateTag] && dfType.width < dfVal.width)
                dfVal.resizeBits(dfType.width).asValTP[DFBits[LW], ic.OutP]
              else if (dfVal.hasTag[DFVal.ExtendTag] && dfType.width > dfVal.width)
                dfVal.resizeBits(dfType.width).asValTP[DFBits[LW], ic.OutP]
              else
                check(dfType.width, dfVal.width)
                dfVal.nameInDFCPosition.asValTP[DFBits[LW], ic.OutP]
        end conv
      end DFBitsFromCandidate
      given DFBitsFromSEV[LW <: Int, T <: BitOrBool, V <: SameElementsVector[T]]: TC[DFBits[LW], V]
      with
        type OutP = CONST
        def conv(dfType: DFBits[LW], value: V)(using DFC): Out =
          SameElementsVector.bitsValOf(dfType.width, value, named = true)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[LW <: Int, R, IC <: Candidate[R], Op <: FuncOp, C <: Boolean](
          using ic: IC
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        type OutP = ic.OutP
        def conv(dfType: DFBits[LW], arg: R)(using DFC): Out =
          val dfValArg = ic(arg)
          check(dfType.width, dfValArg.dfType.width)
          dfValArg.asValTP[DFBits[LW], ic.OutP]
      given DFBitsCompareSEV[
          LW <: Int,
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
          SameElementsVector.bitsValOf(dfType.width, arg, named = true)
      end DFBitsCompareSEV
    end Compare

    // this was defined separately from `Ops` to avoid collision with `.bits` used in `Ops`
    object TupleOps:
      // explicit conversion of a tuple to bits (concatenation)
      extension (inline tpl: NonEmptyTuple)
        transparent inline def toBits: Any = ${ bitsMacro('tpl) }
      private def bitsMacro(tpl: Expr[NonEmptyTuple])(using Quotes): Expr[Any] =
        import quotes.reflect.*
        val tplTerm = tpl.asTerm.exactTerm
        import Width.*
        val rTpe = tplTerm.tpe
        val wType = rTpe.calcValWidth(false).asTypeOf[Int]
        '{
          Val.Candidate
            .valueToBits($tpl)(using compiletime.summonInline[DFC])
            .asValOf[DFBits[wType.Underlying]]
        }
    end TupleOps

    object Ops:
      extension [W <: Int, P](lhs: DFValTP[DFBits[W], P])
        def truncate(using DFC): DFValTP[DFBits[Int], P] =
          lhs.tag(DFVal.TruncateTag).asValTP[DFBits[Int], P]
        private[DFBits] def resizeBits[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValTP[DFBits[RW], P] =
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFBits[RW]]
//          else
          DFVal.Alias.AsIs(DFBits(updatedWidth), lhs)
      end extension
      extension [T <: Int, P](iter: Iterable[DFValTP[DFBits[T], P]])
        protected[core] def concatBits(using DFC): DFValTP[DFBits[Int], P] =
          val width = Inlined.forced[Int](iter.map(_.width.value).sum)
          DFVal.Func(DFBits(width), FuncOp.++, iter.toList)
      extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
        def extend(using DFC): DFValTP[DFBits[Int], icL.OutP] =
          icL(lhs).tag(DFVal.ExtendTag).asValTP[DFBits[Int], icL.OutP]
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValTP[DFBits[RW], icL.OutP] = trydf { icL(lhs).resizeBits(updatedWidth) }
        def &[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: `LW == RW`.Check[icL.OutW, icR.OutW]
        ): DFValTP[DFBits[icL.OutW], icL.OutP | icR.OutP] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = icR(rhs)
          check(lhsVal.width, rhsVal.width)
          DFVal.Func(lhsVal.dfType, FuncOp.&, List(lhsVal, rhsVal))
        }
        def |[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: `LW == RW`.Check[icL.OutW, icR.OutW]
        ): DFValOf[DFBits[icL.OutW]] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = icR(rhs)
          check(lhsVal.width, rhsVal.width)
          DFVal.Func(lhsVal.dfType, FuncOp.|, List(lhsVal, rhsVal))
        }
        def ^[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: `LW == RW`.Check[icL.OutW, icR.OutW]
        ): DFValOf[DFBits[icL.OutW]] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = icR(rhs)
          check(lhsVal.width, rhsVal.width)
          DFVal.Func(lhsVal.dfType, FuncOp.^, List(lhsVal, rhsVal))
        }
        def repeat[N <: Int](num: Inlined[N])(using
            check: Arg.Positive.Check[N],
            dfc: DFC
        ): DFValOf[DFBits[icL.OutW * N]] = trydf {
          val lhsVal = icL(lhs)
          check(num)
          DFVal.Func(
            DFBits(lhsVal.dfType.width * num),
            FuncOp.++,
            List.fill(num)(lhsVal)
          )
        }
        def ++[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC
        ): DFValOf[DFBits[icL.OutW + icR.OutW]] = trydf {
          val lhsVal = icL(lhs)
          val rhsVal = icR(rhs)
          val width = lhsVal.width + rhsVal.width
          DFVal.Func(DFBits(width), FuncOp.++, List(lhsVal, rhsVal))
        }
      end extension

      extension [W <: Int, A, C, I, P](
          lhs: DFVal[DFBits[W], Modifier[A, C, I, P]]
      )
        def as[AT <: DFType.Supported](
            aliasType: AT
        )(using tc: DFType.TC[AT])(using
            aW: Width[tc.Type],
            dfc: DFC
        )(using check: `AW == TW`.Check[aW.Out, W]): DFValTP[tc.Type, P] = trydf {
          val aliasDFType = tc(aliasType)
          check.apply(aliasDFType.asIR.width, lhs.width)
          DFVal.Alias.AsIs(aliasDFType, lhs)
        }
        def uint(using DFC): DFValTP[DFUInt[W], P] = trydf { as(DFUInt(lhs.width)) }
        def sint(using DFC): DFValTP[DFSInt[W], P] = trydf { as(DFSInt(lhs.width)) }
        def apply[Idx](
            relIdx: Exact[Idx]
        )(using
            c: DFUInt.Val.UBArg[W, Idx],
            dfc: DFC
        ): DFVal[DFBit, Modifier[A, Any, Any, P]] = trydf {
          DFVal.Alias.ApplyIdx(DFBit, lhs, c(lhs.width, relIdx)(using dfc.anonymize))
        }
        def apply[H <: Int, L <: Int](
            relBitHigh: Inlined[H],
            relBitLow: Inlined[L]
        )(using
            checkHigh: BitIndex.Check[H, W],
            checkLow: BitIndex.Check[L, W],
            checkHiLo: BitsHiLo.Check[H, L],
            dfc: DFC
        ): DFVal[DFBits[H - L + 1], Modifier[A, Any, Any, P]] = trydf {
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          DFVal.Alias.ApplyRange(lhs, relBitHigh, relBitLow)
        }
        def unary_~(using DFC): DFValTP[DFBits[W], P] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_~, List(lhs))
        }
        def msbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any, P]] =
          lhs.apply(lhs.width.value - 1)
        def lsbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any, P]] =
          lhs.apply(0)
        def msbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.Check[W, RW],
            dfc: DFC
        ): DFValTP[DFBits[RW], P] = trydf {
          check(lhs.width, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, lhs.width - 1, lhs.width - updatedWidth)
            .asValTP[DFBits[RW], P]
        }
        def lsbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.Check[W, RW],
            dfc: DFC
        ): DFValTP[DFBits[RW], P] = trydf {
          check(lhs.width, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, updatedWidth - 1, 0)
            .asValTP[DFBits[RW], P]
        }
        @targetName("shiftRightDFBits")
        def >>[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValOf[DFBits[W]] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.>>, List(lhs, shiftVal))
        }
        @targetName("shiftLeftDFBits")
        def <<[R](shift: Exact[R])(using
            c: DFUInt.Val.UBArg[W, R],
            dfc: DFC
        ): DFValOf[DFBits[W]] = trydf {
          val shiftVal = c(lhs.width, shift)(using dfc.anonymize)
          DFVal.Func(lhs.dfType, FuncOp.<<, List(lhs, shiftVal))
        }
      end extension
      extension [L](lhs: L)
        def ++[RW <: Int](
            rhs: DFValOf[DFBits[RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            c: Candidate[es.Out]
        ): DFValOf[DFBits[c.OutW + RW]] = trydf {
          val lhsVal = c(es(lhs))
          val width = lhsVal.width + rhs.width
          DFVal.Func(DFBits(width), FuncOp.++, List(lhsVal, rhs))
        }
        def &[RW <: Int](
            rhs: DFValOf[DFBits[RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.Check[c.OutW, RW]): DFValOf[DFBits[RW]] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.width, rhs.width)
          DFVal.Func(rhs.dfType, FuncOp.&, List(lhsVal, rhs))
        }
        def |[RW <: Int](
            rhs: DFValOf[DFBits[RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.Check[c.OutW, RW]): DFValOf[DFBits[RW]] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.width, rhs.width)
          DFVal.Func(rhs.dfType, FuncOp.|, List(lhsVal, rhs))
        }
        def ^[RW <: Int](
            rhs: DFValOf[DFBits[RW]]
        )(using es: Exact.Summon[L, lhs.type])(using
            dfc: DFC,
            c: Candidate[es.Out]
        )(using check: `LW == RW`.Check[c.OutW, RW]): DFValOf[DFBits[RW]] = trydf {
          val lhsVal = c(es(lhs))
          check(lhsVal.width, rhs.width)
          DFVal.Func(rhs.dfType, FuncOp.^, List(lhsVal, rhs))
        }
      end extension

    end Ops
  end Val
end DFBits
