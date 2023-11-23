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
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W])(using
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

  type Token[W <: Int] = DFToken[DFBits[W]]
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR)(data).asTokenOf[DFBits[W]]
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        valueBits: BitVector,
        bubbleBits: BitVector
    ): Token[W] =
      Token(DFBits(width), (valueBits, bubbleBits))
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        value: Bubble
    ): Token[W] =
      Token(
        width,
        BitVector.low(width.value),
        BitVector.high(width.value)
      )
    protected[core] def apply[W <: Int, T <: BitOrBool](
        width: Inlined[W],
        sev: SameElementsVector[T]
    ): Token[W] =
      val boolVal = sev.value match
        case b: Boolean => b
        case i: Int     => i > 0
      Token(
        width,
        BitVector.fill(width.value)(boolVal),
        BitVector.low(width.value)
      )
    extension [W <: Int](token: DFBits.Token[W])
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    object Conversions:
      given DFBitsTokenConversionSing[W <: Int & Singleton, V](using
          tc: DFToken.TC[DFBits[W], V],
          w: ValueOf[W]
      ): Conversion[V, DFBits[W] <> TOKEN] = value => tc(DFBits(valueOf[W]), value)
    end Conversions

    @implicitNotFound(
      "Argument of type ${R} is not a proper candidate for a Bits token."
    )
    trait Candidate[R]:
      type OutW <: Int
      def apply(arg: R): Token[OutW]
    object Candidate:
      protected[DFBits] inline val intErrMsg =
        "An integer value cannot be a candidate for a Bits type.\nTry explicitly using a decimal token via the `d\"<width>'<number>\"` string interpolation."
      inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(intErrMsg)
      type Aux[R, W <: Int] = Candidate[R] { type OutW = W }
      given fromDFBitsToken[W <: Int, R <: Token[W]]: Candidate[R] with
        type OutW = W
        def apply(arg: R): Token[OutW] = arg
      given fromDFUIntToken[W <: Int, R <: DFUInt.Token[W]]: Candidate[R] with
        type OutW = W
        def apply(arg: R): Token[OutW] =
          import DFToken.Ops.bits
          arg.bits
      given fromDFBitCandidate[R, T <: DFBoolOrBit](using
          ic: DFBoolOrBit.Token.Candidate.Aux[R, T]
      )(using T =:= DFBit): Candidate[R] with
        type OutW = 1
        def apply(arg: R): Token[1] =
          import DFToken.Ops.bits
          ic(arg).bits
      private def valueToBits(value: Any): DFBits[Int] <> TOKEN =
        import DFBits.Token.Ops.++
        value match
          case x: NonEmptyTuple =>
            x.toList
              .map(valueToBits)
              .reduce((l, r) => (l ++ r).asTokenOf[DFBits[Int]])
          case i: Int =>
            Token(1, BitVector.bit(i > 0), BitVector.zero)
          case token: DFToken[_] =>
            val tokenIR = token.asIR
            val tokenOut = tokenIR.dfType match
              case _: ir.DFBits => tokenIR.asTokenOf[DFBits[Int]]
              case _            => tokenIR.bits.asTokenOf[DFBits[Int]]
            tokenOut
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple, V <: NonEmptyTuple]: Candidate[V] = ${
        DFBitsMacro[V]
      }
      def DFBitsMacro[V](using
          Quotes,
          Type[V]
      ): Expr[Candidate[V]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[V]
        val wType = rTpe.calcValWidth(true).asTypeOf[Int]
        '{
          new Candidate[V]:
            type OutW = wType.Underlying
            def apply(value: V): DFToken[DFBits[OutW]] =
              valueToBits(value).asTokenOf[DFBits[OutW]]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFToken.TC
      protected object `W == VW`
          extends Check2[
            Int,
            Int,
            [W <: Int, VW <: Int] =>> W == VW,
            [W <: Int, VW <: Int] =>> "The token width (" + VW +
              ") is different than the DFType width (" + W + ")."
          ]

      given DFBitsTokenFromCandidate[W <: Int, R, IC <: Candidate[R]](using ic: IC)(using
          check: `W == VW`.Check[W, ic.OutW]
      ): TC[DFBits[W], R] with
        def conv(dfType: DFBits[W], value: R)(using Ctx): Out =
          val tokenArg = ic(value)
          check(dfType.width, tokenArg.asIR.width)
          tokenArg.asInstanceOf[Out]

      given DFBitsTokenFromSEV[W <: Int, T <: BitOrBool, V <: SameElementsVector[T]]
          : TC[DFBits[W], V] with
        def conv(dfType: DFBits[W], value: V)(using Ctx): Out =
          DFBits.Token(dfType.width, value)
    end TC

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

    object StrInterp:
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
          *     width is determined by the length of the char sequence. Otherwise, the width is set
          *     as required. If the required width is longer than the char sequence length, then
          *     zeros are added as the MSBits. If the required width is shorter then the char
          *     sequence, it is accepted only if the MSBits it is truncating are zeros. Otherwise, a
          *     compilation error is generated.
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
          *   - `hex` is a char sequence of '0'-'9','A'-'F','a'-'f','?' (to indicate a 4-bit
          *     bubble). Each character is equivalent to a 4-bits nibble.
          *   - `hex` also allows separators of space ' ' or underscore '_' that are ignored.
          *   - `hex` also supports a binary mode within `{bin}`, where bin is equivalent to the
          *     char sequence of the binary string interpolator (see [[b]]). So between 4-bit hex
          *     nibbles, it is possible to insert a binary bit sequence of any length that is not
          *     necessarily dividable by 4.
          *   - `width` (with the following tag char `'`) is optional. If it's not specified, the
          *     width is determined by the length of the char sequence. Otherwise, the width is set
          *     as required. If the required width is longer than the char sequence length, then
          *     zeros are added as the MSBits. If the required width is shorter then the char
          *     sequence, it is accepted only if the MSBits it is truncating are zeros. Otherwise, a
          *     compilation error is generated.
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
      )(using Quotes, Type[P]): Expr[DFTokenAny] =
        scParts.scPartsWithArgs(args).interpolate(opExpr)

      extension (using Quotes)(fullTerm: quotes.reflect.Term)
        private def interpolate(
            opExpr: Expr[String]
        ): Expr[DFTokenAny] =
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
            val width =
              dfhdl.internals.Inlined
                .forced[widthType.Underlying](valueBits.length.toInt)
            Token[widthType.Underlying](width, valueBits, bubbleBits)
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
          val token =
            SIParts
              .tupleToExprs(scParts)
              .head
              .asTerm
              .interpolate(opForcedExpr)
          val tokenType = token.asTerm.tpe.asTypeOf[DFTokenAny]
          '{
            val tc = compiletime
              .summonInline[
                DFVal.Compare[T, tokenType.Underlying, FuncOp.===.type, false]
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

    object Compare:
      import DFToken.Compare
      given [LW <: Int, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        def conv(dfType: DFBits[LW], arg: R)(using Ctx): DFBits[LW] <> TOKEN =
          val tokenArg = ic(arg)
          check(
            dfType.width,
            tokenArg.dfType.width
          )
          tokenArg.asTokenOf[DFBits[LW]]
      end given
      given [LW <: Int, Op <: FuncOp, C <: Boolean, T <: BitOrBool, V <: SameElementsVector[T]](
          using
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], V, Op, C] with
        def conv(dfType: DFBits[LW], arg: V)(using Ctx): DFBits[LW] <> TOKEN =
          Token(dfType.width, arg)
    end Compare

    object Ops:
      extension [LW <: Int](lhs: DFBits.Token[LW])
        def as[A <: DFType.Supported](
            aliasType: A
        )(using tc: DFType.TC[A])(using
            aW: Width[tc.Type]
        )(using check: `AW == TW`.Check[aW.Out, LW]): DFToken[tc.Type] =
          val dfType = tc(aliasType).asIR
          check(dfType.width, lhs.width)
          lhs.asIR
            .asInstanceOf[ir.DFBits.Token]
            .as(dfType)
            .asTokenOf[tc.Type]
        def uint: DFUInt.Token[LW] = as(DFUInt(lhs.width))
        def sint: DFSInt.Token[LW] = as(DFSInt(lhs.width))
        def repeat[N <: Int](num: Inlined[N])(using
            check: Arg.Positive.Check[N]
        ): DFToken[DFBits[LW * N]] =
          check(num)
          Token(
            lhs.width * num,
            BitVector.concat(List.fill(num)(lhs.data._1)),
            BitVector.concat(List.fill(num)(lhs.data._2))
          )
        def apply[I <: Int](
            relIdx: Inlined[I]
        )(using check: BitIndex.Check[I, LW]): DFBoolOrBit.Token =
          check(relIdx, lhs.width)
          val value = lhs.valueBits.bit(relIdx.toLong)
          val bubble = lhs.bubbleBits.bit(relIdx.toLong)
          val tokenData = if (bubble) None else Some(value)
          DFBoolOrBit.Token(DFBit, tokenData)
        def msbit: DFBoolOrBit.Token = apply(lhs.width - 1)
        def lsbit: DFBoolOrBit.Token = apply(0)

        def apply[H <: Int, L <: Int](
            relBitHigh: Inlined[H],
            relBitLow: Inlined[L]
        )(using
            checkHigh: BitIndex.Check[H, LW],
            checkLow: BitIndex.Check[L, LW],
            checkHiLo: BitsHiLo.Check[H, L]
        ): DFBits.Token[H - L + 1] =
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          val valueBits =
            lhs.valueBits.bits(relBitHigh.toLong, relBitLow.toLong)
          val bubbleBits =
            lhs.bubbleBits.bits(relBitHigh.toLong, relBitLow.toLong)
          val width = relBitHigh - relBitLow + 1
          DFBits.Token(width, valueBits, bubbleBits)
        end apply

        @targetName("bitsResize")
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            check: Arg.Width.Check[RW]
        ): Token[RW] =
          if (updatedWidth == lhs.width) lhs.asTokenOf[DFBits[RW]]
          else
            check(updatedWidth)
            val data = lhs.data
            import dfhdl.internals.{resize => resizeBV}
            Token(
              updatedWidth,
              data._1.resizeBV(updatedWidth),
              data._2.resizeBV(updatedWidth)
            )

        @targetName("concat")
        def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
          val width = lhs.width + rhs.width
          val valueBits = lhs.valueBits ++ rhs.valueBits
          val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
          Token(width, valueBits, bubbleBits)

        @targetName("bitwiseAnd")
        def &[RW <: Int](rhs: DFBits.Token[RW])(using
            bb: Bubble.Behaviour
        ): DFBits.Token[LW] =
          require(lhs.width == rhs.width)
          val width = lhs.width
          bb match
            case Bubble.Behaviour.Stall =>
              Token(
                width,
                lhs.valueBits & rhs.valueBits,
                lhs.bubbleBits | rhs.bubbleBits
              )
            case Bubble.Behaviour.DontCare =>
              val valueBits =
                (lhs.valueBits | lhs.bubbleBits) & (rhs.valueBits | rhs.bubbleBits)
              val bubbleBits =
                (lhs.bubbleBits & rhs.bubbleBits) | (lhs.bubbleBits & rhs.valueBits) |
                  (rhs.bubbleBits & lhs.valueBits)
              Token(width, valueBits, bubbleBits)
        end &
      end extension
    end Ops
  end Token

  object Val:
    trait Candidate[R]:
      type OutW <: Int
      def apply(value: R)(using DFC): DFValOf[DFBits[OutW]]
    object Candidate:
      inline given errorOnInt[V <: Int]: Candidate[V] =
        compiletime.error(Token.Candidate.intErrMsg)
      given fromDFBits[W <: Int, R <: DFValOf[DFBits[W]]]: Candidate[R] with
        type OutW = W
        def apply(value: R)(using DFC): DFValOf[DFBits[W]] =
          value
      given fromDFBoolOrBit[R <: DFValOf[DFBoolOrBit]]: Candidate[R] with
        type OutW = 1
        def apply(value: R)(using DFC): DFValOf[DFBits[1]] =
          import DFVal.Ops.bits
          value.bits
      given fromDFUInt[W <: Int, R <: DFValOf[DFUInt[W]]]: Candidate[R] with
        type OutW = W
        def apply(value: R)(using DFC): DFValOf[DFBits[W]] =
          import DFVal.Ops.bits
          if (value.hasTag[DFVal.TruncateTag]) value.bits.tag(DFVal.TruncateTag)
          else if (value.hasTag[DFVal.ExtendTag]) value.bits.tag(DFVal.ExtendTag)
          else value.bits
      inline given errDFEncoding[E <: DFEncoding]: Candidate[E] =
        compiletime.error(
          "Cannot apply an enum entry value to a bits variable."
        )
      inline given errDFSInt[W <: Int, R <: DFValOf[DFSInt[W]]]: Candidate[R] =
        compiletime.error(
          "Cannot apply a signed value to a bits variable.\nConsider applying `.bits` conversion to resolve this issue."
        )
      given fromDFBitsTokenCandidate[R, IC <: Token.Candidate[R]](using ic: IC): Candidate[R] with
        type OutW = ic.OutW
        def apply(arg: R)(using DFC): DFValOf[DFBits[OutW]] =
          DFVal.Const(ic(arg), named = true)

      private[Val] def valueToBits(value: Any)(using dfc: DFC): DFValOf[DFBits[Int]] =
        import DFBits.Val.Ops.concatBits
        val dfcAnon = dfc.anonymize
        value match
          case x: NonEmptyTuple =>
            x.toList.map(x => valueToBits(x)(using dfcAnon)).concatBits
          case i: Int =>
            DFVal.Const(Token(1, BitVector.bit(i > 0), BitVector.zero), named = true)
          case token: DFToken[_] =>
            val tokenIR = token.asIR
            val tokenOut = tokenIR.dfType match
              case _: ir.DFBits => tokenIR.asTokenOf[DFBits[Int]]
              case _            => tokenIR.bits.asTokenOf[DFBits[Int]]
            DFVal.Const(tokenOut, named = true)
          case dfVal: DFVal[_, _] =>
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
        '{
          new Candidate[R]:
            type OutW = wType.Underlying
            def apply(value: R)(using DFC): DFValOf[DFBits[OutW]] =
              valueToBits(value).asValOf[DFBits[OutW]]
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
      given DFBitsFromCandidate[LW <: Int, V](using ic: Candidate[V])(using
          check: `LW == RW`.Check[LW, ic.OutW]
      ): TC[DFBits[LW], V] with
        def conv(dfType: DFBits[LW], value: V)(using dfc: Ctx): DFValOf[DFBits[LW]] =
          import Ops.resizeBits
          val dfVal = ic(value)
          if (dfVal.hasTag[DFVal.TruncateTag] && dfType.width < dfVal.width)
            dfVal.anonymize.resizeBits(dfType.width)
          else if (dfVal.hasTag[DFVal.ExtendTag] && dfType.width > dfVal.width)
            dfVal.anonymize.resizeBits(dfType.width)
          else
            check(dfType.width, dfVal.width)
            dfVal.asValOf[DFBits[LW]]
      end DFBitsFromCandidate
      given DFBitsFromSEV[LW <: Int, T <: BitOrBool, V <: SameElementsVector[T]]: TC[DFBits[LW], V]
      with
        def conv(dfType: DFBits[LW], value: V)(using Ctx): DFValOf[DFBits[LW]] =
          DFVal.Const(Token(dfType.width, value), named = true)
    end TC

    object Compare:
      import DFVal.Compare
      given DFBitsCompareCandidate[LW <: Int, R, Op <: FuncOp, C <: Boolean](using
          ic: Candidate[R]
      )(using
          check: CompareCheck[LW, ic.OutW, C],
          op: ValueOf[Op],
          castling: ValueOf[C]
      ): Compare[DFBits[LW], R, Op, C] with
        def conv(dfType: DFBits[LW], arg: R)(using Ctx): DFValOf[DFBits[LW]] =
          val dfValArg = ic(arg)
          check(dfType.width, dfValArg.dfType.width)
          dfValArg.asValOf[DFBits[LW]]
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
        def conv(dfType: DFBits[LW], arg: V)(using Ctx): DFValOf[DFBits[LW]] =
          DFVal.Const(Token(dfType.width, arg), named = true)
      end DFBitsCompareSEV
    end Compare

    // this was defined separately from `Ops` to avoid collision with `.bits` used in `Ops`
    object TupleOps:
      // explicit conversion of a tuple to bits (conctatenation)
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
            .asIR
            .asValOf[DFBits[wType.Underlying]]
        }
    end TupleOps

    object Ops:
      extension [W <: Int](lhs: DFValOf[DFBits[W]])
        def truncate(using DFC): DFValOf[DFBits[Int]] =
          lhs.tag(DFVal.TruncateTag).asValOf[DFBits[Int]]
        private[DFBits] def resizeBits[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValOf[DFBits[RW]] =
          import Token.Ops.{resize => resizeToken}
          // TODO: why this causes anonymous references?
//          if (lhs.width == updatedWidth) lhs.asValOf[DFBits[RW]]
//          else
          DFVal.Alias.AsIs(
            DFBits(updatedWidth),
            lhs,
            _.resizeToken(updatedWidth)
          )
      end extension
      extension [T <: Int](iter: Iterable[DFValOf[DFBits[T]]])
        protected[core] def concatBits(using DFC): DFValOf[DFBits[Int]] =
          val width = Inlined.forced[Int](iter.map(_.width.value).sum)
          DFVal.Func(DFBits(width), FuncOp.++, iter.toList)
      extension [L <: DFValAny](lhs: L)(using icL: Candidate[L])
        def extend(using DFC): DFValOf[DFBits[Int]] =
          icL(lhs).tag(DFVal.ExtendTag).asValOf[DFBits[Int]]
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValOf[DFBits[RW]] = trydf { icL(lhs).resizeBits(updatedWidth) }
        def &[R](rhs: Exact[R])(using icR: Candidate[R])(using
            dfc: DFC,
            check: `LW == RW`.Check[icL.OutW, icR.OutW]
        ): DFValOf[DFBits[icL.OutW]] = trydf {
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

      extension [W <: Int, A, C, I](
          lhs: DFVal[DFBits[W], Modifier[A, C, I]]
      )
        def as[AT <: DFType.Supported](
            aliasType: AT
        )(using tc: DFType.TC[AT])(using
            aW: Width[tc.Type],
            dfc: DFC
        )(using check: `AW == TW`.Check[aW.Out, W]): DFValOf[tc.Type] = trydf {
          import Token.Ops.{as => asToken}
          val aliasDFType = tc(aliasType)
          check.apply(aliasDFType.asIR.width, lhs.width)
          DFVal.Alias.AsIs(aliasDFType, lhs, _.asToken(aliasType))
        }
        def uint(using DFC): DFValOf[DFUInt[W]] = trydf { as(DFUInt(lhs.width)) }
        def sint(using DFC): DFValOf[DFSInt[W]] = trydf { as(DFSInt(lhs.width)) }
        def apply[Idx](
            relIdx: Exact[Idx]
        )(using
            c: DFUInt.Val.UBArg[W, Idx],
            dfc: DFC
        ): DFVal[DFBit, Modifier[A, Any, Any]] = trydf {
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
        ): DFVal[DFBits[H - L + 1], Modifier[A, Any, Any]] = trydf {
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          DFVal.Alias.ApplyRange(lhs, relBitHigh, relBitLow)
        }
        def unary_~(using DFC): DFValOf[DFBits[W]] = trydf {
          DFVal.Func(lhs.dfType, FuncOp.unary_~, List(lhs))
        }
        def msbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any]] =
          lhs.apply(lhs.width - 1)
        def lsbit(using DFC): DFVal[DFBit, Modifier[A, Any, Any]] =
          lhs.apply(0)
        def msbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.Check[W, RW],
            dfc: DFC
        ): DFValOf[DFBits[RW]] = trydf {
          check(lhs.width, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, lhs.width - 1, lhs.width - updatedWidth)
            .asValOf[DFBits[RW]]
        }
        def lsbits[RW <: Int](updatedWidth: Inlined[RW])(using
            check: `LW >= RW`.Check[W, RW],
            dfc: DFC
        ): DFValOf[DFBits[RW]] = trydf {
          check(lhs.width, updatedWidth)
          DFVal.Alias.ApplyRange(lhs, updatedWidth - 1, 0)
            .asValOf[DFBits[RW]]
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
