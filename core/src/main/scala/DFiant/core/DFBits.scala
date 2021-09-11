package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import scala.quoted.*

type DFBits[W <: Int] = OpaqueDFBits.DFBits[W]
val DFBits = OpaqueDFBits.DFBits
//export OpaqueDFBits.DFBits

private object OpaqueDFBits:
  opaque type DFBits[W <: Int] <: DFType.Of[DFiant.compiler.ir.DFBits] =
    DFType.Of[DFiant.compiler.ir.DFBits]
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

    type Token[W <: Int] = CompanionsDFBits.Token[W]
    val Token = CompanionsDFBits.Token
    val Val = CompanionsDFBits.Val
    export CompanionsDFBits.Extensions.*
  end DFBits
end OpaqueDFBits

//TODO: simplify after https://github.com/lampepfl/dotty/issues/13120 is fixed
private object CompanionsDFBits:
  object Extensions:
    extension [W <: Int](dfType: DFBits[W])
      def width: Inlined[W] = Inlined.forced[W](dfType.asIR.width)

  protected object `AW == TW`
      extends Check2[
        Int,
        Int,
        [AW <: Int, TW <: Int] =>> AW == TW,
        [AW <: Int, TW <: Int] =>> "The alias width (" +
          ToString[AW] +
          ") is different than the dataflow value width (" +
          ToString[TW] +
          ")."
      ]

  type Token[W <: Int] = DFToken.Of[DFBits[W]]
  //TODO: remove after https://github.com/lampepfl/dotty/issues/12927 is fixed
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR, data).asTokenOf[DFBits[W]]
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
    protected[core] def apply[W <: Int](
        width: Inlined[W],
        value: SameBitsVector
    ): Token[W] =
      val level = value match
        case SameBitsVector.b0s => false
        case SameBitsVector.b1s => true
      Token(
        width,
        BitVector.fill(width.value)(level),
        BitVector.low(width.value)
      )
    extension [W <: Int](token: DFBits.Token[W])
//      def width: Inlined[W] = Inlined.forced[W](token.asIR.width)
      def data: (BitVector, BitVector) =
        token.asIR.data.asInstanceOf[(BitVector, BitVector)]
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    object Conversions:
      given DFBitsTokenConversionSing[W <: Int & Singleton, V](using
          tc: DFToken.TC[DFBits[W], V],
          w: ValueOf[W]
      ): Conversion[V, DFBits[W] <> TOKEN] = value =>
        tc(DFBits(valueOf[W]), value)
    end Conversions

    object TC:
      import DFToken.TC
      protected object `W == VW`
          extends Check2[
            Int,
            Int,
            [W <: Int, VW <: Int] =>> W == VW,
            [W <: Int, VW <: Int] =>> "The token width (" +
              ToString[VW] +
              ") is different than the DFType width (" +
              ToString[W] +
              ")."
          ]
      //TODO: instead of DFToken.Of[DFBits[VW]] use DFBits[VW] <> TOKEN
      //Currently (Aug 27th 2021) it causes an issue for summoning a token TC
      //from a tuple: `val t1 = (DFBits(8), DFBits(8)) token (h"11", b"10010110")`
      //The issue goes away when we get rid of the match type `<>` or
      //if we change `given ... with` into `transparent inline given`.
      //Recheck after match type issues are resolved:
      //https://github.com/lampepfl/dotty/issues/13377
      //https://github.com/lampepfl/dotty/issues/12944
      given DFBitsTokenFromDFBitsToken[W <: Int, VW <: Int](using
          check: `W == VW`.Check[W, VW]
      ): TC[DFBits[W], DFToken.Of[DFBits[VW]]] with
        def apply(dfType: DFBits[W], value: DFToken.Of[DFBits[VW]]): Out =
          check(dfType.width, value.asIR.width)
          value.asInstanceOf[Out]

      given DFBitsTokenFromDFUIntToken[W <: Int, VW <: Int](using
          check: `W == VW`.Check[W, VW]
      ): TC[DFBits[W], DFToken.Of[DFUInt[VW]]] with
        def apply(dfType: DFBits[W], value: DFToken.Of[DFUInt[VW]]): Out =
          import DFTokenOps.bits
          check(dfType.width, value.asIR.width)
          value.bits.asInstanceOf[Out]

      given DFBitsTokenFromSBV[W <: Int]: TC[DFBits[W], SameBitsVector] with
        def apply(dfType: DFBits[W], value: SameBitsVector): Out =
          DFBits.Token[W](dfType.width, value)
    end TC

    private val widthExp = "([0-9]+)'(.*)".r
    private def fromBinString(
        bin: String
    ): Either[String, (BitVector, BitVector)] =
      val (explicitWidth, word) = bin match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, bin)
      val (valueBits, bubbleBits) =
        word.foldLeft((BitVector.empty, BitVector.empty)) {
          case (t, '_') => t //ignoring underscore
          case ((v, b), c) =>
            c match //bin mode
              case '?' => (v :+ false, b :+ true)
              case '0' => (v :+ false, b :+ false)
              case '1' => (v :+ true, b :+ false)
              case x   => return Left(s"Found invalid binary character: $x")
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
    end fromBinString
    private def fromHexString(
        hex: String
    ): Either[String, (BitVector, BitVector)] =
      val isHex = "[0-9a-fA-F]".r
      val (explicitWidth, word) = hex match
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _                           => (None, hex)
      val (valueBits, bubbleBits, binMode) =
        word.foldLeft((BitVector.empty, BitVector.empty, false)) {
          case (t, '_' | ' ') => t //ignoring underscore or space
          case ((v, b, false), c) =>
            c match //hex mode
              case '{' => (v, b, true)
              case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
              case isHex() =>
                (
                  v ++ BitVector.fromHex(c.toString).get,
                  b ++ BitVector.low(4),
                  false
                )
              case x => return Left(s"Found invalid hex character: $x")
          case ((v, b, true), c) =>
            c match //bin mode
              case '}' => (v, b, false)
              case '?' => (v :+ false, b :+ true, true)
              case '0' => (v :+ false, b :+ false, true)
              case '1' => (v :+ true, b :+ false, true)
              case x =>
                return Left(
                  s"Found invalid binary character in binary mode: $x"
                )
        }
      if (binMode) Left(s"Missing closing braces of binary mode")
      else
        val actualWidth = valueBits.length.toInt
        explicitWidth match
          case Some(width) if width < actualWidth =>
            Left(
              s"Explicit given width ($width) is smaller than the actual width ($actualWidth)"
            )
          case Some(width) =>
            Right((valueBits.resize(width), bubbleBits.resize(width)))
          case None => Right((valueBits, bubbleBits))
    end fromHexString

    object StrInterp:
      extension (inline sc: StringContext)
        transparent inline def b(inline args: Any*): DFToken =
          ${
            interpMacro('{ "b" })('sc, 'args)
          }
        transparent inline def h(inline args: Any*): DFToken =
          ${
            interpMacro('{ "h" })('sc, 'args)
          }

      private def interpMacro(op: Expr[String])(
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
        val opStr = op.value.get
        val widthTpe: TypeRepr = fullTerm match
          case Literal(StringConstant(t)) =>
            val res = opStr match
              case "b" => fromBinString(t)
              case "h" => fromHexString(t)
            res match
              case Right((valueBits, bubbleBits)) =>
                ConstantType(IntConstant(valueBits.length.toInt))
              case Left(msg) =>
                report.error(msg)
                ???
          case _ => TypeRepr.of[Int]
        val widthType = widthTpe.asType.asInstanceOf[Type[Int]]
        val fullExpr = fullTerm.asExprOf[String]
        '{
          val res = $op match
            case "b" => fromBinString($fullExpr)
            case "h" => fromHexString($fullExpr)
          //TODO: remove unchecked annotation (and type signature) once
          //https://github.com/lampepfl/dotty/issues/13405 is resolved
          val (valueBits, bubbleBits): (BitVector, BitVector) @unchecked =
            res.toOption.get
          val width =
            DFiant.internals.Inlined
              .forced[widthType.Underlying](valueBits.length.toInt)
          Token[widthType.Underlying](width, valueBits, bubbleBits)
        }
      end interpMacro
    end StrInterp

    object Ops:
      extension [LW <: Int](lhs: DFBits.Token[LW])
        def as[A](
            aliasType: A
        )(using
            tc: DFType.TC[A],
            aW: Width[A]
        )(using check: `AW == TW`.Check[aW.Out, LW]): DFToken.Of[tc.Type] =
          val dfType = tc(aliasType).asIR
          check(dfType.width, lhs.width)
          lhs.asIR.asInstanceOf[ir.DFBits.Token].as(dfType).asTokenOf[tc.Type]
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
          assert(lhs.width == rhs.width)
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
    trait Candidate[-R]:
      type OutW <: Int
      def apply(value: R): DFBits[OutW] <> VAL
    object Candidate:
      given fromDFBits[W <: Int]: Candidate[DFBits[W] <> VAL] with
        type OutW = W
        def apply(value: DFBits[W] <> VAL): DFBits[W] <> VAR =
          value.asIR.asValOf[DFBits[W]]
      given fromDFUInt[W <: Int](using DFC): Candidate[DFUInt[W] <> VAL] with
        type OutW = W
        def apply(value: DFUInt[W] <> VAL): DFBits[W] <> VAL =
          import DFVal.Ops.bits
          value.bits
      private def valueToBits(value: Any)(using dfc: DFC): DFBits[Int] <> VAL =
        import DFVal.Ops.bits
        import DFBits.Val.Ops.concatBits
        given dfcAnon: DFC = dfc.anonymize
        value match
          case v: ValueOf[?] =>
            valueToBits(v.value)
          case x: NonEmptyTuple =>
            x.toList.map(valueToBits).concatBits
          case i: Int =>
            valueToBits(i > 0)
          case bool: Boolean =>
            DFVal.Const(Token(1, BitVector.bit(bool), BitVector.zero))
          case token: ir.DFType.Token =>
            DFVal.Const(token.bits.asTokenOf[DFBits[Int]])
          case dfVal: ir.DFVal =>
            dfVal.dfType match
              case _: ir.DFBits => dfVal.asValOf[DFBits[Int]]
              case _            => dfVal.asValAny.bits
        end match
      end valueToBits
      transparent inline given fromTuple[R <: NonEmptyTuple](using
          DFC
      ): Candidate[ValueOf[R]] = ${ DFBitsMacro[ValueOf[R]] }
      def DFBitsMacro[R](using
          Quotes,
          Type[R]
      ): Expr[Candidate[R]] =
        import quotes.reflect.*
        import Width.*
        val rTpe = TypeRepr.of[R]
        extension (tpe: TypeRepr)
          def calcValWidth: TypeRepr =
            tpe.dealias match
              case applied: AppliedType
                  if applied <:< TypeRepr.of[ValueOf[_]] =>
                applied.args.head.calcValWidth
              case AppliedType(tycon, tpe :: _)
                  if tycon <:< TypeRepr.of[DFVal] =>
                tpe.dealias.calcWidth
              case AppliedType(tycon, tpe :: _)
                  if tycon <:< TypeRepr.of[DFToken.Of] =>
                tpe.calcWidth
              case AppliedType(tycon, args)
                  if tycon <:< TypeRepr.of[NonEmptyTuple] =>
                val widths = args.map(a => a.calcValWidth)
                widths.reduce(_ + _)
              case ConstantType(IntConstant(v)) if (v == 1 || v == 0) =>
                ConstantType(IntConstant(1))
              case ConstantType(BooleanConstant(v)) =>
                ConstantType(IntConstant(1))
              case ref: TermRef =>
                ref.widen.calcValWidth
              case x =>
                report.error(
                  s"Unsupported argument value ${x.show} for dataflow receiver type DFBits"
                )
                ???
            end match
        val wType = rTpe.calcValWidth.asTypeOf[Int]
        val dfcExpr = '{ compiletime.summonInline[DFC] }
        '{
          new Candidate[R]:
            type OutW = wType.Underlying
            def apply(value: R): DFValOf[DFBits[OutW]] =
              val valueBits =
                valueToBits(value)(using ${ dfcExpr })
              valueBits.asIR.asValOf[DFBits[OutW]]
        }
      end DFBitsMacro
    end Candidate

    object TC:
      import DFVal.TC
      def apply(
          dfType: DFBits[Int],
          dfVal: DFBits[Int] <> VAL
      ): DFBits[Int] <> VAL =
        `LW == RW`(dfType.width, dfVal.width)
        dfVal
      protected object `LW == RW`
          extends Check2[
            Int,
            Int,
            [LW <: Int, RW <: Int] =>> LW == RW,
            [LW <: Int, RW <: Int] =>> "The argument width (" +
              ToString[RW] +
              ") is different than the reciever width (" +
              ToString[LW] +
              "). \nConsider applying `.resize` to resolve this issue."
          ]
      given DFBitsFromCandidate[
          LW <: Int,
          R
      ](using dfc: DFC, candidate: Candidate[R])(using
          check: `LW == RW`.Check[LW, candidate.OutW]
      ): TC[DFBits[LW], R] with
        def apply(dfType: DFBits[LW], value: R): DFValOf[DFBits[LW]] =
          val dfVal = candidate(value)
          check(dfType.width, dfVal.width.value)
          dfVal.asIR.asValOf[DFBits[LW]]
    end TC

    object Conversions:
      given DFBitsValConversionSing[LW <: Int & Singleton, R](using
          v: ValueOf[LW],
          tc: CompanionsDFVal.TC[DFBits[LW], R]
      ): Conversion[R, DFValOf[DFBits[LW]]] = from =>
        tc(DFBits(valueOf[LW]), from)
      given DFBitsValConversion[R](using
          candidate: Candidate[R]
      ): Conversion[R, DFValOf[DFBits[Int]]] = from => candidate(from)

    object Ops:
      protected object BitIndex
          extends Check2[
            Int,
            Int,
            [I <: Int, W <: Int] =>> (I < W) && (I >= 0),
            [I <: Int, W <: Int] =>> "Index " + ToString[I] +
              " is out of range of width/length " + ToString[W]
          ]
      protected object BitsHiLo
          extends Check2[
            Int,
            Int,
            [H <: Int, L <: Int] =>> H >= L,
            [H <: Int, L <: Int] =>> "Low index " + ToString[L] +
              " is bigger than High bit index " + ToString[H]
          ]
      extension [T <: Int](iter: Iterable[DFBits[T] <> VAL])
        protected[core] def concatBits(using DFC): DFBits[Int] <> VAL =
          val width = iter.map(_.width.value).sum
          DFVal.Func(DFBits(width), ir.DFVal.Func.Op.++, iter.toList)
      extension [W <: Int, M <: ir.DFVal.Modifier](
          lhs: DFVal[DFBits[W], M]
      )
        def as[A](
            aliasType: A
        )(using
            tc: DFType.TC[A],
            aW: Width[A],
            dfc: DFC
        )(using check: `AW == TW`.Check[aW.Out, W]): DFValOf[tc.Type] =
          val aliasDFType = tc(aliasType)
          check.apply(aliasDFType.asIR.width, lhs.width)
          DFVal.Alias.AsIs(aliasDFType, lhs)
        def apply[I <: Int](
            relIdx: Inlined[I]
        )(using
            check: BitIndex.Check[I, W],
            dfc: DFC
        ): DFVal[DFBit, M] =
          check(relIdx, lhs.width)
          DFVal.Alias.ApplyIdx(lhs, relIdx)
        def apply[H <: Int, L <: Int](
            relBitHigh: Inlined[H],
            relBitLow: Inlined[L]
        )(using
            checkHigh: BitIndex.Check[H, W],
            checkLow: BitIndex.Check[L, W],
            checkHiLo: BitsHiLo.Check[H, L],
            dfc: DFC
        ): DFVal[DFBits[H - L + 1], M] =
          checkHigh(relBitHigh, lhs.width)
          checkLow(relBitLow, lhs.width)
          checkHiLo(relBitHigh, relBitLow)
          DFVal.Alias.ApplyRange(lhs, relBitHigh, relBitLow)
        def repeat[N <: Int](num: Inlined[N])(using
            check: Arg.Positive.Check[N],
            dfc: DFC
        ): DFValOf[DFBits[W * N]] =
          check(num)
          DFVal.Func(
            DFBits(lhs.dfType.width * num),
            ir.DFVal.Func.Op.++,
            List.fill(num)(lhs)
          )
        def resize[RW <: Int](updatedWidth: Inlined[RW])(using
            Arg.Width.Check[RW],
            DFC
        ): DFValOf[DFBits[RW]] =
          DFVal.Alias.AsIs(DFBits(updatedWidth), lhs)
      end extension
    end Ops
  end Val
end CompanionsDFBits
