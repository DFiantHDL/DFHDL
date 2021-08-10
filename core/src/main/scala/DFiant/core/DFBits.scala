package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import scala.quoted.*

//TODO: simplify after https://github.com/lampepfl/dotty/issues/13120 is fixed
opaque type DFBits[W <: Int] <: DFType.Of[DFiant.compiler.ir.DFBits] =
  DFType.Of[DFiant.compiler.ir.DFBits]

object DFBits:
  def apply[W <: Int](width: Inlined.Int[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] =
    ir.DFBits(width).asInstanceOf[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W])(using
      Arg.Width.Check[W]
  ): DFBits[W] =
    DFBits[W](Inlined.Int.forced[W](valueOf[W])).asInstanceOf[DFBits[W]]
  extension [W <: Int](dfType: DFBits[W])
    def width: Inlined.Int[W] = Inlined.Int.forced[W](dfType.asIR.width)

  type Token[W <: Int] = DFToken.Of[DFBits[W]]
  //TODO: remove after https://github.com/lampepfl/dotty/issues/12927 is fixed
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR, data).asInstanceOf[Token[W]]
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
        valueBits: BitVector,
        bubbleBits: BitVector
    ): Token[W] =
      Token(DFBits(width), (valueBits, bubbleBits))
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
        value: Bubble
    ): Token[W] =
      Token(
        width,
        BitVector.low(width.value),
        BitVector.high(width.value)
      )
    protected[core] def apply[W <: Int](
        width: Inlined.Int[W],
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
//      def width: Inlined.Int[W] = Inlined.Int.forced[W](token.asIR.width)
      def data: (BitVector, BitVector) =
        token.asIR.data.asInstanceOf[(BitVector, BitVector)]
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2

    object Conversions:
      given toDFBitsKnownWidth[W <: Int & Singleton, V](using
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
      transparent inline given DFBitsTokenFromDFBitsToken[W <: Int, VW <: Int](
          using check: `W == VW`.Check[W, VW]
      ): TC[DFBits[W], DFBits[VW] <> TOKEN] =
        new TC[DFBits[W], DFBits[VW] <> TOKEN]:
          type Out = DFBits[W] <> TOKEN
          def apply(dfType: DFBits[W], value: DFBits[VW] <> TOKEN): Out =
            check(dfType.width, value.asIR.width)
            value.asInstanceOf[Out]

      transparent inline given DFBitsTokenFromDFUIntToken[W <: Int, VW <: Int](
          using check: `W == VW`.Check[W, VW]
      ): TC[DFBits[W], DFUInt[VW] <> TOKEN] =
        new TC[DFBits[W], DFUInt[VW] <> TOKEN]:
          type Out = DFBits[W] <> TOKEN
          def apply(dfType: DFBits[W], value: DFUInt[VW] <> TOKEN): Out =
            import DFToken.Ops.bits
            check(dfType.width, value.asIR.width)
            value.bits

      transparent inline given DFBitsTokenFromSBV[W <: Int, V <: SameBitsVector]
          : TC[DFBits[W], V] = new TC[DFBits[W], V]:
        type Out = DFToken.Of[DFBits[W]]
        def apply(dfType: DFBits[W], value: V): Out =
          DFBits.Token[W](dfType.width, value)
    end TC

    private val widthExp = "([0-9]+)'(.*)".r
    def fromBinString(bin: String): Either[String, (BitVector, BitVector)] =
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
    def fromHexString(hex: String): Either[String, (BitVector, BitVector)] =
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
          val (valueBits, bubbleBits) = res.toOption.get
          val width =
            DFiant.internals.Inlined.Int
              .forced[widthType.Underlying](valueBits.length.toInt)
          Token[widthType.Underlying](width, valueBits, bubbleBits)
        }
    end StrInterp

    object Ops:
      extension [LW <: Int](lhs: DFBits.Token[LW])
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
    end Ops
  end Token

  object DFValTC:
    import DFVal.TC
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
    transparent inline given DFBitsArg[
        LW <: Int,
        RW <: Int
    ](using
        check: `LW == RW`.Check[LW, RW]
    ): TC[DFBits[LW], DFBits[RW] <> VAL] =
      new TC[DFBits[LW], DFBits[RW] <> VAL]:
        type TType = DFBits[LW]
        def apply(
            dfType: DFBits[LW],
            value: DFBits[RW] <> VAL
        ): DFBits[RW] <> VAL =
          check(dfType.width, value.width.value)
          value.asIR.asValOf[DFBits[LW]]

  end DFValTC

  //TODO: remove workaround for https://github.com/lampepfl/dotty/issues/13128
  type WA[T <: DFType, W0 <: Int] = WA.Internal[T] { type W = W0 }
  object WA:
    trait Internal[T <: DFType]:
      type W <: Int
    given [W0 <: Int]: Internal[DFBits[W0]] with
      type W = W0
  object Ops:
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

    extension [T <: DFType, W <: Int, M <: DFVal.Modifier](
        lhs: DFVal[T, M]
    )(using DFBits.WA[T, W])
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
          relBit: Inlined.Int[I]
      )(using
          check: BitIndex.Check[I, W],
          dfc: DFC
      ): DFBit <> M =
        check(relBit, lhs.width)
        ???
      def apply[H <: Int, L <: Int](
          relBitHigh: Inlined.Int[H],
          relBitLow: Inlined.Int[L]
      )(using
          checkHigh: BitIndex.Check[H, W],
          checkLow: BitIndex.Check[L, W],
          checkHiLo: BitsHiLo.Check[H, L],
          dfc: DFC
      ): DFBits[H - L + 1] <> M =
        checkHigh(relBitHigh, lhs.width)
        checkLow(relBitLow, lhs.width)
        checkHiLo(relBitHigh, relBitLow)
        ???
  end Ops
end DFBits
