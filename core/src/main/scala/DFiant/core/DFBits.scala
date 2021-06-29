package DFiant.core
import DFiant.compiler.ir
import DFiant.internals.*
import scala.annotation.targetName
import scala.quoted.*

opaque type DFBits[W <: Int] <: DFType.Of[ir.DFBits] = DFType.Of[ir.DFBits]
object DFBits:
  def apply[W <: Int](width: Inlined.Int[W]): DFBits[W] =
    ir.DFBits(width).asInstanceOf[DFBits[W]]
  @targetName("applyNoArg")
  def apply[W <: Int with Singleton](using ValueOf[W]): DFBits[W] =
    DFBits[W](Inlined.Int.forced[W](valueOf[W])).asInstanceOf[DFBits[W]]
  extension [W <: Int](dfType: DFBits[W])
    def width: Inlined.Int[W] = Inlined.Int.forced[W](dfType.asIR.width)

  opaque type Token[W <: Int] <: DFToken.Of[DFBits[W], (BitVector, BitVector)] =
    DFToken.Of[DFBits[W], (BitVector, BitVector)]
  //TODO: remove after https://github.com/lampepfl/dotty/issues/12927 is fixed
  extension [W <: Int](token: Token[W])
    def width: Inlined.Int[W] = Inlined.Int.forced[W](token.asIR.width)
    def data: (BitVector, BitVector) =
      token.asIR.data.asInstanceOf[(BitVector, BitVector)]
  object Token:
    protected[core] def apply[W <: Int](
        dfType: DFBits[W],
        data: (BitVector, BitVector)
    ): Token[W] =
      ir.DFToken(dfType.asIR, (valueBits, bubbleBits))
        .asInstanceOf[Token[W]]
    //TODO: change to protected[core] after https://github.com/lampepfl/dotty/issues/12948 is resolved
    def apply[W <: Int](
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
            c match { //hex mode
              case '{' => (v, b, true)
              case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
              case isHex() =>
                (
                  v ++ BitVector.fromHex(c.toString).get,
                  b ++ BitVector.low(4),
                  false
                )
              case x => return Left(s"Found invalid hex character: $x")
            }
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

    extension [W <: Int](token: Token[W])
      def valueBits: BitVector = token.data._1
      def bubbleBits: BitVector = token.data._2
    object StrInterp:
      def interpMacro(op: Expr[String])(
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

    extension [LW <: Int](lhs: DFBits.Token[LW])
      @targetName("concat")
      def ++[RW <: Int](rhs: DFBits.Token[RW]): DFBits.Token[LW + RW] =
        val width = lhs.width + rhs.width
        val valueBits = lhs.valueBits ++ rhs.valueBits
        val bubbleBits = lhs.bubbleBits ++ rhs.bubbleBits
        DFBits.Token(width, valueBits, bubbleBits)

      @targetName("bitwiseAnd")
      def &[RW <: Int](rhs: DFBits.Token[RW])(using
          bb: Bubble.Behaviour
      ): DFBits.Token[LW] =
        assert(lhs.width == rhs.width)
        val width = lhs.width
        bb match
          case Bubble.Behaviour.Stall =>
            DFBits.Token(
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
            DFBits.Token(width, valueBits, bubbleBits)
