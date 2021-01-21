package DFiant

import singleton.ops._
import singleton.twoface._
import DFiant.internals._
import DFAny.{Func2, `Op==,!=`}
import DFiant.DFDecimal.{`LF == RF`, `LF >= RF`, `LS == RS`, `LS signMatch RS`, `VarF >= ConstF`, `VarS signMatch ConstS`, `VarW >= ConstW`}
import DFiant.internals.BitsWidthOf.SignedCfg.IntAux
import compiler.csprinter.CSPrinter
import singleton.ops.OpContainer.Eq

object DFUInt {
  type Type[W] = DFDecimal.Type[false, W, 0]
  object Type {
    def apply[W](width : TwoFace.Int[W]) : Type[W] = DFDecimal.Type(false, width, 0)
    def unapply[S, W, F](
      dfType : DFDecimal.Type[S, W, F]
    ) : Option[TwoFace.Int[W]] =
      (dfType.signed.getValue, dfType.fractionWidth.getValue) match {
        case (false, 0) => Some(dfType.width)
        case _ => None
      }
  }
  type Token = DFDecimal.Token
  type TokenW[W] = DFDecimal.TokenW[false, W, 0]
  object Token {
    def apply(width : Int, value : BigInt) : Token = DFDecimal.Token(false, width, 0, value)
    def unapply(token : DFDecimal.Token) : Option[(Int, Option[BigInt])] =
      (token.signed, token.fractionWidth) match {
        case (false, 0) => Some(token.width, token.value)
        case _ => None
      }
    def bubble(width : Int) : Token = DFDecimal.Token.bubble(false, width, 0)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Type[W] =
    Type(checkedWidth.unsafeCheck())
  def apply[W](
    implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : Type[W] = Type(checkedWidth.unsafeCheck())
  def unapply(arg: DFAny.Member): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
    case _ => None
  }
  def max[U](maxValue : Positive.Checked[U])(
    implicit w : BitsWidthOf.Int[U]
  ) : Type[w.Out] = Type(w(maxValue.getValue))
  def until[U](supremum : Positive.Checked[U])(
    implicit w : BitsWidthOf.Int[U-1]
  ) : Type[w.Out] = Type(w(supremum.getValue-1))
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
object DFSInt {
  type Type[W] = DFDecimal.Type[true, W, 0]
  object Type {
    def apply[W](width : TwoFace.Int[W]) : Type[W] = DFDecimal.Type(true, width, 0)
    def unapply[S, W, F](
      dfType : DFDecimal.Type[S, W, F]
    ) : Option[TwoFace.Int[W]] =
      (dfType.signed.getValue, dfType.fractionWidth.getValue) match {
        case (true, 0) => Some(dfType.width)
        case _ => None
      }
  }
  type Token = DFDecimal.Token
  type TokenW[W] = DFDecimal.TokenW[true, W, 0]
  object Token {
    def apply(width : Int, value : BigInt) : Token = DFDecimal.Token(true, width, 0, value)
    def unapply(token : DFDecimal.Token) : Option[(Int, Option[BigInt])] =
      (token.signed, token.fractionWidth) match {
        case (true, 0) => Some(token.width, token.value)
        case _ => None
      }
    def bubble(width : Int) : Token = DFDecimal.Token.bubble(true, width, 0)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def apply[W](checkedWidth : SIntWidth.Checked[W]) : Type[W] =
    Type(checkedWidth.unsafeCheck())
  def apply[W](
    implicit checkedWidth : SIntWidth.Checked[W], di: DummyImplicit
  ) : Type[W] = Type(checkedWidth.unsafeCheck())
  def unapply(arg: DFAny.Member): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}

object DFDecimal extends DFAny.Companion {
  final case class Type[S, W, F](
    signed : TwoFace.Boolean[S], width : TwoFace.Int[W], fractionWidth : TwoFace.Int[F]
  ) extends DFAny.Type {
    type Width = W
    type Magnitude = W - F
    val magnitudeWidth : TwoFace.Int[Magnitude] = TwoFace.Int.create[Magnitude](width - fractionWidth)
    type TToken = Token
    type TPattern = DFDecimal.Pattern
    type TPatternAble[+R] = DFDecimal.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFDecimal.Pattern.Builder[LType]
    def getBubbleToken: TToken = Token.bubble(signed, width, fractionWidth)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token =
      (signed.getValue, fractionWidth.getValue) match {
        case (false, 0) => fromToken.toUInt
        case (true, 0) => fromToken.toSInt
        case _ => ??? //TODO: ufix/sfix
      }
    override def toString: String = (signed.getValue, fractionWidth.getValue) match {
      case (false, 0) => s"DFUInt[$width]"
      case (true, 0) => s"DFSInt[$width]"
      case (false, _) => s"DFUFix[$magnitudeWidth, $fractionWidth]"
      case (true, _) => s"DFSFix[$magnitudeWidth, $fractionWidth]"
    }
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = from match {
      case r @ DFDecimal(_,_,_) =>
        import DFDesign.Frontend._
        val op = implicitly[DFAny.`Op:=,<>`.Builder[Type[S, W, F], DFDecimal[Boolean, Int, Int]]]
        op(this, r.asValOf[Type[Boolean, Int, Int]])
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      (signed.getValue, fractionWidth.getValue) match {
        case (false, 0) => s"$TP DFUInt($LIT$width)"
        case (true, 0) => s"$TP DFSInt($LIT$width)"
        case (false, _) => s"$TP DFUFix($LIT$magnitudeWidth, $LIT$fractionWidth)"
        case (true, _) => s"$TP DFSFix($LIT$magnitudeWidth, $LIT$fractionWidth)"
      }
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(signed, width, fractionWidth) =>
        this.signed.getValue == signed.getValue && this.width.getValue == width.getValue &&
          this.fractionWidth.getValue == fractionWidth.getValue
      case _ => false
    }
  }
  trait Extendable

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def unapply(arg: DFAny.Member): Option[(Boolean, Int, Int)] = arg.dfType match {
    case Type(signed, width, fractionWidth) => Some(signed.getValue, width.getValue, fractionWidth.getValue)
    case _ => None
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Frontend
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Frontend {
    trait Inherited extends Op.Frontend.Inherited with `Op:=,<>`.Frontend.Inherited with Token.Frontend.Inherited
    trait Imported extends Op.Frontend.Imported with `Op:=,<>`.Frontend.Imported with Token.Frontend.Imported
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  type TokenW[S, W, F] = DFAny.TokenT[Token, Type[S, W, F]]
  final case class Token(
    signed : Boolean, width : Int, fractionWidth : Int, value : Option[BigInt]
  ) extends DFAny.Token.Of[Type[Boolean, Int, Int], BigInt] { left =>
    val dfType : DFAny.Type = Type(signed, width, fractionWidth)
    val magnitudeWidth : Int = width - fractionWidth
    private def mkToken(
      right : Token, f : (BigInt, BigInt) => BigInt, resultMagnitudeWidth : Int, resultFractionWidth : Int
    ) : Token = {
      val resultWidth = resultMagnitudeWidth + resultFractionWidth
      val leftFixed = left.resize(resultWidth, resultFractionWidth)
      val rightFixed = right.resize(resultWidth, resultFractionWidth)
      (leftFixed.value, rightFixed.value) match {
        case (Some(l), Some(r)) =>
          val resultValue = if (signed) f(l, r) else f(l, r).asUnsigned(resultWidth)
          Token(signed, resultWidth, resultFractionWidth, resultValue)
        case _ => Token.bubble(signed, resultWidth, resultFractionWidth)
      }
    }
    def +  (right : Token) : Token =
      mkToken(right, _ + _, left.magnitudeWidth max right.magnitudeWidth, left.fractionWidth max right.fractionWidth)
    def +^ (right : Token) : Token =
      mkToken(right, _ + _, (left.magnitudeWidth max right.magnitudeWidth) + 1, left.fractionWidth max right.fractionWidth)

    def -  (right : Token) : Token =
      mkToken(right, _ - _, left.magnitudeWidth max right.magnitudeWidth, left.fractionWidth max right.fractionWidth)
    def -^ (right : Token) : Token =
      mkToken(right, _ - _, (left.magnitudeWidth max right.magnitudeWidth) + 1, left.fractionWidth max right.fractionWidth)

    def *  (right : Token) : Token = {
      val carryMul = left *^ right
      val actualWidth = left.width max right.width
      val actualFractionWidth = left.fractionWidth max right.fractionWidth
      val selectedMul = carryMul.bitsWL(actualWidth, carryMul.width - actualFractionWidth)
      if (signed) selectedMul.toSInt
      else selectedMul.toUInt
    }
    def *^ (right : Token) : Token = mkToken(right, _ * _, left.width + right.width, left.fractionWidth + right.fractionWidth)
    def /  (right : Token) : Token = mkToken(right, _ / _, left.width, left.fractionWidth)
    def %  (right : Token) : Token = {
      assert(left.fractionWidth == 0)
      assert(right.fractionWidth == 0)
      mkToken(right, _ % _, right.width, 0)
    }

    def <  (right : Token) : DFBool.Token = mkTokenB(right, _ < _)
    def >  (right : Token) : DFBool.Token = mkTokenB(right, _ > _)
    def <= (right : Token) : DFBool.Token = mkTokenB(right, _ <= _)
    def >= (right : Token) : DFBool.Token = mkTokenB(right, _ >= _)
    def == (right : Token) : DFBool.Token = mkTokenB(right, _ == _)
    def != (right : Token) : DFBool.Token = mkTokenB(right, _ != _)
    def << (right : Token) : Token = right match {
      case DFUInt.Token(width, _) if width < 31 =>
        if (signed) mkToken(right, _ << _.toInt, width, fractionWidth)
        else (left.bits << right).toUInt
      case _ => ???
    }
    def >> (right : Token) : Token = right match {
      case DFUInt.Token(width, _) if width < 31 =>
        if (signed) mkToken(right, _ >> _.toInt, width, fractionWidth)
        else (left.bits >> right).toUInt
      case _ => ???
    }
    def unary_- : Token = copy(value = value.map(-_))
    def resize(toWidth : Int) : Token = {
      if (toWidth > left.width) Token(signed, toWidth, fractionWidth, value)
      else if (toWidth < left.width) {
        val resizedBits = left.bits.resize(toWidth)
        if (signed) resizedBits.toSInt
        else resizedBits.toUInt
      }
      else this.asInstanceOf[Token]
    }
    def resize(toWidth : Int, toFractionWidth : Int) : Token = {
      if(toFractionWidth == fractionWidth) resize(toWidth)
      else {
        //TODO: ufix/sfix support
        ???
      }
    }
    def valueToBitVector(value : BigInt) : BitVector = value.toBitVector(width)
    def valueCodeString(value : BigInt)(implicit printer: CSPrinter) : String = {
      import printer.config._
      if (value.isValidInt) s"$LIT$value"
      else if (value.isValidLong) s"$LIT${value}L"
      else s"""$LIT BigInt($STR"$value")"""
    }
  }

  protected object `LF >= RF` extends Checked1Param.Int {
    type Cond[LF, RF] = LF >= RF
    type Msg[LF, RF] = "This operation does not permit applying a wider RHS fraction width. Found: LHS-fractionWidth = "+ ToString[LF] + " and RHS-fractionWidth = " + ToString[RF]
    type ParamFace = Int
    type CheckedExtendable[LF, LE, RF] = CheckedShell[LF, ITE[LE, 0, RF]]
  }
  protected object `LF == RF` extends Checked1Param.Int {
    type Cond[LF, RF] = LF == RF
    type Msg[LF, RF] = "This operation does not permit applying different fraction width. Found: LHS-fractionWidth = "+ ToString[LF] + " and RHS-fractionWidth = " + ToString[RF]
    type ParamFace = Int
  }
  protected object `LS signMatch RS` extends Checked1Param.Boolean {
    type Cond[LS, RS] = ITE[RS, LS, true] //if the RHS is signed, the LHS must be signed as well
    type Msg[LS, RS] = "This operation does not permit the RHS to be signed while the LHS is unsigned."
    type ParamFace = Boolean
  }
  protected object `LS == RS` extends Checked1Param.Boolean {
    type Cond[LS, RS] = LS == RS
    type Msg[LS, RS] = ITE[
      LS,
      "This operation does not permit the RHS to be unsigned while the LHS is signed.",
      "This operation does not permit the RHS to be signed while the LHS is unsigned."
    ]
    type ParamFace = Boolean
  }

  object Token {
    def apply(signed : Boolean, width : Int, fractionWidth : Int, value : Int) : Token =
      Token(signed, width, fractionWidth, BigInt(value))
    def apply(signed : Boolean, width : Int, fractionWidth : Int, value : BigInt) : Token = {
      assert(value.bitsWidth(signed) <= width, s"\nThe init value $value width must be smaller or equal to $width")
      Token(signed, width, fractionWidth, Some(value))
    }
    def bubble(signed : Boolean, width : Int, fractionWidth : Int) : Token =
      Token(signed, width, fractionWidth, None)
    def apply(signed : Boolean, width : Int, fractionWidth : Int, token : Token) : Token = {
      if (signed) assert(token.signed, s"\nThe init value $token is unsigned. Expecting signed tokens only.")
      else assert(!token.signed, s"\nThe init value $token is signed. Expecting unsigned tokens only.")
      assert(token.width <= width, s"\nThe init value $token width must be smaller or equal to $width")
      Token(signed, width, fractionWidth, token.value)
    }

    private val widthIntExp = "(\\d+)'(-?\\d+)".r
    private val widthFixedExp = "(\\d+)\\.(\\d+)'(-?\\d+)\\.?(\\d*)".r
    private val intExp = "(-?\\d+)".r
    def fromDecString(dec : String) : Either[String, Token] = {
      def tokenFromStr(numStr : String) : Token = {
        val value = BigInt(numStr)
        val signed = value < 0
        val actualWidth = value.bitsWidth(signed)
        Token(signed, actualWidth, 0, value)
      }
      dec match {
        case widthFixedExp(magnitudeWidthStr, fractionWidthStr, magnitudeStr, fractionStr) =>
          val explicitMagnitudeWidth = magnitudeWidthStr.toInt
          val explicitFractionWidth = fractionWidthStr.toInt
          val magnitude = BigInt(magnitudeStr)
          val fraction = if (fractionStr.isEmpty) BigInt(0) else BigInt(fractionStr)
          Left("Fixed-point decimal literals are not yet supported")
        case widthIntExp(widthStr, numStr) =>
          val explicitWidth = widthStr.toInt
          val token = tokenFromStr(numStr)
          if (explicitWidth < token.width)
            Left(s"Explicit given width ($explicitWidth) is smaller than the actual width (${token.width})")
          else
            Right(token.resize(explicitWidth))
        case intExp(numStr) => Right(tokenFromStr(numStr))
        case _ =>
          Left(s"Invalid decimal pattern found: $dec")
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // String interpolation macros
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    import scala.reflect.macros.whitebox
    def decImplStringInterpolator(c: whitebox.Context) : c.Tree = {
      import c.universe._
      val Apply(TypeApply(Select(properTree,_), _), argsTrees) = c.enclosingImplicits.last.tree
      val Apply(_, List(Apply(_, parts))) = properTree
      val fullExpressionParts = Seq(parts,argsTrees).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1)
      val fullExpressionTree = fullExpressionParts.reduce[Tree] {
        case (Literal(Constant(l)), Literal(Constant(r))) => Literal(Constant(l.toString + r.toString))
        case (l, r) => q"${l}.toString + ${r}.toString"
      }
      def constantType(value : Any) : c.Type = c.internal.constantType(Constant(value))
      val (signTpe, widthTpe, fractionWidthTpe) : (c.Type, c.Type, c.Type) = fullExpressionTree match {
        case Literal(Constant(t : String)) =>
          DFDecimal.Token.fromDecString(t) match {
            case Right(token) =>
              (constantType(token.signed), constantType(token.width), constantType(token.fractionWidth))
            case Left(msg) => c.abort(msg)
          }
        case _ => (typeOf[Boolean],typeOf[Int],typeOf[Int])
      }
      val buildTree = q"DFiant.DFDecimal.Token.fromDecString($fullExpressionTree).toOption.get"
      q"""
         new DFiant.Interpolator[DFiant.DFDecimal.Token, "d"] {
           type Out = DFiant.DFDecimal.TokenW[$signTpe, $widthTpe, $fractionWidthTpe]
           val value : Out = $buildTree.asInstanceOf[Out]
         }
       """
    }

    type ToFit[LS, LW, LF, V] = DFAny.Token.ToFit.Summon.SAM[Type[LS, LW, LF], V, TokenW[LS, LW, LF]]
    type AsIs[LS, LW, LF, T, OS, OW, OF] = DFAny.Token.AsIs.Summon.Aux[Type[LS, LW, LF], T, Token, TokenW[OS, OW, OF]]
    type IsSignedCalc[V] = V < 0
    type IsSignedAux[V, Out] = TwoFace.Boolean.Shell1Aux[IsSignedCalc, V, scala.Int, Out]
    sealed trait Frontend {
      protected implicit def __DFDecimalTokenAsIsInt[LS, LW, LF, V <: Int, OS, OW](
        implicit
        isSigned : IsSignedAux[V, OS],
        oWidthOf : BitsWidthOf.SignedCfg.IntAux[LS, V, OW]
      ) : AsIs[LS, LW, LF, V, OS, OW, 0] = new DFAny.Token.AsIs.Summon[Type[LS, LW, LF], V, Token] {
        type Out = TokenW[OS, OW, 0]
        def apply(from : Type[LS, LW, LF], value : V) : Out = {
          Token(from.signed, oWidthOf(from.signed, value), from.fractionWidth, value).typeTag[Type[OS, OW, 0]]
        }
      }
      protected implicit def __DFDecimalTokenTokenW[LS, LW, LF, RS, RW, RF] : AsIs[LS, LW, LF, TokenW[RS, RW, RF], RS, RW, RF] =
        new DFAny.Token.AsIs.Summon[Type[LS, LW, LF], TokenW[RS, RW, RF], Token] {
          type Out = TokenW[RS, RW, RF]
          def apply(from : Type[LS, LW, LF], value : TokenW[RS, RW, RF]) : Out = value
        }
      protected implicit def __DFDecimalTokenToken[LS, LW, LF] : AsIs[LS, LW, LF, Token, Boolean, Int, Int] =
        new DFAny.Token.AsIs.Summon[Type[LS, LW, LF], Token, Token] {
          type Out = TokenW[Boolean, Int, Int]
          def apply(from : Type[LS, LW, LF], value : Token) : Out = value.asInstanceOf[Out]
        }
      protected implicit def __DFDecimalTokenToFit[LS, LW, LF, V, RS, RW, RF](
        implicit
        summonedToken : AsIs[LS, LW, LF, V, RS, RW, RF],
        signMatch : `LS signMatch RS`.CheckedShell[LS, RS],
        fitsWidth : `LW >= RW`.CheckedShell[LW, RW],
        fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF],
      ) : ToFit[LS, LW, LF, V] = (from, value) => {
        val token = summonedToken(from, value)
        signMatch.unsafeCheck(from.signed, token.signed)
        fitsWidth.unsafeCheck(from.width, token.width)
        fitsFractionWidth.unsafeCheck(from.fractionWidth, token.fractionWidth)
        token.resize(from.width, from.fractionWidth).typeTag[Type[LS, LW, LF]]
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFDecimalTokenAsIsInt[LS, LW, LF, V <: Int, OS, OW](implicit isSigned : IsSignedAux[V, OS], oWidthOf : IntAux[LS, V, OW]) : AsIs[LS, LW, LF, V, OS, OW, 0] = super.__DFDecimalTokenAsIsInt
        final override protected implicit def __DFDecimalTokenToFit[LS, LW, LF, V, RS, RW, RF](implicit summonedToken : AsIs[LS, LW, LF, V, RS, RW, RF], signMatch : `LS signMatch RS`.CheckedShell[LS, RS], fitsWidth : internals.`LW >= RW`.CheckedShell[LW, RW], fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF]) : ToFit[LS, LW, LF, V] = super.__DFDecimalTokenToFit
        final override protected implicit def __DFDecimalTokenToken[LS, LW, LF] : AsIs[LS, LW, LF, Token, Boolean, Int, Int] = super.__DFDecimalTokenToken
        final override protected implicit def __DFDecimalTokenTokenW[LS, LW, LF, RS, RW, RF] : AsIs[LS, LW, LF, TokenW[RS, RW, RF], RS, RW, RF] = super.__DFDecimalTokenTokenW
      }
      trait Imported extends Frontend {
        final override implicit def __DFDecimalTokenAsIsInt[LS, LW, LF, V <: Int, OS, OW](implicit isSigned : IsSignedAux[V, OS], oWidthOf : IntAux[LS, V, OW]) : AsIs[LS, LW, LF, V, OS, OW, 0] = super.__DFDecimalTokenAsIsInt
        final override implicit def __DFDecimalTokenToFit[LS, LW, LF, V, RS, RW, RF](implicit summonedToken : AsIs[LS, LW, LF, V, RS, RW, RF], signMatch : `LS signMatch RS`.CheckedShell[LS, RS], fitsWidth : internals.`LW >= RW`.CheckedShell[LW, RW], fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF]) : ToFit[LS, LW, LF, V] = super.__DFDecimalTokenToFit
        final override implicit def __DFDecimalTokenToken[LS, LW, LF] : AsIs[LS, LW, LF, Token, Boolean, Int, Int] = super.__DFDecimalTokenToken
        final override implicit def __DFDecimalTokenTokenW[LS, LW, LF, RS, RW, RF] : AsIs[LS, LW, LF, TokenW[RS, RW, RF], RS, RW, RF] = super.__DFDecimalTokenTokenW
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  class Pattern(
    dfType : Type[Boolean, Int, Int], intervalSet : IntervalSet[BigInt]
  ) extends DFAny.Pattern.OfIntervalSet[Type[Boolean, Int, Int], BigInt, Pattern](dfType, intervalSet) {
    protected def matchCond(matchVal: DFDecimal[Boolean, Int, Int], interval : Interval[BigInt])(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import continuum.bound._
      import DFDesign.Frontend._
      def tokenOf(bigInt : BigInt) : Token =
        Token(matchVal.dfType.signed, matchVal.width, matchVal.dfType.fractionWidth, bigInt)
      val (lower, lowerCond) = interval.lower.bound match {
        case Closed(v) => (v, matchVal >= tokenOf(v))
        case Open(v) => (v + 1, matchVal > tokenOf(v))
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      val (upper, upperCond) = interval.upper.bound match {
        case Closed(v) => (v, matchVal <= tokenOf(v))
        case Open(v) => (v - 1, matchVal < tokenOf(v))
        case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
      }
      if (lower == upper) (matchVal === tokenOf(lower)).anonymize
      else (lowerCond.anonymize && upperCond.anonymize).anonymize
    }
  }
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val interval : Interval[BigInt]
    }
    object Able {
      implicit class DFDecimalPatternInt[R <: Int](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.point(BigInt(right))
      }
      implicit class DFDecimalPatternRange[R <: Range](val right : R) extends Able[R] {
        val interval: Interval[BigInt] = Interval.fromRange(right).toBigIntInterval
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LS, LW, LF] : Builder[Type[LS, LW, LF]] = new Builder[Type[LS, LW, LF]] {
        def apply[R](left: Type[LS, LW, LF], right: Seq[Able[R]]): Pattern = {
          val reqInterval = (left.signed.getValue, left.fractionWidth.getValue) match {
            case (false, 0) => IntervalSet(Interval.closed(BigInt(0), BigInt.maxUnsignedFromWidth(left.width)))
            case (true, 0) => IntervalSet(Interval.closed(BigInt.minSignedFromWidth(left.width), BigInt.maxSignedFromWidth(left.width)))
            case _ => ??? //TODO ufix/sfix
          }
          val patternSet = right.map(e => e.interval).foldLeft(IntervalSet.empty[BigInt])((set, interval) => {
            if (set.intersect(interval).nonEmpty) throw new IllegalArgumentException(s"\nThe interval $interval already intersects with $set")
            if (!reqInterval.contains(interval)) throw new IllegalArgumentException(s"\nThe interval $interval is outside of range allowed: $reqInterval")
            set + interval
          })

          require(patternSet.intersect(reqInterval).nonEmpty, s"\nPattern must intersect with $reqInterval. Pattern is: $patternSet")
          new Pattern(left.asInstanceOf[Type[Boolean, Int, Int]], patternSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    class Able[L](val value : L) extends DFAny.Op.Able[L]
    class AbleOps[L](value : L) extends Able[L](value) {
      val left = value
      final def +   [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op+`.Builder[L, false, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def +^  [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op+^`.Builder[L, true, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def -   [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op-`.Builder[L, false, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def -^  [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op-^`.Builder[L, true, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def <   [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op<`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def >   [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op>`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def <=  [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op<=`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def >=  [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: `Op>=`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def === [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: DFAny.`Op==`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
      final def =!= [RS, RW, RF](right : DFDecimal[RS, RW, RF])(implicit op: DFAny.`Op!=`.Builder[L, DFDecimal[RS, RW, RF]]) = op(left, right)
    }

    sealed trait Frontend {
      protected implicit def __DFDecimalWiden[S, FW, FF, TW, TF](c : DFDecimal[S, FW, FF])(
        implicit eqW : OpContainer.Eq[FW, TW, Int], eqF : OpContainer.Eq[FF, TF, Int]
      ) : DFDecimal[S, TW, TF] = c.asInstanceOf[DFDecimal[S, TW, TF]]
      sealed class __DFDecimalFromInt[L <: Int](left : L) extends AbleOps[L](left)
      protected implicit def __DFDecimalFromInt[L <: Int](left: L): __DFDecimalFromInt[L] = new __DFDecimalFromInt(left)
      sealed class __DFDecimalFromXInt[L <: XInt](left : L) extends AbleOps[ValueOf[L]](new ValueOf(left))
      protected implicit def __DFDecimalFromXInt[L <: XInt](left: L): __DFDecimalFromXInt[L] = new __DFDecimalFromXInt(left)
      sealed class __DFDecimalFromToken(left : Token) extends AbleOps[Token](left)
      protected implicit def __DFDecimalFromToken(left: Token): __DFDecimalFromToken = new __DFDecimalFromToken(left)
      sealed class __DFDecimalFromTokenW[S, W, F](left : TokenW[S, W, F]) extends AbleOps[TokenW[S, W, F]](left)
      protected implicit def __DFDecimalFromTokenW[S, W, F](left: TokenW[S, W, F]): __DFDecimalFromTokenW[S, W, F] = new __DFDecimalFromTokenW[S, W, F](left)
      protected implicit def __ofDFDecimal[S, W, F](left : DFDecimal[S, W, F]) : Able[DFDecimal[S, W, F]] = new Able(left)
      protected implicit class __ExtendableDecimalOps[LS, LW, LF](val left : DFDecimal[LS, LW, LF] with Extendable){
        def +  [R](right : Exact[R])(implicit op: `Op+`.Builder[DFDecimal[LS, LW, LF], true, R]) = op(left, right)
        def -  [R](right : Exact[R])(implicit op: `Op-`.Builder[DFDecimal[LS, LW, LF], true, R]) = op(left, right)
      }
      protected implicit class __DFDecimalOps[LS, LW, LF](val left : DFDecimal[LS, LW, LF]){
        def +   [R](right : Exact[R])(implicit op: `Op+`.Builder[DFDecimal[LS, LW, LF], false, R]) = op(left, right)
        def +^  [R](right : Exact[R])(implicit op: `Op+^`.Builder[DFDecimal[LS, LW, LF], true, R]) = op(left, right)
        def -   [R](right : Exact[R])(implicit op: `Op-`.Builder[DFDecimal[LS, LW, LF], false, R]) = op(left, right)
        def -^  [R](right : Exact[R])(implicit op: `Op-^`.Builder[DFDecimal[LS, LW, LF], true, R]) = op(left, right)
        def *   [R](right : Exact[R])(implicit op: `Op*`.Builder[DFDecimal[LS, LW, LF], false, R]) = op(left, right)
        def *^  [R](right : Exact[R])(implicit op: `Op*^`.Builder[DFDecimal[LS, LW, LF], true, R]) = op(left, right)
        def <   [R](right : Exact[R])(implicit op: `Op<`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def >   [R](right : Exact[R])(implicit op: `Op>`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def <=  [R](right : Exact[R])(implicit op: `Op<=`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def >=  [R](right : Exact[R])(implicit op: `Op>=`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def === [R](right : Exact[R])(implicit op: DFAny.`Op==`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def =!= [R](right : Exact[R])(implicit op: DFAny.`Op!=`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def <<  [R](right : Exact[R])(implicit op: `Op<<`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)
        def >>  [R](right : Exact[R])(implicit op: `Op>>`.Builder[DFDecimal[LS, LW, LF], R]) = op(left, right)

        def extendable : DFDecimal[LS, LW, LF] with Extendable = left.@@[Extendable]
        private[DFDecimal] def resizeWF[RW, RF](toWidth : TwoFace.Int[RW], toFractionWidth : TwoFace.Int[RF])(
          implicit ctx : DFAny.Context
        ) : DFDecimal[LS, RW, RF] = {
          left.member match {
            case DFAny.Const(_, token : Token, _, _) =>
              DFAny.Const.forced[Type[LS, RW, RF]](token.resize(toWidth, toFractionWidth))
            case _ =>
              if (
                left.width.getValue == toWidth.getValue &&
                  left.dfType.fractionWidth.getValue == toFractionWidth.getValue
              ) left.asValOf[Type[LS, RW, RF]]
              else {
                val dfType = Type(left.dfType.signed, toWidth, toFractionWidth)
                val resizeCS =
                  if (toFractionWidth.getValue == left.dfType.fractionWidth.getValue) cs"$left.${CSFunc(_.DF)}resize($toWidth)"
                  else cs"$left.${CSFunc(_.DF)}resize($toWidth, $toFractionWidth)"
                DFAny.Alias.AsIs(dfType, left.member.asValOf[Type[LS, LW, LF]]) tag resizeCS
              }
          }
        }

        private type ResizeCheck[S, W, F] =
          ITE[
            S,
            ITE[
              F == 0,
              //DFSInt
              RequireMsg[W > 1, "Signed integer width must be larger than 1. Width = " + ToString[W]],
              //DFSFix
              RequireMsg[W > F, "Signed fixed point width must be larger than the fraction width. Width = " + ToString[W] + ", Fraction Width = " + ToString[F]],
            ],
            ITE[
              F == 0,
              //DFUInt
              RequireMsg[W > 0, "Unsigned integer width must be positive. Width = " + ToString[W]],
              //DFUFix
              RequireMsg[W >= F, "Unsigned fixed point width must not be smaller than the fraction width. Width = " + ToString[W] + ", Fraction Width = " + ToString[F]],
            ],
          ]

        def resize[RW](toWidth : TwoFace.Int[RW])(
          implicit
          ctx : DFAny.Context,
          resizeCheck : TwoFace.Boolean.Shell3[ResizeCheck, LS, scala.Boolean, RW, scala.Int, LF, scala.Int]
        ) = {
          resizeCheck(left.dfType.signed, toWidth, left.dfType.fractionWidth)
          resizeWF(toWidth, left.dfType.fractionWidth)
        }

        def resize[RW, RF](toWidth : TwoFace.Int[RW], toFractionWidth : Natural.Int.Checked[RF])(
          implicit
          ctx : DFAny.Context,
          resizeCheck : TwoFace.Boolean.Shell3[ResizeCheck, LS, scala.Boolean, RW, scala.Int, RF, scala.Int]
        ) = {
          resizeCheck(left.dfType.signed, toWidth, toFractionWidth)
          resizeWF(toWidth, toFractionWidth)
        }
      }
      protected implicit class __DFUnsignedDecimalOps[LW, LF](val left : DFDecimal[false, LW, LF]){
        def signed(
          implicit
          ctx : DFAny.Context,
          widthPlus1 : TwoFace.Int.Shell2[+,LW,Int,0,Int]
        ) : DFDecimal[true, widthPlus1.Out, LF] = {
          val width = widthPlus1(left.width, 1)
          DFAny.Alias.AsIs(Type(true, width, left.dfType.fractionWidth), left.resizeWF(width, left.dfType.fractionWidth))
        }
      }
      protected implicit class __DFSignedDecimalOps[LW, LF](val left : DFDecimal[true, LW, LF]){
        def signBit(implicit ctx : DFAny.Context) : DFBit = left.bit(left.width-1)
        def unary_-(implicit ctx : DFAny.Context) : DFDecimal[true, LW, LF] =
          DFAny.Func1(left.dfType, left, DFAny.Func1.Op.unary_-)(-_)

      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFDecimalFromInt[L <: Int](left : L) : __DFDecimalFromInt[L] = super.__DFDecimalFromInt(left)
        final override protected implicit def __DFDecimalFromToken(left : Token) : __DFDecimalFromToken = super.__DFDecimalFromToken(left)
        final override protected implicit def __DFDecimalFromTokenW[S, W, F](left : TokenW[S, W, F]) : __DFDecimalFromTokenW[S, W, F] = super.__DFDecimalFromTokenW(left)
        final override protected implicit def __DFDecimalFromXInt[L <: XInt](left : L) : __DFDecimalFromXInt[L] = super.__DFDecimalFromXInt(left)
        final override protected implicit def __DFDecimalWiden[S, FW, FF, TW, TF](c : DFDecimal[S, FW, FF])(implicit eqW : Eq[FW, TW, Int], eqF : Eq[FF, TF, Int]) : DFDecimal[S, TW, TF] = super.__DFDecimalWiden(c)
        final override protected implicit def __ofDFDecimal[S, W, F](left : DFDecimal[S, W, F]) : Able[DFDecimal[S, W, F]] = super.__ofDFDecimal(left)
        final override protected implicit def __DFDecimalOps[LS, LW, LF](left : DFDecimal[LS, LW, LF]) : __DFDecimalOps[LS, LW, LF] = super.__DFDecimalOps(left)
        final override protected implicit def __DFSignedDecimalOps[LW, LF](left : DFDecimal[true, LW, LF]) : __DFSignedDecimalOps[LW, LF] = super.__DFSignedDecimalOps(left)
        final override protected implicit def __DFUnsignedDecimalOps[LW, LF](left : DFDecimal[false, LW, LF]) : __DFUnsignedDecimalOps[LW, LF] = super.__DFUnsignedDecimalOps(left)
        final override protected implicit def __ExtendableDecimalOps[LS, LW, LF](left : DFDecimal[LS, LW, LF] with Extendable) : __ExtendableDecimalOps[LS, LW, LF] = super.__ExtendableDecimalOps(left)
      }
      trait Imported extends Frontend {
        final override implicit def __DFDecimalFromInt[L <: Int](left : L) : __DFDecimalFromInt[L] = super.__DFDecimalFromInt(left)
        final override implicit def __DFDecimalFromToken(left : Token) : __DFDecimalFromToken = super.__DFDecimalFromToken(left)
        final override implicit def __DFDecimalFromTokenW[S, W, F](left : TokenW[S, W, F]) : __DFDecimalFromTokenW[S, W, F] = super.__DFDecimalFromTokenW(left)
        final override implicit def __DFDecimalFromXInt[L <: XInt](left : L) : __DFDecimalFromXInt[L] = super.__DFDecimalFromXInt(left)
        final override implicit def __DFDecimalWiden[S, FW, FF, TW, TF](c : DFDecimal[S, FW, FF])(implicit eqW : Eq[FW, TW, Int], eqF : Eq[FF, TF, Int]) : DFDecimal[S, TW, TF] = super.__DFDecimalWiden(c)
        final override implicit def __ofDFDecimal[S, W, F](left : DFDecimal[S, W, F]) : Able[DFDecimal[S, W, F]] = super.__ofDFDecimal(left)
        final override implicit def __DFDecimalOps[LS, LW, LF](left : DFDecimal[LS, LW, LF]) : __DFDecimalOps[LS, LW, LF] = super.__DFDecimalOps(left)
        final override implicit def __DFSignedDecimalOps[LW, LF](left : DFDecimal[true, LW, LF]) : __DFSignedDecimalOps[LW, LF] = super.__DFSignedDecimalOps(left)
        final override implicit def __DFUnsignedDecimalOps[LW, LF](left : DFDecimal[false, LW, LF]) : __DFUnsignedDecimalOps[LW, LF] = super.__DFUnsignedDecimalOps(left)
        final override implicit def __ExtendableDecimalOps[LS, LW, LF](left : DFDecimal[LS, LW, LF] with Extendable) : __ExtendableDecimalOps[LS, LW, LF] = super.__ExtendableDecimalOps(left)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Warning when comparison with a constant will always yield a false result.
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `VarS signMatch ConstS` extends Checked1Param.Boolean {
    type Cond[VS, CS] = ITE[CS, VS, true]
    type Msg[VS, CS] = "A static boolean result detected, when an unsigned variable is compared with a signed constant."
    type ParamFace = Boolean
  }
  object `VarW >= ConstW` extends Checked1Param.Int {
    type Cond[VW, CW] = VW >= CW
    type Msg[VW, CW] = "A static boolean result detected, due to a comparison between a DF variable and a wider constant. Found: DFVar-width = "+ ToString[VW] + " and Num-width = " + ToString[CW]
    type ParamFace = Int
  }
  object `VarF >= ConstF` extends Checked1Param.Int {
    type Cond[VF, CF] = VF >= CF
    type Msg[VF, CF] = "A static boolean result detected, due to a comparison between a DF variable and a wider fraction constant. Found: DFVar-width = "+ ToString[VF] + " and Num-width = " + ToString[CF]
    type ParamFace = Int
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    import DFAny.`Op:=,<>`.Builder
    sealed trait Frontend {
      protected implicit def __DFDecimal_ac_DFDecimal[LS, LW, LF, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        signMatch : `LS == RS`.CheckedShell[LS, RS],
        fitsWidth : `LW >= RW`.CheckedShell[LW, RW],
        fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF],
      ) : Builder[Type[LS, LW, LF], DFDecimal[RS, RW, RF]] = (left, right) => {
        signMatch.unsafeCheck(left.signed, right.dfType.signed)
        fitsWidth.unsafeCheck(left.width, right.width)
        fitsFractionWidth.unsafeCheck(left.fractionWidth, right.dfType.fractionWidth)
        import DFDesign.Frontend._
        right.resize(left.width, left.fractionWidth).asValOf[Type[LS, LW, LF]]
      }

      protected implicit def __DFDecimal_eq_Capable[LS, LW, LF, RS, RW, RF](
        implicit
        signMatch : `LS == RS`.CheckedShell[LS, RS],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW],
        checkLFvRF : `LF == RF`.CheckedShell[LF, RF]
      ) : DFAny.`Op==,!=`.Capable[Type[LS, LW, LF], Type[RS, RW, RF]] = (left, right) => {
        signMatch.unsafeCheck(left.signed, right.signed)
        checkLWvRW.unsafeCheck(left.width, right.width)
        checkLFvRF.unsafeCheck(left.fractionWidth, right.fractionWidth)
      }

      protected implicit def __DFDecimal_eq_ConstCapable[VS, VW, VF, CS, CW, CF](
        implicit
        checkVSvCS : `VarS signMatch ConstS`.CheckedShellSym[Warn, VS, CS],
        checkVWvCW : `VarW >= ConstW`.CheckedShellSym[Warn, VW, CW],
        checkVFvCF : `VarF >= ConstF`.CheckedShellSym[Warn, VF, CF]
      ) : DFAny.`Op==,!=`.ConstCapable[Type[VS, VW, VF], Type[CS, CW, CF]] = (dfVar, const) => {
        checkVSvCS.unsafeCheck(dfVar.signed, const.signed)
        checkVWvCW.unsafeCheck(dfVar.width, const.width)
        checkVFvCF.unsafeCheck(dfVar.fractionWidth, const.fractionWidth)
      }

    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFDecimal_ac_DFDecimal[LS, LW, LF, RS, RW, RF](implicit ctx : DFAny.Context, signMatch : `LS == RS`.CheckedShell[LS, RS], fitsWidth : internals.`LW >= RW`.CheckedShell[LW, RW], fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF]) : Builder[Type[LS, LW, LF], DFDecimal[RS, RW, RF]] = super.__DFDecimal_ac_DFDecimal
        final override protected implicit def __DFDecimal_eq_Capable[LS, LW, LF, RS, RW, RF](implicit signMatch : `LS == RS`.CheckedShell[LS, RS], checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW], checkLFvRF : `LF == RF`.CheckedShell[LF, RF]) : `Op==,!=`.Capable[Type[LS, LW, LF], Type[RS, RW, RF]] = super.__DFDecimal_eq_Capable
        final override protected implicit def __DFDecimal_eq_ConstCapable[VS, VW, VF, CS, CW, CF](implicit checkVSvCS : `VarS signMatch ConstS`.CheckedShellSym[Warn, VS, CS], checkVWvCW : `VarW >= ConstW`.CheckedShellSym[Warn, VW, CW], checkVFvCF : `VarF >= ConstF`.CheckedShellSym[Warn, VF, CF]) : `Op==,!=`.ConstCapable[Type[VS, VW, VF], Type[CS, CW, CF]] = super.__DFDecimal_eq_ConstCapable
      }
      trait Imported extends Frontend {
        final override implicit def __DFDecimal_ac_DFDecimal[LS, LW, LF, RS, RW, RF](implicit ctx : DFAny.Context, signMatch : `LS == RS`.CheckedShell[LS, RS], fitsWidth : internals.`LW >= RW`.CheckedShell[LW, RW], fitsFractionWidth : `LF >= RF`.CheckedShell[LF, RF]) : Builder[Type[LS, LW, LF], DFDecimal[RS, RW, RF]] = super.__DFDecimal_ac_DFDecimal
        final override implicit def __DFDecimal_eq_Capable[LS, LW, LF, RS, RW, RF](implicit signMatch : `LS == RS`.CheckedShell[LS, RS], checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW], checkLFvRF : `LF == RF`.CheckedShell[LF, RF]) : `Op==,!=`.Capable[Type[LS, LW, LF], Type[RS, RW, RF]] = super.__DFDecimal_eq_Capable
        final override implicit def __DFDecimal_eq_ConstCapable[VS, VW, VF, CS, CW, CF](implicit checkVSvCS : `VarS signMatch ConstS`.CheckedShellSym[Warn, VS, CS], checkVWvCW : `VarW >= ConstW`.CheckedShellSym[Warn, VW, CW], checkVFvCF : `VarF >= ConstF`.CheckedShellSym[Warn, VF, CF]) : `Op==,!=`.ConstCapable[Type[VS, VW, VF], Type[CS, CW, CF]] = super.__DFDecimal_eq_ConstCapable
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comparison operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}

    object Builder {
      def create[L, LS, LW, LF, R, RS, RW, RF](properLR : (L, R) => (DFDecimal[LS, LW, LF], DFDecimal[RS, RW, RF]))(
        implicit ctx : DFAny.Context
      ) : Builder[L, R] = (leftL, rightR) => {
        val (left, right) = properLR(leftL, rightR)
        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
      }

      implicit def evDFDecimal_op_DFDecimal[LS, LW, LF, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        signMatch : `LS == RS`.CheckedShell[LS, RS],
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW],
        checkLFvRF : `LF == RF`.CheckedShell[LF, RF]
      ) : Builder[DFDecimal[LS,LW,LF], DFDecimal[RS,RW,RF]] =
        create[DFDecimal[LS,LW,LF], LS, LW, LF, DFDecimal[RS,RW,RF], RS, RW, RF]((left, right) => {
          signMatch.unsafeCheck(left.dfType.signed, right.dfType.signed)
          checkLWvRW.unsafeCheck(left.width, right.width)
          checkLFvRF.unsafeCheck(left.dfType.fractionWidth, right.dfType.fractionWidth)
          (left, right)
        })

      implicit def evDFDecimal_op_Const[LS, LW, LF, R, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[Type[LS, LW, LF], R, _ <: Type[RS, RW, RF]],
        checkVSvCS : `VarS signMatch ConstS`.CheckedShellSym[Warn, LS, RS],
        checkVWvCW : `VarW >= ConstW`.CheckedShellSym[Warn, LW, RW],
        checkVFvCF : `VarF >= ConstF`.CheckedShellSym[Warn, LF, RF]
      ) : Builder[DFDecimal[LS, LW, LF], R] =
        create[DFDecimal[LS, LW, LF], LS, LW, LF, R, RS, RW, RF]((left, rightNum) => {
          val right = rConst(left.dfType, rightNum).asValOf[Type[RS, RW, RF]]
          checkVSvCS.unsafeCheck(left.dfType.signed, right.dfType.signed)
          checkVWvCW.unsafeCheck(left.width, right.width)
          checkVFvCF.unsafeCheck(left.dfType.fractionWidth, right.dfType.fractionWidth)
          (left, right)
        })

      implicit def evConst_op_DFDecimal[L, LS, LW, LF, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.AsIs.Aux[Type[RS, RW, RF], L, _ <: Type[LS, LW, LF]],
        checkVSvCS : `VarS signMatch ConstS`.CheckedShellSym[Warn, RS, LS],
        checkVWvCW : `VarW >= ConstW`.CheckedShellSym[Warn, RW, LW],
        checkVFvCF : `VarF >= ConstF`.CheckedShellSym[Warn, RF, LF]
      ) : Builder[L, DFDecimal[RS, RW, RF]] =
        create[L, LS, LW, LF, DFDecimal[RS, RW, RF], RS, RW, RF]((leftNum, right) => {
          val left = lConst(right.dfType, leftNum).asValOf[Type[LS, LW, LF]]
          checkVSvCS.unsafeCheck(right.dfType.signed, left.dfType.signed)
          checkVWvCW.unsafeCheck(right.width, left.width)
          checkVFvCF.unsafeCheck(right.dfType.fractionWidth, left.dfType.fractionWidth)
          (left, right)
        })
    }
  }
  object `Op<`   extends OpsCompare(Func2.Op.< )(_ <  _)
  object `Op>`   extends OpsCompare(Func2.Op.> )(_ >  _)
  object `Op<=`  extends OpsCompare(Func2.Op.<=)(_ <= _)
  object `Op>=`  extends OpsCompare(Func2.Op.>=)(_ >= _)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // +/- operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Ops+Or-`[Op <: Func2.Op.Negateable](val op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `+` or `-` with the type ${R}")
    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[-L, LE, -R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = Max[LW, RW] + ITE[op.WC, 1, 0]
        type OutW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
        type CalcF[LF, RF] = Max[LF, RF]
        type OutF[LF, RF, ResF] = TwoFace.Int.Shell2Aux[CalcF, LF, Int, RF, Int, ResF]
      }

      object `Var signMatch Const` extends Checked1Param.Boolean {
        type Cond[VS, CS] = ITE[CS, VS, true]
        type Msg[VS, CS] = "A signed number on the LHS cannot accept this operation with an unsigned variable on the RHS."
        type ParamFace = Boolean
      }

      trait DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF] {
        type Out
        def apply(properLR : (L, R) => (DFDecimal[LS, LW, LF], DFDecimal[RS, RW, RF])) : Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LS, LW, LF, LE, R, RS, RW, RF, OutW, OutF](
          implicit
          ctx : DFAny.Context,
          outW : Inference.OutW[LW, RW, OutW],
          outF : Inference.OutF[LF, RF, OutF],
          doCheck : SafeBoolean[LE],
          checkLWvRW : `LW >= RW`.CheckedExtendable[LW, LE, RW],
          checkLFvRF : `LF >= RF`.CheckedExtendable[LF, LE, RF]
        ) : DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF]{type Out = DFDecimal[LS, OutW, OutF]} =
          new DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF]{
            type Out = DFDecimal[LS, OutW, OutF]
            def apply(properLR : (L, R) => (DFDecimal[LS, LW, LF], DFDecimal[RS, RW, RF])) : Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFDecimal[LS, OutW, OutF]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  if (doCheck) {
                    checkLWvRW.unsafeCheck(left.width, right.width)
                    checkLFvRF.unsafeCheck(left.dfType.fractionWidth, right.dfType.fractionWidth)
                  }
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val opFractionWidth = outF(left.dfType.fractionWidth, right.dfType.fractionWidth)
                  val out = Type(left.dfType.signed, opWidth, opFractionWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = op match {
                    case _ : Func2.Op.+ => _ + _
                    case _ : Func2.Op.- => _ - _
                    case _ : Func2.Op.+^ => _ +^ _
                    case _ : Func2.Op.-^ => _ -^ _
                    case _ => ???
                  }
                  DFAny.Func2(out, left, op, right)(func)
                }
              }
          }
      }

      implicit def evDFDecimal_op_DFDecimal[LS, LW, LF, LE, RS, RW, RF](
        implicit
        signMatch : `LS == RS`.CheckedShell[LS, RS],
        detailedBuilder: DetailedBuilder[DFDecimal[LS, LW, LF], LS, LW, LF, LE, DFDecimal[RS, RW, RF], RS, RW, RF]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFDecimal_op_Const[LS, LW, LF, LE, R, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[Type[LS, LW, LF], R, _ <: Type[RS, RW, RF]],
        detailedBuilder: DetailedBuilder[DFDecimal[LS, LW, LF], LS, LW, LF, LE, R, RS, RW, RF]
      ) = detailedBuilder((left, rightNum) => {
        val right = rConst(left.dfType, rightNum).asValOf[Type[RS, RW, RF]]
        (left, right)
      })

      implicit def evConst_op_DFDecimal[L, LS, LW, LF, LE, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.AsIs.Aux[Type[RS, RW, RF], L, _ <: Type[LS, LW, LF]],
        signMatch : `Var signMatch Const`.CheckedShell[RS, LS],
        detailedBuilder: DetailedBuilder[L, LS, LW, LF, LE, DFDecimal[RS, RW, RF], RS, RW, RF]
      ) = detailedBuilder((leftNum, right) => {
        val left = lConst(right.dfType, leftNum).asValOf[Type[LS, LW, LF]]
        (left, right)
      })
    }
  }
  object `Op+`  extends `Ops+Or-`(Func2.Op.+)
  object `Op+^` extends `Ops+Or-`(Func2.Op.+^)
  object `Op-`  extends `Ops+Or-`(Func2.Op.-)
  object `Op-^` extends `Ops+Or-`(Func2.Op.-^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // * operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class `Op*`[Op <: Func2.Op.OptionalCarry](val op : Op) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Ops `*` with the type ${R}")
    trait Builder[-L, LE, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[L, LE, R, Comp0] = Builder[L, LE, R] {
        type Out = Comp0
      }

      object Inference {
        import singleton.ops.math.Max
        type CalcW[LW, RW] = ITE[op.WC, LW + RW, Max[LW, RW]]
        type OutW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF] {
        type Out
        def apply(properLR : (L, R) => (DFDecimal[LS, LW, LF], DFDecimal[RS, RW, RF])) : Builder.Aux[L, LE, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LS, LW, LF, LE, R, RS, RW, RF, OutW, OutF](
          implicit
          ctx : DFAny.Context,
          signMatch : `LS == RS`.CheckedShell[LS, RS],
          outW : Inference.OutW[LW, RW, OutW],
          outF : Inference.OutW[LF, RF, OutF],
          doCheck : SafeBoolean[LE],
          checkLWvRW : `LW >= RW`.CheckedExtendable[LW, LE, RW],
          checkLFvRF : `LF >= RF`.CheckedExtendable[LF, LE, RF]
        ) : DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF]{type Out = DFDecimal[LS, OutW, OutF]} =
          new DetailedBuilder[L, LS, LW, LF, LE, R, RS, RW, RF]{
            type Out = DFDecimal[LS, OutW, OutF]
            def apply(properLR : (L, R) => (DFDecimal[LS, LW, LF], DFDecimal[RS, RW, RF])) : Builder.Aux[L, LE, R, Out] =
              new Builder[L, LE, R] {
                type Out = DFDecimal[LS, OutW, OutF]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  signMatch.unsafeCheck(left.dfType.signed, right.dfType.signed)
                  if (doCheck) {
                    checkLWvRW.unsafeCheck(left.width, right.width)
                    checkLFvRF.unsafeCheck(left.dfType.fractionWidth, right.dfType.fractionWidth)
                  }
                  // Constructing op
                  val opWidth = outW(left.width, right.width)
                  val opFractionWidth = outF(left.dfType.fractionWidth, right.dfType.fractionWidth)
                  val out = Type(left.dfType.signed, opWidth, opFractionWidth)
                  val func : (left.TToken, right.TToken) => out.TToken = op match {
                    case _ : Func2.Op.* => _ * _
                    case _ : Func2.Op.*^ => _ *^ _
                    case _ => ???
                  }
                  DFAny.Func2(out, left, op, right)(func)
                }
              }
          }
      }

      implicit def evDFDecimal_op_DFDecimal[LS, LW, LF, LE, RS, RW, RF](
        implicit
        detailedBuilder: DetailedBuilder[DFDecimal[LS, LW, LF], LS, LW, LF, LE, DFDecimal[RS, RW, RF], RS, RW, RF]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFDecimal_op_Const[LS, LW, LF, LE, R, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[Type[LS, LW, LF], R, _ <: Type[RS, RW, RF]],
        detailedBuilder: DetailedBuilder[DFDecimal[LS, LW, LF], LS, LW, LF, LE, R, RS, RW, RF]
      ) = detailedBuilder((left, rightNum) => {
        val right = rConst(left.dfType, rightNum).asValOf[Type[RS, RW, RF]]
        (left, right)
      })

      implicit def evConst_op_DFDecimal[L, LS, LW, LF, LE, RS, RW, RF](
        implicit
        ctx : DFAny.Context,
        lConst : DFAny.Const.AsIs.Aux[Type[RS, RW, RF], L, _ <: Type[LS, LW, LF]],
        detailedBuilder: DetailedBuilder[L, LS, LW, LF, LE, DFDecimal[RS, RW, RF], RS, RW, RF]
      ) = detailedBuilder((leftNum, right) => {
        val left = lConst(right.dfType, leftNum).asValOf[Type[LS, LW, LF]]
        (left, right)
      })
    }
  }
  object `Op*`  extends `Op*`(Func2.Op.*)
  object `Op*^` extends `Op*`(Func2.Op.*^)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Shift operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsShift(op : Func2.Op.Shift) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Shift Ops with the type ${R}")
    trait Builder[L <: DFAny, -R] extends DFAny.Op.Builder[L, R] {
      type Out = L
    }

    object Builder {
      object SmallShift extends Checked1Param.Int {
        type Cond[LW, RW] = BitsWidthOf.CalcInt[LW-1] >= RW
        type Msg[LW, RW] = "The shift vector is too large. Found: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }
      def create[LS, LW, LF, RW](left : DFDecimal[LS, LW, LF], right : DFUInt[RW])(
        implicit
        ctx : DFAny.Context,
        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : DFDecimal[LS, LW, LF] = {
        checkLWvRW.unsafeCheck(left.width, right.width)

        val out = left.dfType
        val func : (left.TToken, right.TToken) => out.TToken = op match {
          case _ : Func2.Op.>> => _ >> _
          case _ : Func2.Op.<< => _ << _
        }
        DFAny.Func2(out, left, op, right)(func)
      }

      implicit def evDFDecimal_op_DFUInt[LS, LW, LF, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : Builder[DFDecimal[LS, LW, LF], DFUInt[RW]] = (left, right) => create(left, right)

      implicit def evDFDecimal_op_Const[LS, LW, LF, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[DFUInt.Type[LW], R, _ <: DFUInt.Type[RW]],
        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : Builder[DFDecimal[LS, LW, LF], R] = (left, rightR) =>
        create(left, rConst(DFUInt.Type[LW](left.width), rightR).asValOf[DFUInt.Type[RW]])
    }
  }

  object `Op<<` extends OpsShift(Func2.Op.<<)
  object `Op>>` extends OpsShift(Func2.Op.>>)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
