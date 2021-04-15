package DFiant

import singleton.ops._
import DFiant.internals._
import DFAny.{Func2, Modifier, `Op==,!=`}
import compiler.csprinter._
import singleton.ops.OpContainer.Eq

import scala.annotation.nowarn

/**
  * A unconstrained-width literal vector of zeros
  *
  * @example
  * {{{
  * val x = DFBits(8) init b0s
  * x := b0s
  * }}}
  *
  * @note Some vector operations are not possible with this literal.
  *       E.g., `x ++ b0s` is forbidden because concatenation cannot infer
  *       the output width from this operation.
  */
object b0s extends DFBits.SameBitsVector(false)
/**
  * A unconstrained-width literal vector of ones
  *
  * @example
  * {{{
  * val x = DFBits(8) init b1s
  * x := b1s
  * }}}
  *
  * @note Some vector operations are not possible with this literal.
  *       E.g., `x ++ b1s` is forbidden because concatenation cannot infer
  *       the output width from this operation.
  */
object b1s extends DFBits.SameBitsVector(true)

object DFBits extends DFAny.Companion {
  final case class Type[W](width : TwoFace.Int[W]) extends DFAny.Type {
    type Width = W
    type TToken = Token
    type TPattern = DFBits.Pattern
    type TPatternAble[+R] = DFBits.Pattern.Able[R]
    type TPatternBuilder[LType <: DFAny.Type] = DFBits.Pattern.Builder[LType]
    def getBubbleToken: TToken = Token.bubble(width)
    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = fromToken
    def assignCheck(from : DFAny.Member)(implicit ctx : DFAny.Context) : Unit = trydf {
      from match {
        case r @ DFBits(w) =>
          import DFDesign.Frontend._
          val op = implicitly[DFAny.`Op:=,<>`.Builder[Type[W], DFBits[Int]]]
          op(this, r.asValOf[Type[Int]])
      }
    }
    def valueCodeString(value : BitVector)(implicit printer : CSPrinter) : String = ???
    def valueToBitVector(value : BitVector) : BitVector = value
    override def toString: String = s"DFBits[$width]"
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      s"$TP DFBits($LIT$width)"
    }
    override def equals(obj: Any): Boolean = obj match {
      case Type(width) => this.width.getValue == width.getValue
      case _ => false
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Public Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Construct a new dataflow bit vector according to the given width.
    * @param checkedWidth the required width. Must be positive.
    * @param ctx An implicit dataflow design context
    */
  def apply[W](checkedWidth : BitsWidth.Checked[W]) : Type[W] =
    Type(checkedWidth.unsafeCheck())

  /**
    * Construct a new dataflow bit vector according to the given width type argument.
    * @tparam W The required width. Must be positive.
    * @param ctx An implicit dataflow design context
    */
  def apply[W](
    implicit checkedWidth : BitsWidth.Checked[W], di: DummyImplicit
  ) : Type[W] = Type(checkedWidth)

  def unapply(arg: DFAny.Member): Option[Int] = arg.dfType match {
    case Type(width) => Some(width.getValue)
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
  type TokenW[W] = DFAny.TokenT[Token, Type[W]] with WidthTag[W]
  final case class Token(valueBits : BitVector, bubbleMask : BitVector) extends DFAny.Token {left =>
    assert(valueBits.length == bubbleMask.length)
    val width : Int = valueBits.length.toInt
    val dfType : DFAny.Type = Type(width)
    def & (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      assert(right.width == width)
      bb match {
        case Bubble.Stall =>
          Token(left.valueBits & right.valueBits, left.bubbleMask | right.bubbleMask)
        case Bubble.DontCare =>
          val valueBits = (left.valueBits | left.bubbleMask) & (right.valueBits | right.bubbleMask)
          val bubbleMask = (left.bubbleMask & right.bubbleMask) | (left.bubbleMask & right.valueBits) |
            (right.bubbleMask & left.valueBits)
          Token(valueBits, bubbleMask)
      }
    }
    def | (right : Token)(implicit bb : Bubble.Behaviour) : Token = {
      assert(right.width == width)
      bb match {
        case Bubble.Stall =>
          Token(left.valueBits & right.valueBits, left.bubbleMask | right.bubbleMask)
        case Bubble.DontCare =>
          val valueBits = (left.valueBits & left.bubbleMask.unary_~()) | (right.valueBits & right.bubbleMask.unary_~())
          val bubbleMask = (left.bubbleMask & right.bubbleMask) | (left.bubbleMask & right.valueBits.unary_~()) |
            (right.bubbleMask & left.valueBits.unary_~())
          Token(valueBits, bubbleMask)
      }
    }
    //dontcare in xor will always produce dontcare, like stall bubbles
    def ^ (right : Token) : Token = {
      assert(right.width == width)
      val valueBits = left.valueBits ^ right.valueBits
      val bubbleMask = left.bubbleMask | right.bubbleMask
      Token(valueBits, bubbleMask)
    }
    def ++ (right : Token) : Token = {
      Token(left.valueBits ++ right.valueBits, left.bubbleMask ++ right.bubbleMask)
    }
    def << (shift : DFUInt.Token) : Token = shift.value match {
      case Some(value) => Token(left.valueBits << value.toInt, left.bubbleMask << value.toInt)
      case None => Token.bubble(width)
    }
    def << (shift : Int) : Token = this << DFUInt.Token(shift.bitsWidth(false), shift)
    def >> (shift : DFUInt.Token) : Token = shift.value match {
      case Some(value) => Token(left.valueBits >>> value.toInt, left.bubbleMask >>> value.toInt)
      case None => Token.bubble(width)
    }
    def >> (shift : Int) : Token = this >> DFUInt.Token(shift.bitsWidth(false), shift)
    def unary_~ : Token = Token(left.valueBits.unary_~(), left.bubbleMask)
    def reverse : Token = Token(left.valueBits.reverseBitOrder, left.bubbleMask.reverseBitOrder)
    def resize(toWidth : Int) : Token = {
      if (toWidth < width) bitsWL(toWidth, 0)
      else if (toWidth > width) (Token.zero(toWidth - width) ++ this)
      else this
    }
    def bit(idx : DFUInt.Token) : DFBool.Token = idx.value match {
      case Some(value) => DFBool.Token(valueBits.bit(value.toLong), bubbleMask.bit(value.toLong))
      case None => DFBool.Token.bubble(false)
    }
    def == (right : DFAny.Token)(implicit bb : Bubble.Behaviour) : DFBool.Token = right match {
      case right : Token =>
        assert(right.width == width)
        bb match {
          case Bubble.Stall =>
            if (left.isBubble || right.isBubble) DFBool.Token.bubble(logical = true)
            else DFBool.Token(logical = true, left.valueBits == right.valueBits)
          case Bubble.DontCare =>
            val valueBits = (left.bubbleMask | right.bubbleMask | (left.valueBits ^ right.valueBits).unary_~()) == BitVector.high(width)
            DFBool.Token(logical = true, valueBits)
        }
      case _ => ???
    }
    def toUInt : DFUInt.Token = {
      if (isBubble) DFUInt.Token.bubble(width)
      else DFUInt.Token(width, BigInt(this.valueBits.padToMulsOf(8).toByteArray).asUnsigned(width))
    }
    def toSInt : DFSInt.Token = {
      if (isBubble) DFSInt.Token.bubble(width)
      else DFSInt.Token(width, BigInt(this.valueBits.padToMulsOf(8).toByteArray))
    }
    def getUIntValue : BigInt = valueBits.toBigInt.asUnsigned(width)
    def getSIntValue : BigInt = valueBits.toBigInt
    private def binZip(v : BitVector, b : BitVector, bubbleChar : Char) : String = v.toBin.zip(b.toBin).map {
      case (_, '1') => bubbleChar
      case (zeroOrOne, _) => zeroOrOne
    }.mkString
    private def hexZip(v : BitVector, b : BitVector, bubbleChar : Char, allowBinMode : Boolean) : Option[String] =
      Some(v.toHex.zip(b.toHex).flatMap {
        case (_, 'F' | 'f') => s"$bubbleChar"
        case (h, '0') => s"$h"
        case (h, b) if allowBinMode => s"{${binZip(BitVector(h), BitVector(b), bubbleChar)}}"
        case _ => return None
      }.mkString)
    def toBinString(bubbleChar : Char) : String = binZip(valueBits, bubbleMask, bubbleChar)
    def toHexString(bubbleChar : Char, allowBinMode : Boolean) : Option[String] = {
      if (width % 4 == 0) hexZip(valueBits, bubbleMask, bubbleChar, allowBinMode)
      else  {
        val headWidth = width % 4
        val (headValue, theRestValue) = valueBits.splitAt(headWidth)
        val (headBubble, theRestBubble) = bubbleMask.splitAt(headWidth)

        val headOption = {
          if (headBubble == BitVector.high(headWidth)) Some(s"$bubbleChar")
          else hexZip(headValue.resize(4), headBubble.resize(4), bubbleChar, allowBinMode)
        }
        val theRestOption = hexZip(theRestValue, theRestBubble, bubbleChar, allowBinMode)
        for (h <- headOption; tr <- theRestOption) yield h + tr
      }
    }
    def codeString(implicit printer: CSPrinter) : String = {
      import printer.config._
      import io.AnsiColor.BOLD
      val binRep = toBinString('?')
      val hexRep = s"${width}'${toHexString('?', allowBinMode = true).get}"
      //choosing the shorter representation for readability
      if (binRep.length <= hexRep.length) s"""$BOLD b$STR"$binRep""""
      else s"""$BOLD h$STR"$hexRep""""
    }
  }
  object Token {
    def zero(width : Int) : Token = Token(BitVector.low(width))
    def apply(value : BitVector) : Token = Token(value, BitVector.low(value.length))
    def apply(width : Int, value : BigInt) : Token = Token(value.toBitVector(width))
    def bubble(width : Int) : Token = Token(BitVector.low(width), BitVector.high(width))

    private val widthExp = "([0-9]+)'(.*)".r
    def fromBinString(bin : String) : Either[String, Token] = {
      val (explicitWidth, word) = bin match {
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _ => (None, bin)
      }
      val (valueBits, bubbleMask) = word.foldLeft((BitVector.empty, BitVector.empty)) {
        case (t, '_') => t //ignoring underscore
        case ((v, b), c) => c match { //bin mode
          case '?' => (v :+ false, b :+ true)
          case '0' => (v :+ false, b :+ false)
          case '1' => (v :+ true, b :+ false)
          case x => return Left(s"Found invalid binary character: $x")
        }
      }
      val token = Token(valueBits, bubbleMask)
      val actualWidth = (valueBits.lengthOfValue max bubbleMask.lengthOfValue).toInt
      explicitWidth match {
        case Some(value) if value < actualWidth => Left(s"Explicit given width ($value) is smaller than the actual width ($actualWidth)")
        case Some(value) => Right(token.resize(value))
        case None => Right(token)
      }
    }
    def fromHexString(hex : String) : Either[String, Token] = {
      val isHex = "[0-9a-fA-F]".r
      val (explicitWidth, word) = hex match {
        case widthExp(widthStr, wordStr) => (Some(widthStr.toInt), wordStr)
        case _ => (None, hex)
      }
      val (valueBits, bubbleMask, binMode) = word.foldLeft((BitVector.empty, BitVector.empty, false)) {
        case (t, '_') => t //ignoring underscore
        case ((v, b, false), c) => c match { //hex mode
          case '{' => (v, b, true)
          case '?' => (v ++ BitVector.low(4), b ++ BitVector.high(4), false)
          case isHex() => (v ++ BitVector.fromHex(c.toString).get, b ++ BitVector.low(4), false)
          case x => return Left(s"Found invalid hex character: $x")
        }
        case ((v, b, true), c) => c match { //bin mode
          case '}' => (v, b, false)
          case '?' => (v :+ false, b :+ true, true)
          case '0' => (v :+ false, b :+ false, true)
          case '1' => (v :+ true, b :+ false, true)
          case x => return Left(s"Found invalid binary character in binary mode: $x")
        }
      }
      if (binMode) Left(s"Missing closing braces of binary mode")
      else {
        val token = Token(valueBits, bubbleMask)
        val actualWidth = (valueBits.lengthOfValue max bubbleMask.lengthOfValue).toInt
        explicitWidth match {
          case Some(value) if value < actualWidth => Left(s"Explicit given width ($value) is smaller than the actual width ($actualWidth)")
          case Some(value) => Right(token.resize(value))
          case None => Right(token)
        }
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // String interpolation macros
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    import scala.reflect.macros.whitebox
    def binImplStringInterpolator(c: whitebox.Context) : c.Tree = stringInterpolatorGen("b")(c)
    def hexImplStringInterpolator(c: whitebox.Context) : c.Tree = stringInterpolatorGen("h")(c)
    def stringInterpolatorGen(k : String)(c: whitebox.Context) : c.Tree = {
      import c.universe._
      val kTpe = c.internal.constantType(Constant(k))
      val Apply(TypeApply(Select(properTree,_), _), argsTrees) = c.enclosingImplicits.last.tree
      val Apply(_, List(Apply(_, parts))) = properTree
      val fullExpressionParts = Seq(parts,argsTrees).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1)
      val fullExpressionTree = fullExpressionParts.reduce[Tree] {
        case (Literal(Constant(l)), Literal(Constant(r))) => Literal(Constant(l.toString + r.toString))
        case (l, r) => q"${l}.toString + ${r}.toString"
      }
      val widthTpe : c.Type = fullExpressionTree match {
        case Literal(Constant(t : String)) => k match {
          case "b" => DFBits.Token.fromBinString(t) match {
            case Right(value) => c.internal.constantType(Constant(value.width))
            case Left(msg) => c.abort(msg)
          }
          case "h" => DFBits.Token.fromHexString(t) match {
            case Right(value) => c.internal.constantType(Constant(value.width))
            case Left(msg) => c.abort(msg)
          }
        }
        case _ => typeOf[Int]
      }
      val buildTree = k match {
        case "b" => q"DFiant.DFBits.Token.fromBinString($fullExpressionTree).toOption.get"
        case "h" => q"DFiant.DFBits.Token.fromHexString($fullExpressionTree).toOption.get"
      }
      q"""
         new DFiant.Interpolator[DFiant.DFBits.Token, $kTpe] {
           type Out = DFiant.DFBits.TokenW[$widthTpe]
           val value : DFiant.DFBits.TokenW[$widthTpe] = $buildTree.asInstanceOf[DFiant.DFBits.TokenW[$widthTpe]]
         }
       """
    }
    type ToFit[LW, V] = DFAny.Token.ToFit.Summon.SAM[Type[LW], V, TokenW[LW]]
    type AsIs[LW, T, OW] = DFAny.Token.AsIs.Summon.Aux[Type[LW], T, Token, TokenW[OW]]
    sealed trait Frontend {
      protected implicit def __DFBitsTokenSBV[LW, V <: SameBitsVector]
      : AsIs[LW, V, LW] = new DFAny.Token.AsIs.Summon[Type[LW], V, Token] {
        type Out = TokenW[LW]
        def apply(from : Type[LW], value : V) : Out = {
        Token(BitVector.fill(from.width.getValue)(value.value)).typeTag[Type[LW]].@@[WidthTag[LW]]
        }
      }
      protected implicit def __DFBitsTokenTokenW[LW, RW](
        implicit
        sameWidth : `LW == RW`.CheckedShell[LW, RW]
      ) : AsIs[LW, TokenW[RW], LW] = new DFAny.Token.AsIs.Summon[Type[LW], TokenW[RW], Token] {
        type Out = TokenW[LW]
        def apply(from : Type[LW], value : TokenW[RW]) : Out = {
          sameWidth.unsafeCheck(from.width, value.width)
          value.asInstanceOf[Out]
        }
      }
      protected implicit def __DFBitsTokenToken[LW](
        implicit
        sameWidth : `LW == RW`.CheckedShell[LW, Int]
      ) : AsIs[LW, Token, LW] = new DFAny.Token.AsIs.Summon[Type[LW], Token, Token] {
        type Out = TokenW[LW]
        def apply(from : Type[LW], value : Token) : Out = {
          sameWidth.unsafeCheck(from.width, value.width)
          value.asInstanceOf[Out]
        }
      }
      protected implicit def __DFBitsTokenToFit[LW, V, RW](
        implicit
        summonedToken : AsIs[LW, V, RW],
        fitsWidth : `LW == RW`.CheckedShell[LW, RW]
      ) : ToFit[LW, V] = (from, value) => {
        val token = summonedToken(from, value)
        fitsWidth.unsafeCheck(from.width, token.width)
        token.asInstanceOf[TokenW[LW]]
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFBitsTokenSBV[LW, V <: SameBitsVector] : AsIs[LW, V, LW] = super.__DFBitsTokenSBV
        final override protected implicit def __DFBitsTokenToFit[LW, V, RW](implicit summonedToken : AsIs[LW, V, RW], fitsWidth : internals.`LW == RW`.CheckedShell[LW, RW]) : ToFit[LW, V] = super.__DFBitsTokenToFit
        final override protected implicit def __DFBitsTokenToken[LW](implicit sameWidth : internals.`LW == RW`.CheckedShell[LW, Int]) : AsIs[LW, Token, LW] = super.__DFBitsTokenToken
        final override protected implicit def __DFBitsTokenTokenW[LW, RW](implicit sameWidth : internals.`LW == RW`.CheckedShell[LW, RW]) : AsIs[LW, TokenW[RW], LW] = super.__DFBitsTokenTokenW
      }
      trait Imported extends Frontend {
        final override implicit def __DFBitsTokenSBV[LW, V <: SameBitsVector] : AsIs[LW, V, LW] = super.__DFBitsTokenSBV
        final override implicit def __DFBitsTokenToFit[LW, V, RW](implicit summonedToken : AsIs[LW, V, RW], fitsWidth : internals.`LW == RW`.CheckedShell[LW, RW]) : ToFit[LW, V] = super.__DFBitsTokenToFit
        final override implicit def __DFBitsTokenToken[LW](implicit sameWidth : internals.`LW == RW`.CheckedShell[LW, Int]) : AsIs[LW, Token, LW] = super.__DFBitsTokenToken
        final override implicit def __DFBitsTokenTokenW[LW, RW](implicit sameWidth : internals.`LW == RW`.CheckedShell[LW, RW]) : AsIs[LW, TokenW[RW], LW] = super.__DFBitsTokenTokenW
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val patternCodeString : CodeStringOf[Token] = new CodeStringOf[Token] {
    def apply(t : Token)(implicit printer : CSPrinter) : String = t.codeString
  }
  class Pattern(set : Set[Token]) extends DFAny.Pattern.OfSet[Type[Int], Token, Pattern](set)(patternCodeString) {
    protected def matchCond(matchVal: DFAny.Of[Type[Int]], value : Token)(
      implicit ctx: DFAny.Context
    ): DFBool = {
      import DFDesign.Frontend._
      matchVal === value
    }
  }
  object Pattern extends PatternCO {
    trait Able[+R] extends DFAny.Pattern.Able[R] {
      val token : Token
    }
    object Able {
      implicit class DFBitsPattern[R <: Token](val right : R) extends Able[R] {
        val token : Token = right
      }
    }
    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
    object Builder {
      implicit def ev[LW] : Builder[Type[LW]] = new Builder[Type[LW]] {
        def apply[R](left: Type[LW], right: Seq[Able[R]]): Pattern = {
          val patternSet = right.map(e => e.token).foldLeft(Set.empty[Token])((set, token) => {
            if (set.contains(token)) throw new IllegalArgumentException(s"\nThe bitvector $token already intersects with $set")
            if (token.width > left.width) throw new IllegalArgumentException(s"\nThe bitvector $token is wider than ${left.width}")
            set + token
          })

          new Pattern(patternSet)
        }
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Constant Builder
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Const {
    trait Builder[N] {
      type W
      def apply(value : N) : DFAny.Of[Type[W]]
    }
    object Builder {
      type Aux[N, W0] = Builder[N]{type W = W0}
      implicit def fromValueOf[N](
        implicit const : Builder[N]
      ) : Aux[ValueOf[N], const.W] = new Builder[ValueOf[N]] {
        type W = const.W
        def apply(value : ValueOf[N]) : DFAny.Of[Type[W]] = const(value.value)
      }
      implicit def fromToken(implicit ctx : DFAny.Context)
      : Aux[Token, Int] = new Builder[Token] {
        type W = Int
        def apply(value : Token) : DFAny.Of[Type[W]] = {
          DFAny.Const[Type[Int]](Type(value.width), value)
        }
      }
      implicit def fromTokenW[W0](implicit ctx : DFAny.Context)
      : Aux[TokenW[W0], W0] = new Builder[TokenW[W0]] {
        type W = W0
        def apply(value : TokenW[W0]) : DFAny.Of[Type[W]] = {
          val width = TwoFace.Int.create[W0](value.width)
          DFAny.Const[Type[W0]](Type(width), value)
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
      final val left = value
      /**
        * @return the dataflow Bitwise Or result.
        */
      final def |   [RW](right : DFBits[RW])(implicit op: `Op|`.Builder[L, DFBits[RW]]) = op(left, right)
      /**
        * @return the dataflow Bitwise And result.
        */
      final def &   [RW](right : DFBits[RW])(implicit op: `Op&`.Builder[L, DFBits[RW]]) = op(left, right)
      /**
        * @return the dataflow Bitwise Xor result.
        */
      final def ^   [RW](right : DFBits[RW])(implicit op: `Op^`.Builder[L, DFBits[RW]]) = op(left, right)
      /**
        * @return the dataflow comparison equality result.
        */
      final def === [RW](right : DFBits[RW])(implicit op: DFAny.`Op==`.Builder[L, DFBits[RW]]) = op(left, right)
      /**
        * @return the dataflow comparison inequality result.
        */
      final def =!= [RW](right : DFBits[RW])(implicit op: DFAny.`Op!=`.Builder[L, DFBits[RW]]) = op(left, right)
      /**
        * @return the dataflow Bit Concatenation result.
        */
      final def ++  [RW](right : DFBits[RW])(implicit op: `Op++`.Builder[L, DFBits[RW]]) = op(left, right)
    }
    sealed trait Frontend {
      protected implicit def __DFBitsWiden[FW, TW](c : DFBits[FW])(implicit eq : OpContainer.Eq[FW, TW, Int]) : DFBits[TW] = c.asInstanceOf[DFBits[TW]]
      sealed class __DFBitsFromToken(left : Token) extends AbleOps[Token](left)
      protected implicit def __DFBitsFromToken(left: Token): __DFBitsFromToken = new __DFBitsFromToken(left)
      sealed class __DFBitsFromTokenW[W](left : TokenW[W]) extends AbleOps[TokenW[W]](left)
      protected implicit def __DFBitsFromTokenW[W](left: TokenW[W]): __DFBitsFromTokenW[W] = new __DFBitsFromTokenW[W](left)
      sealed class __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) extends AbleOps[SBV](left)
      protected implicit def __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : __DFBitsFromZeros[SBV] = new __DFBitsFromZeros(left)
      protected implicit def __ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = new Able(left)
      protected implicit def __DFBits_eq_Capable[LW, RW](
        implicit checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : DFAny.`Op==,!=`.Capable[Type[LW], Type[RW]] =
        (left, right) => checkLWvRW.unsafeCheck(left.width, right.width)

      protected implicit def __DFBits_eq_ConstCapable[LW, RW](
        implicit checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : DFAny.`Op==,!=`.ConstCapable[Type[LW], Type[RW]] =
        (left, right) => checkLWvRW.unsafeCheck(left.width, right.width)
      protected implicit class __DFBitsOps[LW](val left : DFBits[LW]){
        /**
          * @return the dataflow Bitwise Or result.
          */
        def |   [R](right : Exact[R])(implicit op: `Op|`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow Bitwise And result.
          */
        def &   [R](right : Exact[R])(implicit op: `Op&`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow Bitwise Xor result.
          */
        def ^   [R](right : Exact[R])(implicit op: `Op^`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow comparison equality result.
          */
        def === [R](right : Exact[R])(implicit op: DFAny.`Op==`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow comparison inequality result.
          */
        def =!= [R](right : Exact[R])(implicit op: DFAny.`Op!=`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow Bit Concatenation result.
          */
        def ++  [R](right : Exact[R])(implicit op: `Op++`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow Bit Inversion result.
          */
        def unary_~(implicit ctx : DFAny.Context) : DFBits[LW] =
          DFAny.Func1(left.dfType, left, DFAny.Func1.Op.unary_~)(~_)
        /**
          * @return the dataflow logical Left-Shift result.
          */
        def << [R](right: Exact[R])(implicit op: `Op<<`.Builder[DFBits[LW], R]) = op(left, right)
        /**
          * @return the dataflow logical Right-Shift result.
          *
          * @note For an arithmetic right-shift, cast to DFSInt (using `.sint`) and then right-shift
          */
        def >> [R](right: Exact[R])(implicit op: `Op>>`.Builder[DFBits[LW], R]) = op(left, right)

        /**
          * Bit Vector Resizing
          *
          * - If the target width is identical to the current width, the vector is returned as-is.
          * - If the target width is wider than the current width, extra zeros are added to the left (the MSbits).
          * - If the target width is shorted than the current width, the extra left (MSbits) are discarded.
          *
          * @param toWidth the required target width. Must be positive.
          * @return the resized dataflow bitvector.
          */
        def resize[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] = trydf {
          left.member match {
            case const @ DFAny.Const(_, token : Token, _, _) if const.isAnonymous =>
              DFAny.Const.forced[Type[RW]](token.resize(toWidth))
            case _ =>
              if (left.width.getValue == toWidth.getValue) left.asInstanceOf[DFBits[RW]]
              else
                DFAny.Alias.AsIs(Type(toWidth), left.asValOf[Type[RW]]) tag cs"$left.${CSFunc(_.DF)}resize($toWidth)"
          }
        }

        /**
          * Bit Vector Resizing, on the right (LSbits)
          *
          * The same behavior as `resize`, but on the right side.
          *
          * @param toWidth the required target width. Must be positive.
          * @return the resized dataflow bitvector.
          */
        def resizeRight[RW](toWidth : BitsWidth.Checked[RW])(implicit ctx : DFAny.Context) : DFBits[RW] = trydf {
          val ret = if (left.width < toWidth) {
            val zeroWidth = toWidth - left.width
            val zeros = DFAny.Const.forced[Type[Int]](Token.zero(zeroWidth))
            `Op++`.forced(left, zeros)
          }
          else if (left.width > toWidth)
            DFAny.Alias.BitsWL[DFAny.Modifier.Val, RW](left.member, toWidth, left.width - toWidth)
          else left
          ret.asInstanceOf[DFBits[RW]]
        }
        /**
          * @return a dataflow alias as an unsigned integer.
          */
        def uint(implicit ctx : DFAny.Context) : DFUInt[LW] = left.member match {
          case DFAny.Const(_, token : DFBits.Token, _, _) =>
            DFAny.Const.forced(token.toUInt).asValOf[DFUInt.Type[LW]]
          case _ =>
            left.as(DFUInt(left.width)) tag cs"$left.uint"
        }

        /**
          * @return a dataflow alias as a signed integer.
          */
        def sint(implicit ctx : DFAny.Context) : DFSInt[LW] = left.member match {
          case DFAny.Const(_, token : DFBits.Token, _, _) =>
            DFAny.Const.forced(token.toSInt).asValOf[DFSInt.Type[LW]]
          case _ =>
            left.as(DFSInt(left.width)) tag cs"$left.sint"
        }

        /**
          * Cast the dataflow variable as specified by the dataflow type
          * @param dfTemplate The dataflow type to cast the variable as.
          *                   Must match the same width as the current variable width.
          * @return the casted variable
          * @example
          * {{{
          *   val x = DFBits(8) <> IN
          *   val y = x.as(DFSInt(8))
          * }}}
          */
        final def as[TT, AT <: DFAny.Type](dfTemplate : TT)(
          implicit tc : TT => AT, ctx : DFAny.Context, equalWidth : AsWidth.CheckedShell[AT#Width, LW]
        ) : DFAny.Of[AT] = trydf {
          val dfType = tc(dfTemplate)
          if (left.dfType == dfType) left.asValOf[AT]
          else {
            equalWidth.unsafeCheck(dfType.width, left.dfType.width)
            DFAny.Alias.AsIs(dfType, left)
          }
        }
      }
      protected implicit class __DFBitsAliases[LW, Mod <: DFAny.Modifier.Val](val left : DFAny.Value[Type[LW], Mod]) {
        /**
          * Partial Bit Vector Selection
          * @param relBitHigh relative high bit index.
          *                   Must be within bound of [0 : width-1].
          *                   Must be larger than `relBitLow`.
          * @param relBitLow relative low bit index
          *                   Must be within bound of [0 : width-1].
          *                   Must be smaller than `relBitHigh`.
          * @return the dataflow partial selected bits from the given vector
          */
        def apply[H, L](relBitHigh : BitIndex.Checked[H, left.Width], relBitLow : BitIndex.Checked[L, left.Width])(
          implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
        ) : DFAny.Value[DFBits.Type[relWidth.Out], Mod] = trydf {
          left.bits(relBitHigh, relBitLow).asValModOf[DFBits.Type[relWidth.Out], Mod] tag
            cs"$left(${CSFunc(_.LIT)}$relBitHigh, ${CSFunc(_.LIT)}$relBitLow)"
        }

        /**
          * Bit Selection
          *
          * @param relBit relative bit index. Must be within bound of [0 : width-1]
          * @return the dataflow bit at the given index
          */
        def apply[I](relBit: BitIndex.Checked[I, left.Width])(
          implicit ctx : DFAny.Context
        ) : DFAny.Value[DFBool.Type, Mod] = trydf {
          left.bit(relBit).asValModOf[DFBool.Type, Mod] tag cs"$left(${CSFunc(_.LIT)}$relBit)"
        }

        /**
          * @return the most (left) significant dataflow bit
          */
        def msbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit[Mod](left.member, left.width.getValue-1)tag cs"$left.msbit"

        /**
          * @return the least (right) significant dataflow bit
          */
        def lsbit(implicit ctx : DFAny.Context): DFAny.Value[DFBool.Type, Mod] =
          DFAny.Alias.BitsWL.bit[Mod](left.member, 0) tag cs"$left.lsbit"
      }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Tuple-handling Implicits
      ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      sealed abstract class __VarProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.asInstanceOf[List[DFAny]].map(f => f.width.getValue).sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFAny.VarOf[Type[w.Out]] = ???
//          new DFBits.Alias[w.Out](DFAny.Alias.Reference.Concat(e.productIterator.toList.asInstanceOf[List[DFAny]], ".bits"))
      }

      sealed abstract class __ValProductExtender(e : Product) {
        type WSum
        protected val wsum : Int = e.productIterator.toList.collect{
          case dfAny : DFAny => dfAny.width.getValue
          case token : Token => token.width
        }.sum
        def bits(implicit ctx : DFAny.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFBits[w.Out] = {
          val list : List[DFBits[Int]] = e.productIterator.toList.collect{
            case dfAny : DFAny.Value[_,DFAny.Modifier.Val @unchecked] => dfAny.bits.asInstanceOf[DFBits[Int]]
            case token : Token => DFAny.Const.forced[Type[Int]](token)
          }
          list.reduce((l, r) => `Op++`.forced(l, r)).asInstanceOf[DFBits[w.Out]]
        }
      }

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 1
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple1[T1 <: DFAny.Type](
//        val e : Tuple1[DFAny.VarOf[T1]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width
//      }

      protected implicit class __ValTuple1[T1 <: HasWidth](
        val e : Tuple1[T1]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 2
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple2[T1 <: DFAny.Type, T2 <: DFAny.Type](
//        val e : Tuple2[DFAny.VarOf[T1], DFAny.VarOf[T2]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width
//      }

      protected implicit class __ValTuple2[T1 <: HasWidth, T2 <: HasWidth](
        val e : Tuple2[T1, T2]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 3
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple3[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type](
//        val e : Tuple3[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width
//      }

      protected implicit class __ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](
        val e : Tuple3[T1, T2, T3]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 4
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple4[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type](
//        val e : Tuple4[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
//      }

      protected implicit class __ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](
        val e : Tuple4[T1, T2, T3, T4]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

      /////////////////////////////////////////////////////////////////////////////////////
      // Tuple 5
      /////////////////////////////////////////////////////////////////////////////////////
//      implicit class VarTuple5[T1 <: DFAny.Type, T2 <: DFAny.Type, T3 <: DFAny.Type, T4 <: DFAny.Type, T5 <: DFAny.Type](
//        val e : Tuple5[DFAny.VarOf[T1], DFAny.VarOf[T2], DFAny.VarOf[T3], DFAny.VarOf[T4], DFAny.VarOf[T5]]
//      ) extends VarProductExtender(e) {
//        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
//      }

      protected implicit class __ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](
        val e : Tuple5[T1, T2, T3, T4, T5]
      ) extends __ValProductExtender(e){
        type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
      }
      /////////////////////////////////////////////////////////////////////////////////////

    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFBitsFromToken(left : Token) : __DFBitsFromToken = super.__DFBitsFromToken(left)
        final override protected implicit def __DFBitsFromTokenW[W](left : TokenW[W]) : __DFBitsFromTokenW[W] = super.__DFBitsFromTokenW(left)
        final override protected implicit def __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : __DFBitsFromZeros[SBV] = super.__DFBitsFromZeros(left)
        final override protected implicit def __DFBitsWiden[FW, TW](c : DFBits[FW])(implicit eq : Eq[FW, TW, Int]) : DFBits[TW] = super.__DFBitsWiden(c)
        final override protected implicit def __ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = super.__ofDFBits(left)
        final override protected implicit def __DFBitsAliases[LW, Mod <: Modifier.Val](left : DFAny.Value[Type[LW], Mod]) : __DFBitsAliases[LW, Mod] = super.__DFBitsAliases(left)
        final override protected implicit def __DFBitsOps[LW](left : DFBits[LW]) : __DFBitsOps[LW] = super.__DFBitsOps(left)
        final override protected implicit def __DFBits_eq_Capable[LW, RW](implicit checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : `Op==,!=`.Capable[Type[LW], Type[RW]] = super.__DFBits_eq_Capable
        final override protected implicit def __DFBits_eq_ConstCapable[LW, RW](implicit checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : `Op==,!=`.ConstCapable[Type[LW], Type[RW]] = super.__DFBits_eq_ConstCapable
        final override protected implicit def __ValTuple1[T1 <: HasWidth](e : Tuple1[T1]) : __ValTuple1[T1] = super.__ValTuple1(e)
        final override protected implicit def __ValTuple2[T1 <: HasWidth, T2 <: HasWidth](e : (T1, T2)) : __ValTuple2[T1, T2] = super.__ValTuple2(e)
        final override protected implicit def __ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](e : (T1, T2, T3)) : __ValTuple3[T1, T2, T3] = super.__ValTuple3(e)
        final override protected implicit def __ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](e : (T1, T2, T3, T4)) : __ValTuple4[T1, T2, T3, T4] = super.__ValTuple4(e)
        final override protected implicit def __ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](e : (T1, T2, T3, T4, T5)) : __ValTuple5[T1, T2, T3, T4, T5] = super.__ValTuple5(e)
      }
      trait Imported extends Frontend {
        final override implicit def __DFBitsFromToken(left : Token) : __DFBitsFromToken = super.__DFBitsFromToken(left)
        final override implicit def __DFBitsFromTokenW[W](left : TokenW[W]) : __DFBitsFromTokenW[W] = super.__DFBitsFromTokenW(left)
        final override implicit def __DFBitsFromZeros[SBV <: SameBitsVector](left : SBV) : __DFBitsFromZeros[SBV] = super.__DFBitsFromZeros(left)
        final override implicit def __DFBitsWiden[FW, TW](c : DFBits[FW])(implicit eq : Eq[FW, TW, Int]) : DFBits[TW] = super.__DFBitsWiden(c)
        final override implicit def __ofDFBits[W](left : DFBits[W]) : Able[DFBits[W]] = super.__ofDFBits(left)
        final override implicit def __DFBitsAliases[LW, Mod <: Modifier.Val](left : DFAny.Value[Type[LW], Mod]) : __DFBitsAliases[LW, Mod] = super.__DFBitsAliases(left)
        final override implicit def __DFBitsOps[LW](left : DFBits[LW]) : __DFBitsOps[LW] = super.__DFBitsOps(left)
        final override implicit def __DFBits_eq_Capable[LW, RW](implicit checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : `Op==,!=`.Capable[Type[LW], Type[RW]] = super.__DFBits_eq_Capable
        final override implicit def __DFBits_eq_ConstCapable[LW, RW](implicit checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : `Op==,!=`.ConstCapable[Type[LW], Type[RW]] = super.__DFBits_eq_ConstCapable
        final override implicit def __ValTuple1[T1 <: HasWidth](e : Tuple1[T1]) : __ValTuple1[T1] = super.__ValTuple1(e)
        final override implicit def __ValTuple2[T1 <: HasWidth, T2 <: HasWidth](e : (T1, T2)) : __ValTuple2[T1, T2] = super.__ValTuple2(e)
        final override implicit def __ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](e : (T1, T2, T3)) : __ValTuple3[T1, T2, T3] = super.__ValTuple3(e)
        final override implicit def __ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](e : (T1, T2, T3, T4)) : __ValTuple4[T1, T2, T3, T4] = super.__ValTuple4(e)
        final override implicit def __ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](e : (T1, T2, T3, T4, T5)) : __ValTuple5[T1, T2, T3, T4, T5] = super.__ValTuple5(e)
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // SameBitsVector for repeated zeros or ones
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected[DFiant] sealed class SameBitsVector(val value : Boolean)
  object SameBitsVector {
    trait Builder[SBV, W] {
      def apply(bits : Type[W], sbv : SBV) : DFAny.Of[Type[W]]
    }
    object Builder {
      implicit def fromValueOf[SBV, W](
        implicit sbvBld : Builder[SBV, W]
      ) : Builder[ValueOf[SBV], W] = (bits : Type[W], sbv : ValueOf[SBV]) => sbvBld(bits, sbv.value)
      implicit def ev[SBV <: SameBitsVector, W](implicit ctx : DFAny.Context)
      : Builder[SBV, W] = (bits, sbv) => DFAny.Const[Type[W]](Type(bits.width), Token(BitVector.fill(bits.width.getValue.toLong)(sbv.value)))
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign & Connect
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op:=,<>` {
    sealed trait Frontend {
      protected implicit def __DFBits_ac_DFBits[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : DFAny.`Op:=,<>`.Builder[Type[LW], DFBits[RW]] = (left, right) => trydf {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        right.asValOf[Type[LW]]
      }
      protected implicit def __DFBits_ac_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt[RW]] = (left, right) => trydf {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        right.bits.asValOf[Type[LW]]
      }

      protected implicit def __DFBits_ac_DFUIntToken[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
      ) : DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt.TokenW[RW]] = (left, right) => {
        checkLWvRW.unsafeCheck(left.width, right.width)
        import DFDesign.Frontend._
        DFAny.Const.forced(right.bits).asValOf[Type[LW]]
      }
    }
    object Frontend {
      trait Inherited extends Frontend {
        final override protected implicit def __DFBits_ac_DFBits[LW, RW](implicit ctx : DFAny.Context, checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : DFAny.`Op:=,<>`.Builder[Type[LW], DFBits[RW]] = super.__DFBits_ac_DFBits
        final override protected implicit def __DFBits_ac_DFUInt[LW, RW](implicit ctx : DFAny.Context, checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt[RW]] = super.__DFBits_ac_DFUInt
        final override protected implicit def __DFBits_ac_DFUIntToken[LW, RW](implicit ctx: DFAny.Context, checkLWvRW: internals.`LW == RW`.CheckedShell[LW, RW]): DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt.TokenW[RW]] = super.__DFBits_ac_DFUIntToken
      }
      trait Imported extends Frontend {
        final override implicit def __DFBits_ac_DFBits[LW, RW](implicit ctx : DFAny.Context, checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : DFAny.`Op:=,<>`.Builder[Type[LW], DFBits[RW]] = super.__DFBits_ac_DFBits
        final override implicit def __DFBits_ac_DFUInt[LW, RW](implicit ctx : DFAny.Context, checkLWvRW : internals.`LW == RW`.CheckedShell[LW, RW]) : DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt[RW]] = super.__DFBits_ac_DFUInt
        final override implicit def __DFBits_ac_DFUIntToken[LW, RW](implicit ctx: DFAny.Context, checkLWvRW: internals.`LW == RW`.CheckedShell[LW, RW]): DFAny.`Op:=,<>`.Builder[Type[LW], DFUInt.TokenW[RW]] = super.__DFBits_ac_DFUIntToken
      }
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Logic operations
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  protected abstract class OpsLogic[Op <: Func2.Op](op : Op)(tokenOp : (Token, Token) => Token) {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Logic Ops with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]

    object Builder {
      type Aux[-L, -R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW](
          implicit
          ctx : DFAny.Context,
          checkLWvRW : `LW == RW`.CheckedShell[LW, RW]
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[LW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[LW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[LW]
                def apply(leftL : L, rightR : R) : Out = trydf {
                  val (left, right) = properLR(leftL, rightR)
                  // Completing runtime checks
                  checkLWvRW.unsafeCheck(left.width, right.width)
                  // Constructing op
                  DFAny.Func2[Type[LW], DFBits[LW], Op, DFBits[RW]](Type[LW](left.width), left, op, right)(tokenOp)
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsg[false, "An unconstrained-width literal cannot be used in a logic operation"]

      implicit def evDFBits_op_SBV[LW, SBV](
        implicit
        sbv : SameBitsVector.Builder[SBV, LW],
        error : UnconstrainedLiteralError
      ) : Aux[DFBits[LW], SBV, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW, SBV](
        implicit
        sbv : SameBitsVector.Builder[SBV, RW],
        error : UnconstrainedLiteralError
      ) : Aux[SBV, DFBits[RW], DFBits[RW]] = ???
    }
  }
  object `Op|` extends OpsLogic(Func2.Op.|)(_ | _)
  object `Op&` extends OpsLogic(Func2.Op.&)(_ & _)
  object `Op^` extends OpsLogic(Func2.Op.^)(_ ^ _)
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
        type Msg[LW, RW] = "The shift vector is too large.\nFound: LHS-width = "+ ToString[LW] + " and RHS-width = " + ToString[RW]
        type ParamFace = Int
      }
      def create[LW, RW](left : DFBits[LW], right : DFUInt[RW])(
        implicit
        ctx : DFAny.Context,
//        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : DFBits[LW] = {
//        checkLWvRW.unsafeCheck(left.width, right.width)

        val out = left.dfType
        val func : (left.TToken, right.TToken) => out.TToken = op match {
          case _ : Func2.Op.>> => _ >> _
          case _ : Func2.Op.<< => _ << _
        }
        DFAny.Func2(out, left, op, right)(func)
      }
      implicit def evDFSInt_op_DFUInt[LW, RW](
        implicit
        ctx : DFAny.Context,
        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], DFUInt[RW]] = (left, right) => create(left, right)

      implicit def evDFSInt_op_Const[LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : DFAny.Const.AsIs.Aux[DFUInt.Type[LW], R, _ <: DFUInt.Type[RW]],
        checkLWvRW : SmallShift.CheckedShell[LW, RW]
      ) : Builder[DFBits[LW], R] = (left, rightR) =>
        create(left, rConst(DFUInt.Type[LW](left.width), rightR).asValOf[DFUInt.Type[RW]])
    }
  }
  object `Op<<` extends OpsShift(Func2.Op.<<)
  object `Op>>` extends OpsShift(Func2.Op.>>)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Concatenation operation
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object `Op++` {
    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]

    def forced[LW, RW](left : DFBits[LW], right : DFBits[RW])(implicit ctx : DFAny.Context) : DFBits[Int] =
      DFAny.Func2(Type(left.width.getValue + right.width.getValue), left, DFAny.Func2.Op.++, right)(_ ++ _)
    object Builder {
      type Aux[L, R, Comp0] = Builder[L, R] {
        type Out = Comp0
      }

      object Inference {
        type CalcW[LW, RW] = LW + RW
        type OW[LW, RW, ResW] = TwoFace.Int.Shell2Aux[CalcW, LW, Int, RW, Int, ResW]
      }

      trait DetailedBuilder[L, LW, R, RW] {
        type Out
        def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out]
      }
      object DetailedBuilder {
        implicit def ev[L, LW, R, RW, OW](
          implicit
          ctx : DFAny.Context,
          oW : Inference.OW[LW, RW, OW],
        ) : DetailedBuilder[L, LW, R, RW]{type Out = DFBits[OW]} =
          new DetailedBuilder[L, LW, R, RW]{
            type Out = DFBits[OW]
            def apply(properLR : (L, R) => (DFBits[LW], DFBits[RW])) : Builder.Aux[L, R, Out] =
              new Builder[L, R] {
                type Out = DFBits[OW]
                def apply(leftL : L, rightR : R) : Out = {
                  val (left, right) = properLR(leftL, rightR)
                  // Constructing op
                  val oWidth = oW(left.width, right.width)
                  val out = DFAny.Func2(Type(oWidth), left, DFAny.Func2.Op.++, right)(_ ++ _)
                  out
                }
              }
          }
      }

      implicit def evDFBits_op_DFBits[L <: DFBits[LW], LW, R <: DFBits[RW], RW](
        implicit
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, DFBits[RW], RW]
      ) = detailedBuilder((left, right) => (left, right))

      implicit def evDFBits_op_Const[L <: DFBits[LW], LW, R, RW](
        implicit
        ctx : DFAny.Context,
        rConst : Const.Builder.Aux[R, RW],
        detailedBuilder: DetailedBuilder[DFBits[LW], LW, R, RW]
      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))

      implicit def evConst_op_DFBits[L, LW, LE, R <: DFBits[RW], RW](
        implicit
        ctx : DFAny.Context,
        lConst : Const.Builder.Aux[L, LW],
        detailedBuilder: DetailedBuilder[L, LW, DFBits[RW], RW]
      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))

      type UnconstrainedLiteralError =
        RequireMsgSym[false, "An unconstrained-width literal cannot be used in a concatenation operation", Builder[_,_]]

      implicit def evDFBits_op_SBV[LW, SBV](
        implicit
        sbv : SameBitsVector.Builder[SBV, LW],
        error : UnconstrainedLiteralError
      ) : Aux[DFBits[LW], SBV, DFBits[LW]] = ???
      implicit def evSBV_op_DFBits[RW, SBV](
        implicit
        sbv : SameBitsVector.Builder[SBV, RW],
        error : UnconstrainedLiteralError
      ) : Aux[SBV, DFBits[RW], DFBits[RW]] = ???
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}