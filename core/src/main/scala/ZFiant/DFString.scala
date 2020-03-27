//package ZFiant
//import java.nio.charset.StandardCharsets
//
//import singleton.ops._
//import singleton.twoface._
//import DFiant.internals._
//import ZFiant.compiler.printer.Printer
//object DFString extends DFAny.Companion {
//  final case class Type[L](length : TwoFace.Int[L]) extends DFAny.Type {
//    type Length = L
//    type Width = Length * 8
//    type TToken = Token[Length]
//    type TPattern = DFString.Pattern
//    type TPatternAble[+R] = DFString.Pattern.Able[R]
//    type TPatternBuilder[LType <: DFAny.Type] = DFString.Pattern.Builder[LType]
////    type OpAble[R] = DFString.Op.Able[R]
////    type `Op==Builder`[L0, R] = DFString.`Op==`.Builder[L0, R]
////    type `Op!=Builder`[L0, R] = DFString.`Op!=`.Builder[L0, R]
////    type `Op<>Builder`[LType <: DFAny.Type, R] = DFString.`Op<>`.Builder[LType, R]
////    type `Op:=Builder`[LType <: DFAny.Type, R] = DFString.`Op:=`.Builder[LType, R]
////    type InitAble[L0 <: DFAny] = DFString.Init.Able[L0]
////    type InitBuilder[L0 <: DFAny] = DFString.Init.Builder[L0, TToken]
//    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](length * 8)
//    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
//    def getTokenFromBits(fromToken : DFBits.Token[_]) : DFAny.Token = {
//      assert(fromToken.width.getValue == width.getValue)
//      Token(fromToken.value.toByteArray.toVector, fromToken.isBubble)
//    }
//
//    //    override def toString: String = if (logical) "DFString" else "DFBit"
//    def codeString(implicit printConfig : Printer.Config) : String = {
//      import printConfig._
//      s"$TP DFString($LIT$length)"
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Public Constructors
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  def apply[L](checkedLength : Positive.Checked[L])(implicit ctx : DFAny.Context) = DFAny.NewVar(Type(checkedLength))
//  def apply[L](
//    implicit ctx : DFAny.Context, checkedLength : Positive.Checked[L], di: DummyImplicit
//  ) = DFAny.NewVar(Type(checkedLength))
//  def unapply(arg: DFAny): Option[TwoFace.Int[Int]] = arg.dfType match {
//    case Type(length) => Some(length)
//    case _ => None
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Token
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  final case class Token[L](value : Vector[Byte], bubble : Boolean) extends DFAny.Token.Of[Vector[Byte], L * 8] {
//    val width: TwoFace.Int[L * 8] = TwoFace.Int.create[L * 8](value.length * 8)
//    lazy val valueBits : XBitVector[L * 8] = value.map(b => b.toInt.toBitVector(8) : BitVector).reduce(_ ++ _).asInstanceOf[XBitVector[L * 8]]
//    lazy val bubbleMask: XBitVector[L * 8] = bubble.toBitVector(width)
//    def +  [RL](that : Token[RL]) : Token[L + RL] = Token[L + RL](this.value ++ that.value, this.isBubble || that.isBubble)
//    def == [RL](that : Token[RL]) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
//    def != [RL](that : Token[RL]) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
//
//    def codeString(implicit printConfig : Printer.Config) : String = {
//      import printConfig._
//      val valueStr = new String(value.toArray, StandardCharsets.ISO_8859_1)
//      s""""$valueStr""""
//    }
//  }
//
//  object Token {
//    implicit def bubbleOfToken[L] : DFAny.Token.BubbleOfToken[Token[L]] = ??? // t => Token[L](t.length, Bubble)
//    implicit def bubbleOfDFType[L] : DFAny.Token.BubbleOfDFType[Type[L]] =  ??? //t => Token[L](t.length, Bubble)
//    def apply[L](length : L, value : String) : Token[L] = Token[L](value.getBytes(StandardCharsets.ISO_8859_1).toVector, bubble = false)
//    def apply(value : String) : Token[Int] = Token[Int](value.getBytes(StandardCharsets.ISO_8859_1).toVector, bubble = false)
//    def apply(value : Vector[Byte]) : Token[Int] = Token[Int](value, bubble = false)
////    def fromValue(logical : Boolean, value : Boolean) : Token = new Token(logical, value, false)
////    def apply(logical : Boolean, value : Bubble) : Token = new Token(logical, false, true)
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Match Pattern
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  class Pattern(set : Set[Vector[Byte]]) extends DFAny.Pattern.OfSet[Vector[Byte], Pattern](set)
//  object Pattern extends PatternCO {
//    trait Able[+R] extends DFAny.Pattern.Able[R] {
//      val byteVector : Vector[Byte]
//    }
//    object Able {
//      implicit class DFStringPatternByteVector[R <: Vector[Byte]](val right : R) extends Able[R] {
//        val byteVector : Vector[Byte] = right
//      }
//      implicit class DFStringPatternString[R <: String](val right : R) extends Able[R] {
//        val byteVector : Vector[Byte] = right.getBytes(StandardCharsets.ISO_8859_1).toVector
//      }
//    }
//    trait Builder[LType <: DFAny.Type] extends DFAny.Pattern.Builder[LType, Able]
//    object Builder {
//      implicit def ev[LL] : Builder[Type[LL]] = new Builder[Type[LL]] {
//        def apply[R](left: Type[LL], right: Seq[Able[R]]): Pattern = {
//          val patternSet = right.map(e => e.byteVector).foldLeft(Set.empty[Vector[Byte]])((set, byteVector) => {
//            if (set.contains(byteVector)) throw new IllegalArgumentException(s"\nThe bitvector $byteVector already intersects with $set")
//            if (byteVector.length > left.width) throw new IllegalArgumentException(s"\nThe bitvector $byteVector is wider than ${left.width}")
//            set + byteVector
//          })
//
//          new Pattern(patternSet)
//        }
//      }
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Init
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  object Init extends InitCO {
//    trait Able[L <: DFAny] extends DFAny.Init.Able[L]
//    object Able {
//      implicit class DFStringBubble[LL](val right : Bubble) extends Able[DFString[LL]]
//      implicit class DFStringToken[LL](val right : Token[LL]) extends Able[DFString[LL]]
//      implicit class DFStringTokenSeq[LL](val right : Seq[Token[LL]]) extends Able[DFString[LL]]
//      implicit class DFStringBitVector[LL](val right : BitVector) extends Able[DFString[LL]]
//      implicit class DFStringSeqOfBitVector[LL](val right : Seq[BitVector]) extends Able[DFString[LL]]
//      implicit class DFStringXBitVector[LL](val right : XBitVector[LL]) extends Able[DFString[LL]]
//
//      def toTokenSeq[LL](length : TwoFace.Int[LL], right : Seq[Able[DFString[LL]]]) : Seq[Token[LL]] =
//        right.toSeqAny.collect {
//          case t : Bubble => Token[LL](Vector.fill(length)(0.toByte), bubble = true)
//          case t : Token[_] =>
//            assert(t.value.length == length.getValue)
//            t.asInstanceOf[Token[LL]]
//          case t : Vector[Byte] => Token[LL](t, bubble = false)
//          case t : String => Token(length, t)
//        }
//    }
//    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
//    object Builder {
//      implicit def ev[LL] : Builder[DFString[LL], Token[LL]] = (left, right) => Able.toTokenSeq(left.dfType.length, right)
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Constant Builder
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  type Const[L] = DFAny.Const.Of[Type[L]]
//  object Const {
//    trait Builder[N] {
//      type L
//      def apply(value : N) : Const[L]
//    }
//    object Builder {
//      type Aux[N, L0] = Builder[N]{type L = L0}
//      implicit def fromByteVector(implicit ctx : DFAny.Context)
//      : Aux[Vector[Byte], Int] = new Builder[Vector[Byte]] {
//        type L = Int
//        def apply(value : Vector[Byte]) : Const[L] = {
//          DFAny.Const[Type[Int]](Type(value.length), Token(value))
//        }
//      }
//      implicit def fromString(implicit ctx : DFAny.Context)
//      : Aux[String, Int] = new Builder[String] {
//        type L = Int
//        def apply(value : String) : Const[L] = {
//          DFAny.Const[Type[Int]](Type(value.length), Token(value))
//        }
//      }
//      implicit def fromXString[S <: XString](implicit ctx : DFAny.Context, length : SafeInt[Length[S]])
//      : Aux[S, length.Out] = new Builder[S] {
//        type L = length.Out
//        def apply(value : S) : Const[L] = {
//          DFAny.Const[Type[L]](Type(length.value), Token[L](length.value, value))
//        }
//      }
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//}
