//package DFiant
//import java.nio.charset.StandardCharsets
//
//import singleton.ops._
//import singleton.twoface._
//import DFiant.internals._
//import DFiant.DFAny.Func2
//import DFiant.csprinter.{CSPrinter, CodeStringOf}
//
//object DFString extends DFAny.Companion {
//  final case class Type[L](length : TwoFace.Int[L]) extends DFAny.Type {
//    type Length = L
//    type Width = Length * 8
//    type TToken = Token
//    type TPattern = DFString.Pattern
//    type TPatternAble[+R] = DFString.Pattern.Able[R]
//    type TPatternBuilder[LType <: DFAny.Type] = DFString.Pattern.Builder[LType]
//    type `Op==Builder`[L0, R] = DFString.`Op==`.Builder[L0, R]
//    type `Op!=Builder`[L0, R] = DFString.`Op!=`.Builder[L0, R]
//    type InitAble[L0 <: DFAny] = DFString.Init.Able[L0]
//    type InitBuilder[L0 <: DFAny] = DFString.Init.Builder[L0, TToken]
//    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](length * 8)
//    def getBubbleToken: TToken = Token.bubbleOfDFType(this)
//    def getTokenFromBits(fromToken : DFBits.Token) : DFAny.Token = {
//      assert(fromToken.value.length == length.getValue)
//      Token(fromToken.value.toByteArray.toVector, fromToken.isBubble)
//    }
//    def assignCheck(from : DFAny)(implicit ctx : DFAny.Context) : Unit = from match {
//      case r @ DFString(_) =>
//        val op = implicitly[DFAny.`Op:=,<>`.Builder[Type[L], DFString[Int]]]
//        op(this, r.asInstanceOf[DFString[Int]])
//    }
//    def codeString(implicit printer: CSPrinter) : String = {
//      import printer.config._
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
//  def unapply(arg: DFAny.Member): Option[TwoFace.Int[Int]] = arg.dfType match {
//    case Type(length) => Some(length)
//    case _ => None
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Token
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  final case class Token(value : Vector[Byte], bubble : Boolean) extends DFAny.Token.Of[Vector[Byte]] {
//    val width : Int = value.length * 8
//    lazy val valueBits : BitVector = value.map(b => b.toInt.toBitVector(8) : BitVector).reduce(_ ++ _)
//    lazy val bubbleMask: BitVector = bubble.toBitVector(width)
//    def ++ (that : Token) : Token = Token(this.value ++ that.value, this.isBubble || that.isBubble)
//    def == (that : Token) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
//    def != (that : Token) : DFBool.Token = DFBool.Token(logical = true, this.value == that.value, this.isBubble || that.isBubble)
//
//    def codeString(implicit printer: CSPrinter) : String = {
//      import printer.config._
//      val valueStr = new String(value.toArray, StandardCharsets.ISO_8859_1)
//      s""""$valueStr""""
//    }
//  }
//
//  object Token {
//    implicit val bubbleOfToken : DFAny.Token.BubbleOfToken[Token] = t => Token(t.value.length, Bubble)
//    implicit def bubbleOfDFType[L] : DFAny.Token.BubbleOfDFType[Type[L]] =  t => Token(t.length, Bubble)
//    def apply(length : Int, bubble : Bubble) : Token = Token(Vector.fill(length)(0.toByte), bubble = true)
//    def apply(value : String) : Token = Token(value.getBytes(StandardCharsets.ISO_8859_1).toVector, bubble = false)
//    def apply(value : Vector[Byte]) : Token = Token(value, bubble = false)
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Match Pattern
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  implicit val codeStringOf: CodeStringOf[Vector[Byte]] = new CodeStringOf[Vector[Byte]] {
//    override def apply(t : Vector[Byte])(implicit printer: CSPrinter) : String = new String(t.toArray, StandardCharsets.ISO_8859_1)
//  }
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
//            if (set.contains(byteVector)) throw new IllegalArgumentException(s"\nThe string $byteVector already intersects with $set")
//            if (byteVector.length > left.length) throw new IllegalArgumentException(s"\nThe string $byteVector is longer than ${left.length}")
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
//      implicit class DFStringToken[LL](val right : Token) extends Able[DFString[LL]]
//      implicit class DFStringTokenSeq[LL](val right : Seq[Token]) extends Able[DFString[LL]]
//      implicit class DFStringByteVector[LL](val right : Vector[Byte]) extends Able[DFString[LL]]
//      implicit class DFStringSeqOfByteVector[LL](val right : Seq[Vector[Byte]]) extends Able[DFString[LL]]
//      implicit class DFStringXBitVector[LL](val right : XBitVector[LL]) extends Able[DFString[LL]]
//
//      def toTokenSeq[LL](length : Int, right : Seq[Able[DFString[LL]]]) : Seq[Token] =
//        right.toSeqAny.collect {
//          case t : Bubble => Token(length, Bubble)
//          case t : Token => assert(t.value.length == length); t
//          case t : Vector[_] => assert(t.length == length); Token(t.asInstanceOf[Vector[Byte]])
//          case t : String => assert(t.length == length); Token(t)
//        }
//    }
//    trait Builder[L <: DFAny, Token <: DFAny.Token] extends DFAny.Init.Builder[L, Able, Token]
//    object Builder {
//      implicit def ev[LL] : Builder[DFString[LL], Token] = (left, right) => Able.toTokenSeq(left.dfType.length, right)
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Constant Builder
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  type Const[L] = DFAny.Of[Type[L]]
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
//          DFAny.Const[Type[L]](Type(length.value), Token(value))
//        }
//      }
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Op
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  object Op {
//    class Able[L](val value : L) extends DFAny.Op.Able[L]
//    class AbleOps[L](value : L) extends Able[L](value) {
//      final val left = value
//      final def === [RL](right : DFString[RL])(implicit op: `Op===`.Builder[L, DFString[RL]]) = op(left, right)
//      final def =!= [RL](right : DFString[RL])(implicit op: `Op=!=`.Builder[L, DFString[RL]]) = op(left, right)
//      final def ++  [RL](right : DFString[RL])(implicit op: `Op++`.Builder[L, DFString[RL]]) = op(left, right)
//    }
//    trait Implicits {
//      sealed class __DFStringFromByteVector(left : Vector[Byte]) extends AbleOps[Vector[Byte]](left)
//      final implicit def __DFStringFromByteVector(left: Vector[Byte]): __DFStringFromByteVector = new __DFStringFromByteVector(left)
//      sealed class __DFStringFromString(left : String) extends AbleOps[String](left)
//      final implicit def __DFStringFromString(left: String): __DFStringFromString = new __DFStringFromString(left)
//      sealed class __DFStringFromXString[S <: XString](left : S) extends AbleOps[S](left)
//      final implicit def __DFStringFromXString[S <: XString](left: S): __DFStringFromXString[S] = new __DFStringFromXString[S](left)
//      sealed class __DFStringFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) extends AbleOps[DFString[W]](left)
//      final implicit def __DFStringFromDefaultRet[W](left : DFAny.DefaultRet[Type[W]])(implicit ctx : DFAny.Context) : __DFStringFromDefaultRet[W] = new __DFStringFromDefaultRet(left)
//      final implicit def __ofDFString[W](left : DFString[W]) : Able[DFString[W]] = new Able(left)
//      final implicit class __DFStringOps[LL](val left : DFString[LL]){
//        def === [R](right : Precise[R])(implicit op: `Op===`.Builder[DFString[LL], R]) = op(left, right)
//        def =!= [R](right : Precise[R])(implicit op: `Op=!=`.Builder[DFString[LL], R]) = op(left, right)
//        def ++  [R](right : Precise[R])(implicit op: `Op++`.Builder[DFString[LL], R]) = op(left, right)
//      }
////      final implicit class DFStringAliases[LL, Mod <: DFAny.Modifier](val left : DFAny.Value[Type[LL], Mod]) {
////        def apply[H, L](relBitHigh : BitIndex.Checked[H, left.dfType.Length], relBitLow : BitIndex.Checked[L, left.dfType.Width])(
////          implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Context
////        ) : DFAny.Value[DFString.Type[relWidth.Out], Mod] =
////          left.bits(relBitHigh, relBitLow).overrideCodeString(rs => s"$rs($relBitHigh, $relBitLow)")
////        def apply[I](relBit: BitIndex.Checked[I, left.Width])(
////          implicit ctx : DFAny.Context
////        ) : DFAny.Value[DFBool.Type, Mod] =
////          left.bit(relBit).overrideCodeString(rs => s"$rs($relBit)")
////      }
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Assign & Connect
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  object `Op:=,<>` {
//    import DFAny.`Op:=,<>`.Builder {
//      type Out = DFAny.Of[LType]
//    }
//
//    object Builder {
//      object `LL == RL` extends Checked1Param.Int {
//        type Cond[LL, RL] = LL == RL
//        type Msg[LL, RL] = "An assignment/connection operation does not permit different length. Found: LHS-length = "+ ToString[LL] + " and RHS-length = " + ToString[RL]
//        type ParamFace = Int
//      }
//
//      implicit def evDFString_op_DFString[LL, RL](
//        implicit
//        ctx : DFAny.Context,
//        checkLWvRW : `LL == RL`.CheckedShell[LL, RL]
//      ) : Builder[Type[LL], DFString[RL]] = (left, right) => {
//        checkLWvRW.unsafeCheck(left.length, right.dfType.length)
//        right.asInstanceOf[DFAny.Of[Type[LL]]]
//      }
//
//      implicit def evDFString_op_Const[LL, R, RL](
//        implicit
//        ctx : DFAny.Context,
//        rConst : Const.Builder.Aux[R, RL],
//        checkLWvRW : `LL == RL`.CheckedShellSym[Builder[_,_], LL, RL]
//      ) : Builder[Type[LL], R] = (left, rightNum) => {
//        val right = rConst(rightNum)
//        checkLWvRW.unsafeCheck(left.length, right.dfType.length)
//        right.asInstanceOf[DFAny.Of[Type[LL]]]
//      }
//    }
//  }
////  object `Op<>` extends `Ops:=,<>`
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Comparison operations
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  protected abstract class OpsCompare[Op <: Func2.Op](op : Op)(func : (Token, Token) => DFBool.Token) {
//    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support Comparison Ops with the type ${R}")
//    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]{type Out = DFBool}
//    object Builder {
//      object `LL == RL` extends Checked1Param.Int {
//        type Cond[LL, RL] = LL == RL
//        type Msg[LL, RL] = "Comparison operations do not permit different length DF variables. Found: LHS-length = "+ ToString[LL] + " and RHS-length = " + ToString[RL]
//        type ParamFace = Int
//      }
//
//      def create[L, LL, R, RL](properLR : (L, R) => (DFString[LL], DFString[RL]))(
//        implicit ctx : DFAny.Context
//      ) : Builder[L, R] = (leftL, rightR) => {
//        val (left, right) = properLR(leftL, rightR)
//        DFAny.Func2(DFBool.Type(logical = true), left, op, right)(func)
//      }
//
//      implicit def evDFString_op_DFString[LL, RL](
//        implicit
//        ctx : DFAny.Context,
//        checkLWvRW : `LL == RL`.CheckedShell[LL, RL]
//      ) : Builder[DFString[LL], DFString[RL]] =
//        create[DFString[LL], LL, DFString[RL], RL]((left, right) => {
//          checkLWvRW.unsafeCheck(left.dfType.length, right.dfType.length)
//          (left, right)
//        })
//
//      implicit def evDFString_op_Const[LL, R, RL](
//        implicit
//        ctx : DFAny.Context,
//        rConst : Const.Builder.Aux[R, RL],
//        checkLWvRW : `LL == RL`.CheckedShell[LL, RL]
//      ) : Builder[DFString[LL], R] = create[DFString[LL], LL, R, RL]((left, rightNum) => {
//        val right = rConst(rightNum)
//        checkLWvRW.unsafeCheck(left.dfType.length, right.dfType.length)
//        (left, right)
//      })
//
//      implicit def evConst_op_DFString[L, LL, RL](
//        implicit
//        ctx : DFAny.Context,
//        lConst : Const.Builder.Aux[L, LL],
//        checkLWvRW : `LL == RL`.CheckedShell[LL, RL]
//      ) : Builder[L, DFString[RL]] = create[L, LL, DFString[RL], RL]((leftNum, right) => {
//        val left = lConst(leftNum)
//        checkLWvRW.unsafeCheck(left.dfType.length, right.dfType.length)
//        (left, right)
//      })
//    }
//  }
//  object `Op==` extends OpsCompare(Func2.Op.==)((l, r) => l == r) with `Op==`
//  object `Op!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r) with `Op!=`
//  object `Op===` extends OpsCompare(Func2.Op.==)((l, r) => l == r)
//  object `Op=!=` extends OpsCompare(Func2.Op.!=)((l, r) => l != r)
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  // Concatenation operation
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  object `Op++` {
//    @scala.annotation.implicitNotFound("Dataflow variable ${L} does not support a Concatenation Op with the type ${R}")
//    trait Builder[-L, -R] extends DFAny.Op.Builder[L, R]
//
//    def forced[LL, RL](left : DFString[LL], right : DFString[RL])(implicit ctx : DFAny.Context) : DFString[Int] =
//      DFAny.Func2(Type(left.dfType.length + right.dfType.length), left, DFAny.Func2.Op.++, right)(_ ++ _).asInstanceOf[DFString[Int]]
//    object Builder {
//      type Aux[L, R, Comp0] = Builder[L, R] {
//        type Out = Comp0
//      }
//
//      object Inference {
//        type CalcL[LL, RL] = LL + RL
//        type OL[LL, RL, ResL] = TwoFace.Int.Shell2Aux[CalcL, LL, Int, RL, Int, ResL]
//      }
//
//      trait DetailedBuilder[L, LL, R, RL] {
//        type Out
//        def apply(properLR : (L, R) => (DFString[LL], DFString[RL])) : Builder.Aux[L, R, Out]
//      }
//      object DetailedBuilder {
//        implicit def ev[L, LL, R, RL, OL](
//          implicit
//          ctx : DFAny.Context,
//          oL : Inference.OL[LL, RL, OL],
//        ) : DetailedBuilder[L, LL, R, RL]{type Out = DFString[OL]} =
//          new DetailedBuilder[L, LL, R, RL]{
//            type Out = DFString[OL]
//            def apply(properLR : (L, R) => (DFString[LL], DFString[RL])) : Builder.Aux[L, R, Out] =
//              new Builder[L, R] {
//                type Out = DFString[OL]
//                def apply(leftL : L, rightR : R) : Out = {
//                  val (left, right) = properLR(leftL, rightR)
//                  // Constructing op
//                  val oLength = oL(left.dfType.length, right.dfType.length)
//                  val out = DFAny.Func2(Type(oLength), left, DFAny.Func2.Op.++, right)(_ ++ _)
//                  out
//                }
//              }
//          }
//      }
//
//      implicit def evDFBits_op_DFBits[L <: DFString[LL], LL, R <: DFString[RL], RL](
//        implicit
//        detailedBuilder: DetailedBuilder[DFString[LL], LL, DFString[RL], RL]
//      ) = detailedBuilder((left, right) => (left, right))
//
//      implicit def evDFBits_op_Const[L <: DFString[LL], LL, R, RL](
//        implicit
//        ctx : DFAny.Context,
//        rConst : Const.Builder.Aux[R, RL],
//        detailedBuilder: DetailedBuilder[DFString[LL], LL, R, RL]
//      ) = detailedBuilder((left, rightNum) => (left, rConst(rightNum)))
//
//      implicit def evConst_op_DFBits[L, LL, LE, R <: DFString[RL], RL](
//        implicit
//        ctx : DFAny.Context,
//        lConst : Const.Builder.Aux[L, LL],
//        detailedBuilder: DetailedBuilder[L, LL, DFString[RL], RL]
//      ) = detailedBuilder((leftNum, right) => (lConst(leftNum), right))
//    }
//  }
//  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//}
