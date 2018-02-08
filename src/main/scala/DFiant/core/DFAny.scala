package DFiant.core

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import scodec.bits._

sealed trait DFAny {
  type IN = TVal
  type OUT = DFPortOut[TVar]
  type TVal <: DFAny
  type TVar <: TVal with DFAny.Var[TVal, TVar]
  type TAlias <: TVal
  type TBool <: DFBool
  type TBits[W2] <: DFBits[W2]
  type TCompanion <: DFAny.Companion
  //  type TToken = protComp.Token //Good-code red in intellij, so using type projection instead
  type TToken = TCompanion#Token //
  type TAble[R] = TCompanion#Able[R]
  type TUnbounded = TCompanion#Unbounded
//  type TUInt <: DFUInt
  type Width
  val width : TwoFace.Int[Width]
  protected val protComp : TCompanion

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  protected final def protBit[I](relBit : TwoFace.Int[I])(implicit dsn : DFDesign) : TBool =
    DFBool.alias(this, relBit).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit dsn : DFDesign) : TBool = protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit dsn : DFDesign, relBit : BitIndex.Checked[I, Width], di : DummyImplicit) : TBool = protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits()(implicit dsn : DFDesign) : TBits[Width] = DFBits.alias(this, width, 0).asInstanceOf[TBits[Width]]

  protected final def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit dsn : DFDesign, relWidth : RelWidth.TF[H, L]
  ) : TBits[relWidth.Out] = DFBits.alias(this, relWidth(relBitHigh, relBitLow), relBitLow).asInstanceOf[TBits[relWidth.Out]]

  final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
    implicit dsn : DFDesign, checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L]
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def bits[H, L](implicit dsn : DFDesign, relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width],
    checkHiLow : BitsHiLo.Checked[H, L], relWidth : RelWidth.TF[H, L], di : DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Partial Bits at Position selection
  //////////////////////////////////////////////////////////////////////////
  protected final def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(implicit dsn : DFDesign)
  : TBits[W] = DFBits.alias(this, relWidth, relBitLow).asInstanceOf[TBits[W]]

  import singleton.ops.-
  final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
    implicit dsn : DFDesign, checkRelWidth : PartWidth.CheckedShell[W, Width - L]
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }

  final def bitsWL[W, L](implicit dsn : DFDesign, relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width],
    checkRelWidth : PartWidth.CheckedShell[W, Width - L], di : DummyImplicit
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Init (for use with Prev)
  //////////////////////////////////////////////////////////////////////////
  protected val protInit : Seq[TToken]
  final def getInit : Seq[TToken] = protInit
//  def init(updatedInit : Seq[TToken]) : TAlias
  def init(that : protComp.Init.Able[TVal]*)(implicit op : protComp.Init.Builder[TVal]) : TAlias =
    op(this.asInstanceOf[TVal], that).asInstanceOf[TAlias]
  final def reInit(cond : DFBool) : Unit = ???
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Prev
  //////////////////////////////////////////////////////////////////////////
  final def prev()(implicit op : protComp.Prev.Builder[TVal]) : TVal = prev(1)
  final def prev[P](step : Natural.Int.Checked[P])(implicit op : protComp.Prev.Builder[TVal]) : TVal =
    op(this.asInstanceOf[TVal], step)
  //////////////////////////////////////////////////////////////////////////

  final def next(step : Int = 1) : TVal = ???
//  final def getNextSeq(seqNum : Int, slidingWindow : Boolean = false) : Seq[TVal] = {
//    val seq = Seq.tabulate(seqNum)(_ => this.newEmptyDFVar.dontProduce())
//    if (slidingWindow)
//      seq.zipWithIndex.foreach{case (e, i) => e := this.next(i)}
//    else {
//      ifdf (this.tokensCounter(seqNum) == 0) { //TODO: think about tokenCnt limit here (maybe seqNum-1 ??)
//        seq.zipWithIndex.foreach{case (e, i) => e := this.next(i)}
//      }
//    }
//    seq
//  }

  final def consume() : TAlias = {
    ???
    this.asInstanceOf[TAlias]
  }
  final def dontConsume() : TAlias = {
    ???
    this.asInstanceOf[TAlias]
  }
  final def isNotEmpty : DFBool = ???
//  final def tokensCounter(supremLimit : Int) : DFUInt = TokensCounter(this, supremLimit)
//  def newEmptyDFVar : TVar
//  def newCopyDFVar : TVar = newEmptyDFVar := this.asInstanceOf[TVal]

  protected val protDesign : DFDesign
  final implicit protected lazy val protAlmanac : Almanac = protDesign.protAlmanac

  protected[DFiant] val almanacEntry : AlmanacEntry

  protected[DFiant] final def getCurrentEntry : AlmanacEntry = AlmanacEntryGetDFVar(almanacEntry)

  protected[DFiant] final def assign(that : DFAny) : TVar = {
    AlmanacEntryAssign(this.almanacEntry, that.getCurrentEntry)
    this.asInstanceOf[TVar]
  }

  def forceOut : Unit = getCurrentEntry
//  def ==[T](that: T)(implicit r : RequireMsg[false, "Use '===' instead of '=='"]) : DFBool = ???
//  final def == (that : TVal) : DFBool = ??? //DFBool.op(AlmanacEntryOpEq(this.getCurrentEntry, that.getCurrentEntry))
//  final def != (that : TVal) : DFBool = !(this == that)
  def simInject(that : BigInt) : Boolean = almanacEntry.simInject(that)
  def simWatch : BigInt = ???
//  override def toString: String = s"$dfTypeName($width).init${getInit.codeString}"
}



object DFAny {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Head Types
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded[T <: DFAny.Companion] extends DFAny {
    type TCompanion = T
  }

  trait Val[Val0 <: DFAny, Var0 <: Val0 with DFAny.Var[Val0, Var0]] extends DFAny {
    this : Val0 =>
    type TVal = Val0
    type TVar = Var0
  }

  trait Var[Val0 <: DFAny, Var0 <: Val0 with DFAny.Var[Val0, Var0]] extends DFAny.Val[Val0, Var0] {
    this : Val0 with Var0 =>
    type TAlias = TVar
    type TBool = DFBool.Var//DFBool#TVar
    type TBits[W2] = DFBits.Var[W2]//DFBits[W2]#TVar
    type AssignAble[R]

    //    type TUInt = DFUInt#TVar

    final def dontProduce() : TAlias = {
      ???
      this.asInstanceOf[TAlias]
    }
    final def isNotFull : DFBool = ???
    final def := (that : TVal) : TVar = assign(that)
    final def assignNext(step : Int, that : TVal) : Unit = ???
    final def assignNext(step : Int, that : BigInt) : Unit = ???
    final def <-- (that : Iterable[ TVal]) : TVar = {
      that.zipWithIndex.foreach{case (e, i) => this.assignNext(i, e)}
      this
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar[Comp <: Companion](_width : Int, _init : Seq[Token])(implicit dsn : DFDesign, cmp : Comp) extends DFAny {
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
    final protected val protDesign : DFDesign = dsn
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected val protInit : Seq[TToken] = _init.asInstanceOf[Seq[TToken]]
    def codeString(idRef : String) : String
    protected[DFiant] lazy val almanacEntry : AlmanacEntry = AlmanacEntryNewDFVar(width, protInit, codeString)
  }

  abstract class Alias[Comp <: Companion](aliasedVar : DFAny, relWidth : Int, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(implicit dsn : DFDesign, cmp : Comp)
    extends DFAny {
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](relWidth)
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken
    final protected val protDesign : DFDesign = dsn
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected val protInit : Seq[TToken] = {
      val initTemp : Seq[Token] = if (updatedInit.isEmpty) aliasedVar.getInit else updatedInit
      val prevInit = if (deltaStep < 0) initTemp.prevInit(-deltaStep) else initTemp //TODO: What happens for `next`?
      val bitsInit = prevInit.bitsWL(relWidth, relBitLow)
      bitsInit.map(protTokenBitsToTToken)
    }
    def codeString(idRef : String) : String
    protected[DFiant] lazy val almanacEntry : AlmanacEntry = {
      val timeRef = aliasedVar.almanacEntry.timeRef.stepBy(deltaStep)
      AlmanacEntryAliasDFVar(aliasedVar.almanacEntry, BitsRange(relBitLow + relWidth - 1, relBitLow), timeRef, protInit, codeString)
    }
  }

  abstract class Const[Comp <: Companion](token : Token)(implicit dsn : DFDesign, cmp : Comp) extends DFAny {
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](token.width)
    final protected val protDesign : DFDesign = dsn
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected val protInit : Seq[TToken] = Seq(token).asInstanceOf[Seq[TToken]]
    protected[DFiant] lazy val almanacEntry : AlmanacEntry = AlmanacEntryConst(token)
  }

  abstract class Op[Comp <: Companion](opWidth : Int, opString : String, opInit : Seq[Token], args : Seq[DFAny])(implicit dsn : DFDesign, cmp : Comp) extends DFAny {
    val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](opWidth)
    final protected val protDesign : DFDesign = dsn
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected val protInit : Seq[TToken] = opInit.asInstanceOf[Seq[TToken]]
    def codeString(idRef : String) : String = args.length match {
      case 1 =>
        if (opString.startsWith("unary_")) s"val $idRef = $opString${args(0).almanacEntry.refCodeString}"
        else s"val $idRef = ${args(0).almanacEntry.refCodeString}.$opString"
      case 2 => s"val $idRef = ${args(0).almanacEntry.refCodeString} $opString ${args(1).almanacEntry.refCodeString}"
      case _ => throw new IllegalArgumentException("Unsupported number of arguments")
    }
    def refCodeString(idRef : String) : String = idRef
    protected[DFiant] lazy val almanacEntry : AlmanacEntry =
      AlmanacEntryOp(width, opString, opInit, args.map(a => a.almanacEntry), codeString, refCodeString)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token {
    //maximum token value width
    val width : Int
    final lazy val widthOfValue : Int = scala.math.max(valueBits.lengthOfValue, bubbleMask.lengthOfValue).toInt
    val valueBits : BitVector
    val bubbleMask : BitVector
    //leading zero counter
    final lazy val lzc : Int = scala.math.min(valueBits.lzc, bubbleMask.lzc).toInt
    final def isBubble : Boolean = !(bubbleMask === BitVector.low(width))

    final def bit(relBit : Int) : DFBool.Token = {
      val outBitsValue = valueBits.bit(relBit)
      val outBubbleMask = bubbleMask.bit(relBit)
      new DFBool.Token(outBitsValue, outBubbleMask)
    }
    final def bits() : DFBits.Token = new DFBits.Token(width, valueBits, bubbleMask)
    final def bits(relBitHigh : Int, relBitLow : Int) : DFBits.Token = {
      val outWidth = relBitHigh - relBitLow + 1
      val outBitsValue = valueBits.bits(relBitHigh, relBitLow)
      val outBubbleMask = bubbleMask.bits(relBitHigh, relBitLow)
      new DFBits.Token(outWidth, outBitsValue, outBubbleMask)
    }
    final def bitsWL(relWidth : Int, relBitLow : Int) : DFBits.Token = bits(relWidth + relBitLow - 1, relBitLow)
    final def == (that : this.type) : DFBool.Token = {
      if (this.isBubble || that.isBubble) DFBool.Token(Bubble)
      else DFBool.Token(this.valueBits == that.valueBits)
    }
    final def != (that : this.type) : DFBool.Token = {
      if (this.isBubble || that.isBubble) DFBool.Token(Bubble)
      else DFBool.Token(this.valueBits != that.valueBits)
    }

    def bubbleString : String = "Φ"
    def valueString : String = valueBits.toShortString
    override def toString: String = if (isBubble) bubbleString else valueString

    def codeString : String = toString
  }

  object Token {
    implicit class TokenSeqInit[T <: DFAny.Token](tokenSeq : Seq[T]) {
      def prevInit(step : Int) : Seq[T] = {
        val length = tokenSeq.length
        //No init at all, so invoking prev does not change anything (bubble tokens will be used)
        if ((length == 0) || (step == 0)) tokenSeq
        //The step is larger or equals to the init sequence, so only the last init token remains
        else if (length <= step) Seq(tokenSeq.last)
        //More tokens are available than the step size, so we drop the first, according to the step count
        else tokenSeq.drop(step)
      }
      def bitsWL(relWidth : Int, relBitLow : Int) : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      def codeString : String = tokenSeq.mkString("(", ",", ")")
    }
  }

  object TokenSeq {
    def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
      leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
    def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
      seq.map(t => op(t))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Init
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Init {
    trait Able[L <: DFAny] {
      val right : Any

      override def toString: String = right.toString
    }
    object Able {
      implicit class AbleSeq[L <: DFAny](s : Seq[Able[L]]) {
        private def flatten(s: Seq[Any]): Seq[Any] = s flatMap {
          case ss: Seq[_] => flatten(ss)
          case e => Seq(e)
        }
        def toSeqAny : Seq[Any] = {
          flatten(s.map(e => e.right))
        }

        override def toString: String = s.toString()
      }
    }
    trait Builder[L <: DFAny, Able[L0 <: DFAny] <: Init.Able[L0]] {
      def apply(left : L, right : Seq[Able[L]]) : L
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Prev
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Prev {
    trait Builder[L <: DFAny] {
      def apply[P](left : L, right : Natural.Int.Checked[P]) : L
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Able
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Able[R](val value : R)
  object Able {
    implicit def fromAble[A[R0] <: Able[R0], R](able : A[R]) : R = able.value

  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Assign
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Assign {
    abstract class Able[R](val right : R)
    trait Builder[L <: DFAny, R] {
      def apply(left : L, rightR : R) : L
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    type Unbounded <: DFAny.Unbounded[this.type]
    type Token <: DFAny.Token
    type Able[R] <: DFAny.Able[R]
    trait Init {
      type Able[L <: DFAny] <: DFAny.Init.Able[L]
      type Builder[L <: DFAny] <: DFAny.Init.Builder[L, Able]
    }
    val Init : Init
    trait Prev {
      type Builder[L <: DFAny] <: DFAny.Prev.Builder[L]
    }
    val Prev : Prev
    trait Assign {
      type Builder[L <: DFAny, R] <: DFAny.Assign.Builder[L, R]
    }
    val Assign : Assign
    implicit val cmp = this
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}




