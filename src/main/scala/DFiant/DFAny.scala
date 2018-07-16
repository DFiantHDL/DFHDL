package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import scodec.bits._

import scala.collection.mutable.ListBuffer

sealed trait DFAny extends HasProperties with Nameable with TypeNameable with Discoverable {
  type TVal <: DFAny
  type TVar <: TVal with DFAny.Var
  type TAlias <: TVal
  type TBool <: DFBool
  type TBits[W2] <: DFBits[W2]
  type TUInt[W2] <: DFUInt[W2]
  type TCompanion <: DFAny.Companion
  //  type TToken = protComp.Token //Good-code red in intellij, so using type projection instead
  type TToken <: DFAny.Token
  type TUnbounded = TCompanion#Unbounded
//  type TUInt <: DFUInt
  type Width
  val width : TwoFace.Int[Width]
  protected val protComp : TCompanion
  import protComp._
  final protected[DFiant] val tVal = this.asInstanceOf[TVal]
  final protected[DFiant] val left = tVal

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final protected def protBit[I](relBit : TwoFace.Int[I])(implicit dsn : DFDesign, n : NameIt) : TBool =
    DFBool.alias(this, relBit).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit dsn : DFDesign, n : NameIt) : TBool =
    protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit relBit : BitIndex.Checked[I, Width], dsn : DFDesign, n : NameIt, di : DummyImplicit) : TBool =
    protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits()(implicit dsn : DFDesign, n : NameIt) : TBits[Width] = DFBits.alias(this, width, 0).asInstanceOf[TBits[Width]]

  final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit relWidth : RelWidth.TF[H, L], dsn : DFDesign, n : NameIt
  ) : TBits[relWidth.Out] = DFBits.alias(this, relWidth(relBitHigh, relBitLow), relBitLow).asInstanceOf[TBits[relWidth.Out]]

  final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], dsn : DFDesign, n : NameIt
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def bits[H, L](implicit relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width],
    checkHiLow : BitsHiLo.Checked[H, L], relWidth : RelWidth.TF[H, L], dsn : DFDesign, n : NameIt, di : DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

  final def bits[H <: Int, L <: Int](range : XRange.Int[L, H])(
    implicit relBitHigh : BitIndex.CheckedShell[H, Width], relBitLow : BitIndex.CheckedShell[L, Width],
    relWidth : RelWidth.TF[H, L], dsn : DFDesign, n : NameIt
  ) = {
    relBitHigh.unsafeCheck(range.end, width)
    relBitLow.unsafeCheck(range.start, width)
    protBits[H, L](TwoFace.Int.create[H](range.end), TwoFace.Int.create[L](range.start))
  }
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Partial Bits at Position selection
  //////////////////////////////////////////////////////////////////////////
  final protected def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(implicit dsn : DFDesign, n : NameIt)
  : TBits[W] = DFBits.alias(this, relWidth, relBitLow).asInstanceOf[TBits[W]]

  import singleton.ops.-
  final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkRelWidth : PartWidth.CheckedShell[W, Width - L], dsn : DFDesign, n : NameIt
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }

  final def bitsWL[W, L](implicit relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width],
    checkRelWidth : PartWidth.CheckedShell[W, Width - L], dsn : DFDesign, n : NameIt, di : DummyImplicit
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
  final def init(that : Init.Able[TVal]*)(implicit op : Init.Builder[TVal]) : TAlias =
    op(left, that).asInstanceOf[TAlias]
  final def reInit(cond : DFBool) : Unit = ???
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Prev
  //////////////////////////////////////////////////////////////////////////
  final def prev()(implicit op : Prev.Builder[TVal]) : TVal = prev(1)
  final def prev[P](step : Natural.Int.Checked[P])(implicit op : Prev.Builder[TVal]) : TVal =
    op(this.asInstanceOf[TVal], step)
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Future Stuff
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
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Naming
  //////////////////////////////////////////////////////////////////////////
  val isAnonymous : Boolean
  lazy val fullName : String = s"${dsn.fullName}.$name"
  override def toString : String = s"$fullName : $typeName"
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Equality
  //////////////////////////////////////////////////////////////////////////
  def == [R <: Unbounded](right : R)(implicit op: `Op==`.Builder[TVal, right.TVal]) : DFBool = op(left, right.tVal)
  def != [R <: Unbounded](right : R)(implicit op: `Op!=`.Builder[TVal, right.TVal]) : DFBool = op(left, right.tVal)
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Administration
  //////////////////////////////////////////////////////////////////////////
  implicit protected val dsn : DFDesign
  protected def discoveryDepenencies : List[Discoverable] = List()
  final implicit protected lazy val protAlmanac : Almanac = dsn.protAlmanac
  def keep : this.type = {
    dsn.keepList += this
    this
  }
  protected[DFiant] val almanacEntry : AlmanacEntry
  final protected[DFiant] def getCurrentEntry : AlmanacEntry = AlmanacEntryGetDFVar(almanacEntry)
  val isPort : Boolean
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Simulation
  //////////////////////////////////////////////////////////////////////////
  def simInject(that : BigInt) : Boolean = almanacEntry.simInject(that)
  def simWatch : BigInt = ???
  //////////////////////////////////////////////////////////////////////////

  def casedf(a: TVal)(block : => Unit)(implicit dsn : DFDesign) : DFCase[TVal] = {
//    def casedf_(block : => Unit) : Unit = {}
    ???
  }
}



object DFAny {
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Head Types
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded[T <: DFAny.Companion] extends DFAny {
    type TCompanion = T
  }

  trait Var extends DFAny {
    type TAlias = TVar
    type TBool = DFBool.Var//DFBool#TVar
    type TBits[W2] = DFBits.Var[W2]//DFBits[W2]#TVar
    type TUInt[W2] = DFUInt.Var[W2]//DFUInt[W2]#TVar

    //////////////////////////////////////////////////////////////////////////
    // Future Stuff
    //////////////////////////////////////////////////////////////////////////
    final def dontProduce() : TAlias = {
      ???
      this.asInstanceOf[TAlias]
    }
    final def isNotFull : DFBool = ???

    final def assignNext(step : Int, that : TVal) : Unit = ???
    final def assignNext(step : Int, that : BigInt) : Unit = ???
    final def <-- (that : Iterable[TVal]) : TVar = {
      that.zipWithIndex.foreach{case (e, i) => this.assignNext(i, e)}
      this.asInstanceOf[TVar]
    }
    //////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////
    // Administration
    //////////////////////////////////////////////////////////////////////////
    private val privAssignDependencies : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
    override protected def discoveryDepenencies : List[Discoverable] = privAssignDependencies.toList
    //////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////
    // Assignment (Mutation)
    //////////////////////////////////////////////////////////////////////////
    final def := [R](right: protComp.Op.Able[R])(implicit op: protComp.`Op:=`.Builder[TVal, R]) = assign(op(left, right))
    final protected[DFiant] def assign(that : DFAny) : TVar = {
      privAssignDependencies += that
      AlmanacEntryAssign(this.almanacEntry, that.getCurrentEntry)
      this.asInstanceOf[TVar]
    }
    //////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar(_width : Int, _init : Seq[Token])(
    implicit protected val dsn : DFDesign, cmp : Companion, n : NameIt
  ) extends DFAny.Var {
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected lazy val protInit : Seq[TToken] = _init.asInstanceOf[Seq[TToken]]
    def codeString(idRef : String) : String
    final protected[DFiant] lazy val almanacEntry : AlmanacEntry = AlmanacEntryNewDFVar(width, protInit, codeString)
    final protected[DFiant] def discovery : Unit = almanacEntry
    final val isPort = false
    final val id = dsn.newDFValGetID(this)
    final val isAnonymous : Boolean = n.value == "implementation" || n.value == "$anon"
    //Port Construction
    //TODO: Implement generically after upgrading to 2.13.0-M5
    //Also see https://github.com/scala/bug/issues/11026
    //    def <> [DIR <: DFDir](dir : DIR)(implicit port : protComp.Port.Builder[TVal, DIR])
    //     : TVal <> DIR = port(this.asInstanceOf[TVal], dir)
    override protected def nameDefault: String = {
      if (isAnonymous) "$" + s"anon$id"
      else n.value
    }
  }

  abstract class Alias(aliasedVar : DFAny, relWidth : Int, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(
    implicit protected val dsn : DFDesign, cmp : Companion, n : NameIt
  ) extends DFAny.Var {
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](relWidth)
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = {
      val initTemp : Seq[Token] = if (updatedInit.isEmpty) aliasedVar.getInit else updatedInit
      val prevInit = if (deltaStep < 0) initTemp.prevInit(-deltaStep) else initTemp //TODO: What happens for `next`?
      val bitsInit = prevInit.bitsWL(relWidth, relBitLow)
      bitsInit.map(protTokenBitsToTToken)
    }
    def codeString(idRef : String) : String
    final protected[DFiant] lazy val almanacEntry : AlmanacEntry = {
      val timeRef = aliasedVar.almanacEntry.timeRef.stepBy(deltaStep)
      AlmanacEntryAliasDFVar(aliasedVar.almanacEntry, BitsRange(relBitLow + relWidth - 1, relBitLow), timeRef, protInit, codeString)
    }
    final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies :+ aliasedVar
    final protected[DFiant] def discovery : Unit = almanacEntry
    final val isPort = false

    final val id = dsn.newDFValGetID(this)
    final val isAnonymous : Boolean = n.value == "implementation" || n.value == "$anon"
    private lazy val derivedName : String = if (deltaStep < 0) s"${aliasedVar.fullName}__prev${-deltaStep}"
                                           else s"${aliasedVar.fullName}__???"
    override protected def nameDefault: String =
      if (isAnonymous) "$" + s"anon$id" + "$$" + derivedName else n.value
  }

  abstract class Const(token : Token)(
    implicit protected val dsn : DFDesign, cmp : Companion, n : NameIt
  ) extends DFAny {
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](token.width)
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = Seq(token).asInstanceOf[Seq[TToken]]
    final protected[DFiant] lazy val almanacEntry : AlmanacEntry = AlmanacEntryConst(token)
    final protected[DFiant] def discovery : Unit = almanacEntry
    final val isPort = false
    override def toString : String = s"$token"
    final val isAnonymous : Boolean = false
//    dsn.newDFVal(this)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Port[DF <: DFAny, DIR <: DFDir](dfVar : DF, val dir : DIR)(
    implicit protected val dsn : DFDesign, cmp : Companion, n : NameIt
  ) extends DFAny {
    type TAlias = TVal <> DIR
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](dfVar.width)

    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = dfVar.getInit.asInstanceOf[Seq[TToken]]
    final protected[DFiant] lazy val almanacEntry : AlmanacEntryPort = AlmanacEntryPort(dfVar.almanacEntry, dir, name, dsn.name)
    private val privAssignDependencies : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
    private val privComponentDependency : ListBuffer[DFInterface] = ListBuffer.empty[DFInterface]
    final protected[DFiant] def setComponentDependency(comp : DFInterface) : Unit = {
      if (privComponentDependency.nonEmpty)
        throw new IllegalArgumentException(s"The dataflow value $name is already connected to an output port in the component ${privComponentDependency.head.fullName}")
      else privComponentDependency += comp
    }
    final override protected def discoveryDepenencies : List[Discoverable] = privAssignDependencies.toList ++ privComponentDependency
    final protected[DFiant] def discovery : Unit = almanacEntry
//    final lazy val isOpen : Boolean = conn match {
//      case OPEN => true
//      case _ => false
//    }
    final protected[DFiant] def portAssign(that : DFAny) : Port[DF, DIR] with DF = {
      privAssignDependencies += that
      AlmanacEntryAssign(this.almanacEntry, that.getCurrentEntry)
      this.asInstanceOf[Port[DF, DIR] with DF]
    }
    private type MustBeOut = RequireMsg[ImplicitFound[DIR <:< OUT], "Cannot assign to an input port"]
    final def portConnect(that : DFAny, dsn : DFDesign) : Unit = {
      (this.dsn, that.dsn, dsn) match {
        case (a, b, c) if (a eq b) => ???
      }
    }
    final def <> [R](right: protComp.Op.Able[R])(
      implicit op: protComp.`Op:=`.Builder[TVal, R], dsn : DFDesign
    ) : Unit = portConnect(op(left, right), dsn)
    //Connection should be constrained accordingly:
    //* For IN ports, supported: All Op:= operations, and TOP
    //* For OUT ports, supported only TVar and TOP
    final def := [R](right: protComp.Op.Able[R])(
      implicit dir : MustBeOut, op: protComp.`Op:=`.Builder[TVal, R]
    ) = portAssign(op(left, right))
    final val isPort = true
    override protected def nameDefault: String = n.value
    override def toString : String = s"$fullName : $typeName <> $dir"
    final val isAnonymous : Boolean = false

    final val id = dsn.newPortGetID(this.asInstanceOf[Port[DFAny, DFDir]])
  }
  object Port {
    trait Builder[L <: DFAny, DIR <: DFDir] {
      def apply(right : L, dir : DIR) : L <> DIR
    }
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
      if (leftSeq.isEmpty || rightSeq.isEmpty) Seq() else
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
  // Op
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  object Op {
    trait Able[R]{val value : R}
    object Able {
      implicit def fromAble[R](able : Able[R]) : R = able.value
    }
    trait Builder[L, R] {
      type Comp <: DFAny
      def apply(left : L, rightR : R) : Comp
    }
    trait Implicits[A[T] <: Able[T], UB <: DFAny] {
//      import shapeless._
//        implicit def ofTVal[R <: UB](value : R)(implicit gen : Generic[A[value.TVal]])
//        : A[value.TVal] = gen.from((value.asInstanceOf[value.TVal] :: HNil).asInstanceOf[gen.Repr])
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    type Unbounded <: DFAny.Unbounded[this.type]

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Port
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait Port {
      type Builder[L <: DFAny, DIR <: DFDir] <: DFAny.Port.Builder[L, DIR]
    }
    val Port : Port
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    trait Op {
      type Able[R] <: DFAny.Op.Able[R]
      trait Implicits extends DFAny.Op.Implicits[Able, Unbounded]
      val Able : Implicits
    }
    val Op : Op
    trait `Op:=` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]
    }
    val `Op:=` : `Op:=`
    trait `Op==` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]{type Comp = DFBool}
    }
    val `Op==` : `Op==`
    trait `Op!=` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]{type Comp = DFBool}
    }
    val `Op!=` : `Op!=`
    trait Init {
      type Able[L <: DFAny] <: DFAny.Init.Able[L]
      type Builder[L <: DFAny] <: DFAny.Init.Builder[L, Able]
    }
    val Init : Init
    trait Prev {
      type Builder[L <: DFAny] <: DFAny.Prev.Builder[L]
    }
    val Prev : Prev
    implicit val cmp = this
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}




