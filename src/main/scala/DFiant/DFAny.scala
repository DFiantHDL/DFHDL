package DFiant

import DFiant.BasicLib.DFBasicLib
import DFiant.DFAny.Op.Context
import DFiant.internals._
import singleton.ops._
import singleton.twoface._

import scala.collection.{GenSet, SetLike}
import scala.collection.mutable.ListBuffer

trait DFAny extends DFAnyMember with HasWidth {
  type TVal <: DFAny.Unbounded[TCompanion]
  type TVar <: TVal with DFAny.Var
  type TAlias <: TVal
  type TBool <: DFBool
  type TBits[W2] <: DFBits[W2]
  type TUInt[W2] <: DFUInt[W2]
  type TSInt[W2] <: DFSInt[W2]
  type TCompanion <: DFAny.Companion
  //  type TToken = protComp.Token //Good-code red in intellij, so using type projection instead
  type TToken <: DFAny.Token
  type TPattern <: DFAny.Pattern[TPattern]
  type TPatternAble[+R] <: DFAny.Pattern.Able[R]
  type TPatternBuilder[L <: DFAny] <: DFAny.Pattern.Builder[L, TPatternAble]
  type TUnbounded = TCompanion#Unbounded
//  type TUInt <: DFUInt
  val width : TwoFace.Int[Width]
  protected[DFiant] val protComp : TCompanion
  import protComp._
  final protected[DFiant] val tVal = this.asInstanceOf[TVal]
  final protected[DFiant] val left = tVal

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final protected def protBit[I](relBit : TwoFace.Int[I])(implicit ctx : DFAny.Alias.Context) : TBool =
    new DFBool.Alias(List(this), DFAny.Alias.Reference.BitsWL(1, relBit, s".bit($relBit)")).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit ctx : DFAny.Alias.Context) : TBool =
    protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit relBit : BitIndex.Checked[I, Width], ctx : DFAny.Alias.Context, di : DummyImplicit) : TBool =
    protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits(implicit ctx : DFAny.Alias.Context) : TBits[Width] =
    new DFBits.Alias[Width](List(this), DFAny.Alias.Reference.BitsWL(width, 0, ".bits")).asInstanceOf[TBits[Width]]

  final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) : TBits[relWidth.Out] =
    new DFBits.Alias[relWidth.Out](List(this), DFAny.Alias.Reference.BitsWL(relWidth(relBitHigh, relBitLow), relBitLow, s".bits($relBitHigh, $relBitLow)")).asInstanceOf[TBits[relWidth.Out]]

  final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def bits[H <: Int, L <: Int](range : XRange.Int[L, H])(
    implicit relBitHigh : BitIndex.CheckedShell[H, Width], relBitLow : BitIndex.CheckedShell[L, Width],
    relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) = {
    relBitHigh.unsafeCheck(range.end, width)
    relBitLow.unsafeCheck(range.start, width)
    protBits[H, L](TwoFace.Int.create[H](range.end), TwoFace.Int.create[L](range.start))
  }
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Partial Bits at Position selection
  //////////////////////////////////////////////////////////////////////////
  final protected def protBitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L])(implicit ctx : DFAny.Alias.Context)
  : TBits[W] = new DFBits.Alias[W](List(this), DFAny.Alias.Reference.BitsWL(relWidth, relBitLow, s".bits($relWidth, $relBitLow)")).asInstanceOf[TBits[W]]

  import singleton.ops.-
  final def bitsWL[W, L](relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkRelWidth : PartWidth.CheckedShell[W, Width - L], ctx : DFAny.Alias.Context
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }

  final def bitsWL[W, L](implicit relWidth : TwoFace.Int[W], relBitLow : BitIndex.Checked[L, Width],
    checkRelWidth : PartWidth.CheckedShell[W, Width - L], ctx : DFAny.Alias.Context, di : DummyImplicit
  ) = {
    checkRelWidth.unsafeCheck(relWidth, width-relBitLow)
    protBitsWL(relWidth, relBitLow.unsafeCheck(width))
  }
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Init (for use with Prev)
  //////////////////////////////////////////////////////////////////////////
  protected val protInit : Seq[TToken]
  //Only call within lazy val calculation of `protInit` when dependent on other init values
  final protected[DFiant] def getInit : Seq[TToken] = protInit
  protected[DFiant] val constVal : TToken
  final def isConstant : Boolean = !constVal.isBubble
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
//  protected[DFiant] def copyAsNewVar : DFAny.NewVar with TVar = ???
  protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context) : TVal <> Dir
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Naming
  //////////////////////////////////////////////////////////////////////////
  final def isAnonymous : Boolean = name.startsWith("ǂ") || isInstanceOf[DSLFoldableOwnerConstruct]
  final override private[DFiant] def nameDefault: String = ctx.getName
  private var autoConstructCodeString : String = ""
  final private[DFiant] def setAutoConstructCodeString(cs : String) : this.type = {autoConstructCodeString = cs; this}
  private[DFiant] def constructCodeStringDefault : String
  private[DFiant] def showAnonymous : Boolean = config.showAnonymousEntries || this.isInstanceOf[DFAny.NewVar[_]]
  private def constructCodeString : String =
    if (autoConstructCodeString.isEmpty || showAnonymous) constructCodeStringDefault else autoConstructCodeString
  override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = {
    val ref = if (isAnonymous && !showAnonymous) relativeName(constructCodeString)(callOwner) else relativeName(callOwner)
    ref.applyBrackets() //TODO: consider other way instead of this hack
  }
  private def initCommentString : String =
    if (config.commentInitValues) s"//init = ${getInit.codeString}" else ""
  private def valCodeString : String = s"\nval $name = $constructCodeString"
  def codeString : String = f"$valCodeString%-60s$initCommentString"
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
  val isPort : Boolean
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Simulation
  //////////////////////////////////////////////////////////////////////////
  def simInject(that : BigInt) : Boolean = ???
  def simWatch : BigInt = ???
  //////////////////////////////////////////////////////////////////////////
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
    type TSInt[W2] = DFSInt.Var[W2]//DFSInt[W2]#TVar
    type TDir <: DFDir

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
    protected val protAssignDependencies : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
    override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ protAssignDependencies.toList
    //////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////
    // Assignment (Mutation)
    //////////////////////////////////////////////////////////////////////////
    private type MustBeOut = RequireMsg[![ImplicitFound[TDir <:< IN]], "Cannot assign to an input port"]
    final def := [R](right: protComp.Op.Able[R])(
      implicit dir : MustBeOut, op: protComp.`Op:=`.Builder[TVal, R], ctx : DFAny.Op.Context
    ) = assign(op(left, right))
    final protected var assigned : Boolean = false
    protected[DFiant] def assign(that : DFAny)(implicit ctx : DFAny.Op.Context) : TVar = {
      assigned = true
      if (!ctx.owner.callSiteSameAsOwnerOf(that))
        throw new IllegalArgumentException(s"\nTarget assignment variable (${this.fullName}) is not at the same design as this assignment call (${ctx.owner.fullName})")
      protAssignDependencies += Assignment(this, that)
      protAssignDependencies += that
      this.asInstanceOf[TVar]
    }
    //////////////////////////////////////////////////////////////////////////
  }

  trait Uninitialized extends DFAny.Var {
    type TPostInit <: TVal
    final def init(that : protComp.Init.Able[TVal]*)(
      implicit op : protComp.Init.Builder[TVal, TToken], ctx : Alias.Context
    ) : TPostInit = {
      initialize(op(left, that), ctx.owner)
      this.asInstanceOf[TPostInit]
    }
    final private var initialized : Boolean = false
    private var updatedInit : () => Seq[TToken] = () => Seq() //just for codeString
    final protected[DFiant] def initialize(updatedInit0 : => Seq[TToken], owner : DFAnyOwner) : Unit = {
      if (initialized) throw new IllegalArgumentException(s"${this.fullName} already initialized")
      if (this.owner ne owner) throw new IllegalArgumentException(s"\nInitialization of variable (${this.fullName}) is not at the same design as this call (${owner.fullName})")
      updatedInit = () => updatedInit0
      initialized = true
      setInitFunc(updatedInit0)
    }
    final def reInit(cond : DFBool) : Unit = ???
    final private var _initFunc : () => Seq[TToken] = () => Seq()
    final protected[DFiant] def setInitFunc(value : => Seq[TToken]) : Unit = {
//      println(s"setInitFunc $fullName")
      _initFunc = () => value
    }
    private[DFiant] object setInitFunc {
      def forced(value : => Seq[Token]) : Unit = setInitFunc(value.asInstanceOf[Seq[TToken]])
    }
    final private val initLB = LazyBox({
//      println(s"initLB $fullName")
      _initFunc()
    })
    final protected lazy val protInit : Seq[TToken] = initLB getOrElse(throw new IllegalArgumentException("\nCircular initialization detected"))
    final def initCodeString : String = if (initialized) s" init${updatedInit().codeString}" else ""

    //////////////////////////////////////////////////////////////////////////
    // Connectivity
    //////////////////////////////////////////////////////////////////////////
    final def <> [RDIR <: DFDir](right: TVal <> RDIR)(implicit ctx : Connector.Context) : Unit = right.connectVal2Port(this)
    final private[DFiant] var connectedSource : Option[DFAny] = None
    final private[DFiant] def connected : Boolean = connectedSource.isDefined
    final private[DFiant] def connectFrom(fromVal : DFAny)(implicit ctx : Connector.Context) : Unit = {
      val toVar = this
      //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${fromVal.fullName} <> ${toVar.fullName}")
      if (toVar.width < fromVal.width) throwConnectionError(s"Target port width (${toVar.width}) is smaller than source port width (${fromVal.width}).")
      if (toVar.connected) throwConnectionError(s"Target port ${toVar.fullName} already has a connection: ${toVar.connectedSource.get.fullName}")
      if (toVar.assigned) throwConnectionError(s"Target port ${toVar.fullName} was already assigned to. Cannot apply both := and <> operators on a port.")
      //All is well. We can now connect fromVal->toVar
      toVar.setInitFunc(fromVal.getInit.asInstanceOf[Seq[toVar.TToken]])
      toVar.connectedSource = Some(fromVal)
      toVar.protAssignDependencies += Connector(toVar, fromVal)
      toVar.protAssignDependencies += fromVal
    }
    final override protected[DFiant] def assign(that : DFAny)(implicit ctx : DFAny.Op.Context) : TVar = {
      if (this.connected) throw new IllegalArgumentException(s"\nTarget assignment dataflow variable ${this.fullName} was already connected to. Cannot apply both := and <> operators on a dataflow variable.")
      super.assign(that)
    }
    //////////////////////////////////////////////////////////////////////////
  }

  case class Connector(toPort : DFAny, fromVal : DFAny)(implicit ctx0 : Connector.Context) extends DFAnyMember {
    val ctx = ctx0
    override private[DFiant] def nameDefault = "ǂconnect"
    private def connectCodeString : String = s"\n${toPort.refCodeString} <> ${fromVal.refCodeString}"
    def codeString : String = toPort.owner match {
      case f : DSLSelfConnectedFoldableOwnerConstruct if f.isFolded => ""
      case _ => connectCodeString
    }
    final val id = getID
  }
  object Connector {
    type Context = DFAnyOwner.Context[DFBlock]
  }

  case class Assignment(toVar : DFAny, fromVal : DFAny)(implicit ctx0 : DFAny.Op.Context) extends DFAnyMember {
    val ctx = ctx0
    override private[DFiant] def nameDefault = "ǂassign"
    def codeString : String = s"\n${toVar.refCodeString} := ${fromVal.refCodeString}"
    final val id = getID
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar[DF <: DFAny](_width : Int, newVarCodeString : String)(
    implicit ctx0 : NewVar.Context, cmp : Companion, bubbleToken : DF => DF#TToken
  ) extends DFAny.Uninitialized {
    type TPostInit = TVar
    val ctx = ctx0
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
    final protected[DFiant] val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final private[DFiant] def constructCodeStringDefault : String = s"$newVarCodeString$initCodeString"
    final val isPort = false
    final protected[DFiant] lazy val constVal : TToken = bubbleToken(this.asInstanceOf[DF]).asInstanceOf[TToken] //TODO: set dependency on assignment
    //Port Construction
    //TODO: Implement generically after upgrading to 2.13.0-M5
    //Also see https://github.com/scala/bug/issues/11026
//        def <> [Dir <: DFDir](dir : Dir)(implicit port : protComp.Port.Builder[TVal, Dir])
//         : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    //TODO: Implement generically after upgrading to 2.13.0-M5
    //Also see https://github.com/scala/bug/issues/11026
    //final object ifdf extends ConditionalBlock.WithRetVal[TVal, protComp.Op.Able, protComp.`Op:=`.Builder](NewVar.this)

    def selectdf[T, E](cond : DFBool)(thenSel : protComp.Op.Able[E], elseSel : protComp.Op.Able[T]) : TVal = ???
    def selectdf[SW, T](sel : DFUInt[SW], default : => Option[TVal] = None)(args : protComp.Op.Able[T]*) : TVal = ???
    final val id = getID
  }
  object NewVar {
    type Context = DFAnyOwner.Context[DFAnyOwner]
  }

  abstract class Alias[DF <: DFAny](aliasedVars : List[DFAny], reference : DFAny.Alias.Reference)(
    implicit ctx0 : Alias.Context, cmp : Companion, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends DFAny.Var {
    val ctx = ctx0
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width] ({
      val widthSeq : List[Int] = aliasedVars.map(aliasedVar => reference match {
        case DFAny.Alias.Reference.BitsWL(relWidth, _) => relWidth
        case _ => aliasedVar.width.getValue
      })
      widthSeq.sum
    })
    final protected[DFiant] val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = {
      val initList : List[Seq[DFBits.Token]] = aliasedVars.map(aliasedVar => {
        val currentInit: Seq[DFBits.Token] = aliasedVar.getInit.bits
        val updatedInit: Seq[DFBits.Token] = reference match {
          case DFAny.Alias.Reference.BitsWL(relWidth, relBitLow) => currentInit.bitsWL(relWidth, relBitLow)
          case DFAny.Alias.Reference.Prev(step) => currentInit.prevInit(step)
          case DFAny.Alias.Reference.AsIs() => currentInit
          case DFAny.Alias.Reference.BitReverse() => DFBits.Token.reverse(currentInit)
          case DFAny.Alias.Reference.Invert() => DFBits.Token.unary_~(currentInit)
        }
        updatedInit
      })
      initList.reduce(DFBits.Token.##).map(protTokenBitsToTToken.asInstanceOf[DFBits.Token => TToken])
    }
    final protected[DFiant] lazy val constVal : TToken = {
      val constList : List[DFBits.Token] = aliasedVars.map(aliasedVar => {
        val currentConst: DFBits.Token = aliasedVar.constVal.bits
        val updatedConst: DFBits.Token = reference match {
          case DFAny.Alias.Reference.BitsWL(relWidth, relBitLow) => currentConst.bitsWL(relWidth, relBitLow)
          case DFAny.Alias.Reference.Prev(step) => currentConst //TODO: Fix when referencing a previous of a constant
          case DFAny.Alias.Reference.AsIs() => currentConst
          case DFAny.Alias.Reference.BitReverse() => currentConst.reverse
          case DFAny.Alias.Reference.Invert() => ~currentConst
        }
        updatedConst
      })
      protTokenBitsToTToken(constList.reduce((a, b) => a ## b)).asInstanceOf[TToken]
    }
    final private[DFiant] def constructCodeStringDefault : String =
      if (aliasedVars.length == 1) s"${aliasedVars.head.refCodeString}${reference.aliasCodeString}"
      else s"${aliasedVars.map(a => a.refCodeString).mkString("(",", ",")")}${reference.aliasCodeString}"
    final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ aliasedVars
    final val isPort = false
    final val id = getID
  }
  object Alias {
    trait Tag
    type Context = DFAnyOwner.Context[DFAnyOwner]

    sealed abstract class Reference(aliasCodeString_ : => String) {
      lazy val aliasCodeString : String = aliasCodeString_
    }
    object Reference {
      class AsIs(aliasCodeString : => String) extends Reference(aliasCodeString)
      object AsIs {
        def apply(aliasCodeString : => String) = new AsIs(aliasCodeString)
        def unapply(arg: AsIs): Boolean = true
      }
      class BitsWL(val relWidth : Int, val relBitLow : Int, aliasCodeString : => String) extends Reference(aliasCodeString)
      object BitsWL {
        def apply(relWidth: Int, relBitLow : Int, aliasCodeString : => String) = new BitsWL(relWidth, relBitLow, aliasCodeString)
        def unapply(arg : BitsWL): Option[(Int, Int)] = Some((arg.relWidth, arg.relBitLow))
      }
      class Prev(val step : Int) extends Reference(if (step == 1) ".prev" else s".prev($step)")
      object Prev {
        def apply(step : Int) = new Prev(step)
        def unapply(arg: Prev): Option[Int] = Some(arg.step)
      }
      class BitReverse(aliasCodeString : => String) extends Reference(aliasCodeString)
      object BitReverse {
        def apply(aliasCodeString : => String) = new BitReverse(aliasCodeString)
        def unapply(arg: BitReverse): Boolean = true
      }
      class Invert(aliasCodeString : => String) extends Reference(aliasCodeString)
      object Invert {
        def apply(aliasCodeString : => String) = new Invert(aliasCodeString)
        def unapply(arg: Invert): Boolean = true
      }
//      class Selection(val selector : DFAny, val default : DFAny, aliasCodeString : => String) extends Reference(aliasCodeString)
//      object Selection {
//        def apply(selector: DFAny, default : DFAny, aliasCodeString : => String) = new Selection(selector, default, aliasCodeString)
//        def unapply(arg : Selection): Option[(DFAny, DFAny)] = Some((arg.selector, arg.default))
//      }
    }
  }

  abstract class Const(token : Token)(
    implicit ctx0 : Const.Context, cmp : Companion
  ) extends DFAny {
    val ctx = ctx0
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](token.width)
    final protected[DFiant] val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = Seq(token).asInstanceOf[Seq[TToken]]
    final override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = constructCodeStringDefault
    private[DFiant] def constructCodeStringDefault : String = s"${token.codeString}"
    final protected[DFiant] val constVal = token.asInstanceOf[TToken]
    final val isPort = false
    final val id = getID
  }
  object Const {
    type Context = DFAnyOwner.Context[DFAnyOwner]
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Port[DF <: DFAny, Dir <: DFDir](dfVar : DF, val dir : Dir)(
    implicit ctx0 : Port.Context, cmp : Companion, bubbleToken : DF => DF#TToken
  ) extends DFAny.Uninitialized {
    this : DF <> Dir =>
    type TPostInit = TVal <> Dir
    type TDir = Dir
    val ctx = ctx0
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](dfVar.width)
    final protected[DFiant] lazy val constVal = connectedSource match {
      case Some(v) => v.constVal.asInstanceOf[TToken]
      case _ => bubbleToken(this.asInstanceOf[DF]).asInstanceOf[TToken]
    }
    final protected[DFiant] val protComp : TCompanion = cmp.asInstanceOf[TCompanion]

    private[DFiant] def injectDependencies(dependencies : List[Discoverable]) : Unit = protAssignDependencies ++= dependencies
    final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies

    private def sameDirectionAs(right : Port[_ <: DFAny,_ <: DFDir]) : Boolean = this.dir == right.dir
    private[DFiant] def connectPort2Port(right : Port[_ <: DFAny,_ <: DFDir])(implicit ctx : Connector.Context) : Unit = {
      implicit val theOwnerToBe : DSLOwnerConstruct = ctx.owner
      val left = this
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${this.fullName} <> ${right.fullName}\nConnected at ${ctx.owner.fullName}")
      val (fromPort, toPort) =
        //Ports in the same design, connected at the same design
        if ((left hasSameOwnerAs right) && isConnectedAtOwnerOf(left)) (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect two input ports of the same design.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect two output ports of the same design.")
          case (ld : IN,  rd : OUT) => (left, right)
          case (ld : OUT, rd : IN)  => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Ports in the same design, connected at the design's owner.
        //This is a loopback connection from a design's output to one of its inputs
        else if ((left hasSameOwnerAs right) && isConnectedAtOwnerOf(left.owner)) (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect two input ports of the same design.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect two output ports of the same design.")
          case (ld : IN,  rd : OUT) => (right, left)
          case (ld : OUT, rd : IN)  => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is left and child port is right.
        else if (right.isDownstreamMemberOf(left.owner) && isConnectedAtEitherSide(left, right)) (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (left, right)
          case (ld : OUT, rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is right and child port is left.
        else if (left.isDownstreamMemberOf(right.owner) && isConnectedAtEitherSide(left, right)) (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (right, left)
          case (ld : OUT, rd : OUT) => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting sibling designs.
        else if ((left.owner hasSameOwnerAs right.owner) && isConnectedAtOwnerOf(left.owner)) (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : IN)  => (left, right)
          case (ld : IN,  rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        else if (!left.isDownstreamMemberOf(right.owner) || !right.isDownstreamMemberOf(left.owner))
          throwConnectionError(s"Connection must be made between ports that are either in the same design, or in a design and its owner, or between two design siblings.")
        else if (!isConnectedAtEitherSide(left, right))
          throwConnectionError(s"The connection call must be placed at the same design as one of the ports or their mutual owner. Call placed at ${ctx.owner.fullName}")
        else throwConnectionError("Unexpected connection error")

      toPort.connectFrom(fromPort)
    }
    final private[DFiant] def connectVal2Port(dfVal : DFAny)(implicit ctx : Connector.Context) : Unit = {
      implicit val theOwnerToBe : DSLOwnerConstruct = ctx.owner
      val port = this
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${port.fullName} <> ${dfVal.fullName}")
      if (dfVal.isInstanceOf[Port[_ <: DFAny, _ <: DFDir]]) dfVal.asInstanceOf[Port[_ <: DFAny, _ <: DFDir]].connectPort2Port(port)
      else {
        //Connecting external value from/to a output/input port
        if (port.owner.isDownstreamMemberOf(dfVal.owner)) {
          if (!isConnectedAtEitherSide(dfVal, port)) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${ctx.owner.fullName}")
          //Connecting from output port to external value
          if (port.dir.isOut) dfVal match {
            case u : Uninitialized => u.connectFrom(port)
            case _ => throwConnectionError(s"Cannot connect an external value to an output port.")
          }
          //Connecting from external value to input port
          else port.connectFrom(dfVal)
        }
        //Connecting internal value and output port
        else if (port hasSameOwnerAs dfVal) {
          if (port.dir.isIn) throwConnectionError(s"Cannot connect an internal non-port value to an input port.")
          if (ctx.owner ne dfVal.owner) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${ctx.owner.fullName}")
          port.connectFrom(dfVal)
        }
        else throwConnectionError(s"Unsupported connection between a non-port and a port, ${ctx.owner.fullName}")
      }
    }
    final def <> [R](right: protComp.Op.Able[R])(
      implicit op: protComp.`Op<>`.Builder[TVal, R], ctx : DFAny.Connector.Context
    ) : Unit = connectVal2Port(op(left, right))
    //Connection should be constrained accordingly:
    //* For IN ports, supported: All Op:= operations, and TOP
    //* For OUT ports, supported only TVar and TOP
    final val isPort = true
    private[DFiant] def constructCodeStringDefault : String = s"${dfVar.constructCodeStringDefault} <> $dir$initCodeString"
    override def toString : String = s"$fullName : $typeName <> $dir"
    final val id = getID
  }
  object Port {
    type Context = DFAnyOwner.Context[DFInterface]
    trait Builder[L <: DFAny, Dir <: DFDir] {
      def apply(right : L, dir : Dir) : L <> Dir
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token extends HasCodeString {
    Self =>
    type TValue
    type TPattern <: DFAny.Pattern[TPattern]{type TValue = Self.TValue}
    val value : TValue
    //maximum token value width
    val width : Int
    final lazy val widthOfValue : Int = scala.math.max(valueBits.lengthOfValue, bubbleMask.lengthOfValue).toInt
    val valueBits : BitVector
    val bubbleMask : BitVector
    //leading zero counter
    final lazy val lzc : Int = scala.math.min(valueBits.lzc, bubbleMask.lzc).toInt
    final def isBubble : Boolean = !(bubbleMask === BitVector.low(width))
    def toBubbleToken : Token

    final def bit(relBit : Int) : DFBool.Token = {
      val outBitsValue = valueBits.bit(relBit)
      val outBubbleMask = bubbleMask.bit(relBit)
      new DFBool.Token(outBitsValue, outBubbleMask)
    }
    final def bits : DFBits.Token = new DFBits.Token(width, valueBits, bubbleMask)
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
    final def patternMatch(that : TPattern) : DFBool.Token = DFBool.Token(that.matches(this.value), this.isBubble)

    final override def toString: String = codeString
  }

  object Token {
    abstract class Of[V, P <: DFAny.Pattern[P]{type TValue = V}](val width: Int, val value : V)(implicit codeStringOf : CodeStringOf[V]) extends Token {
      type TValue = V
      type TPattern = P
      final def codeString : String = if (isBubble) "Φ" else value.codeString
    }
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
      def bits : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bits)
      def bitsWL(relWidth : Int, relBitLow : Int) : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      def codeString : String = tokenSeq.map(t => t.codeString).mkString("(", ", ", ")")
      def patternMatch(pattern : T#TPattern) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
    }
  }

  object TokenSeq {
    def apply[O <: Token, T1 <: Token, T2 <: Token, T3 <: Token](t1 : Seq[T1], t2 : Seq[T2], t3 : Seq[T3])(op : (T1, T2, T3) => O) : Seq[O] =
      if (t1.isEmpty || t2.isEmpty || t3.isEmpty) Seq() else{
        val leftSeq = t1
        val rightSeq = t2
        val leftSeq2 = leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last)
        val rightSeq2 = t3
        leftSeq2.zipAll(rightSeq2, leftSeq2.last, rightSeq2.last).map(t => op(t._1._1, t._1._2, t._2))
      }
    def apply[O <: Token, L <: Token, R <: Token](leftSeq : Seq[L], rightSeq : Seq[R])(op : (L, R) => O) : Seq[O] =
      if (leftSeq.isEmpty || rightSeq.isEmpty) Seq() else
      leftSeq.zipAll(rightSeq, leftSeq.last, rightSeq.last).map(t => op(t._1, t._2))
    def apply[O <: Token, L <: Token, R](leftSeq : Seq[L], rightConst : R)(op : (L, R) => O) : Seq[O] =
      leftSeq.map(t => op(t, rightConst))
    def apply[O <: Token, T <: Token](seq : Seq[T])(op : T => O) : Seq[O] =
      seq.map(t => op(t))
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Match Pattern
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  sealed trait Pattern[P <: Pattern[P]] extends HasCodeString {
    type TValue
    def matches(value : TValue) : Boolean
    def overlapsWith(pattern: P) : Boolean
    def codeString : String
    override def toString: String = codeString
  }
  object Pattern {
    abstract class OfIntervalSet[T, P <: OfIntervalSet[T, P]](val patternSet : IntervalSet[T])(implicit codeStringOf: CodeStringOf[Interval[T]]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.containsPoint(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    abstract class OfSet[T, P <: OfSet[T, P]](val patternSet : Set[T])(implicit codeStringOf: CodeStringOf[T]) extends Pattern[P] {
      type TValue = T
      def matches(value : TValue) : Boolean = patternSet.contains(value)
      def overlapsWith(pattern: P) : Boolean = patternSet.intersect(pattern.patternSet).nonEmpty
      def codeString : String = patternSet.map(t => t.codeString).mkString(", ")
    }
    trait Able[+R] {
      val right : R
    }

    trait Builder[L <: DFAny, Able[+R] <: Pattern.Able[R]] {
      def apply[R](left : L, right : Seq[Able[R]]) : L#TPattern
    }
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
    trait Builder[L <: DFAny, Able[L0 <: DFAny] <: Init.Able[L0], Token <: DFAny.Token] {
      def apply(left : L, right : Seq[Able[L]]) : Seq[Token]
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
    type Context = DFBlock.Context
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Create Companion object of DFXXX extenders of DFAny
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Companion {
    type Unbounded <: DFAny.Unbounded[this.type]

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Alias
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait AliasCO {
      def apply[M <: Unbounded](left : DFAny, mold : M)(implicit ctx : DFAny.Alias.Context) : DFAny
    }
    val Alias : AliasCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Port
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait PortCO {
      type Builder[L <: DFAny, Dir <: DFDir] <: DFAny.Port.Builder[L, Dir]
    }
    val Port : PortCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Token
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait TokenCO
    val Token : TokenCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Init
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait InitCO {
      type Able[L <: DFAny] <: DFAny.Init.Able[L]
      type Builder[L <: DFAny, Token <: DFAny.Token] <: DFAny.Init.Builder[L, Able, Token]
    }
    val Init : InitCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Match Pattern
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait PatternCO {
      type Able[+R] <: DFAny.Pattern.Able[R]
      type Builder[L <: DFAny] <: DFAny.Pattern.Builder[L, Able]
    }
    val Pattern : PatternCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Prev
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait PrevCO {
      type Builder[L <: DFAny] <: DFAny.Prev.Builder[L]
    }
    val Prev : PrevCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // General Op
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait OpCO {
      type Able[R] <: DFAny.Op.Able[R]
      type Implicits
      val Able : Implicits
    }
    val Op : OpCO
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Common Ops
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////
    trait `Op:=` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]
    }
    val `Op:=` : `Op:=`
    trait `Op<>` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]
    }
    val `Op<>` : `Op<>`
    trait `Op==` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]{type Comp = DFBool}
    }
    val `Op==` : `Op==`
    trait `Op!=` {
      type Builder[L, R] <: DFAny.Op.Builder[L, R]{type Comp = DFBool}
    }
    val `Op!=` : `Op!=`
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    implicit val cmp = this
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tuple-handling Implicits
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class VarProductExtender(e : Product) {
    type WSum
    protected val wsum : Int = e.productIterator.toList.asInstanceOf[List[DFAny]].map(f => f.width.getValue).sum
    def bits(implicit ctx : DFAny.Alias.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFBits.Var[w.Out] =
      new DFBits.Alias[w.Out](e.productIterator.toList.asInstanceOf[List[DFAny]], DFAny.Alias.Reference.AsIs(".bits"))
  }

  abstract class ValProductExtender(e : Product) {
    type WSum
    protected val wsum : Int = e.productIterator.toList.collect{
      case dfAny : DFAny => dfAny.width.getValue
      case bv : BitVector => bv.length.toInt
    }.sum
    def bits(implicit ctx : DFAny.Alias.Context, w : TwoFace.Int.Shell1[Id, WSum, Int]) : DFBits[w.Out] = {
      val list : List[DFAny] = e.productIterator.toList.collect{
        case dfAny : DFAny => dfAny
        case bv : BitVector => new DFBits.Const[Int](DFBits.Token(bv))
      }
      new DFBits.Alias[w.Out](list, DFAny.Alias.Reference.AsIs(".bits"))
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////
  // Tuple 1
  /////////////////////////////////////////////////////////////////////////////////////
  implicit class VarTuple1[T1 <: DFAny.Var](val e : Tuple1[T1])
    extends VarProductExtender(e) {
    type WSum = e._1.Width
  }

  implicit class ValTuple1[T1 <: HasWidth](val e : Tuple1[T1])
    extends ValProductExtender(e){
    type WSum = e._1.Width
  }
  /////////////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////////////
  // Tuple 2
  /////////////////////////////////////////////////////////////////////////////////////
  implicit class VarTuple2[T1 <: DFAny.Var, T2 <: DFAny.Var](val e : Tuple2[T1, T2])
    extends VarProductExtender(e) {
    type WSum = e._1.Width + e._2.Width
  }

  implicit class ValTuple2[T1 <: HasWidth, T2 <: HasWidth](val e : Tuple2[T1, T2])
    extends ValProductExtender(e){
    type WSum = e._1.Width + e._2.Width
  }
  /////////////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////////////
  // Tuple 3
  /////////////////////////////////////////////////////////////////////////////////////
  implicit class VarTuple3[T1 <: DFAny.Var, T2 <: DFAny.Var, T3 <: DFAny.Var](val e : Tuple3[T1, T2, T3])
    extends VarProductExtender(e) {
    type WSum = e._1.Width + e._2.Width + e._3.Width
  }

  implicit class ValTuple3[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth](val e : Tuple3[T1, T2, T3])
    extends ValProductExtender(e){
    type WSum = e._1.Width + e._2.Width + e._3.Width
  }
  /////////////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////////////
  // Tuple 4
  /////////////////////////////////////////////////////////////////////////////////////
  implicit class VarTuple4[T1 <: DFAny.Var, T2 <: DFAny.Var, T3 <: DFAny.Var, T4 <: DFAny.Var](val e : Tuple4[T1, T2, T3, T4])
    extends VarProductExtender(e) {
    type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
  }

  implicit class ValTuple4[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth](val e : Tuple4[T1, T2, T3, T4])
    extends ValProductExtender(e){
    type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width
  }
  /////////////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////////////
  // Tuple 5
  /////////////////////////////////////////////////////////////////////////////////////
  implicit class VarTuple5[T1 <: DFAny.Var, T2 <: DFAny.Var, T3 <: DFAny.Var, T4 <: DFAny.Var, T5 <: DFAny.Var](val e : Tuple5[T1, T2, T3, T4, T5])
    extends VarProductExtender(e) {
    type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
  }

  implicit class ValTuple5[T1 <: HasWidth, T2 <: HasWidth, T3 <: HasWidth, T4 <: HasWidth, T5 <: HasWidth](val e : Tuple5[T1, T2, T3, T4, T5])
    extends ValProductExtender(e){
    type WSum = e._1.Width + e._2.Width + e._3.Width + e._4.Width + e._5.Width
  }
  /////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
}




