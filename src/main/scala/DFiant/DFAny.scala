package DFiant

import DFiant.basiclib.DFBasicLib
import DFiant.internals._
import singleton.ops._
import singleton.twoface._
import scodec.bits._

import scala.collection.mutable.ListBuffer

sealed trait DFAny extends DSLOwnableConstruct {
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
  final protected def protBit[I](relBit : TwoFace.Int[I])(implicit ctx : DFAny.Alias.Context) : TBool =
    DFBool.alias(this, relBit).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit ctx : DFAny.Alias.Context) : TBool =
    protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit relBit : BitIndex.Checked[I, Width], ctx : DFAny.Alias.Context, di : DummyImplicit) : TBool =
    protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits()(implicit ctx : DFAny.Alias.Context) : TBits[Width] = DFBits.alias(this, width, 0).asInstanceOf[TBits[Width]]

  final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) : TBits[relWidth.Out] = DFBits.alias(this, relWidth(relBitHigh, relBitLow), relBitLow).asInstanceOf[TBits[relWidth.Out]]

  final def bits[H, L](relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width])(
    implicit checkHiLow : BitsHiLo.CheckedShell[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) = {
    checkHiLow.unsafeCheck(relBitHigh, relBitLow)
    protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))
  }

  final def bits[H, L](implicit relBitHigh : BitIndex.Checked[H, Width], relBitLow : BitIndex.Checked[L, Width],
    checkHiLow : BitsHiLo.Checked[H, L], relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context, di : DummyImplicit
  ) = protBits(relBitHigh.unsafeCheck(width), relBitLow.unsafeCheck(width))

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
  : TBits[W] = DFBits.alias(this, relWidth, relBitLow).asInstanceOf[TBits[W]]

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
  final protected def getInit : Seq[TToken] = protInit
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
  lazy val fullName : String = s"${owner.fullName}.$name"
  def codeString : String
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
  implicit val owner : DFAnyOwner
  final implicit protected lazy val protAlmanac : Almanac = owner.protAlmanac
  protected[DFiant] val almanacEntry : AlmanacEntryNamed
  final protected[DFiant] def getCurrentEntry : AlmanacEntryGetDFVar = AlmanacEntryGetDFVar(almanacEntry)
  val isPort : Boolean
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Simulation
  //////////////////////////////////////////////////////////////////////////
  def simInject(that : BigInt) : Boolean = almanacEntry.simInject(that)
  def simWatch : BigInt = ???
  //////////////////////////////////////////////////////////////////////////

  def casedf(a: TVal)(block : => Unit)(implicit ctx : DFAny.Op.Context) : DFCase[TVal] = {
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
    ) = assign(op(left, right), ctx.owner)
    final protected var assigned : Boolean = false
    protected[DFiant] def assign(that : DFAny, owner : DFBlock)(implicit ctx : DFAny.Op.Context) : TVar = {
      assigned = true
      //TODO: fix check with if hierarchy
//      if (this.owner ne owner) throw new IllegalArgumentException(s"Target assignment variable (${this.fullName}) is not at the same design as this assignment call (${owner.fullName})")
      protAssignDependencies += AssignPlaceholder(this, that)
      protAssignDependencies += that
      AlmanacEntryAssign(this.almanacEntry, that.getCurrentEntry)
      this.asInstanceOf[TVar]
    }
    //////////////////////////////////////////////////////////////////////////
  }

  trait Uninitialized extends DFAny {
    type TPostInit <: TVal
    final def init(that : protComp.Init.Able[TVal]*)(
      implicit op : protComp.Init.Builder[TVal, TToken], ctx : Alias.Context
    ) : TPostInit = {
      initialize(() => op(left, that), ctx.owner)
      this.asInstanceOf[TPostInit]
    }
    final private var initialized : Boolean = false
    final protected def initialize(updatedInit : () => Seq[TToken], owner : DFAnyOwner) : Unit = {
      if (initialized) throw new IllegalArgumentException(s"${this.fullName} already initialized")
      if (this.owner ne owner) throw new IllegalArgumentException(s"Initialization of variable (${this.fullName}) is not at the same design as this call (${owner.fullName})")
      initialized = true
      setInitFunc(updatedInit)
    }
    final def reInit(cond : DFBool) : Unit = ???
    final private var _initFunc : () => Seq[TToken] = () => Seq()
    final protected def setInitFunc(value : () => Seq[TToken]) : Unit = _initFunc = value
    final private val initLB = LazyBox(_initFunc())
    final protected lazy val protInit : Seq[TToken] = initLB getOrElse(throw new IllegalArgumentException("Ciruclar initialization detected"))
    final def initCodeString : String = if (initialized) s"init${protInit.codeString}" else ""
  }

  case class ConnectorPlaceholder(toPort : DFAny, fromVal : DFAny)(implicit ctx : DFAny.Op.Context) extends DSLOwnableConstruct {
    def relativeRef(dfVal : DFAny) : String = {
      //TODO: fix for the general case
      if (ctx.owner eq dfVal.owner) dfVal.name
      else s"${dfVal.owner.name}.${dfVal.name}"
    }
    final implicit val owner : DFAnyOwner = ctx.owner
    def codeString : String = s"${relativeRef(toPort)} <> ${relativeRef(fromVal)}"
    final val id = getID
  }

  case class AssignPlaceholder(toVar : DFAny, fromVal : DFAny)(implicit ctx : DFAny.Op.Context) extends DSLOwnableConstruct {
    final implicit val owner : DFAnyOwner = ctx.owner
    def codeString : String = s"${toVar.name} := ${fromVal.name}"
    final val id = getID
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar(_width : Int)(
    implicit ctx : NewVar.Context, cmp : Companion
  ) extends DFAny.Var with DFAny.Uninitialized {
    type TPostInit = TVar
    final implicit val owner : DFAnyOwner = ctx.owner
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    protected def constructCodeString : String
    final def codeString : String = s"val $name = $constructCodeString $initCodeString\n"
    final protected[DFiant] lazy val almanacEntry = AlmanacEntryNewDFVar(width, protInit, name, codeString)
    //final protected[DFiant] def discovery : Unit = almanacEntry
    final val isPort = false
    final val isAnonymous : Boolean = ctx.n.value == "$anon"
    //Port Construction
    //TODO: Implement generically after upgrading to 2.13.0-M5
    //Also see https://github.com/scala/bug/issues/11026
    //    def <> [Dir <: DFDir](dir : Dir)(implicit port : protComp.Port.Builder[TVal, Dir])
    //     : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    override protected def nameDefault: String = {
      if (isAnonymous) "$" + s"anon$id"
      else ctx.n.value
    }
    final val id = getID
  }
  object NewVar {
    type Context = DFAnyOwner.Context[DFBlock]
  }

  abstract class Alias(aliasedVar : DFAny, relWidth : Int, relBitLow : Int, deltaStep : Int = 0, updatedInit : Seq[Token] = Seq())(
    implicit ctx : Alias.Context, cmp : Companion
  ) extends DFAny.Var {
    final implicit val owner : DFAnyOwner = ctx.owner
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](relWidth)
    protected def protTokenBitsToTToken(token : DFBits.Token) : TToken
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = {
      val initTemp : Seq[Token] = if (updatedInit.isEmpty) aliasedVar.getInit else updatedInit
      val prevInit = if (deltaStep < 0) initTemp.prevInit(-deltaStep) else initTemp //TODO: What happens for `next`?
      val bitsInit = prevInit.bitsWL(relWidth, relBitLow)
      bitsInit.map(protTokenBitsToTToken)
    }
    protected def constructCodeString : String
    final def codeString : String = s"val $name = $constructCodeString\n"
    final protected[DFiant] lazy val almanacEntry = {
      val timeRef = aliasedVar.almanacEntry.timeRef.stepBy(deltaStep)
      AlmanacEntryAliasDFVar(aliasedVar.almanacEntry, BitsRange(relBitLow + relWidth - 1, relBitLow), timeRef, protInit, name, codeString)
    }
    //final protected[DFiant] def discovery : Unit = almanacEntry
    final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies :+ aliasedVar
    final val isPort = false

    final val isAnonymous : Boolean = ctx.n.value == "$anon"
    private lazy val derivedName : String = if (deltaStep < 0) s"${aliasedVar.fullName}__prev${-deltaStep}"
                                           else s"${aliasedVar.fullName}__???"
    override protected def nameDefault: String =
      if (isAnonymous) "$" + s"anon$id" + "$$" + derivedName else ctx.n.value
    final val id = getID
  }
  object Alias {
    type Context = DFAnyOwner.Context[DFAnyOwner]
  }

  abstract class Const(token : Token)(
    implicit ctx : Const.Context, cmp : Companion
  ) extends DFAny {
    final implicit val owner : DFAnyOwner = ctx.owner
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](token.width)
    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    final protected lazy val protInit : Seq[TToken] = Seq(token).asInstanceOf[Seq[TToken]]
    final def codeString : String = s"$token\n"
    final protected[DFiant] lazy val almanacEntry = AlmanacEntryConst(token, name, codeString)
    //final protected[DFiant] def discovery : Unit = almanacEntry
    final val isPort = false
    final val isAnonymous : Boolean = false
    override protected def nameDefault: String = s"$token"
    final val id = getID
  }
  object Const {
    type Context = DFAnyOwner.Context[DFBlock]
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Port
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Port[DF <: DFAny, Dir <: DFDir](dfVar : DF, val dir : Dir)(
    implicit ctx : Port.Context, cmp : Companion
  ) extends DFAny.Var with DFAny.Uninitialized {
    this : DF <> Dir =>
    type TPostInit = TVal <> Dir
    type TDir = Dir
    final implicit val owner : DFInterface = ctx.owner
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](dfVar.width)

    final protected val protComp : TCompanion = cmp.asInstanceOf[TCompanion]
    private var connectedSource : Option[DFAny] = None
    private val almanacEntryLB = LazyBox {
      val sourceEntry = if (connectedSource.isDefined) Some(connectedSource.get.almanacEntry) else None
      AlmanacEntryPort(width, protInit, sourceEntry, dir, name, codeString)
    }
    final protected[DFiant] lazy val almanacEntry = almanacEntryLB.getOrElse(throw new IllegalArgumentException("Circular dependency detected"))
    //final protected[DFiant] def discovery : Unit = almanacEntry
    final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies
    protected def connected : Boolean = connectedSource.isDefined
    final override protected[DFiant] def assign(that : DFAny, owner : DFBlock)(implicit ctx : DFAny.Op.Context) : TVar = {
      if (this.connected) throw new IllegalArgumentException(s"Target assignment port ${this.fullName} was already connected to. Cannot apply both := and <> operators on a port.")
      super.assign(that, owner)
    }
    private def connect(fromVal : DFAny, toPort :Port[_ <: DFAny,_ <: DFDir])(implicit ctx : DFAny.Op.Context) : Unit = {
      //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"$msg\nAttempted connection: ${fromVal.fullName} <> ${toPort.fullName}")
      if (toPort.width < fromVal.width) throwConnectionError(s"Target port width (${toPort.width}) is smaller than source port width (${fromVal.width}).")
      if (toPort.connected) throwConnectionError(s"Target port ${toPort.fullName} already has a connection: ${toPort.connectedSource.get.fullName}")
      if (toPort.assigned) throwConnectionError(s"Target port ${toPort.fullName} was already assigned to. Cannot apply both := and <> operators on a port.")
      //All is well. We can now connect fromVal->toPort
      toPort.setInitFunc(() => fromVal.getInit.asInstanceOf[Seq[toPort.TToken]])
      toPort.connectedSource = Some(fromVal)
      toPort.protAssignDependencies += ConnectorPlaceholder(toPort, fromVal)
      toPort.protAssignDependencies += fromVal
    }
    private def connectPort2Port(right : Port[_ <: DFAny,_ <: DFDir], owner : DFBlock)(implicit ctx : DFAny.Op.Context) : Unit = {
      val left = this
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"$msg\nAttempted connection: ${this.fullName} <> ${right.fullName}")
      val (fromPort, toPort) = (left.owner, right.owner, owner) match {
        //Ports in the same design, connected at the same design
        case (lDsn, rDsn, cDsn) if (lDsn eq rDsn) && (lDsn eq cDsn) => (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect two input ports of the same design.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect two output ports of the same design.")
          case (ld : IN,  rd : OUT) => (left, right)
          case (ld : OUT, rd : IN)  => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Ports in the same design, connected at the design's owner.
        //This is a loopback connection from a design's output to one of its inputs
        case (lDsn, rDsn, cDsn) if (lDsn eq rDsn) && (lDsn.owner eq cDsn) => (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect two input ports of the same design.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect two output ports of the same design.")
          case (ld : IN,  rd : OUT) => (right, left)
          case (ld : OUT, rd : IN)  => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is left and child port is right.
        case (lDsn, rDsn, cDsn) if rDsn.owner!=null && (lDsn eq rDsn.owner) && (lDsn eq cDsn) => (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (left, right)
          case (ld : OUT, rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is right and child port is left.
        case (lDsn, rDsn, cDsn) if lDsn.owner!=null && (lDsn.owner eq rDsn) && (rDsn eq cDsn) => (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (right, left)
          case (ld : OUT, rd : OUT) => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting sibling designs.
        case (lDsn, rDsn, cDsn) if lDsn.owner!=null && rDsn.owner!=null && (lDsn.owner eq rDsn.owner) && (lDsn.owner eq cDsn) => (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : IN)  => (left, right)
          case (ld : IN,  rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        case (l, r, c) if !((l eq r) || (l.owner!=null && (l.owner eq r)) || (r.owner!=null && (r.owner eq l)) || (l.owner!=null && r.owner!=null && (l.owner eq r.owner))) =>
          throwConnectionError(s"Connection must be made between ports that are either in the same design, or in a design and its owner, or between two design siblings.")
        case (l, r, c) if !((l eq c) || (r eq c) || (l.owner!=null && r.owner!=null && (l.owner eq c) && (r.owner eq c))) =>
          throwConnectionError(s"The connection call must be placed at the same design as one of the ports or their mutual owner. Call placed at ${owner.fullName}")
        case _ => throwConnectionError("Unexpected connection error")
      }
      connect(fromPort, toPort)
    }
    final def <> [RDIR <: DFDir](right: DF <> RDIR)(implicit ctx : DFAny.Op.Context) : Unit = connectPort2Port(right, ctx.owner)
    final protected[DFiant] def connectVal2Port(dfVal : DFAny, owner : DFBlock)(implicit ctx : DFAny.Op.Context) : Unit = {
      val port = this
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"$msg\nAttempted connection: ${port.fullName} <> ${dfVal.fullName}")
      if (dfVal.isPort) connectPort2Port(dfVal.asInstanceOf[Port[_ <: DFAny, _ <: DFDir]], owner)
      else {
        if (port.owner.owner!=null && (port.owner.owner eq dfVal.owner)) {
          if (port.dir.isOut) throwConnectionError(s"Cannot connect an external non-port value to an output port.")
          if (owner ne dfVal.owner) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${owner.fullName}")
        }
        else if (port.owner eq dfVal.owner) {
          if (port.dir.isIn) throwConnectionError(s"Cannot connect an internal non-port value to an input port.")
          if (owner ne dfVal.owner) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${owner.fullName}")
        }
        else throwConnectionError(s"Unsupported connection between a non-port and a port")
        connect(dfVal, port)
      }
    }
    final def <> [R](right: protComp.Op.Able[R])(
      implicit op: protComp.`Op<>`.Builder[TVal, R], ctx : DFAny.Op.Context
    ) : Unit = connectVal2Port(op(left, right), ctx.owner)
    //Connection should be constrained accordingly:
    //* For IN ports, supported: All Op:= operations, and TOP
    //* For OUT ports, supported only TVar and TOP
    final val isPort = true
    protected def constructCodeString : String
    final def codeString : String = s"val $name = $constructCodeString <> $dir $initCodeString\n"
    override protected def nameDefault: String = ctx.n.value
    override def toString : String = s"$fullName : $typeName <> $dir"
    final val isAnonymous : Boolean = false
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
      def codeString : String = tokenSeq.mkString("(", ", ", ")")
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
    type Context = DFAnyOwner.ContextWithLib
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
      type Builder[L <: DFAny, Dir <: DFDir] <: DFAny.Port.Builder[L, Dir]
    }
    val Port : Port
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    trait Op {
      type Able[R] <: DFAny.Op.Able[R]
      type Implicits
      val Able : Implicits
    }
    val Op : Op
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
    trait Init {
      type Able[L <: DFAny] <: DFAny.Init.Able[L]
      type Builder[L <: DFAny, Token <: DFAny.Token] <: DFAny.Init.Builder[L, Able, Token]
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




