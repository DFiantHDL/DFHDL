package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._

import scala.collection.mutable.ListBuffer

trait DFAny extends DFAnyMember with HasWidth {
  type TUnbounded <: DFAny
  type TVal <: TUnbounded
  type TVar <: TVal with DFAny.Var
  type TAlias <: TVal
  type TBool <: DFBool
  type TBits[W2] <: DFBits[W2]
  type TUInt[W2] <: DFUInt[W2]
  type TSInt[W2] <: DFSInt[W2]
  type TCompanion <: DFAny.Companion
  type TToken <: DFAny.Token
  type TPattern <: DFAny.Pattern[TPattern]
  type TPatternAble[+R] <: DFAny.Pattern.Able[R]
  type TPatternBuilder[L <: DFAny] <: DFAny.Pattern.Builder[L, TPatternAble]
  type OpAble[R] <: DFAny.Op.Able[R]
  type `Op<>Builder`[R] <: DFAny.Op.Builder[TVal, R]
  type `Op:=Builder`[R] <: DFAny.Op.Builder[TVal, R]
  type `Op==Builder`[R] <: DFAny.`Op==Builder`[TVal, R]
  type `Op!=Builder`[R] <: DFAny.`Op!=Builder`[TVal, R]
  type InitAble[L <: DFAny] <: DFAny.Init.Able[L]
  type InitBuilder <: DFAny.Init.Builder[TVal, InitAble, TToken]
  type PortBuilder[Dir <: DFDir] <: DFAny.Port.Builder[TVal, Dir]
//  type TUInt <: DFUInt
  val width : TwoFace.Int[Width]
  type ThisOwner <: DFAnyOwner
  final protected[DFiant] val tVal = this.asInstanceOf[TVal]
  final protected[DFiant] val left = tVal

  trait __DevDFAny extends super.__DevDFAnyMember {

  }
  override val __dev : __DevDFAny = new __DevDFAny {}
  import __dev._

  //////////////////////////////////////////////////////////////////////////
  // Single bit (Bool) selection
  //////////////////////////////////////////////////////////////////////////
  final protected def protBit[I](relBit : TwoFace.Int[I])(implicit ctx : DFAny.Alias.Context) : TBool =
    new DFBool.Alias(DFAny.Alias.Reference.BitsWL(this, 1, relBit, s".bit($relBit)")).asInstanceOf[TBool]

  final def bit[I](relBit : BitIndex.Checked[I, Width])(implicit ctx : DFAny.Alias.Context) : TBool =
    protBit(relBit.unsafeCheck(width))
  final def bit[I](implicit relBit : BitIndex.Checked[I, Width], ctx : DFAny.Alias.Context, di : DummyImplicit) : TBool =
    protBit(relBit.unsafeCheck(width))
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Bit range selection
  //////////////////////////////////////////////////////////////////////////
  final def bits(implicit ctx : DFAny.Alias.Context) : TBits[Width] =
    new DFBits.Alias[Width](DFAny.Alias.Reference.BitsWL(this, width, 0, ".bits")).asInstanceOf[TBits[Width]]

  final protected def protBits[H, L](relBitHigh : TwoFace.Int[H], relBitLow : TwoFace.Int[L])(
    implicit relWidth : RelWidth.TF[H, L], ctx : DFAny.Alias.Context
  ) : TBits[relWidth.Out] =
    new DFBits.Alias[relWidth.Out](DFAny.Alias.Reference.BitsWL(this, relWidth(relBitHigh, relBitLow), relBitLow, s".bits($relBitHigh, $relBitLow)")).asInstanceOf[TBits[relWidth.Out]]

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
  : TBits[W] = new DFBits.Alias[W](DFAny.Alias.Reference.BitsWL(this, relWidth, relBitLow, s".bits($relWidth, $relBitLow)")).asInstanceOf[TBits[W]]

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
  protected[DFiant] val initLB : LazyBox[Seq[TToken]]
  protected[DFiant] val constLB : LazyBox[TToken]
  final def isConstant : Boolean = !constLB.get.isBubble
  private[DFiant] var privRefCount : Int = 0
  private[DFiant] def pipeGet : Int = 0
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Prev
  //////////////////////////////////////////////////////////////////////////
  final protected[DFiant] def protPrev(step : Int)(implicit ctx : DFAny.Alias.Context)
  : TVal = alias(DFAny.Alias.Reference.Prev(this, step))
  final def prev()(implicit ctx : DFAny.Alias.Context) : TVal = protPrev(1)
  final def prev[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Alias.Context) : TVal =
    protPrev(step)
  private[DFiant] var maxPrevUse = 0 //TODO: hack. Remove this
  //////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  // Pipe
  //////////////////////////////////////////////////////////////////////////
  final protected[DFiant] def protPipe(step : Int)(implicit ctx : DFAny.Alias.Context)
  : TVal = alias(DFAny.Alias.Reference.Pipe(this, step))
  final def pipe()(implicit ctx : DFAny.Alias.Context) : TVal = protPipe(1)
  final def pipe[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Alias.Context) : TVal =
    protPipe(step)
  final def pipeBreak : TVal = ???
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

  private[DFiant] def consume(fromRelWidth : Int, fromRelBitLow : Int) : Unit = {
    //Do Nothing
    //TODO: consider adding stuff here
  }
  def consume() : TAlias = {
    consume(width, 0)
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
  protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) : TAlias
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Naming
  //////////////////////////////////////////////////////////////////////////
  final def isAnonymous : Boolean = name.startsWith(Name.AnonStart) //|| isInstanceOf[DSLFoldableOwnerConstruct]
  final override private[DFiant] def nameDefault: String = ctx.getName
  private var autoConstructCodeStringFunc : () => String = () => ""
  private lazy val autoConstructCodeString : String = autoConstructCodeStringFunc()
  final private[DFiant] def setAutoConstructCodeString(cs : => String) : this.type = {autoConstructCodeStringFunc = () => cs; this}
  private[DFiant] def constructCodeStringDefault : String
  private[DFiant] def showAnonymous : Boolean = config.showAnonymousEntries || this.isInstanceOf[DFAny.NewVar[_]]
  private def constructCodeString : String =
    if (autoConstructCodeString.isEmpty || showAnonymous) constructCodeStringDefault else autoConstructCodeString
  override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = {
    val ref = if (isAnonymous && !showAnonymous) relativeName(constructCodeString)(callOwner) else relativeName(callOwner)
    ref.applyBrackets() //TODO: consider other way instead of this hack
  }
  private def initCommentString : String =
    if (config.commentInitValues || owner.privShowInits) s"//init = ${initLB.get.codeString}" else ""
  private def latencyCommentString : String =
    if (config.commentLatencyValues || owner.privShowLatencies) s"//latency = ${thisSourceLB.get.latencyString}" else ""
  private def connCommentString : String =
    if (config.commentConnection || owner.privShowConnections) s"//conn = ${getFoldedSource.refCodeString}" else ""
  private def valCodeString : String = s"\nval $name = $constructCodeString"
  def codeString : String = f"$valCodeString%-60s$initCommentString$latencyCommentString$connCommentString"
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Equality
  //////////////////////////////////////////////////////////////////////////
  final def == [R <: TUnbounded](right : R)(implicit op: `Op==Builder`[right.TVal]) = op(left, right.tVal)
  final def != [R <: TUnbounded](right : R)(implicit op: `Op!=Builder`[right.TVal]) = op(left, right.tVal)
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Administration
  //////////////////////////////////////////////////////////////////////////
  private[DFiant] def thisSourceLB : LazyBox[Source] =
    LazyBox.Args1[Source, Source](this)(f => f.copyWithNewDFVal(this), inletSourceLB)
  private[DFiant] lazy val prevSourceLB : LazyBox[Source] =
    LazyBox.Const[Source](this)(Source.zeroLatency(this).prev(1))
  private[DFiant] def inletSourceLB : LazyBox[Source]
  final private[DFiant] lazy val getFoldedSource : Source = inletSourceLB.get
  val isPort : Boolean
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

    trait __DevDFAnyVar extends super.__DevDFAny {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private[DFiant] val protAssignDependencies : ListBuffer[Discoverable] = ListBuffer.empty[Discoverable]
      override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ protAssignDependencies.toList
    }
    override val __dev : __DevDFAnyVar = new __DevDFAnyVar {}
    import __dev._

    //////////////////////////////////////////////////////////////////////////
    // Future Stuff
    //////////////////////////////////////////////////////////////////////////
    final def dontProduce() : TAlias = {
      ???
      this.asInstanceOf[TAlias]
    }
    final def isNotFull : DFBool = ???
//
//    final def assignNext(step : Int, that : TVal) : Unit = ???
//    final def assignNext(step : Int, that : BigInt) : Unit = ???
//    final def <-- (that : Iterable[TVal]) : TVar = {
//      that.zipWithIndex.foreach{case (e, i) => this.assignNext(i, e)}
//      this.asInstanceOf[TVar]
//    }
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Assignment (Mutation)
    //////////////////////////////////////////////////////////////////////////
    private[DFiant] type MustBeOut = RequireMsg[![ImplicitFound[TDir <:< IN]], "Cannot assign to an input port"]
    final def := [R](right: OpAble[R])(
      implicit dir : MustBeOut, op: `Op:=Builder`[R], ctx : DFAny.Op.Context
    ) = assign(op(left, right))
    final private[DFiant] def isAssigned : Boolean = assignedIndication.nonEmpty
    private[DFiant] val assignedIndication = collection.mutable.BitSet.empty
    private[DFiant] lazy val assignedSourceLB = LazyBox.Mutable[Source](this)(Source.none(width))

    override private[DFiant] def consume(fromRelWidth : Int, fromRelBitLow : Int) : Unit = {
      val fromRelBitHigh = fromRelBitLow + fromRelWidth - 1
      val fromBitSet = collection.immutable.BitSet.empty ++ (fromRelBitLow to fromRelBitHigh)

      if ((assignedIndication | fromBitSet) != assignedIndication) //not all used bits are assigned to
        maxPrevUse = scala.math.max(maxPrevUse, 1)
    }

    private[DFiant] def assign(toRelWidth : Int, toRelBitLow : Int, fromSourceLB : LazyBox[Source])(implicit ctx : DFAny.Op.Context) : Unit = {
      assignedSourceLB.set(LazyBox.Args2[Source, Source, Source](this)((t, f) => t.replaceWL(toRelWidth, toRelBitLow, f), assignedSourceLB.getBox, fromSourceLB))
    }
    protected[DFiant] def assign(toRelWidth : Int, toRelBitLow : Int, fromVal : DFAny)(implicit ctx : DFAny.Op.Context) : Unit = {
      val toVar = this
      //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
      val toRelBitHigh = toRelBitLow + toRelWidth-1
      val toBitSet = collection.immutable.BitSet.empty ++ (toRelBitLow to toRelBitHigh)
      if (!ctx.owner.callSiteSameAsOwnerOf(this))
        throw new IllegalArgumentException(s"\nTarget assignment variable (${this.fullName}) is not at the same design as this assignment call (${ctx.owner.fullName})")
      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted assignment: $toVar := $fromVal}")
      if (toRelWidth != fromVal.width.getValue) throwConnectionError(s"Target width ($toRelWidth) is different than source width (${fromVal.width}).")
//      fromVal match {
//        case x : Var  if ((collection.immutable.BitSet.empty ++ (0 until toRelWidth)) &~ x.assignedIndication).nonEmpty =>
//          x.maxPrevUse = scala.math.max(x.maxPrevUse, 1)
////          println(s"$fullName ${x.maxPrevUse}")
//      }
      fromVal.consume()
      assignedIndication ++= toRelBitLow to toRelBitHigh
      assign(toRelWidth, toRelBitLow, fromVal.thisSourceLB)
      protAssignDependencies += Assignment(toVar, fromVal)
      protAssignDependencies += fromVal
    }
    protected[DFiant] def assign(that : DFAny)(implicit ctx : DFAny.Op.Context) : Unit = {
      assign(width, 0, that)
    }
    //////////////////////////////////////////////////////////////////////////
  }


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // General Common Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Constructor[DF <: DFAny](_width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends DFAny {
    trait __DevConstructor extends super.__DevDFAny {

    }
    override val __dev : __DevConstructor = new __DevConstructor {}
    import __dev._
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Connectable Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Connectable[DF <: DFAny](width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Constructor[DF](width) with DFAny.Var {
    trait __Dev extends super.__DevConstructor with super.__DevDFAnyVar {

    }
    override val __dev : __Dev = new __Dev {}
    import __dev._

    final def <> [RDIR <: DFDir](right: TVal <> RDIR)(implicit ctx : Connector.Context) : Unit = right.connectVal2Port(this)
    private[DFiant] val connectedIndication = collection.mutable.BitSet.empty
    private[DFiant] lazy val connectedSourceLB = LazyBox.Mutable[Source](this)(Source.none(width))
    override private[DFiant] def inletSourceLB : LazyBox[Source] =
      LazyBox.Args3[Source, Source, Source, Source](this)((c, a, p) => c orElse a orElse p, connectedSourceLB, assignedSourceLB.getBox, prevSourceLB)

    final private[DFiant] def connectFrom(toRelWidth : Int, toRelBitLow : Int, fromVal : DFAny)(implicit ctx : Connector.Context) : Unit = {
      val toVar = this
      //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
      val toRelBitHigh = toRelBitLow + toRelWidth-1
      val toBitSet = collection.immutable.BitSet.empty ++ (toRelBitLow to toRelBitHigh)

      def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${toVar.fullName} <> ${fromVal.fullName}")
      if (fromVal.width != toVar.width) throwConnectionError(s"Target width (${toVar.width}) is different than source width (${fromVal.width}).")
      if ((connectedIndication & toBitSet).nonEmpty) throwConnectionError(s"Target ${toVar.fullName} already has a connection: ${connectedSourceLB.get}")
      if ((assignedIndication & toBitSet).nonEmpty) throwConnectionError(s"Target ${toVar.fullName} was already assigned to: ${connectedSourceLB.get}.\nCannot apply both := and <> operators for the same target")
      //All is well. We can now connect fromVal->toVar
      fromVal.consume()
      connectedIndication ++= toRelBitLow to toRelBitHigh
      connectedSourceLB.set(LazyBox.Args2[Source, Source, Source](this)((t, f) => t.replaceWL(toRelWidth, toRelBitLow, f), connectedSourceLB.getBox, fromVal.thisSourceLB))
    }
    private[DFiant] def connectFrom(fromVal : DFAny)(implicit ctx : Connector.Context) : Unit = {
      val toVar = this
      connectFrom(width, 0, fromVal)
      //All is well. We can now connect fromVal->toVar
      toVar.__dev.protAssignDependencies += Connector(toVar, fromVal)
      toVar.__dev.protAssignDependencies += fromVal
    }
    override protected[DFiant] def assign(toRelWidth : Int, toRelBitLow : Int, fromVal : DFAny)(implicit ctx : DFAny.Op.Context) : Unit = {
      val toVar = this
      val toRelBitHigh = toRelBitLow + toRelWidth-1
      val toBitSet = collection.immutable.BitSet.empty ++ (toRelBitLow to toRelBitHigh)
      def throwAssignmentError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted assignment: $toVar := $fromVal}")
      if ((connectedIndication & toBitSet).nonEmpty) throwAssignmentError(s"Target ${toVar.fullName} already has a connection: ${connectedSourceLB.get}.\nCannot apply both := and <> operators for the same target")
      super.assign(toRelWidth, toRelBitLow, fromVal)
    }
    final private[DFiant] def isConnected : Boolean = !connectedSourceLB.get.isEmpty

    //////////////////////////////////////////////////////////////////////////
    // Initialization
    //////////////////////////////////////////////////////////////////////////
    private def initFunc(source : Source) : Seq[TToken] = {
      val bitsTokenSeq : Seq[DFBits.Token] = source.elements.map(x =>
        x.aliasTag match {
        case Some(t) =>
          val selBits = t.dfVal.initLB.get.bitsWL(x.relWidth, x.relBitLow)
          val revBits = if (x.reverseBits) DFBits.Token.reverse(selBits) else selBits
          val invBits = if (t.inverted) DFBits.Token.unary_~(revBits) else revBits
          if (t.prevStep > 0) invBits.prevInit(t.prevStep) else invBits
        case None => Seq()
      }).reduce(DFBits.Token.concat)
      bitsTokenSeq.map(b => protTokenBitsToTToken(b).asInstanceOf[TToken])
    }

    private[DFiant] lazy val initSourceLB : LazyBox[Source] = connectedSourceLB
    private[DFiant] lazy val initConnectedLB : LazyBox[Seq[TToken]] =
      LazyBox.Args1[Seq[TToken], Source](this)(initFunc, initSourceLB)
    protected[DFiant] lazy val initLB : LazyBox[Seq[TToken]] = initConnectedLB
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Constant propagation
    //////////////////////////////////////////////////////////////////////////
    private def constFunc(source : Source) : TToken = {
      val bitsToken : DFBits.Token = source.elements.map(x =>
        x.aliasTag match {
        case Some(t) =>
          val prvBits = //TODO: fix this. For instance, a steady state token self assigned generator can be considered constant
            if (t.prevStep > 0) DFBits.Token(t.dfVal.width, Bubble)//t.dfVal.initLB.get.prevInit(t.prevStep-1).headOption.getOrElse(bubble)
            else t.dfVal.constLB.get
          val selBits = prvBits.bitsWL(x.relWidth, x.relBitLow)
          val revBits = if (x.reverseBits) selBits.reverse else selBits
          if (t.inverted) ~revBits else revBits
        case None => DFBits.Token(x.relWidth, Bubble)
      }).reduce((l, r) => l ## r)
      protTokenBitsToTToken(bitsToken).asInstanceOf[TToken]
    }
    protected[DFiant] lazy val constLB : LazyBox[TToken] =
      LazyBox.Args1[TToken, Source](this)(constFunc, inletSourceLB, Some(bubbleToken(this.asInstanceOf[DF]).asInstanceOf[TToken]))
    //////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Initializable Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Initializable[DF <: DFAny](width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Connectable[DF](width) {
    trait __Dev extends super.__Dev {

    }
    override val __dev : __Dev = new __Dev {}
    import __dev._

    type TPostInit <: TVal

    final def init(that : InitAble[TVal]*)(
      implicit op : InitBuilder, ctx : Alias.Context
    ) : TPostInit = {
      initialize(LazyBox.Const(this)(op(left, that)), ctx.owner)
      this.asInstanceOf[TPostInit]
    }
    private val initExternalLB = LazyBox.Mutable[Seq[TToken]](this)(Seq())

    //If there is a connection to the specific bits, then the initialization uses that connection.
    //Otherwise, the initialization uses the initialization set externally (via init or initialize)
    private def initFunc(connectedSource : Source, initConnected : Seq[TToken], initExternal : Seq[TToken]) : Seq[TToken] = {
      var lsbitPos : Int = width
      val bitsTokenSeq : Seq[DFBits.Token] = connectedSource.elements.map(x => {
        lsbitPos -= x.relWidth
        x.aliasTag match {
          case Some(t) => initConnected.bitsWL(x.relWidth, lsbitPos)
          case None => initExternal.bitsWL(x.relWidth, lsbitPos)
        }
      }).reduce(DFBits.Token.concat)
      bitsTokenSeq.map(b => protTokenBitsToTToken(b).asInstanceOf[TToken])
    }

    override protected[DFiant] lazy val initLB : LazyBox[Seq[TToken]] =
      LazyBox.Args3[Seq[TToken], Source, Seq[TToken], Seq[TToken]](this)(initFunc, initSourceLB, initConnectedLB, initExternalLB)

    private var updatedInit : () => Seq[TToken] = () => Seq() //just for codeString
    final protected[DFiant] def initialize(updatedInitLB : LazyBox[Seq[TToken]], owner : DFAnyOwner) : Unit = {
      if (initExternalLB.isSet) throw new IllegalArgumentException(s"${this.fullName} already initialized")
      if (this.nonTransparentOwner ne owner.nonTransparent) throw new IllegalArgumentException(s"\nInitialization of variable (${this.fullName}) is not at the same design as this call (${owner.fullName})")
      updatedInit = () => updatedInitLB.get
      initExternalLB.set(updatedInitLB)
    }
//    final def reInit(cond : DFBool) : Unit = ???
    private[DFiant] object setInitFunc {
      def forced(value : LazyBox[Seq[Token]]) : Unit = initExternalLB.set(value.asInstanceOf[LazyBox[Seq[TToken]]])
    }
    final def initCodeString : String = {
      val init = updatedInit()
      if (initExternalLB.isSet && init.nonEmpty) s" init${init.codeString}" else ""
    }
    //////////////////////////////////////////////////////////////////////////
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Connections and Assignments
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  case class Connector(toPort : DFAny, fromVal : DFAny)(implicit ctx0 : Connector.Context) extends DFAnyMember {
    final val ctx = ctx0
    override private[DFiant] def nameDefault = s"${Name.Separator}connect"
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
    final val ctx = ctx0
    override private[DFiant] def nameDefault = s"${Name.Separator}assign"
    def codeString : String = s"\n${toVar.refCodeString} := ${fromVal.refCodeString}"
    final val id = getID
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar[DF <: DFAny](width : Int, newVarCodeString : String)(
    implicit ctx0 : NewVar.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Initializable[DF](width) {
    trait __Dev extends super.__Dev {

    }
    override val __dev : __Dev = new __Dev {}
    import __dev._

    type TPostInit = TVar
    final val ctx = ctx0
    final private[DFiant] def constructCodeStringDefault : String = s"$newVarCodeString$initCodeString"
    final val isPort = false

    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : PortBuilder[Dir])
    : TVal <> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])
    final object selectdf extends ConditionalBlock.SelectWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])

//    def selectdf[T, E](cond : DFBool)(thenSel : protComp.Op.Able[T], elseSel : protComp.Op.Able[E]) : TVal = ???
//    def selectdf[SW, T](sel : DFUInt[SW], default : => Option[TVal] = None)(args : protComp.Op.Able[T]*) : TVal = ???
    final val id = getID
  }
  object NewVar {
    type Context = DFAnyOwner.Context[DFAnyOwner]
  }

  abstract class Alias[DF <: DFAny](val reference : DFAny.Alias.Reference)(
    implicit ctx0 : Alias.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Connectable[DF](reference.width) {
    trait __Dev extends super.__Dev {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies ++ reference.aliasedVars
    }
    override val __dev : __Dev = new __Dev {}
    import __dev._

    final val ctx = ctx0

    override private[DFiant] def inletSourceLB = reference.sourceLB

//    override private[DFiant] def inletSourceLB = thisSourceLB //TODO: Consider dealiasing
    override private[DFiant] lazy val initSourceLB : LazyBox[Source] = inletSourceLB

    final private[DFiant] def constructCodeStringDefault : String = reference.constructCodeString
    final val isPort = false

    final lazy val isAliasOfPort : Boolean = ???

    override def consume(): TAlias = {
      reference match {
        case DFAny.Alias.Reference.BitsWL(aliasedVar, relWidth, relBitLow) =>
          aliasedVar.consume(relWidth, relBitLow)
        case DFAny.Alias.Reference.Prev(aliasedVar, step) =>
          aliasedVar.maxPrevUse = scala.math.max(step, aliasedVar.maxPrevUse)
        case DFAny.Alias.Reference.Pipe(aliasedVar, step) =>
          //Do nothing
        case _ => reference.aliasedVars.map(a => a.consume())
      }
      this.asInstanceOf[TAlias]
    }

    override protected[DFiant] def assign(toRelWidth : Int, toRelBitLow : Int, fromSourceLB : LazyBox[Source])(implicit ctx : DFAny.Op.Context) : Unit = {
      val toVar = this
      val toRelBitHigh = toRelBitLow + toRelWidth-1
      case class absolute(alias : DFAny, high : Int, low : Int)
      //absolutes set as a tuple3 list of aliases with their absolute (high,low) coordinates
      val absolutes = reference.aliasedVars.foldLeft[List[absolute]](List()) {
        case (list, alias) if list.isEmpty => List(absolute(alias, reference.width - 1, reference.width - alias.width))
        case (list, alias) => list :+ absolute(alias, list.last.low - 1, list.last.low - alias.width)
      }
      val assignableAbsolutes = absolutes.filter(a => toRelBitHigh >= a.low || toRelBitLow <= a.high)
//      println(f"${s"$fullName($toRelBitHigh, $toRelBitLow)"}%-30s := ") //${fromVal.fullName}@${fromVal.width}
      assignableAbsolutes.foreach {
        case absolute(alias : DFAny.Port[_,_], high, low) if alias.dir.isIn =>
          throw new IllegalArgumentException(s"\nTarget assignment variable (${this.fullName}) is an immutable alias of an input port ${alias.fullName} at bits ($high, $low) and shouldn't be assigned")
        case absolute(alias : DFAny.Var, high, low) =>
          val partHigh = scala.math.min(high, toRelBitHigh)
          val partLow = scala.math.max(low, toRelBitLow)
          val fromWidth = partHigh - partLow + 1
          val fromLow = partLow + low
//          val partFromSourceLB = LazyBox.Args1[Source, Source](this)(f => f.bitsWL(fromWidth, fromLow), fromSourceLB)
//          println(s"Boom ${alias.fullName}(${fromWidth+fromLow-1}, $fromLow) := ")
          alias.assign(fromWidth, fromLow, fromSourceLB)
        case absolute(alias, high, low) =>
          throw new IllegalArgumentException(s"\nTarget assignment variable (${this.fullName}) is an immutable alias of ${alias.fullName} at bits ($high, $low) and shouldn't be assigned")
      }
    }
    final override protected[DFiant] def assign(that: DFAny)(implicit ctx: DFAny.Op.Context): Unit = {
      reference.aliasedVars.foreach{case a : DFAny.Var =>
        a.__dev.protAssignDependencies ++= List(this, that)
      } //TODO: fix dependency to bit accurate dependency?
      reference match {
        case DFAny.Alias.Reference.BitsWL(aliasedVar, relWidth, relBitLow) =>
          that.consume()
          assign(relWidth, relBitLow, that.inletSourceLB) //LazyBox.Args1[Source, Source](this)(f => f.bitsWL(relWidth, relBitLow), that.currentSourceLB)
        case DFAny.Alias.Reference.AsIs(aliasedVar) =>
          that.consume()
          assign(width, 0, that.inletSourceLB)
        case DFAny.Alias.Reference.Concat(aliasedVars) =>
          that.consume()
          assign(width, 0, that.inletSourceLB)
        case DFAny.Alias.Reference.BitReverse(aliasedVar) => ??? // assign(width, 0, that.reverse)
        case DFAny.Alias.Reference.Invert(aliasedVar) => ???
        case DFAny.Alias.Reference.SignExtend(aliasedVar, toWidth) => ???
        case _ => throw new IllegalArgumentException(s"\nTarget assignment variable (${this.fullName}) is an immutable alias and shouldn't be assigned")
      }
      protAssignDependencies += Assignment(this, that)
      protAssignDependencies += that
    }

    final val id = getID
  }
  object Alias {
    trait Tag
    type Context = DFAnyOwner.Context[DFAnyOwner]

    sealed abstract class Reference(aliasCodeString_ : => String) {
      val width : Int
      lazy val aliasCodeString : String = aliasCodeString_
      def constructCodeString(implicit owner : DSLOwnerConstruct) : String
      val aliasedVars : List[DFAny]
      val sourceLB : LazyBox[Source]
    }
    sealed abstract class SingleReference(val aliasedVar : DFAny, aliasCodeString : => String)
      extends Reference(aliasCodeString) {
      val width : Int = aliasedVar.width
      def constructCodeString(implicit owner : DSLOwnerConstruct) : String =
        s"${aliasedVar.refCodeString}$aliasCodeString"
      val aliasedVars : List[DFAny] = List(aliasedVar)
    }
    object Reference {
      class AsIs(aliasedVar : DFAny, aliasCodeString : => String)
        extends SingleReference(aliasedVar, aliasCodeString) {
        lazy val sourceLB: LazyBox[Source] = aliasedVar.thisSourceLB
      }
      object AsIs {
        def apply(aliasedVar : DFAny, aliasCodeString : => String) = new AsIs(aliasedVar, aliasCodeString)
        def unapply(arg: AsIs): Option[DFAny] = Some(arg.aliasedVar)
      }
      class Concat(val aliasedVars : List[DFAny], aliasCodeString : => String)
        extends Reference(aliasCodeString) {
        val width : Int = aliasedVars.map(a => a.width.getValue).sum
        def constructCodeString(implicit owner : DSLOwnerConstruct) : String =
          s"${aliasedVars.map(a => a.refCodeString).mkString("(",", ",")")}$aliasCodeString"
        //TODO: something with balancing upon reading a complete value
        //      val currentPipe: Pipe = aliasPipeBalance(pipeList.concat)
        lazy val sourceLB: LazyBox[Source] = LazyBox.ArgList[Source, Source](aliasedVars.head)(
          s => Source(s.flatMap(a => a.elements)).coalesce, aliasedVars.map(a => a.thisSourceLB))
      }
      object Concat {
        def apply(aliasedVars : List[DFAny], aliasCodeString : => String) = new Concat(aliasedVars, aliasCodeString)
        def unapply(arg: Concat): Option[List[DFAny]] = Some(arg.aliasedVars)
      }
      class BitsWL(aliasedVar : DFAny, val relWidth : Int, val relBitLow : Int, aliasCodeString : => String)
        extends SingleReference(aliasedVar, aliasCodeString) {
        override val width: Int = relWidth
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.bitsWL(relWidth, relBitLow), aliasedVar.thisSourceLB)
      }
      object BitsWL {
        def apply(aliasedVar : DFAny, relWidth: Int, relBitLow : Int, aliasCodeString : => String) =
          new BitsWL(aliasedVar, relWidth, relBitLow, aliasCodeString)
        def unapply(arg : BitsWL): Option[(DFAny, Int, Int)] = Some((arg.aliasedVar, arg.relWidth, arg.relBitLow))
      }
      class Prev(aliasedVar : DFAny, val step : Int)
        extends SingleReference(aliasedVar, if (step == 0) "" else if (step == 1) ".prev" else s".prev($step)") {
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.prev(step), aliasedVar.thisSourceLB)
      }
      object Prev {
        def apply(aliasedVar : DFAny, step : Int) = new Prev(aliasedVar, step)
        def unapply(arg: Prev): Option[(DFAny, Int)] = Some(arg.aliasedVar, arg.step)
      }
      class Pipe(aliasedVar : DFAny, val step : Int)
        extends SingleReference(aliasedVar, if (step == 0) "" else if (step == 1) ".pipe" else s".pipe($step)") {
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.pipe(step), aliasedVar.thisSourceLB)
      }
      object Pipe {
        def apply(aliasedVar : DFAny, step : Int) = new Pipe(aliasedVar, step)
        def unapply(arg: Pipe): Option[(DFAny, Int)] = Some(arg.aliasedVar, arg.step)
      }
//      class LeftShift(aliasedVar : DFAny, val shift : Int)
//        extends SingleReference(aliasedVar, if (shift == 0) "" else s"$shift") {
//        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
//          s => s.prev(shift), aliasedVar.thisSourceLB)
//      }
//      object LeftShift {
//        def apply(aliasedVar : DFAny, shift : Int) = new LeftShift(aliasedVar, shift)
//        def unapply(arg: LeftShift): Option[(DFAny, Int)] = Some(arg.aliasedVar, arg.shift)
//      }
      class SignExtend(aliasedVar : DFAny, val toWidth : Int)
        extends SingleReference(aliasedVar, if (toWidth == 0) "" else s".extendTo($toWidth)") {
        override val width: Int = toWidth
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.signExtend(toWidth), aliasedVar.thisSourceLB)
      }
      object SignExtend {
        def apply(aliasedVar : DFAny, toWidth : Int) = new SignExtend(aliasedVar, toWidth)
        def unapply(arg: SignExtend): Option[(DFAny, Int)] = Some(arg.aliasedVar, arg.toWidth)
      }
      class BitReverse(aliasedVar : DFAny, aliasCodeString : => String)
        extends SingleReference(aliasedVar, aliasCodeString) {
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.reverse, aliasedVar.thisSourceLB)
      }
      object BitReverse {
        def apply(aliasedVar : DFAny, aliasCodeString : => String) = new BitReverse(aliasedVar, aliasCodeString)
        def unapply(arg: BitReverse): Option[DFAny] = Some(arg.aliasedVar)
      }
      class Invert(aliasedVar : DFAny, aliasCodeString : => String)
        extends SingleReference(aliasedVar, aliasCodeString) {
        lazy val sourceLB: LazyBox[Source] = LazyBox.Args1[Source, Source](aliasedVar)(
          s => s.invert, aliasedVar.thisSourceLB)
      }
      object Invert {
        def apply(aliasedVar : DFAny, aliasCodeString : => String) = new Invert(aliasedVar, aliasCodeString)
        def unapply(arg: Invert): Option[DFAny] = Some(arg.aliasedVar)
      }
    }
  }

  abstract class Const[DF <: DFAny](token : Token)(
    implicit ctx0 : NewVar.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Constructor[DF](token.width) {
    trait __Dev extends super.__DevConstructor {

    }
    override val __dev : __Dev = new __Dev {}
    import __dev._

    final val ctx = ctx0
    final override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = constructCodeStringDefault
    private[DFiant] def constructCodeStringDefault : String = s"${token.codeString}"
    final protected[DFiant] lazy val initLB : LazyBox[Seq[TToken]] = LazyBox.Const(this)(Seq(token).asInstanceOf[Seq[TToken]])
    final protected[DFiant] lazy val constLB : LazyBox[TToken] = LazyBox.Const(this)(token.asInstanceOf[TToken])
    final private[DFiant] lazy val inletSourceLB : LazyBox[Source] = LazyBox.Const(this)(Source.withLatency(this, None))
    final override private[DFiant] lazy val thisSourceLB : LazyBox[Source] = LazyBox.Const(this)(Source.withLatency(this, None))
    override private[DFiant] lazy val prevSourceLB : LazyBox[Source] = LazyBox.Const[Source](this)(Source.withLatency(this, None))
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
    implicit ctx0 : Port.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends DFAny.Initializable[DF](dfVar.width) with CanBePiped {
    this : DF <> Dir =>
    type TPostInit = TVal <> Dir
    type TDir = Dir
    trait __Dev extends super.__Dev {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private[DFiant] def injectDependencies(dependencies : List[Discoverable]) : Unit = protAssignDependencies ++= dependencies
      final override protected def discoveryDepenencies : List[Discoverable] = super.discoveryDepenencies
    }
    override val __dev : __Dev = new __Dev {}
    import __dev._
    final val ctx = ctx0

    override private[DFiant] def inletSourceLB : LazyBox[Source] =
      if (dir.isIn && owner.isTop) LazyBox.Const[Source](this)(Source.none(width))
      else super.inletSourceLB

    override private[DFiant] def thisSourceLB : LazyBox[Source] =
      if (dir.isIn && owner.isTop) LazyBox.Const[Source](this)(Source.zeroLatency(this))
      else super.thisSourceLB

    override private[DFiant] def consume(fromRelWidth : Int, fromRelBitLow : Int) : Unit = {
      //Nothing happens when consuming an input port
      if (dir.isOut) super.consume(fromRelWidth, fromRelBitLow)
    }

    //    private var extraPipe : Int = 0
//    def pipe() : this.type = pipe(1)
//    final private[DFiant] override def pipeGet = extraPipe
//    final def pipe(p : Int) : this.type = {extraPipe = p; this}

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
        else if ((left hasSameOwnerAs right) && isConnectedAtOwnerOf(left.nonTransparentOwner)) (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect two input ports of the same design.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect two output ports of the same design.")
          case (ld : IN,  rd : OUT) => (right, left)
          case (ld : OUT, rd : IN)  => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is left and child port is right.
        else if (right.isDownstreamMemberOf(left.nonTransparentOwner) && isConnectedAtEitherSide(left, right)) (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN) if left.isAssigned => (left, right) //relaxation if the rule when the owner output port is already assigned to
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (left, right)
          case (ld : OUT, rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting owner and child design ports, while owner port is right and child port is left.
        else if (left.isDownstreamMemberOf(right.nonTransparentOwner) && isConnectedAtEitherSide(left, right)) (left.dir, right.dir) match {
          case (ld : IN,  rd : OUT) if right.isAssigned => (right, left)  //relaxation if the rule when the owner output port is already assigned to
          case (ld : IN,  rd : OUT) => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
          case (ld : IN,  rd : IN)  => (right, left)
          case (ld : OUT, rd : OUT) => (left, right)
          case _ => throwConnectionError("Unexpected connection error")
        }
        //Connecting sibling designs.
        else if ((left.nonTransparentOwner hasSameOwnerAs right.nonTransparentOwner) && isConnectedAtOwnerOf(left.nonTransparentOwner)) (left.dir, right.dir) match {
          case (ld : IN,  rd : IN)  => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : OUT) => throwConnectionError(s"Cannot connect ports with the same direction between sibling designs.")
          case (ld : OUT, rd : IN)  => (left, right)
          case (ld : IN,  rd : OUT) => (right, left)
          case _ => throwConnectionError("Unexpected connection error")
        }
        else if (!left.isDownstreamMemberOf(right.nonTransparentOwner) || !right.isDownstreamMemberOf(left.nonTransparentOwner))
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
      dfVal match {
        case p : Port[_,_] => p.connectPort2Port(port)
        case _ =>
          //Connecting external value from/to a output/input port
          if (port.owner.isDownstreamMemberOf(dfVal.nonTransparentOwner)) {
            if (!isConnectedAtEitherSide(dfVal, port)) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${ctx.owner.fullName}")
            //Connecting from output port to external value
            if (port.dir.isOut) dfVal match {
              case u : Initializable[_] => u.connectFrom(port)
//              case a : Alias[_] if a.isAliasOfPort => a.connect
              case _ => throwConnectionError(s"Cannot connect an external value to an output port.")
            }
            //Connecting from external value to input port
            else port.connectFrom(dfVal)
          }
          //Connecting internal value and output port
          else if (port hasSameOwnerAs dfVal) {
            if (port.dir.isIn) dfVal match {
              case u : Initializable[_] => u.connectFrom(port)
              case _ => throwConnectionError(s"Cannot connect an internal non-port value to an input port.")
            } else {
              if (ctx.owner.nonTransparent ne dfVal.nonTransparentOwner) throwConnectionError(s"The connection call must be placed at the same design as the source non-port side. Call placed at ${ctx.owner.fullName}")
              port.connectFrom(dfVal)
            }
          }
          else throwConnectionError(s"Unsupported connection between a non-port and a port, ${ctx.owner.fullName}")
      }
    }
    final def <> [R](right: OpAble[R])(
      implicit ctx : DFAny.Connector.Context, op: `Op<>Builder`[R]
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
    type TToken <: Token
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
    final def replaceWL(relWidth : Int, relBitLow : Int, replacement : DFBits.Token)(
      implicit fromBits : DFBits.Token => TToken
    ) : TToken = {
      val leftWidth = width - (relBitLow + relWidth)
      val leftBitLow = relBitLow + relWidth
      val rightWidth = relBitLow
      val rightBitLow = 0
      val leftOption : Option[DFBits.Token] = if (leftWidth > 0) Some(bitsWL(leftWidth, leftBitLow)) else None
      val rightOption : Option[DFBits.Token] = if (rightWidth > 0) Some(bitsWL(rightWidth, rightBitLow)) else None
      fromBits(List(leftOption, Some(replacement), rightOption).flatten.reduce((l, r) => l ## r))
    }
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
    abstract class Of[V, P <: DFAny.Pattern[P]{type TValue = V}](implicit codeStringOf : CodeStringOf[V]) extends Token {
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
//      def tokenAt(step : Int)(implicit bubbleOf : [T]) : T = prevInit(step - 1).headOption.getOrElse(DFBits.Token(t.dfVal.width, Bubble))
      def bits : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bits)
      def bitsWL(relWidth : Int, relBitLow : Int) : Seq[DFBits.Token] =
        tokenSeq.map(t => t.bitsWL(relWidth, relBitLow))
      def replaceWL(relWidth : Int, relBitLow : Int, replacement : Seq[DFBits.Token])(
        implicit fromBits : DFBits.Token => T
      ) : Seq[T] = TokenSeq(tokenSeq, replacement)((t, r) => t.replaceWL(relWidth, relBitLow, r)(fromBits.asInstanceOf[DFBits.Token => t.TToken]).asInstanceOf[T])
      def codeString : String = tokenSeq.map(t => t.codeString).mkString("(", ", ", ")")
      def patternMatch(pattern : T#TPattern) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
    }
    def patternMatch[T <: Token, P <: Pattern[_]](tokenSeq : Seq[T], pattern : P) : Seq[DFBool.Token] = TokenSeq(tokenSeq, pattern)((l, r) => l.patternMatch(r.asInstanceOf[l.TPattern]))
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
    def apply[O <: Token, T <: Token, L <: Token](seq : Seq[T], list : List[Seq[L]])(op : (T, List[L]) => O) : Seq[O] = ???
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
  type `Op==Builder`[L, R] = Op.Builder[L, R]{type Comp = DFBool with CanBePiped}
  type `Op!=Builder`[L, R] = Op.Builder[L, R]{type Comp = DFBool with CanBePiped}
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
      type Builder[L, R] <: DFAny.`Op==Builder`[L, R]
    }
    val `Op==` : `Op==`
    trait `Op!=` {
      type Builder[L, R] <: DFAny.`Op!=Builder`[L, R]
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
      new DFBits.Alias[w.Out](DFAny.Alias.Reference.Concat(e.productIterator.toList.asInstanceOf[List[DFAny]], ".bits"))
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
      new DFBits.Alias[w.Out](DFAny.Alias.Reference.Concat(list, ".bits"))
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




