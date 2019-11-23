/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package DFiant

import DFiant.internals._
import singleton.ops._
import singleton.twoface._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable

trait DFAny extends DFAnyMember with HasWidth {self =>
  protected[DFiant] type TUnbounded <: DFAny
  protected[DFiant] type TVal <: TUnbounded
  protected[DFiant] type TVar <: TVal with DFAny.Var
  protected[DFiant] type TAlias <: TVal
  protected[DFiant] type TBool <: DFBool
  type In = TVal
  type Out = TVar//DFAny.Connectable[TVal] with TVal
  protected[DFiant] type TBits[W2] <: DFBits[W2]
  protected[DFiant] type TUInt[W2] <: DFUInt[W2]
  protected[DFiant] type TSInt[W2] <: DFSInt[W2]
  protected[DFiant] type TCompanion <: DFAny.Companion
  protected[DFiant] type TToken <: DFAny.Token
  protected[DFiant] type TPattern <: DFAny.Pattern[TPattern]
  protected[DFiant] type TPatternAble[+R] <: DFAny.Pattern.Able[R]
  protected[DFiant] type TPatternBuilder[L <: DFAny] <: DFAny.Pattern.Builder[L, TPatternAble]
  protected[DFiant] type OpAble[R] <: DFAny.Op.Able[R]
  protected[DFiant] type `Op<>Builder`[R] <: DFAny.Op.Builder[TVal, R]
  protected[DFiant] type `Op:=Builder`[R] <: DFAny.Op.Builder[TVal, R]
  protected[DFiant] type `Op==Builder`[R] <: DFAny.`Op==Builder`[TVal, R]
  protected[DFiant] type `Op!=Builder`[R] <: DFAny.`Op!=Builder`[TVal, R]
  protected[DFiant] type InitAble[L <: DFAny] <: DFAny.Init.Able[L]
  protected[DFiant] type InitBuilder <: DFAny.Init.Builder[TVal, InitAble, TToken]
  protected[DFiant] type PortBuilder[Dir <: DFDir] <: DFAny.Port.Builder[TVal, Dir]
//  type TUInt <: DFUInt
  val width : TwoFace.Int[Width]
  protected[DFiant] type ThisOwner <: DFAnyOwner
  final protected[DFiant] val tVal = this.asInstanceOf[TVal]
  final protected[DFiant] val left = tVal

  protected[DFiant] trait __DevDFAny extends __DevDFAnyMember {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Naming
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    private var autoConstructCodeStringFunc : Option[() => String] = None
    private lazy val autoConstructCodeString : String = autoConstructCodeStringFunc.map(x => x()).getOrElse("")
    final private[DFiant] def setAutoConstructCodeString(cs : => String) : self.type = {autoConstructCodeStringFunc = Some(() => cs); self}
    private[DFiant] def constructCodeStringDefault : String
    private[DFiant] def showAnonymous : Boolean = __config.showAnonymousEntries
    private def constructCodeString : String =
      if (autoConstructCodeString.isEmpty || showAnonymous) constructCodeStringDefault else autoConstructCodeString
    override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = {
      val ref = if (isAnonymous && !showAnonymous) relativeName(constructCodeString)(callOwner) else relativeName(callOwner)
      ref.applyBrackets() //TODO: consider other way instead of this hack
    }
    private def initCommentString : String =
      if (__config.commentInitValues || owner.privShowInits) s"//init = ${initCB.unbox.codeString}" else ""
    private def latencyCommentString : String =
      if (__config.commentLatencyValues || owner.privShowLatencies) s"//latency = ${source.latencyString}" else ""
    private def connCommentString : String =
      if (__config.commentConnection || owner.privShowConnections) s"//conn = ${source.refCodeString}" else ""
    private def valCodeString : String = s"\nval $name = $constructCodeString"
    def codeString : String = f"$valCodeString%-60s$initCommentString$latencyCommentString$connCommentString"

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Assignment
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    val isAssignable : Boolean = false
    def consumeAt(relWidth : Int, relBitLow : Int, version : Int, context : DFBlock) : Unit = {}

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Init (for use with Prev)
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    val initCB : CacheBoxRO[Seq[TToken]]

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Constant
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    val constCB : CacheBoxRO[TToken]
    final private[DFiant] def isConstant : Boolean = !constCB.unbox.isBubble

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Transparent Replacement References
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    final def replacement()(implicit ctx : DSLContext) : TAlias =
      if (self.nonTransparentOwner ne ctx.owner.nonTransparent) ctx.owner.nonTransparent match {
        case d : DFDesign => d.transparentPorts.getOrElse(self, self).asInstanceOf[TAlias]
        case _ =>  self.asInstanceOf[TAlias]
      } else self.asInstanceOf[TAlias]

    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Source
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    lazy val source : Source = Source(self)
  }
  override private[DFiant] lazy val __dev : __DevDFAny = ???
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
  // Prev
  //////////////////////////////////////////////////////////////////////////
  final protected[DFiant] def protPrev(step : Int)(implicit ctx : DFAny.Alias.Context)
  : TVal = alias(DFAny.Alias.Reference.Prev(this, step))
  final def prev()(implicit ctx : DFAny.Alias.Context) : TVal = protPrev(1)
  final def prev[P](step : Natural.Int.Checked[P])(implicit ctx : DFAny.Alias.Context) : TVal =
    protPrev(step)
  private[DFiant] var prevImplicitlyUsed : Boolean = false //TODO: hack. Remove this
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
  private[DFiant] def pipeGet : Int = 0
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Future Stuff
  //////////////////////////////////////////////////////////////////////////
  final def next(step : Int = 1) : TVal = ???
  def consume() : TAlias = ???
  final def dontConsume() : TAlias = ???
  final def isNotEmpty : DFBool = ???
  //////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////
  // Generation
  //////////////////////////////////////////////////////////////////////////
  protected[DFiant] def copyAsNewPort [Dir <: DFDir](dir : Dir)(implicit ctx : DFAny.Port.Context) : TVal <~> Dir
  protected[DFiant] def alias(reference : DFAny.Alias.Reference)(
    implicit ctx : DFAny.Alias.Context
  ) : TAlias
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
  final val isPort : Boolean = this match {
    case x : DFAny.Port[_,_] => true
    case _ => false
  }
  //////////////////////////////////////////////////////////////////////////
}



object DFAny {
  implicit def fetchDev(from : DFAny)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Head Types
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Unbounded[T <: DFAny.Companion] extends DFAny {
    protected[DFiant] type TCompanion = T
  }

  trait Var extends DFAny {self =>
    protected[DFiant] type TAlias = TVar
    protected[DFiant] type TBool = DFBool.Var
    protected[DFiant] type TBits[W2] = DFBits.Var[W2]
    protected[DFiant] type TUInt[W2] = DFUInt.Var[W2]
    protected[DFiant] type TSInt[W2] = DFSInt.Var[W2]
    type TDir <: DFDir

    protected[DFiant] trait __DevVar extends __DevDFAny {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private lazy val _discoveryDependencies : CacheBoxRO[Set[DFAnyMember]] =
        CacheDerivedRO(netsDependencies)(discoveryDependenciesStatic ++ netsDependencies)
      @inline override private[DFiant] def discoveryDependencies : CacheBoxRO[Set[DFAnyMember]] = _discoveryDependencies
      @tailrec private def getDepSet(set : Set[DFAnyMember], list : List[Either[Source, DFBlock]]) : Set[DFAnyMember] = list match {
        case Left(src) :: xs =>
          val updatedSet = src.elements.foldLeft(set) {
            case (s, a : SourceElement.Alias) => s + a.dfVal ++ a.dfNet.toList
            case (s, _) => s
          }
          getDepSet(updatedSet, xs)
        case Right(block) :: xs => getDepSet(set + block, xs ++ block.netsTo(self))
        case Nil => set
      }
      final lazy val netsDependencies : CacheBoxRO[Set[DFAnyMember]] = CacheDerivedRO(netsTo) {
        getDepSet(Set(), netsTo)
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Assignment
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private val _netsTo : CacheBoxRO[List[Either[Source, DFBlock]]] = owner match {
        case o : DFDesign => CacheDerivedRO(o.netsTo)(o.netsTo.getOrElse(self, List()))
        case o => CacheBoxRO(List())
      }
      @inline def netsTo : CacheBoxRO[List[Either[Source, DFBlock]]] = _netsTo

      final lazy val assignments = CacheDerivedRO(netsTo) {
        netsTo.flatMap {
          case Left(src) =>
            val assignments = src.assignmentsOnly
            if (assignments.isEmpty) None
            else Some(Left(assignments))
          case r => Some(r)
        }
      }
      final def assignedAt(version : Option[Int], context : DFBlock) : immutable.BitSet = {
        val prevList = context.netsTo.get(self) match {
          case Some(list) => version match {
            case Some(v) => list.splitAt(v)._1
            case None => list
          }
          case None => List()
        }
        val prevBits = (version, context) match {
          case (Some(_), block : ConditionalBlock[_,_]) => block.owner.netsTo.get(self) match {
            case Some(ownerVersions) =>
              var v : Int = ownerVersions.length
              while (v > 0 && ownerVersions(v).isRight) v = v - 1
              if (v > 0) assignedAt(Some(v), block.owner)
              else immutable.BitSet()
            case None => immutable.BitSet()
          }
          case _ => immutable.BitSet()
        }
        prevList.foldLeft(prevBits){
          case (onBits, Left(src)) =>
            onBits ++ src.toUsedBitSet
          case (onBits, Right(condBlock : ConditionalBlock[_,_])) if condBlock.isExhaustive && condBlock.isLastCondBlock =>
            val bbb = condBlock.allBlocks.map(e => assignedAt(None, e))
            onBits ++ bbb.reduce((l, r) => l.intersect(r))
          case (onBits, _) =>
            onBits
        }
      }
      final override def consumeAt(relWidth : Int, relBitLow : Int, version : Int, context : DFBlock) : Unit = {
        val at = assignedAt(Some(version), context)
        val mask = immutable.BitSet() ++ (relBitLow until (relBitLow + relWidth))
        val masked = mask -- at
        if (masked.nonEmpty) prevImplicitlyUsed = true
      }
      override val isAssignable : Boolean = true
      final def isAssigned : Boolean = assignments.nonEmpty
      final def assignmentsAt(toRelWidth : Int, toRelBitLow : Int) : List[Either[Source, DFBlock]] = owner match {
        case o : DFDesign => o.netsToAt(self, toRelWidth, toRelBitLow).flatMap {
          case Left(src) =>
            val assignments = src.assignmentsOnly
            if (assignments.isEmpty) None
            else Some(Left(assignments))
          case r => Some(r)
        }
        case _ => List()
      }

      def assign(toRelWidth : Int, toRelBitLow : Int, that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val toVar = self
        val fromVal = that
        //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
        if (!ctx.owner.callSiteSameAsOwnerOf(toVar))
          throw new IllegalArgumentException(s"\nTarget assignment variable (${toVar.fullName}) is not at the same design as this assignment call (${ctx.owner.fullName})")
        def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted assignment: $toVar := $fromVal}")
        if (toRelWidth != fromVal.width.getValue) throwConnectionError(s"Target width ($toRelWidth) is different than source width (${fromVal.width}).")
        DFNet.Assignment(toVar, fromVal)
      }
      def assign(that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val toVar = self.replacement().asInstanceOf[Var]
        val fromVal = that.replacement()
        toVar.assign(width, 0, fromVal)
      }
    }
    override private[DFiant] lazy val __dev : __DevVar = ???
    import __dev._

    //////////////////////////////////////////////////////////////////////////
    // Future Stuff
    //////////////////////////////////////////////////////////////////////////
    final def dontProduce() : TAlias = ???
    final def isNotFull : DFBool = ???
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    // Assignment (Mutation)
    //////////////////////////////////////////////////////////////////////////
    private[DFiant] type MustBeOut = RequireMsg[![ImplicitFound[TDir <:< IN]], "Cannot assign to an input port"]
    final def := [R](right: OpAble[R])(
      implicit dir : MustBeOut, op: `Op:=Builder`[R], ctx : DFNet.Context
    ) = assign(op(left, right))
    //////////////////////////////////////////////////////////////////////////
  }
  object Var {
    implicit def fetchDev(from : Var)(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  }


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // General Common Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Constructor[DF <: DFAny](_width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends DFAny {
    protected[DFiant] trait __DevConstructor extends __DevDFAny {

    }
    override private[DFiant] lazy val __dev : __DevConstructor = ???
    import __dev._
    final lazy val width : TwoFace.Int[Width] = TwoFace.Int.create[Width](_width)
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Connectable Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Connectable[DF <: DFAny](width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Constructor[DF](width) with DFAny.Var {self : DF =>
    protected[DFiant] trait __DevConnectable extends __DevConstructor with __DevVar {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Assignment
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override def assign(toRelWidth : Int, toRelBitLow : Int, that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val toVar = self
        val fromVal = that
        def throwAssignmentError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted assignment: $toVar := $fromVal at ${ctx.meta.position}")
        val cons = toVar.connectionsAt(toRelWidth, toRelBitLow)
        if (!cons.isEmpty) throwAssignmentError(s"Target ${toVar.fullName} already has a connection: $cons.\nCannot apply both := and <> operators for the same target")
        super.assign(toRelWidth, toRelBitLow, fromVal)
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Connection
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final lazy val connections = CacheDerivedRO(netsTo) {
        netsTo.foldLeft(Source.none(width)) {
          case (cons, Left(src)) => cons.orElse(src.connectionsOnly)
          case (cons, _) => cons
        }
      }

      final lazy val connectionLoop : CacheBoxRO[Option[List[DFAny.Connectable[_]]]] = CacheDerivedRO(connections) {
        val cons = connections.elements.collect {
          case e @ SourceElement.Alias(v : DFAny.Connectable[_],_,_,_,_,_,_,_,_) => (v, v.connectionLoop.getBoxed)
        }
        cons.collectFirst {
          case (v, CacheBoxRO.Boxed.CyclicError) => List(v)
          case (v, CacheBoxRO.Boxed.ValidValue(Some(e))) => v :: e
        }
      }

      private def connectFrom(toRelWidth : Int, toRelBitLow : Int, that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val toVar = self
        val fromVal = that
        //TODO: Check that the connection does not take place inside an ifdf (or casedf/matchdf)
        def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${toVar.fullName} <> ${fromVal.fullName} at ${ctx.owner.fullName}")
        if (fromVal.width != toVar.width) throwConnectionError(s"Target width (${toVar.width}) is different than source width (${fromVal.width}).")
        val cons = toVar.connectionsAt(toRelWidth, toRelBitLow)
        if (!cons.isEmpty)
          throwConnectionError(s"Target ${toVar.fullName} already has a connection: $cons")
        val assigns = toVar.assignmentsAt(toRelWidth, toRelBitLow)
        if (assigns.nonEmpty) throwConnectionError(s"Target ${toVar.fullName} was already assigned to: $assigns.\nCannot apply both := and <> operators for the same target")
        //All is well. We can now connect fromVal->toVar
        DFNet.Connection(toVar, fromVal)
//        println(s"connected ${toVar.fullName} <- ${fromVal.fullName} at ${ctx.owner.fullName}")
      }
      def connectFrom(that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val toVar = self.replacement().asInstanceOf[Connectable[DF]]
        val fromVal = that.replacement()
        toVar.connectFrom(width, 0, fromVal)
      }
      def connectWith(that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        val left = self.replacement()
        val right = that.replacement()
        def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.fullName} <> ${right.fullName}")
        (left, right) match {
          case (p1 : Port[_,_], p2 : Port[_,_]) => p1.connectPort2Port(p2)
          case (p : Port[_,_], v) => p.connectVal2Port(v)
          case (v, p : Port[_,_]) => p.connectVal2Port(v)
          case _ => throwConnectionError(s"Connection must be made between a port and a value or between ports. No ports found.")
        }
      }
      final def connectionsAt(toRelWidth : Int, toRelBitLow : Int) : Source =
        connections.bitsWL(toRelWidth, toRelBitLow)
      final def isConnectedAt(toRelWidth : Int, toRelBitLow : Int) : Boolean =
        !connectionsAt(toRelWidth, toRelBitLow).isEmpty
      final def isConnected : Boolean = isConnectedAt(width, 0)

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Init
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private lazy val connectionInits = CacheDerivedRO(connections, connectionLoop) {
        if (connectionLoop.isDefined) {
          val loop = connectionLoop.get.map(e => e.fullName).mkString(" <> ")
          throw new IllegalArgumentException(s"A cyclic connectivity loop detected\n$loop")
        }
        connections.elements.collect {
          case e : SourceElement.Alias => e.dfVal.initCB
        }
      }
      lazy val initConnectedCB : CacheBoxRO[Seq[TToken]] = CacheDerivedRO(connectionInits) {
        val bitsTokenSeq : Seq[DFBits.Token] = connections.elements.map {
          case x : SourceElement.Alias =>
            val selBits = x.dfVal.initCB.unbox.bitsWL(x.width, x.relBitLow)
            val revBits = if (x.reversed) DFBits.Token.reverse(selBits) else selBits
            val invBits = if (x.inverted) DFBits.Token.unary_~(revBits) else revBits
            x.stage match {
              case SourceStage.Prev(step) => invBits.prevInit(step)
              case _ => invBits
            }
          case x : SourceElement.Empty => Seq()
        }.reduce(DFBits.Token.concat)
        bitsTokenSeq.map(b => protTokenBitsToTToken(b).asInstanceOf[TToken])
      }
      lazy val initCB : CacheBoxRO[Seq[TToken]] = initConnectedCB

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constant
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private lazy val connectionConsts = CacheDerivedRO(connections, connectionLoop) {
        if (connectionLoop.isDefined) {
          val loop = connectionLoop.get.map(e => e.fullName).mkString(" <> ")
          throw new IllegalArgumentException(s"A cyclic connectivity loop detected\n$loop")
        }
        connections.elements.collect {
          case e : SourceElement.Alias => e.dfVal.constCB
        }
      }
      lazy val constConnectedCB : CacheBoxRO[TToken] = CacheDerivedRO(connectionConsts) {
        val bitsToken : DFBits.Token = connections.elements.map {
          case x : SourceElement.Alias =>
            val prvBits = //TODO: fix this. For instance, a steady state token self assigned generator can be considered constant
              x.stage match {
                case SourceStage.Prev(step) => DFBits.Token(x.dfVal.width, Bubble)//t.dfVal.initLB.get.prevInit(t.prevStep-1).headOption.getOrElse(bubble)
                case _ => x.dfVal.constCB.unbox
              }
            val selBits = prvBits.bitsWL(x.width, x.relBitLow)
            val revBits = if (x.reversed) selBits.reverse else selBits
            if (x.inverted) ~revBits else revBits
          case x : SourceElement.Empty => DFBits.Token(x.width, Bubble)
        }.reduce((l, r) => l ## r)
        protTokenBitsToTToken(bitsToken).asInstanceOf[TToken]
      }
      lazy val constCB : CacheBoxRO[TToken] = constConnectedCB
    }
    override private[DFiant] lazy val __dev : __DevConnectable = ???
    import __dev._

    final def <> [RDIR <: DFDir](right: TVal)(implicit ctx : DFNet.Context) : Unit = self.connectWith(right)
  }
  object Connectable {
    implicit def fetchDev(from : Connectable[_])(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Initializable Constructor
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class Initializable[DF <: DFAny](width : Int)(
    implicit cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Connectable[DF](width) {self : DF =>
    protected[DFiant] trait __DevInitializable extends __DevConnectable {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Init
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final val conditionalBlockDriver = CacheBoxRW[Option[ConditionalRetBlock[_,_]]](None)
      private val initExternalCB = CacheBoxRW[Option[Seq[TToken]]](None)
      private lazy val initDeps = CacheDerivedRO(conditionalBlockDriver, initExternalCB, initConnectedCB) {
        conditionalBlockDriver.map(c => c.__dev.initCB).toList :+ initExternalCB :+ initConnectedCB
      }
      lazy val initExternalOrInternalCB: CacheBoxRO[Seq[TToken]] = CacheDerivedRO(initDeps){
        conditionalBlockDriver.unbox match {
          case Some(c) => c.__dev.initCB.unbox.asInstanceOf[Seq[TToken]]
          case None => initExternalCB.unbox match {
            case Some(e) => e
            case None => initConnectedCB.unbox
          }
        }
      }
      override lazy val initCB: CacheBoxRO[Seq[TToken]] = initExternalOrInternalCB

      def isInitialized : Boolean = initExternalCB.isDefined || conditionalBlockDriver.isDefined
      final def initialize(updatedInit : Seq[TToken], owner : DFAnyOwner) : Unit = {
        if (isInitialized) throw new IllegalArgumentException(s"${self.fullName} already initialized")
        if (this.nonTransparentOwner ne owner.nonTransparent) throw new IllegalArgumentException(s"\nInitialization of variable (${self.fullName}) is not at the same design as this call (${owner.fullName})")
        initExternalCB.set(Some(updatedInit))
      }
      final def initCodeString : String = if (isInitialized) s" init${initCB.unbox.codeString}" else ""
    }
    override private[DFiant] lazy val __dev : __DevInitializable = ???
    import __dev._

    type TPostInit <: TVal

    final def init(that : InitAble[TVal]*)(
      implicit op : InitBuilder, ctx : Alias.Context
    ) : TPostInit = {
      initialize(op(left, that), ctx.owner)
      this.asInstanceOf[TPostInit]
    }
    //    final def reInit(cond : DFBool) : Unit = ???
  }
  object Initializable {
    implicit def fetchDev(from : Initializable[_])(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Constructors
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  abstract class NewVar[DF <: DFAny](width : Int, newVarCodeString : String)(
    implicit ctx0 : NewVar.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Initializable[DF](width) {self : DF =>
    final private[DFiant] override lazy val ctx = ctx0
    protected[DFiant] trait __DevNewVar extends __DevInitializable {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Naming
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final override private[DFiant] def showAnonymous : Boolean = true
      final private[DFiant] def constructCodeStringDefault : String = s"$newVarCodeString$initCodeString"
    }
    override private[DFiant] lazy val __dev : __DevNewVar = new __DevNewVar {}
    import __dev._

    type TPostInit = TVar

    //Port Construction
    def <> [Dir <: DFDir](dir : Dir)(implicit port : PortBuilder[Dir])
    : TVal <~> Dir = port(this.asInstanceOf[TVal], dir)
    //Dataflow If
    final object ifdf extends ConditionalBlock.IfWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])
    final object matchdf extends ConditionalBlock.MatchWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])
    final object selectdf extends ConditionalBlock.SelectWithRetVal[TVal, OpAble, `Op:=Builder`](this.asInstanceOf[NewVar[TVal]])

//    def selectdf[T, E](cond : DFBool)(thenSel : protComp.Op.Able[T], elseSel : protComp.Op.Able[E]) : TVal = ???
//    def selectdf[SW, T](sel : DFUInt[SW], default : => Option[TVal] = None)(args : protComp.Op.Able[T]*) : TVal = ???
    id
  }
  object NewVar {
    type Context = DFAnyOwner.Context[DFAnyOwner]
  }

  abstract class Alias[DF <: DFAny](val reference : DFAny.Alias.Reference)(
    implicit ctx0 : Alias.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Connectable[DF](reference.width) {self : DF =>
    final private[DFiant] override lazy val ctx = ctx0
    protected[DFiant] trait __DevAlias extends __DevConnectable {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Naming
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final private[DFiant] def constructCodeStringDefault : String = reference.constructCodeString

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      @inline override private[DFiant] def discoveryDependenciesStatic : Set[DFAnyMember] =
        super.discoveryDependenciesStatic ++ reference.aliasedVals

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Assignment
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override val isAssignable : Boolean = reference.isAssignable
//      override def assign(toRelWidth : Int, toRelBitLow : Int, fromVal : DFAny)(implicit ctx : DFNet.Context) : Unit = {
//        val toVar = self
//        val toRelBitHigh = toRelBitLow + toRelWidth-1
//        case class absolute(alias : DFAny, high : Int, low : Int)
//        //absolutes set as a tuple3 list of aliases with their absolute (high,low) coordinates
//        val absolutes = reference.aliasedVals.foldLeft[List[absolute]](List()) {
//          case (list, alias) if list.isEmpty => List(absolute(alias, reference.width - 1, reference.width - alias.width))
//          case (list, alias) => list :+ absolute(alias, list.last.low - 1, list.last.low - alias.width)
//        }
//        val assignableAbsolutes = absolutes.filter(a => toRelBitHigh >= a.low || toRelBitLow <= a.high)
//        //      println(f"${s"$fullName($toRelBitHigh, $toRelBitLow)"}%-30s := ") //${fromVal.fullName}@${fromVal.width}
//        assignableAbsolutes.foreach {
//          case absolute(alias : DFAny.Port[_,_], high, low) if alias.dir.isIn =>
//            throw new IllegalArgumentException(s"\nTarget assignment variable (${self.fullName}) is an immutable alias of an input port ${alias.fullName} at bits ($high, $low) and shouldn't be assigned")
//          case absolute(alias : DFAny.Var, high, low) =>
//            val partHigh = scala.math.min(high, toRelBitHigh)
//            val partLow = scala.math.max(low, toRelBitLow)
//            val fromWidth = partHigh - partLow + 1
//            val fromLow = partLow + low
//            alias.assign(fromWidth, fromLow, fromVal)
//          case absolute(alias, high, low) =>
//            throw new IllegalArgumentException(s"\nTarget assignment variable (${self.fullName}) is an immutable alias of ${alias.fullName} at bits ($high, $low) and shouldn't be assigned")
//        }
//      }
//      final override def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = {
//        val toVar = self.replacement().asInstanceOf[Alias[DF]]
//        val fromVal = that.replacement()
//        reference match {
//          case DFAny.Alias.Reference.BitsWL(aliasedVar, relWidth, relBitLow) =>
//            toVar.assign(relWidth, relBitLow, fromVal)
//          case DFAny.Alias.Reference.AsIs(aliasedVar) =>
//            toVar.assign(width, 0, fromVal)
//          case DFAny.Alias.Reference.Concat(aliasedVars) =>
//            toVar.assign(width, 0, fromVal)
//          case DFAny.Alias.Reference.BitReverse(aliasedVar) => ??? // assign(width, 0, that.reverse)
//          case DFAny.Alias.Reference.Invert(aliasedVar) => ???
//          case DFAny.Alias.Reference.Resize(aliasedVar, toWidth) => ???
//          case _ => throw new IllegalArgumentException(s"\nTarget assignment variable (${self.fullName}) is an immutable alias and shouldn't be assigned")
//        }
//        DFNet.Assignment(toVar, fromVal)
//      }

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Initialization
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override lazy val initCB: CacheBoxRO[Seq[TToken]] = CacheDerivedRO(reference.initCB) {
        TokenSeq(reference.initCB.unbox)(l => protTokenBitsToTToken(l).asInstanceOf[TToken])
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Source
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override lazy val source : Source = reference match { //TODO: why is this needed?
        case Alias.Reference.AsIs(a) => Source(self)
        case Alias.Reference.Prev(a, step) => Source(self)
        case _ => reference.source
      }
    }
    override private[DFiant] lazy val __dev : __DevAlias = new __DevAlias {}
    import __dev._

    final lazy val isAliasOfPort : Boolean = ???
    id
  }
  object Alias {
    trait Tag
    type Context = DFAnyOwner.Context[DFAnyOwner]

    sealed abstract class Reference(aliasCodeString_ : => String)(implicit ctx : Alias.Context) {
      final protected implicit val cbOwner = CacheBox.Owner(ctx.owner)
      val aliasedVals : List[DFAny]
      val width : Int
      val isAssignable : Boolean
      lazy val aliasCodeString : String = aliasCodeString_
      def constructCodeString(implicit owner : DSLOwnerConstruct) : String
      def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit
      val initCB : CacheBoxRO[Seq[DFBits.Token]]
      val source : Source
    }
    sealed abstract class SingleReference(refVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context)
      extends Reference(aliasCodeString) {
      val aliasedVal : DFAny = refVar.replacement()
      final protected def aliasedVar = aliasedVal.asInstanceOf[DFAny.Connectable[_]]
      val aliasedVals : List[DFAny] = List(aliasedVal)
      val width : Int = aliasedVals.head.width
      def constructCodeString(implicit owner : DSLOwnerConstruct) : String =
        s"${aliasedVal.refCodeString}$aliasCodeString"
    }
    object Reference {
      class AsIs private (refVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, aliasCodeString) {
        val isAssignable : Boolean = refVar.isAssignable
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit =
          aliasedVar.assign(width, 0, that.replacement())
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          aliasedVal.initCB.unbox.bits
        }
        lazy val source : Source = aliasedVal.source
      }
      object AsIs {
        def apply(aliasedVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context) = new AsIs(aliasedVar, aliasCodeString)
        def unapply(arg: AsIs): Option[DFAny] = Some(arg.aliasedVal)
      }
      class Concat private (concatVars : List[DFAny], aliasCodeString : => String)(implicit ctx : Alias.Context)
        extends Reference(aliasCodeString) {
        val aliasedVals : List[DFAny] = concatVars.map(c => c.replacement())
        final protected def aliasedVars = aliasedVals.asInstanceOf[List[DFAny.Connectable[_]]]
        val isAssignable : Boolean = concatVars.collectFirst{case x if !x.isAssignable => false}.getOrElse(true)
        val width : Int = aliasedVals.map(a => a.width.getValue).sum
        def constructCodeString(implicit owner : DSLOwnerConstruct) : String =
          s"${aliasedVals.map(a => a.refCodeString).mkString("(",", ",")")}$aliasCodeString"
        //TODO: something with balancing upon reading a complete value
        //      val currentPipe: Pipe = aliasPipeBalance(pipeList.concat)
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        private lazy val initDeps = aliasedVals.map(a => a.initCB)
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(initDeps){
          initDeps.map(i => i.unbox.bits).reduce(DFBits.Token.concat)
        }
        lazy val source : Source = Source(aliasedVals.flatMap(a => a.source.elements)).coalesce
      }
      object Concat {
        def apply(aliasedVars : List[DFAny], aliasCodeString : => String)(implicit ctx : Alias.Context) = new Concat(aliasedVars, aliasCodeString)
        def unapply(arg: Concat): Option[List[DFAny]] = Some(arg.aliasedVals)
      }
      class BitsWL private (refVar : DFAny, val relWidth : Int, val relBitLow : Int, aliasCodeString : => String)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, aliasCodeString) {
        override val width: Int = relWidth
        val isAssignable : Boolean = refVar.isAssignable
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          aliasedVal.initCB.unbox.bitsWL(relWidth, relBitLow)
        }
        lazy val source : Source = aliasedVal.source.bitsWL(relWidth, relBitLow)
      }
      object BitsWL {
        def apply(aliasedVar : DFAny, relWidth: Int, relBitLow : Int, aliasCodeString : => String)(implicit ctx : Alias.Context) =
          new BitsWL(aliasedVar, relWidth, relBitLow, aliasCodeString)
        def unapply(arg : BitsWL): Option[(DFAny, Int, Int)] = Some((arg.aliasedVal, arg.relWidth, arg.relBitLow))
      }
      class Prev private (refVar : DFAny, val step : Int)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, if (step == 0) "" else if (step == 1) ".prev" else s".prev($step)") {
        val isAssignable : Boolean = false
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          aliasedVal.initCB.unbox.bits.prevInit(step)
        }
        lazy val source : Source = aliasedVal.source.prev(step)
      }
      object Prev {
        def apply(aliasedVar : DFAny, step : Int)(implicit ctx : Alias.Context) = new Prev(aliasedVar, step)
        def unapply(arg: Prev): Option[(DFAny, Int)] = Some(arg.aliasedVal, arg.step)
      }
      class Pipe private (refVar : DFAny, val step : Int)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, if (step == 0) "" else if (step == 1) ".pipe" else s".pipe($step)") {
        val isAssignable : Boolean = false
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          aliasedVal.initCB.unbox.bits.prevInit(step)
        }
        lazy val source : Source = aliasedVal.source.pipe(step)
      }
      object Pipe {
        def apply(aliasedVar : DFAny, step : Int)(implicit ctx : Alias.Context) = new Pipe(aliasedVar, step)
        def unapply(arg: Pipe): Option[(DFAny, Int)] = Some(arg.aliasedVal, arg.step)
      }
//      class LeftShift(aliasedVar : DFAny, val shift : Int)
//        extends SingleReference(aliasedVar, if (shift == 0) "" else s"$shift") {
//      }
//      object LeftShift {
//        def apply(aliasedVar : DFAny, shift : Int) = new LeftShift(aliasedVar, shift)
//        def unapply(arg: LeftShift): Option[(DFAny, Int)] = Some(arg.aliasedVar, arg.shift)
//      }
      class Resize private (refVar : DFAny, val toWidth : Int)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, if (toWidth == refVar.width.getValue) "" else s".toWidth($toWidth)") {
        override val width: Int = toWidth
        val isAssignable : Boolean = false
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          TokenSeq(aliasedVal.initCB.unbox.asInstanceOf[Seq[Token.Resizable]])(t => t.resize(toWidth)).bits
        }
        lazy val source : Source = aliasedVal.source.resize(toWidth)
      }
      object Resize {
        def apply(aliasedVar : DFAny, toWidth : Int)(implicit ctx : Alias.Context) = new Resize(aliasedVar, toWidth)
        def unapply(arg: Resize): Option[(DFAny, Int)] = Some(arg.aliasedVal, arg.toWidth)
      }
      class BitReverse private (refVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, aliasCodeString) {
        val isAssignable : Boolean = refVar.isAssignable
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit =
          aliasedVar.assign(width, 0, that.replacement())
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          DFBits.Token.reverse(aliasedVal.initCB.unbox.bits)
        }
        lazy val source : Source = aliasedVal.source.reverse
      }
      object BitReverse {
        def apply(aliasedVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context) = new BitReverse(aliasedVar, aliasCodeString)
        def unapply(arg: BitReverse): Option[DFAny] = Some(arg.aliasedVal)
      }
      class Invert private (refVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context)
        extends SingleReference(refVar, aliasCodeString) {
        val isAssignable : Boolean = refVar.isAssignable
        def assign(that: DFAny)(implicit ctx: DFNet.Context): Unit = ???
        lazy val initCB : CacheBoxRO[Seq[DFBits.Token]] = CacheDerivedRO(aliasedVal.initCB){
          DFBits.Token.unary_~(aliasedVal.initCB.unbox.bits)
        }
        lazy val source : Source = aliasedVal.source.invert
      }
      object Invert {
        def apply(aliasedVar : DFAny, aliasCodeString : => String)(implicit ctx : Alias.Context) = new Invert(aliasedVar, aliasCodeString)
        def unapply(arg: Invert): Option[DFAny] = Some(arg.aliasedVal)
      }
    }
  }

  abstract class Const[DF <: DFAny](val token : Token)(
    implicit ctx0 : Const.Context, cmp : Companion, bubbleToken : DF => DF#TToken, protTokenBitsToTToken : DFBits.Token => DF#TToken
  ) extends Constructor[DF](token.width) {self : DF =>
    final private[DFiant] override lazy val ctx = ctx0
    protected[DFiant] trait __DevConst extends __DevConstructor {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Naming
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final override def refCodeString(implicit callOwner : DSLOwnerConstruct) : String = constructCodeStringDefault
      private[DFiant] def constructCodeStringDefault : String = s"${token.codeString}"

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Init
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      lazy val initCB : CacheBoxRO[Seq[TToken]] = CacheBoxRO(Seq(token).asInstanceOf[Seq[TToken]])

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Constant
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final lazy val constCB : CacheBoxRO[TToken] = CacheBoxRO(token.asInstanceOf[TToken])
    }
    override private[DFiant] lazy val __dev : __DevConst = new __DevConst {}
    import __dev._
    override def toString: String = token.toString
    id
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
  ) extends DFAny.Initializable[DF](dfVar.width) with CanBePiped {self : DF <~> Dir =>
    type TPostInit = TVal <~> Dir
    type TDir = Dir
    protected[DFiant] type ThisOwner <: DFInterface
    final private[DFiant] override lazy val ctx = ctx0
    protected[DFiant] trait __DevPort extends __DevInitializable {
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Member discovery
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      @inline private[DFiant] override def discoveryDependenciesStatic : Set[DFAnyMember] = owner match {
        case x : DFBlackBox if dir.isOut => super.discoveryDependenciesStatic ++ x.depsOf(self)
        case _ => super.discoveryDependenciesStatic
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Naming
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      final override private[DFiant] def showAnonymous : Boolean = true
      private[DFiant] def constructCodeStringDefault : String =
        s"${dfVar.__dev.constructCodeStringDefault} <> $dir$initCodeString"

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Connection
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      private val _netsTo : CacheBoxRO[List[Either[Source, DFBlock]]] =
        if (dir.isIn) {
          if (owner.isTop) CacheBoxRO(List())
          else {
            val grandfather = owner.owner.asInstanceOf[DFBlock]
            CacheDerivedRO(grandfather.__dev.netsTo)(grandfather.__dev.netsTo.getOrElse(self, List()))
          }
        } else super.netsTo
      @inline override def netsTo : CacheBoxRO[List[Either[Source, DFBlock]]] = _netsTo

      private def sameDirectionAs(right : Port[_ <: DFAny,_ <: DFDir]) : Boolean = self.dir == right.dir
      private[DFiant] def connectPort2Port(that : Port[_ <: DFAny,_ <: DFDir])(implicit ctx : DFNet.Context) : Unit = {
        implicit val __theOwnerToBe : DSLOwnerConstruct = ctx.owner
        val left = self
        val right = that
        def throwConnectionError(msg : String) = throw new IllegalArgumentException(s"\n$msg\nAttempted connection: ${left.fullName} <> ${right.fullName}\nConnected at ${ctx.owner.fullName}")
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
            case (ld : OUT, rd : IN) if left.isAssigned || left.isInitialized => (left, right) //relaxation of the rule when the owner output port is already assigned to or initialized
            case (ld : OUT, rd : IN)  => throwConnectionError(s"Cannot connect different port directions between owner and child designs.")
            case (ld : IN,  rd : IN)  => (left, right)
            case (ld : OUT, rd : OUT) => (right, left)
            case _ => throwConnectionError("Unexpected connection error")
          }
          //Connecting owner and child design ports, while owner port is right and child port is left.
          else if (left.isDownstreamMemberOf(right.nonTransparentOwner) && isConnectedAtEitherSide(left, right)) (left.dir, right.dir) match {
            case (ld : IN,  rd : OUT) if right.isAssigned || right.isInitialized => (right, left)  //relaxation of the rule when the owner output port is already assigned to or initialized
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
          else if (left.dir.isIn && isConnectedAtOwnerOf(left.nonTransparentOwner)) {
            throwConnectionError(s"Via connection is currently not supported")
          }
          else if (right.dir.isIn && isConnectedAtOwnerOf(right.nonTransparentOwner)) {
            throwConnectionError(s"Via connection is currently not supported")
          }
          else if (!left.isDownstreamMemberOf(right.nonTransparentOwner) || !right.isDownstreamMemberOf(left.nonTransparentOwner))
            throwConnectionError(s"Connection must be made between ports that are either in the same design, or in a design and its owner, or between two design siblings.")
          else if (!isConnectedAtEitherSide(left, right))
            throwConnectionError(s"The connection call must be placed at the same design as one of the ports or their mutual owner. Call placed at ${ctx.owner.fullName}")
          else throwConnectionError("Unexpected connection error")

        toPort.connectFrom(fromPort)
      }
      final private[DFiant] def connectVal2Port(that : DFAny)(implicit ctx : DFNet.Context) : Unit = {
        implicit val __theOwnerToBe : DSLOwnerConstruct = ctx.owner
        val port = self
        val dfVal = that
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

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Assignment
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override val isAssignable : Boolean = dir.isOut

      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      // Initialization
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      override lazy val initCB : CacheBoxRO[Seq[TToken]] = owner match {
        case x : DFBlackBox if dir.isOut => x.initOf(self)
        case _ => initExternalOrInternalCB
      }
    }
    override private[DFiant] lazy val __dev : __DevPort = new __DevPort {}
    import __dev._


    //    private var extraPipe : Int = 0
//    def pipe() : this.type = pipe(1)
//    final private[DFiant] override def pipeGet = extraPipe
//    final def pipe(p : Int) : this.type = {extraPipe = p; this}

    final def <> [R](right: OpAble[R])(
      implicit ctx : DFNet.Context, op: `Op<>Builder`[R]
    ) : Unit = connectWith(op(left, right))
    //Connection should be constrained accordingly:
    //* For IN ports, supported: All Op:= operations, and TOP
    //* For OUT ports, supported only TVar and TOP

    override def toString : String = s"$fullName : $typeName <> $dir"
    id
  }
  object Port {
    implicit def fetchDev(from : Port[_,_])(implicit devAccess: DevAccess) : from.__dev.type = from.__dev
    type Context = DFAnyOwner.Context[DFInterface]
    trait Builder[L <: DFAny, Dir <: DFDir] {
      def apply(right : L, dir : Dir) : L <~> Dir
    }
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Token
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Token extends HasCodeString {
    Self =>
    type TValue
    protected[DFiant] type TToken <: Token
    protected[DFiant] type TPattern <: DFAny.Pattern[TPattern]{type TValue = Self.TValue}
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
    trait Resizable extends Token {
      def resize(toWidth : Int) : TToken
    }
    abstract class Of[V, P <: DFAny.Pattern[P]{type TValue = V}](implicit codeStringOf : CodeStringOf[V]) extends Token {
      type TValue = V
      protected[DFiant] type TPattern = P
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




