package DFiant
package dfsm

import collection.immutable
import scala.annotation.{implicitNotFound, tailrec}

import DFDesign.Implicits._
/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FSM
/////////////////////////////////////////////////////////////////////////////////////////////////////////
final case class FSM(
  edges : immutable.ListMap[Step, List[Edge]], firstStep : Step, lastStep : Step
)(implicit ctx : DFBlock.Context) {
  private[dfsm] lazy val owner : FSM.Owner = FSM.Owner()(ctx)
  private def addEdge(stepToEdge : (Step, Edge)) : FSM = copy(edges = addEdges(edges, stepToEdge._1 -> List(stepToEdge._2)))
  private def addEdges(edges : immutable.ListMap[Step, List[Edge]], stepToEdges : (Step, List[Edge])) : immutable.ListMap[Step, List[Edge]] = {
    edges.get(stepToEdges._1) match {
      case Some(edgeList) => edges + (stepToEdges._1 -> (edgeList ++ stepToEdges._2))
      case None => edges + stepToEdges
    }
  }
//  private def addSteps(edges : immutable.ListMap[Step, List[Edge]], steps : List[Step]) : immutable.ListMap[Step, List[Edge]] = {
//    val newSteps = steps.filterNot(s => edges.contains(s))
//    edges ++ newSteps.map(s => (s, List()))
//  }

  private def addEdges(edgeMap : immutable.ListMap[Step, List[Edge]]) : FSM = copy(edges = {
    edgeMap.foldLeft(edges){
      case (edges, stepToEdges) => addEdges(edges, stepToEdges)
    }
  })
  def goto() : Unit = firstStep.goto()
  def addFSM(fsm : FSM)(implicit ctx : DFBlock.Context) : FSM = {
    this.untrack
    fsm.untrack
    this.addEdges(fsm.edges).copy(lastStep = fsm.lastStep).track
  }
  def connectTo(destFSM : FSM, viaEdge : Edge)(implicit ctx : DFBlock.Context) : FSM = {
    this.untrack
    destFSM.untrack
    val withViaEdge = this.addEdge(this.lastStep -> viaEdge)
    val withDestEdges = edges.get(destFSM.firstStep) match {
      case Some(_ :: Nil) =>
        //no need to add the destination step edges if it already exists and has edges, thus detected a circular connection
        withViaEdge
      case _ => withViaEdge.addEdges(destFSM.edges)
    }
    withDestEdges.copy(lastStep = viaEdge.dest).track
  }
  object states extends EnumType.Auto()(ctx.meta.setName(s"${ctx.meta.name}_states"))
  protected[dfsm] lazy val stepEntries : Map[Step, states.Entry] = edges.zipWithIndex.map {
    case ((step, _), i) =>
      val namedMeta =
        if ((step.meta.namePosition == ctx.meta.namePosition) | step.meta.name.anonymous) step.meta.setName(s"ST$i")
        else step.meta
      (step, states.Entry()(namedMeta))
  }.toMap
  protected[dfsm] lazy val state = DFEnum(states) init(stepEntries(firstStep))
  protected[dfsm] def goto(step : Step) : Unit = stepEntries.get(step) match {
    case Some(entry) => state := entry
    case None => throw new IllegalArgumentException("Step unknown")
  }
  private[dfsm] def printEdges() : Unit = {
    println(edges.map{
      case (s, edgeList) => s"${stepEntries(s).name} -> ${edgeList.map(e => stepEntries(e.dest).name)}"
    }.mkString("\n"))
  }
  private[DFiant] lazy val elaborate : FSM = {
    edges.foreach(e => e._1.attachFSM(this))
    ctx.ownerInjector.injectOwnerAndRun(owner) {
      val matchHeader = matchdf(state)
      val matcherFirstCase = matchHeader.casedf(stepEntries(firstStep)){
        firstStep.elaborate()
      }
      edges.drop(1).foldLeft(matcherFirstCase) {
        case (lastCase, (step, _)) =>
          lastCase.casedf(stepEntries(step)) {
            step.elaborate()
          }
      }
    }
    this
  }
  private[dfsm] def track : FSM = ctx.db.trackFSM(this)
  private[dfsm] def untrack : FSM = ctx.db.untrackFSM(this)
}
protected[DFiant] object FSM {
  def apply(step : Step)(implicit ctx : DFBlock.Context) : FSM = FSM(immutable.ListMap(step -> List()), step, step)
  @implicitNotFound("Unsupported FSM step concatenation")
  type TC[T] = T => FSM
  @implicitNotFound("Unsupported FSM step concatenation")
  type TC2[T] = T => FSM

  final case class Owner(
    ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, tags) => this.tags =~ tags
      case _ => false
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply()(
      implicit ctx : DFBlock.Context
    ) : Owner = ctx.db.addMember(Owner(ctx.owner, ctx.meta))
  }
}

protected[DFiant] trait Implicits {
  implicit class FSMExt[S](s : S)(implicit sourceFSM_TC : FSM.TC[S]) {
    def ==>[D](d : D)(implicit destFSM_TC : FSM.TC[D], ctx : DFBlock.Context) : FSM = {
      val destFSM = destFSM_TC(d)
      val sourceFSM = sourceFSM_TC(s)
      val edge = Edge(None, () => {}, destFSM.firstStep)
      sourceFSM.connectTo(destFSM, edge)
    }
    def ==>(fsmCond : FSMCond)(implicit ctx : DFBlock.Context) : FSMCond = {
      val fsm = sourceFSM_TC(s) ==> fsmCond.fsm
      FSMCond(fsm, fsmCond.cond)
    }
    def ==>(fs : firstStep.type)(implicit ctx : DFBlock.Context) : FSM = {
      val sourceFSM = sourceFSM_TC(s)
      sourceFSM ==> sourceFSM.firstStep
    }
    def =?>[C, C2](cond : => C)(
      implicit arg : DFBool.Arg[0]
    ) : FSMCond = FSMCond(sourceFSM_TC(s), () => arg())
    def onExit(block : => Unit) : FSMCondBlock = FSMCondBlock(sourceFSM_TC(s), None, () => block)
    def ++ [D](d : D)(implicit destFSM_TC : FSM.TC[D], ctx : DFBlock.Context) : FSM = sourceFSM_TC(s).addFSM(destFSM_TC(d))
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////

object firstStep


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FSMs with a Dangling Conditional Edge
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[dfsm] final case class FSMCond(fsm : FSM, cond : () => DFBool) {
  def ==>(destFSM : FSM)(implicit ctx : DFBlock.Context) : FSM = {
    val edge = Edge(Some(cond), () => {}, destFSM.firstStep)
    fsm.connectTo(destFSM, edge)
  }
  def ==>(fs : firstStep.type)(implicit ctx : DFBlock.Context) : FSM = {
    val edge = Edge(Some(cond), () => {}, fsm.firstStep)
    fsm.connectTo(fsm, edge)
  }
  def ==>(fsmCond : FSMCond)(implicit ctx : DFBlock.Context) : FSMCond = {
    val fsm = this ==> fsmCond.fsm
    FSMCond(fsm, fsmCond.cond)
  }
  def onExit(block : => Unit) : FSMCondBlock = FSMCondBlock(fsm, Some(cond), () => block)
  def goto() : Unit = fsm.untrack.goto()
}

protected[dfsm] final case class FSMCondBlock(fsm : FSM, condOption : Option[() => DFBool], block : () => Unit) {
  def ==>[D](d : D)(implicit destFSM_TC : FSM.TC[D], ctx : DFBlock.Context) : FSM = {
    val destFSM = destFSM_TC(d)
    val edge = Edge(condOption, block, destFSM.firstStep)
    fsm.connectTo(destFSM, edge)
  }
  def ==>(fs : firstStep.type)(implicit ctx : DFBlock.Context) : FSM = {
    val edge = Edge(condOption, block, fsm.firstStep)
    fsm.connectTo(fsm, edge)
  }
  def ==>(fsmCond : FSMCond)(implicit ctx : DFBlock.Context) : FSMCond = {
    val fsm = this ==> fsmCond.fsm
    FSMCond(fsm, fsmCond.cond)
  }
  def goto() : Unit = fsm.untrack.goto()
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////

