package DFiant
package dfsm

import collection.immutable
import scala.annotation.{implicitNotFound, tailrec}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FSM
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[DFiant] final case class FSM(
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
  private def addEdges(edgeMap : immutable.ListMap[Step, List[Edge]]) : FSM = copy(edges = {
    edgeMap.foldLeft(edges){
      case (edges, stepToEdges) => addEdges(edges, stepToEdges)
    }
  })
  def addFSM(fsm : FSM) : FSM = {
    this.untrack
    fsm.untrack
    this.addEdges(fsm.edges).copy(lastStep = fsm.lastStep).track
  }
  def connectTo(destFSM : FSM, viaEdge : Edge) : FSM = {
    this.untrack
    destFSM.untrack
    this.addEdges(destFSM.edges).addEdge(this.lastStep -> viaEdge).copy(lastStep = destFSM.lastStep).track
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
  protected[dfsm] def goto(step : Step) : Unit = state := stepEntries(step)

  private[DFiant] lazy val elaborate : FSM = {
    ctx.ownerInjector.injectOwnerAndRun(owner) {
      val matchHeader = matchdf(state)
      val matcherFirstCase = matchHeader.casedf(stepEntries(firstStep)){
        firstStep.elaborateAt(this)
      }
      edges.drop(1).foldLeft(matcherFirstCase) {
        case (lastCase, (step, _)) =>
          lastCase.casedf(stepEntries(step)) {
            step.elaborateAt(this)
          }
      }
    }
    this
  }
  private def track : FSM = ctx.db.trackFSM(this)
  private def untrack : FSM = ctx.db.untrackFSM(this)
}
protected[DFiant] object FSM {
  @implicitNotFound("Unsupported FSM step concatenation")
  type TC[T] = T => FSM

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
    def =?>[C, C2](cond : => C)(
      implicit arg : DFBool.Arg[0]
    ) : FSMCond = FSMCond(sourceFSM_TC(s), () => arg())
    def ==>(block : => Unit) : FSMCondBlock = FSMCondBlock(sourceFSM_TC(s), None, () => block)
    def ++ (fsm : FSM)(implicit ctx : DFBlock.Context) : FSM = sourceFSM_TC(s).addFSM(fsm)
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FSMs with a Dangling Conditional Edge
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[dfsm] final case class FSMCond(fsm : FSM, cond : () => DFBool) {
  def ==>[D](d : D)(implicit destFSM_TC : FSM.TC[D], ctx : DFBlock.Context) : FSM = {
    val destFSM = destFSM_TC(d)
    val edge = Edge(Some(cond), () => {}, destFSM.firstStep)
    fsm.connectTo(destFSM, edge)
  }
  def ==>(block : => Unit) : FSMCondBlock = FSMCondBlock(fsm, Some(cond), () => block)
}

protected[dfsm] final case class FSMCondBlock(fsm : FSM, condOption : Option[() => DFBool], block : () => Unit) {
  def ==>[D](d : D)(implicit destFSM_TC : FSM.TC[D], ctx : DFBlock.Context) : FSM = {
    val destFSM = destFSM_TC(d)
    val edge = Edge(condOption, block, destFSM.firstStep)
    fsm.connectTo(destFSM, edge)
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////

