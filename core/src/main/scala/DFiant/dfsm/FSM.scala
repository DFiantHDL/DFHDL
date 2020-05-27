package DFiant
package dfsm

import scala.annotation.implicitNotFound

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// FSM
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[DFiant] final case class FSM(
  edges : Map[Step, List[Edge]], firstStep : Step, lastStep : Step
)(implicit ctx : DFBlock.Context) {
  def addEdge(stepToEdge : (Step, Edge)) : FSM = copy(edges = addEdges(edges, stepToEdge._1 -> List(stepToEdge._2)))
  private def addEdges(edges : Map[Step, List[Edge]], stepToEdges : (Step, List[Edge])) : Map[Step, List[Edge]] = {
    edges.get(stepToEdges._1) match {
      case Some(edgeList) => edges + (stepToEdges._1 -> (edgeList ++ stepToEdges._2))
      case None => edges + stepToEdges
    }
  }
  def addEdges(edgeMap : Map[Step, List[Edge]]) : FSM = copy(edges = {
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

  private[DFiant] lazy val elaborate : Unit = {

  }
  private def track : FSM = ctx.db.addFSM(this)
  private def untrack : FSM = ctx.db.removeFSM(this)
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
    def apply(container : DFOwner.Container)(
      implicit ctx : DFBlock.Context
    ) : Owner = ctx.db.addOwner(container)(Owner(ctx.owner, ctx.meta))
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

