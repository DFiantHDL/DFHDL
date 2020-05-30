package DFiant
package fsm

import scala.collection.immutable
import internals._
/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Step
/////////////////////////////////////////////////////////////////////////////////////////////////////////
sealed abstract class Step(implicit ctx : DFBlock.Context) {
  val meta : Meta = ctx.meta
  import ctx.db.getSet
  private var fsm : FSM = _
  protected def getFSM : FSM = fsm
  private[fsm] def attachFSM(fsm : FSM) : Step = {
    this.fsm = fsm
    this
  }
  def goto() : Unit = fsm.goto(this)
  protected def outIfs(list : List[Edge]) : Unit = {
    def anonymizeCond(cond : DFBool) : DFBool = {
      if (cond.tags.meta.namePosition == fsm.owner.tags.meta.namePosition) cond.anonymize
      else cond
    }
    list match {
      case Edge(Some(cond), block, dest) :: Nil =>
        ifdf(anonymizeCond(cond())){
          block()
          dest.goto()
        }
      case Edge(Some(cond), block, dest) :: Edge(Some(cond2), block2, dest2) :: list =>
        val branch = ifdf(anonymizeCond(cond())){
          block()
          dest.goto()
        }.elseifdf(anonymizeCond(cond2())) {
          block2()
          dest2.goto()
        }
        list.foldLeft[Either[ConditionalBlock.NoRetVal.ElseIfBlock, Unit]](Left(branch)) {
          case (Left(b), Edge(Some(cond), block, dest)) => Left(b.elseifdf(anonymizeCond(cond())){
            block()
            dest.goto()
          })
          case (Left(b), Edge(None, block, dest)) => Right(b.elsedf {
            block()
            dest.goto()
          })
          case (Right(_), _ : Edge) => throw new IllegalArgumentException(s"Unexpected edge after last non-conditional edge for step ${meta.name}")
        }
      case Edge(Some(cond), block, dest) :: Edge(None, block2, dest2) :: Nil =>
        ifdf(anonymizeCond(cond())){
          block()
          dest.goto()
        }.elsedf {
          block2()
          dest2.goto()
        }
      case Edge(None, block, dest) :: Nil =>
        block()
        dest.goto()
      case _ =>
    }
  }

  override def toString : String = meta.name
  def elaborate() : Unit = {}
}
protected[DFiant] object Step {
  implicit def fsmFromStep(implicit ctx : DFBlock.Context) : FSM.TC[Step] = s => FSM(s)
  final class Basic(alwaysBlock : () => Unit)(implicit ctx : DFBlock.Context) extends Step {
    override def elaborate() : Unit = {
      val edgeList = getFSM.edges(this)
      alwaysBlock()
      outIfs(edgeList)
    }
  }

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
    ) : Owner = ctx.db.addContainerOwner(container)(Owner(ctx.owner, ctx.meta))
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Edge
/////////////////////////////////////////////////////////////////////////////////////////////////////////
protected[fsm] final case class Edge(condOption : Option[() => DFBool], block : () => Unit, dest : Step) {
  override def toString : String = s"$dest"
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////

