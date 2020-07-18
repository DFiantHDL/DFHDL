package DFiant
import internals._
import singleton.ops._

import scala.annotation.tailrec

trait DFOwner extends DFMember {
  def connectWith(that : DFOwner)(implicit ctx : DFBlock.Context) : Unit = {
    val leftMembers = this.getMembers
    val rightMembers = that.getMembers
    (leftMembers lazyZip rightMembers).foreach {
      case (left : DFAny.Dcl, right : DFAny.Dcl) if left.dfType == right.dfType => left.connectWith(right)
      case (left : DFInterface.Owner, right : DFInterface.Owner) => left.connectWith(right)
      case (left : DFAny.Dcl, right : DFAny.Dcl) if left.dfType != right.dfType =>
        throw new IllegalArgumentException("mismatch type connection")
      case (left : DFAny.Dcl, right) =>
        throw new IllegalArgumentException("mismatch type connection")
      case (_, _ : DFAny.Dcl) =>
        throw new IllegalArgumentException("mismatch type connection")
      case _ =>
    }
  }
  def nameAndType : String = s"$name : $typeName"
}

object DFOwner {
  trait Container extends HasTypeName with DFDesign.Implicits with DisallowExternalExtensions {
    type Owner <: DFOwner
    private[DFiant] val owner : Owner
    protected[DFiant] val __db: DFDesign.DB.Mutable
    private[DFiant] val __ctx : DFMember.Context
    final protected implicit lazy val __getset : MemberGetSet = __db.getSet
    final protected implicit val __container : Container = this
    final private[DFiant] lazy val __parent : Container = if (__ctx.container == null) this else __ctx.container
    final def isTop : Boolean = __parent == this
    @tailrec private def isInsideParent(thisContainer : Container, thatContainer : Container) : Boolean = {
      (thisContainer, thatContainer) match {
        case (a, b) if (a == b) => true
        case (p, _) if p.isTop => false
        case _ => isInsideParent(thisContainer.__parent, thatContainer)
      }
    }
    final private[DFiant] def isInsideParent(that : Container) : Boolean = isInsideParent(this, that)
    protected[DFiant] def onEnterContainer() : Unit = {}
    protected[DFiant] def onExitContainer() : Unit = {}
    protected[DFiant] def onCreateContainer() : Unit = {}
    def nameAndType : String = s"${__ctx.meta.name} : $typeName"
  }

  implicit class AbstractExt[T <: DFOwner](t : T) {
    def getMembers(implicit getSet: MemberGetSet) : List[DFMember] = getSet.getMembersOf(t)
    //    def <> (r : T)(implicit ctx : DFNet.Context) : Unit = t.owner.connectWith(r.owner)
  }

  type Ref = DFMember.Ref.Of[Ref.Type, DFOwner]
  object Ref {
    trait Type extends DFMember.Ref.Type
    implicit val ev : Type = new Type {}
    def unapply(ref : DFMember.Ref): Boolean = ref.refType match {
      case _ : Type => true
      case _ => false
    }
  }

  //When an interface is flattened, this function is applied on all its members
  trait NameFlatten {
    def apply(memberName : String, ownerName : String) : String
  }
  object NameFlatten {
    object UnderscoreSuffix extends NameFlatten {
      def apply(memberName : String, ownerName : String) : String = s"${ownerName}_${memberName}"
    }
    object NoSuffix extends NameFlatten {
      def apply(memberName : String, ownerName : String) : String = s"${ownerName}${memberName}"
    }
    object IgnoreOwnerName extends NameFlatten { //This function is special cased in FlattenInterfaces
      def apply(memberName : String, ownerName : String) : String = ???
    }
  }

  trait NameFlattenOwner extends DFOwner {
    val nameFlatten : DFOwner.NameFlatten
  }
}

abstract class ContextOf[T](
  val meta : Meta, val container : DFOwner.Container, val dir: DFDir, val db: DFDesign.DB.Mutable, val args : ClassArgs[T]
) extends DFMember.Context { self =>
  def newInterface(updatedCtx : ContextOf[T]) : Any
  final def updateDir(updatedDir : DFDir) : ContextOf[T] = new ContextOf[T](meta, container, updatedDir, db, args) {
    override def newInterface(updatedCtx : ContextOf[T]) : Any = self.newInterface(updatedCtx)
  }
}

object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit ContextOf[T].",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtxContainer[T1, T2](
    implicit runOnce: RunOnce, ctx : ContextOf[T1], mustBeTheClassOf: RequireMsg[ImplicitFound[MustBeTheClassOf[T1]], MissingError.Msg],
    args : ClassArgs[T2]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.container, ctx.dir, ctx.db, args) {
    def newInterface(updatedCtx : ContextOf[T2]) : Any = ctx.newInterface(ctx.updateDir(updatedCtx.dir))
  }
  implicit def evTop[T <: DFDesign](
    implicit topLevel : RequireMsg[ImplicitFound[TopLevel], MissingError.Msg], meta: Meta,
    mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority, args : ClassArgs[T]
  ) : ContextOf[T] = new ContextOf[T](meta, null, ASIS, new DFDesign.DB.Mutable, args) {
    def newInterface(updatedCtx : ContextOf[T]) : Any = ???
  }
}
