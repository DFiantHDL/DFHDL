package DFiant

import DFiant.DFDesign.DB
import DFiant.internals._
import singleton.ops._

import scala.annotation.implicitNotFound

abstract class DFInterface(
  val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
)(implicit ctx : DFInterface.Context) extends DFInterface.Abstract {
  private[DFiant] final lazy val __ctx : DFInterface.Context = ctx
  private[DFiant] final def updateDir(updatedDir : DFDir) : this.type = {
    val actualDir : DFDir = (updatedDir, ctx.dir) match {
      case (ASIS, x) => x
      case (x, ASIS) => x
      case (_, IN | OUT | VAR) => ctx.dir //context forced direction
      case (IN, FLIP) => OUT
      case (OUT, FLIP) => IN
      case (VAR, FLIP) => VAR
      case (FLIP, FLIP) => ASIS
    }
    ctx.newInterface(ctx.updateDir(actualDir)).asInstanceOf[this.type]
  }

  final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
}

object DFInterface {
  trait Abstract extends DFOwner.Container {
    type Owner = DFInterface.Owner
    private[DFiant] val __ctx : DFInterface.Context
    val nameFlatten : DFOwner.NameFlatten
    private[DFiant] final val owner : Owner = DFInterface.Owner(this)(nameFlatten)(__ctx)
    private[DFiant] final val __dir : DFDir = __ctx.dir
    private[DFiant] final lazy val __db: DFDesign.DB.Mutable = __ctx.db
    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    final protected implicit def __interfaceContext(
      implicit meta : Meta, cc : CloneClassWithContext[DFInterface.Context]
    ) : DFInterface.Context = new DFInterface.Context(meta, owner, __dir, __db) {
      def newInterface(updatedCtx : DFInterface.Context) : Any = cc(updatedCtx)
    }
    final protected implicit def __contextOfInterface[T <: DFInterface](
      implicit meta : Meta, cc : CloneClassWithContext[ContextOf[T]], args : ClassArgs[T]
    ) : ContextOf[T] = new ContextOf[T](meta, owner, __dir, __db, args) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = cc(updatedCtx)
    }
    ///////////////////////////////////////////////////////////////////
  }
  implicit class InterfaceExt[T <: DFInterface](t : T) {
    def getMembers(implicit getSet: MemberGetSet) : List[DFMember] = getSet.getMembersOf(t.owner)
    def <> (r : T)(implicit ctx : DFBlock.Context) : Unit = t.owner connectWith r.owner
    private[DFiant] def replaceOwnerAndMembers(from : DFOwner, to : DFOwner)(implicit getSet: MemberGetSet) : Unit = {
      val oldMembers = from.getMembers
      val newMembers = to.getMembers
      getSet.replace(from)(to)
      (oldMembers lazyZip newMembers).foreach {
        case (l : DFOwner, r : DFOwner) => replaceOwnerAndMembers(l, r)
        case (l : DFAny.Dcl, r : DFAny.Dcl) => getSet.replace(l)(r)
        case _ => //Do nothing
      }
      //removing old non-declaration members
      oldMembers.foreach {
        case _ : DFOwner |  _ : DFAny.Dcl => //Do nothing
        case r => getSet.remove(r)
      }
    }
    def setNameFlatten(nameFlatten: DFOwner.NameFlatten)(implicit getSet: MemberGetSet) : T = {
      val owner = t.owner
      getSet.replace(owner)(owner.copy(nameFlatten = nameFlatten))
      t
    }
    def <> (dir : DFDir)(implicit getSet: MemberGetSet) : T = {
      val updated = t.updateDir(dir)
      replaceOwnerAndMembers(t.owner, updated.owner)
      updated
    }
  }
  final class Singular[T <: DFAny.Type](dfType : T)(implicit ctx : ContextOf[Singular[T]]) extends DFInterface {
    final val value = DFAny.NewVar(dfType)
  }
  abstract class Context(val meta : Meta, ownerF : => DFOwner, val dir : DFDir, val db : DFDesign.DB.Mutable)
    extends DFAny.Context { self =>
    def owner : DFOwner = ownerF
    def newInterface(updatedCtx : DFInterface.Context) : Any
    final def updateDir(updatedDir : DFDir) : Context = new Context(meta, ownerF, updatedDir, db) {
      override def newInterface(updatedCtx : DFInterface.Context) : Any = self.newInterface(updatedCtx)
    }
  }
  object Context {
    final object MissingError extends ErrorMsg (
      "The given context type `T` in `ContextOf[T]` is wrong",
      "missing-context"
    ) {final val msg = getMsg}
    implicit def evCtx[T <: DFInterface](
      implicit
      ctx : ContextOf[T],
      mustBeTheClassOf: RequireMsg[ImplicitFound[MustBeTheClassOf[T]], MissingError.Msg]
    ) : Context = new Context(ctx.meta, ctx.owner, ctx.dir, ctx.db){
      def newInterface(updatedCtx : DFInterface.Context) : Any = ctx.newInterface(ctx.updateDir(updatedCtx.dir))
    }
  }

  final case class Owner(
    nameFlatten : DFOwner.NameFlatten, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic
  ) extends DFOwner.NameFlattenOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(_, _, tags) => this.tags =~ tags //Deliberately ignoring nameFlatten. Only the final name (in tags) matters.
      case _ => false
    }
    private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(container : DFOwner.Container)(nameFlatten: DFOwner.NameFlatten)(
      implicit ctx : DFAny.Context
    ) : Owner = ctx.db.addContainerOwner(container)(Owner(nameFlatten, ctx.owner, ctx.meta))
  }
}

