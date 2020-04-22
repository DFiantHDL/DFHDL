package DFiant

import DFiant.DFDesign.DB
import DFiant.internals._
import singleton.ops._

import scala.annotation.implicitNotFound

abstract class DFInterface(namePrefix : String = "", nameSuffix : String = "_")(implicit ctx : DFInterface.Context)
  extends DFInterface.Abstract {
  type Owner = DFOwner
  private[DFiant] final val __ctx : DFInterface.Context = ctx
  private[DFiant] final val owner : Owner = DFInterface.Owner(namePrefix, nameSuffix)(ctx)
  private[DFiant] final val __db: DFDesign.DB.Mutable = ctx.db
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

  final protected implicit val __getset : MemberGetSet = ctx.db.getSet
  final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(false)
  final protected implicit def __contextOf[T <: DFInterface](implicit meta : Meta, cc : CloneClassWithContext[ContextOf[T]]) : ContextOf[T] =
    new ContextOf[T](meta, owner, ctx.dir, __db) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = cc(updatedCtx)
    }
}

object DFInterface {
  trait Abstract extends HasTypeName with DFDesign.Implicits {
    type Owner <: DFOwner
    private[DFiant] val owner : Owner
    private[DFiant] val __ctx : DFAny.Context
    private[DFiant] val __db: DFDesign.DB.Mutable

    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    final protected implicit def __interfaceContext(
      implicit meta : Meta, cc : CloneClassWithContext[DFInterface.Context]
    ) : DFInterface.Context = new DFInterface.Context(meta, owner, __ctx.dir, __db) {
      def newInterface(updatedCtx : DFInterface.Context) : Any = cc(updatedCtx)
    }
    final protected implicit def __contextOfInterface[T <: DFInterface](
      implicit meta : Meta, cc : CloneClassWithContext[ContextOf[T]]
    ) : ContextOf[T] = new ContextOf[T](meta, owner, __ctx.dir, __ctx.db) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = cc(updatedCtx)
    }
    ///////////////////////////////////////////////////////////////////
  }
  implicit class InterfaceExt[T <: DFInterface](t : T) {
    def getMembers(implicit getSet: MemberGetSet) : List[DFMember] = getSet.getMembersOf(t.owner)
    def <> (r : T)(implicit ctx : DFBlock.Context) : Unit = t.owner connectWith r.owner
    private def replaceOwnerAndMembers(from : DFOwner, to : DFOwner)(implicit getSet: MemberGetSet) : Unit = {
      val oldMembers = from.getMembers
      val newMembers = to.getMembers
      getSet.replace(from)(to)
      (oldMembers lazyZip newMembers).foreach {
        case (l : DFOwner, r : DFOwner) => replaceOwnerAndMembers(l, r)
        case (l, r) => getSet.replace(l)(r)
      }
    }
    def <> (dir : DFDir)(implicit getSet: MemberGetSet) : T = {
      val updated = t.updateDir(dir)
      replaceOwnerAndMembers(t.owner, updated.owner)
      updated
    }
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

  final case class Owner(namePrefix : String, nameSuffix : String, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic) extends DFOwner {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = DFMember.CustomTag
    protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
      case Owner(namePrefix, nameSuffix, _, tags) =>
        this.namePrefix == namePrefix && this.nameSuffix == nameSuffix && this.tags =~ tags
      case _ => false
    }
    def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(
      implicit getSet : MemberGetSet
    ) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(namePrefix : String, nameSuffix : String)(
      implicit ctx : DFAny.Context
    ) : Owner = ctx.db.addMember(Owner(namePrefix, nameSuffix, ctx.owner, ctx.meta))
  }
}

