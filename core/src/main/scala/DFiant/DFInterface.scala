package DFiant

import DFiant.internals._
import singleton.ops._

/**
  * The Basic Dataflow Interface
  *
  * An interface holds various ports and possibly other interfaces.
  * The interface can be change its directionality just like a port, via the `<>` construct.
  * When declaring an interface class, use the `@df` annotation to provide context.
  * @example
  * {{{
  *   @df class MemIfc extends DFInterface {
  *     val addr = DFUInt(8) <> IN
  *     val data = DFUInt(8) <> OUT
  *   }
  *   @df class DualPortMemIfc extends DFInterface {
  *     val portA = new MemIfc
  *     val portB = new MemIfc
  *   }
  * }}}
  * @param nameFlatten Sets the name-flattening configuration, according to `DFOwner.NameFlatten`
  * @param ctx An implicit dataflow context for the interface. Do not apply manually.
  */
abstract class DFInterface(
    val nameFlatten: DFOwner.NameFlatten = DFOwner.NameFlatten.UnderscoreSuffix
)(implicit ctx: DFInterface.Context)
    extends DFInterface.Abstract {
  private[DFiant] final lazy val __ctx: DFInterface.Context = ctx
  private[DFiant] final def updateDir(updatedDir: DFDir): this.type = {
    val actualDir: DFDir = (updatedDir, ctx.dir) match {
      case (ASIS, x)           => x
      case (x, ASIS)           => x
      case (_, IN | OUT | VAR) => ctx.dir //context forced direction
      case (IN, FLIP)          => OUT
      case (OUT, FLIP)         => IN
      case (VAR, FLIP)         => VAR
      case (FLIP, FLIP)        => ASIS
    }
    ctx.newInterface(ctx.updateDir(actualDir)).asInstanceOf[this.type]
  }

  /**
    * @return true if the interface is currently at its native directionality
    */
  def hasNativeDir: Boolean =
    ctx.dir match {
      case ASIS => true
      case _    => false
    }

  /**
    * @return true if the interface is currently at its flipped directionality
    */
  def hasFlippedDir: Boolean =
    ctx.dir match {
      case FLIP => true
      case _    => false
    }
  //Use to set the default directionality in the interface
  protected[DFiant] object DEFAULT_DIR extends DEFAULT_DIR
}

object DFInterface {
  trait Abstract extends DFOwner.Container {
    type Owner = DFInterface.Owner
    private[DFiant] val __ctx: DFInterface.Context
    val nameFlatten: DFOwner.NameFlatten
    private[DFiant] final val owner: Owner =
      DFInterface.Owner(this)(nameFlatten)(__ctx)
    protected[DFiant] final implicit val __dir: DFDir = __ctx.dir
    protected[DFiant] final implicit lazy val __db: DFDesign.DB.Mutable =
      __ctx.db
    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    final protected implicit def __contextOfInterface[T <: DFInterface](implicit
        meta: Meta,
        symbol: Meta.SymbolOf[T],
        cc: CloneClassWithContext[ContextOf[T]],
        args: ClassArgs[T]
    ): ContextOf[T] =
      new ContextOf[T](meta, symbol, this, __dir, __db, args) {
        def newInterface(updatedCtx: ContextOf[T]): Any = cc(updatedCtx)
      }
    ///////////////////////////////////////////////////////////////////
    final protected implicit val __lateConstructionConfig
        : LateConstructionConfig       = LateConstructionConfig.Force(false)
    override lazy val typeName: String = __ctx.symbol.value
  }
  implicit class InterfaceExt[T <: DFInterface](t: T) {
    def getMembers(implicit getSet: MemberGetSet): List[DFMember] =
      getSet.getMembersOf(t.owner)
    def <>(r: T)(implicit ctx: DFBlock.Context): Unit =
      t.owner connectWith r.owner
    private[DFiant] def replaceOwnerAndMembers(from: DFOwner, to: DFOwner)(
        implicit getSet: MemberGetSet
    ): Unit = {
      val oldMembers = from.getMembers
      val newMembers = to.getMembers
      getSet.replace(from)(to)
      (oldMembers lazyZip newMembers).foreach {
        case (l: DFOwner, r: DFOwner)     => replaceOwnerAndMembers(l, r)
        case (l: DFAny.Dcl, r: DFAny.Dcl) => getSet.replace(l)(r)
        case _                            => //Do nothing
      }
      //removing old non-declaration members
      oldMembers.foreach {
        case _: DFOwner | _: DFAny.Dcl => //Do nothing
        case r                         => getSet.remove(r)
      }
    }
    def setNameFlatten(
        nameFlatten: DFOwner.NameFlatten
    )(implicit getSet: MemberGetSet): T = {
      val owner = t.owner
      getSet.replace(owner)(owner.copy(nameFlatten = nameFlatten))
      t
    }
    def <>(dir: DFDir)(implicit getSet: MemberGetSet): T = {
      val updated = t.updateDir(dir)
      replaceOwnerAndMembers(t.owner, updated.owner)
      updated
    }
  }
  abstract class Unnamed(implicit ctx: DFInterface.Context)
      extends DFInterface(DFOwner.NameFlatten.IgnoreOwnerName)
  final class Singular[T <: DFAny.Type](dfType: T)(implicit
      ctx: ContextOf[Singular[T]]
  ) extends DFInterface {
    final val value = DFAny.NewVar(dfType)
  }
  abstract class Context(
      val meta: Meta,
      val symbol: Meta.SymbolOf[_],
      val container: DFOwner.Container,
      val dir: DFDir,
      val db: DFDesign.DB.Mutable
  ) extends DFAny.Context { self =>
    def newInterface(updatedCtx : DFInterface.Context) : Any
    final def updateDir(updatedDir : DFDir) : Context = new Context(meta, symbol, container, updatedDir, db) {
      override def newInterface(updatedCtx : DFInterface.Context) : Any = self.newInterface(updatedCtx)
    }
    final def updateMeta(meta : Meta) : Context = new Context(meta, symbol, container, dir, db) {
      override def newInterface(updatedCtx : DFInterface.Context) : Any = self.newInterface(updatedCtx)
    }
    def newInterface(updatedMeta : Meta) : Any = newInterface(updateMeta(updatedMeta))
  }
  object Context {
    final object MissingError
        extends ErrorMsg(
          "The given context type `T` in `ContextOf[T]` is wrong",
          "missing-context"
        ) { final val msg = getMsg }
    implicit def evCtx[T <: DFInterface](implicit
        ctx: ContextOf[T],
        mustBeTheClassOf: RequireMsg[ImplicitFound[
          MustBeTheClassOf[T]
        ], MissingError.Msg]
    ): Context =
      new Context(ctx.meta, ctx.symbol, ctx.container, ctx.dir, ctx.db) {
        def newInterface(updatedCtx: DFInterface.Context): Any =
          ctx.newInterface(ctx.updateDir(updatedCtx.dir))
      }
  }

  final case class Owner(
      nameFlatten: DFOwner.NameFlatten,
      ownerRef: DFOwner.Ref,
      tags: DFMember.Tags
  ) extends DFOwner.NameFlattenOwner {
    protected[DFiant] def =~(
        that: DFMember
    )(implicit getSet: MemberGetSet): Boolean =
      that match {
        case Owner(_, _, tags) =>
          this.tags =~ tags //Deliberately ignoring nameFlatten. Only the final name (in tags) matters.
        case _ => false
      }

    def setTags(tagsFunc: DFMember.Tags => DFMember.Tags)(implicit
        getSet: MemberGetSet
    ): DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
  }
  object Owner {
    def apply(container: DFOwner.Container)(nameFlatten: DFOwner.NameFlatten)(
        implicit ctx: DFAny.Context
    ): Owner =
      ctx.db
        .addContainerOwner(container, Owner(nameFlatten, ctx.owner, ctx.meta))
  }
}
