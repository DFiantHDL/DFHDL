package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock {
  final lazy val meta : Meta = ctx.meta
  final lazy val ownerRef: DFRef[DFBlock] = DFRef(ctx.ownerOption.getOrElse(this))
  final private[ZFiant] val __db : DFDesign.DB = ctx.db
  final override val isTop : Boolean = ctx.ownerOption.isEmpty
  final override val topDesign : DFDesign = if (isTop) this else owner.topDesign
  final override val fullName : String = if (isTop) name else s"${owner.fullName}.${name}"
  override def toString: String = ctx.meta.name
  __db.addMember(this)
}

@implicitNotFound(ContextOf.MissingError.msg)
final case class ContextOf[T <: DFDesign](meta : Meta, ownerOption : Option[DFBlock], db: DFDesign.DB) extends DFMember.Context {
  lazy val owner : DFBlock = ownerOption.get
}
object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtx[T1 <: DFDesign, T2 <: DFDesign](
    implicit ctx : ContextOf[T1], mustBeTheClassOf: MustBeTheClassOf[T1]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.ownerOption, ctx.db)
  implicit def evTop[T <: DFDesign](
    implicit meta: Meta, topLevel : TopLevel, mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority
  ) : ContextOf[T] = ContextOf[T](meta, None, new DFDesign.DB)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context

  implicit class DevAccess(design : DFDesign) {
    def db : DB.Immutable = design.__db.immutable
  }
  class DB {
    private var members : List[DFMember] = List()
    def addMember[M <: DFMember](member : M) : M = {
      members = members :+ member
      member
    }
    def getMembers : List[DFMember] = members
    private var refTable : Map[DFRef[_], DFMember] = Map()
    def addRef[T <: DFMember](ref : DFRef[T], member : DFMember) : DFRef[T] = {
      refTable = refTable + (ref -> member)
      ref
    }
    def getRefTable : Map[DFRef[_], DFMember] = refTable
    def immutable : DB.Immutable = {
      val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert
      val refMembers : List[DFMember] = members.collect {
        case net : DFNet => net
        case m if memberTable.contains(m) => m
        case m : DFDesign if m.isTop => m
      }
      DB.Immutable(refMembers, refTable)
    }
  }

  object DB {
    case class Immutable(members : List[DFMember], refTable : Map[DFRef[_], DFMember]) {
      lazy val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert
    }
  }
}