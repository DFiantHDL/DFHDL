package ZFiant
import DFiant.internals._

import scala.annotation.implicitNotFound

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFBlock {
  final lazy val meta : Meta = ctx.meta
  final lazy val ownerRef: DFRef[DFBlock] = DFRef(ctx.ownerOption.getOrElse(this))
  final private[ZFiant] val __db : DFDesign.DB.Mutable = ctx.db
  final override val isTop : Boolean = ctx.ownerOption.isEmpty
  final override val topDesign : DFDesign = if (isTop) this else owner.topDesign
  final override val fullName : String = if (isTop) name else s"${owner.fullName}.${name}"
  __db.addMember(this)
}

@implicitNotFound(ContextOf.MissingError.msg)
final case class ContextOf[T <: DFDesign](meta : Meta, ownerOption : Option[DFBlock], db: DFDesign.DB.Mutable) extends DFMember.Context {
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
  ) : ContextOf[T] = ContextOf[T](meta, None, new DFDesign.DB.Mutable)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context

  implicit class DevAccess(design : DFDesign) {
    def db : DB = design.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFRef[_], DFMember]) {
    lazy val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert
//    private def ownerTableGen(
//      ot : Map[DFBlock, List[DFMember]], remainingMembers : List[DFMember], currentOwner : DFBlock, currentList : List[DFMember]
//    ) : Map[DFBlock, List[DFMember]] = {
//
//    }

//    def dfsSort(
//      remainingMembers : List[DFMember], currentOwner : DFBlock, currentList : List[DFMember]
//    ) : List[DFMember] = remainingMembers match {
//      case (x : DFBlock) :: xs if (x.owner == currentOwner) => dfsSort(xs, x, )
//    }
    lazy val ownerTable : Map[DFBlock, List[DFMember]] = ???
    def getMembersOf(owner : DFBlock) : List[DFMember] = {
      val ownerIdx = members.indexOf(owner)
      ???
    }
  }

  object DB {
    class Mutable {
      private var members : List[DFMember] = List()
      def addConditionalBlock[CB <: ConditionalBlock[_]](cb : CB) : CB = {
        members = members :+ cb
        cb.applyBlock
        cb
      }
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
      def immutable : DB = {
        val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert
        val refMembers : List[DFMember] = members.collect {
          case net : DFNet => net
          case m if memberTable.contains(m) => m
          case m : DFDesign if m.isTop => m
        }
        DB(refMembers, refTable)
      }
    }
  }
}