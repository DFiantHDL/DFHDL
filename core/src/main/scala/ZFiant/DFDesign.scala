package ZFiant
import DFiant.internals._

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable

abstract class DFDesign(implicit ctx : DFDesign.Context) extends HasTypeName with Implicits {
  val block : DFDesign.Block = DFDesign.Block.Internal(typeName)(ctx)
  private[DFDesign] val __db: DFDesign.DB.Mutable = ctx.db
  __db.__injectedOwner = block
  protected implicit val __getset : MemberGetSet = ctx.db.getset

  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  final protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    new DFAny.Context(meta, __db.__injectedOwner, ctx.db)
  final protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    new DFBlock.Context(meta, Some(__db.__injectedOwner), ctx.db)
  final protected implicit def __designContextOf[T <: DFDesign](implicit meta : Meta) : ContextOf[T] =
    new ContextOf[T](meta, Some(__db.__injectedOwner), ctx.db)
  ///////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////
  // Conditional Constructs
  ///////////////////////////////////////////////////////////////////
  final protected def ifdf[C, B](cond : DFBool.Op.Able[C])(block : => Unit)(
    implicit ctx : DFBlock.Context, condConv : DFBool.`Op:=`.Builder[DFBool.Type, C]
  ) : ConditionalBlock.NoRetVal.IfBlock = ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(),cond))(block)(ctx)
  final protected def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] = ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)(ctx)
  ///////////////////////////////////////////////////////////////////
}

@implicitNotFound(ContextOf.MissingError.msg)
final class ContextOf[T <: DFDesign](val meta : Meta, ownerOptionFunc : => Option[DFBlock], val db: DFDesign.DB.Mutable) extends DFMember.Context {
  lazy val ownerOption : Option[DFBlock] = ownerOptionFunc
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
  ) : ContextOf[T] = new ContextOf[T](meta, None, new DFDesign.DB.Mutable)
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context

  implicit class DesignExtender[T <: DFDesign](design : T) {
    import design.__db.getset
    private def onBlock(b : Block => Unit) : T = {b(design.block); design}
    def setName(value : String) : T = onBlock(_.setName(value))
    def keep : T = onBlock(_.keep)
  }

  sealed trait Block extends DFBlock {
    def headerCodeString(implicit getset: MemberGetSet): String = s"trait $typeName extends DFDesign"
  }
  object Block {
    final case class Internal(ownerRef : DFRef[DFBlock], tags : DFMember.Tags)(designType: String) extends Block {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags)(designType))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(designType : String)(implicit ctx : Context) : Block = ctx.db.addMember(
        if (ctx.ownerOption.isEmpty) Top(ctx.meta)(ctx.db, designType) else Internal(ctx.owner, ctx.meta)(designType))
    }

    final case class Top(tags : DFMember.Tags)(db: DB.Mutable, designType: String) extends Block {
      override lazy val ownerRef : DFRef[DFBlock] = ???
      override def getOwner(implicit getset : MemberGetSet): DFBlock = this
      override val isTop: Boolean = true
      override lazy val typeName : String = designType
      override def getFullName(implicit getset : MemberGetSet): String = name
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags)(db, designType))
    }
  }

  implicit class DevAccess(design : DFDesign) {
    def db : DB = design.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFRef[_], DFMember]) {
    lazy val top : Block.Top = members.head match {
      case m : Block.Top => m
    }
    implicit val getset : MemberGetSet = new MemberGetSet {
      def apply[T <: DFMember](ref: DFRef[T]): T = refTable(ref).asInstanceOf[T]
      def set[T <: DFMember](originalMember : T, newMember: T): T = newMember
    }
    lazy val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert

    //Owner-to-members list generation via a tail recursive function that topologically sorts the blocks according to dependency
    @tailrec private def OMLGen(
      oml : List[(DFBlock, List[DFMember])], globalMembers : List[DFMember], localStack : List[(DFBlock, List[DFMember])]
    ) : List[(DFBlock, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if m.getOwner == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (localMembers :+ m)) :: updatedStack0
          m match {
            case o : DFBlock => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              OMLGen(oml, mList, updatedStack2)
            case _ => //Just a member
              OMLGen(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = oml :+ (localOwner -> localMembers)
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          assert(updatedStack0.isEmpty, updatedStack0) //sanity check
          oml :+ (localOwner -> localMembers)
      }
    }

    //holds the topological order of owner block dependency
    lazy val ownerMemberList : List[(DFBlock, List[DFMember])] =
      OMLGen(List(), members.drop(1), List(top -> List())) //head will always be the TOP block
    def printOwnerMemberList() : Unit =
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val ownerMemberTable : Map[DFBlock, List[DFMember]] = Map(ownerMemberList : _*)

    //replaces all members according to the patch table (origMember -> Some(repMember))
    //If the replacement member is None, then the original member needs to be removed
    def patch(patchTable : Map[DFMember, Option[DFMember]]) : DB = {
      val patchedMembers = members.collect {
        case origMember if patchTable.get(origMember).isDefined && patchTable(origMember).isDefined => patchTable(origMember).get
        case origMember if patchTable.get(origMember).isEmpty => origMember
      }
      val patchedRefTable = patchTable.foldLeft(refTable) {
        case (rt, (origMember, Some(repMember))) => memberTable.get(origMember) match {
          case Some(refs) => refs.foldLeft(rt)((rt2, r) => rt2.updated(r, repMember))
          case None => rt
        }
        case (rt, (origMember, None)) => memberTable.get(origMember) match {
          case Some(refs) => refs.foldLeft(rt)((rt2, r) => rt2 - r)
          case None => rt
        }
      }
      DB(patchedMembers, patchedRefTable)
    }

    def printOwnership() : DB = {
      println(members.map(m => (m -> m.getOwner).toString()).mkString("\n"))
      this
    }
  }

  object DB {
    class Mutable {
      private var members : Vector[DFMember] = Vector()
      private[ZFiant] var __injectedOwner : DFBlock = _
      def addConditionalBlock[Ret, CB <: ConditionalBlock[Ret]](cb : CB, block : => Ret)(implicit getset : MemberGetSet) : CB = {
        addMember(cb)
        cb.applyBlock(block, this)
        cb
      }
      def addMember[M <: DFMember](member : M) : M = {
        memberTable = memberTable + (member -> (Set(), members.length))
        members = members :+ member
        member
      }
      private var refTable : Map[DFRef[_], Int] = Map()
      private var memberTable : Map[DFMember, (Set[DFRef[_]], Int)] = Map()
      def getMember[T <: DFMember](ref : DFRef[T]) : T = members(refTable(ref)).asInstanceOf[T]
      def setMember[T <: DFMember](originalMember : T, newMember : T) : T = {
        val cell = memberTable(originalMember)
        members = members.updated(cell._2, newMember)
        memberTable = memberTable - originalMember
        memberTable = memberTable + (newMember -> cell)
        newMember
      }
      def newRefFor[T <: DFMember](member : T) : DFRef[T] = {
        val cell = memberTable(member)
        val ref = new DFRef[T]
        memberTable = memberTable + (member -> cell.copy(_1 = cell._1 + ref))
        refTable = refTable + (ref -> cell._2)
        ref.asInstanceOf[DFRef[T]]
      }
      def getRefs[T <: DFMember](member : T) : Set[DFRef[T]] = {
        val cell = memberTable(member)
        cell._1.asInstanceOf[Set[DFRef[T]]]
      }
      def immutable : DB = {
        val refMembers : List[DFMember] = members.collect {
          case net : DFNet => net
          case m if memberTable(m)._1.nonEmpty => m
          case m if m.tags.keep => m
          case m : DFDesign.Block.Top => m
        }.toList
        DB(refMembers, refTable.mapValues(i => members(i)))
      }

      implicit val getset : MemberGetSet = new MemberGetSet {
        def apply[T <: DFMember](ref: DFRef[T]): T = getMember(ref)
        def set[T <: DFMember](originalMember : T, newMember: T): T = setMember(originalMember, newMember)
      }
    }
  }
}