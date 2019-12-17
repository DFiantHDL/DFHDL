package ZFiant
import DFiant.internals._

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable

abstract class DFDesign(implicit ctx : DFDesign.Context) extends HasTypeName with Implicits {
  private[ZFiant] val block : DFBlock = DFDesign.Block(typeName)(ctx)
  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  final protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    DFAny.Context(meta, block.__injectedOwner, block.topDesign.__db)
  final protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    DFBlock.Context(meta, Some(block.__injectedOwner), block.topDesign.__db)
  final protected implicit def __designContextOf[T <: DFDesign](implicit meta : Meta) : ContextOf[T] =
    ContextOf[T](meta, Some(block.__injectedOwner), block.topDesign.__db)
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
  final case class Block(ownerRef : DFRef[DFBlock], meta : Meta)(designType: String) extends DFBlock

  final case class TopBlock(meta: Meta)(db: DB.Mutable, designType: String) extends DFBlock {
    override lazy val ownerRef: DFRef[DFBlock] = ???
    override lazy val owner: DFBlock = this
    override val isTop: Boolean = true
    private[ZFiant] val __db: DFDesign.DB.Mutable = db
    override val topDesign: TopBlock = this
    override lazy val typeName : String = designType
    override val fullName: String = name
  }
  object Block {
    def apply(designType : String)(implicit ctx : Context) : DFBlock = ctx.db.addMember(
      if (ctx.ownerOption.isEmpty) TopBlock(ctx.meta)(ctx.db, designType) else Block(ctx.owner, ctx.meta)(designType))
  }

  implicit class DevAccess(design : DFDesign) {
    def db : DB = design.block.topDesign.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFRef[_], DFMember]) {
    lazy val top : TopBlock = members.head match {
      case m : TopBlock => m
    }
    lazy val memberTable : Map[DFMember, Set[DFRef[_]]] = refTable.invert
    @tailrec private def recur(
      oml : List[(DFBlock, List[DFMember])], globalMembers : List[DFMember], localStack : List[(DFBlock, List[DFMember])]
    ) : List[(DFBlock, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if m.owner == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (localMembers :+ m)) :: updatedStack0
          m match {
            case o : DFBlock => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              recur(oml, mList, updatedStack2)
            case _ => //Just a member
              recur(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = oml :+ (localOwner -> localMembers)
          recur(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          assert(updatedStack0.isEmpty, updatedStack0) //sanity check
          oml :+ (localOwner -> localMembers)
      }
    }

    lazy val ownerMemberList : List[(DFBlock, List[DFMember])] =
      recur(List(), members.drop(1), List(top -> List())) //head will always be the TOP block
    def getMembersOf(owner : DFBlock) : List[DFMember] = {
      val ownerIdx = members.indexOf(owner)
      ???
    }

    def printOwnership() : Unit = {
      println(members.map(m => (m -> m.owner).toString()).mkString("\n"))
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
          case m : DFDesign.TopBlock => m
        }
        DB(refMembers, refTable)
      }
    }
  }
}