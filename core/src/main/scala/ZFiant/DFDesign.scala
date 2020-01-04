package ZFiant
import DFiant.internals._

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.mutable

abstract class DFDesign(implicit ctx : DFDesign.Context) extends HasTypeName with Implicits {
  val block : DFDesign.Block = DFDesign.Block.Internal(typeName)(ctx)
  private[DFDesign] val __db: DFDesign.DB.Mutable = ctx.db
  private val ownerInjector : DFMember.OwnerInjector = new DFMember.OwnerInjector(block)
  protected implicit val __getset : MemberGetSet = ctx.db.getset

  ///////////////////////////////////////////////////////////////////
  // Context implicits
  ///////////////////////////////////////////////////////////////////
  final protected implicit def __anyContext(implicit meta : Meta) : DFAny.Context =
    new DFAny.Context(meta, ownerInjector, ctx.db)
  final protected implicit def __blockContext(implicit meta : Meta) : DFBlock.Context =
    new DFBlock.Context(meta, ownerInjector, ctx.db)
  final protected implicit def __designContextOf[T <: DFDesign](implicit meta : Meta) : ContextOf[T] =
    new ContextOf[T](meta, ownerInjector, ctx.db)
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
final class ContextOf[T <: DFDesign](val meta : Meta, val ownerInjector : DFMember.OwnerInjector, val db: DFDesign.DB.Mutable) extends DFMember.Context {
  lazy val owner : DFBlock = ownerInjector.get
}
object ContextOf {
  final object MissingError extends ErrorMsg (
    "Missing an implicit DFDesign Context.",
    "missing-context"
  ) {final val msg = getMsg}
  implicit def evCtx[T1 <: DFDesign, T2 <: DFDesign](
    implicit ctx : ContextOf[T1], mustBeTheClassOf: MustBeTheClassOf[T1]
  ) : ContextOf[T2] = new ContextOf[T2](ctx.meta, ctx.ownerInjector, ctx.db)
  implicit def evTop[T <: DFDesign](
    implicit meta: Meta, topLevel : TopLevel, mustBeTheClassOf: MustBeTheClassOf[T], lp : shapeless.LowPriority
  ) : ContextOf[T] = new ContextOf[T](meta, null, new DFDesign.DB.Mutable)
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
    final case class Internal(ownerRef : DFBlock.Ref, tags : DFMember.Tags)(designType: String) extends Block {
      def setTags(tags : DFMember.Tags)(implicit getset : MemberGetSet) : DFMember = getset.set(this, copy(tags = tags)(designType))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(designType : String)(implicit ctx : Context) : Block = ctx.db.addMember(
        if (ctx.ownerInjector == null) Top(ctx.meta)(ctx.db, designType) else Internal(ctx.owner, ctx.meta)(designType))
    }

    final case class Top(tags : DFMember.Tags)(db: DB.Mutable, designType: String) extends Block {
      override lazy val ownerRef : DFBlock.Ref = ???
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
  final case class DB(members : List[DFMember], refTable : Map[DFMember.Ref[_], DFMember]) {
    lazy val top : Block.Top = members.head match {
      case m : Block.Top => m
    }
    implicit val getset : MemberGetSet = new MemberGetSet {
      def apply[T <: DFMember](ref: DFMember.Ref[T]): T = refTable(ref).asInstanceOf[T]
      def set[T <: DFMember](originalMember : T, newMember: T): T = newMember
    }
    lazy val memberTable : Map[DFMember, Set[DFMember.Ref[_]]] = refTable.invert

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
          val updatedOML = (localOwner -> localMembers) :: oml
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          assert(updatedStack0.isEmpty, updatedStack0) //sanity check
          (localOwner -> localMembers) :: oml
      }
    }

    //holds the topological order of owner block dependency
    lazy val ownerMemberList : List[(DFBlock, List[DFMember])] =
      OMLGen(List(), members.drop(1), List(top -> List())) //head will always be the TOP block
    def printOwnerMemberList() : Unit =
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val ownerMemberTable : Map[DFBlock, List[DFMember]] = Map(ownerMemberList : _*)

    //replaces all members and references according to the patch list
    def patch(patchList : List[(DFMember, DB.Patch)]) : DB = if (patchList.isEmpty) this else {
      //If we attempt to replace with an existing member, then we convert the patch to remove
      //the old member just for the member list (references are replaced).
      val patchTable = patchList.map {
        case (m, DB.Patch.Replace(r, DB.Patch.Replace.Config.FullReplacement)) if memberTable.contains(r) => (m, DB.Patch.Remove)
        case x => x
      }.toMap
      //Patching member list
      val patchedMembers = members.flatMap(m => patchTable.get(m) match {
        case Some(DB.Patch.Replace(r, config)) => config match {
          case DB.Patch.Replace.Config.ChangeRefOnly => Some(m)
          case DB.Patch.Replace.Config.FullReplacement => Some(r)
        }
        case Some(DB.Patch.Add(db, before)) =>
          //adding the members without its Top members either before or after the patched member
          if (before) db.members.drop(1) :+ m
          else m :: db.members.drop(1)
        case Some(DB.Patch.Remove) => None
        case Some(_ : DB.Patch.ChangeRef[_]) => Some(m)
        case None => Some(m) //not in the patch table, therefore remain as-is
      })
      //Patching reference table
      val patchedRefTable = patchList.foldLeft(refTable) {
        case (rt, (origMember, DB.Patch.Replace(repMember, config))) => memberTable.get(origMember) match {
          case Some(refs) => refs.foldLeft(rt)((rt2, r) => rt2.updated(r, repMember))
          case None => rt
        }
        case (rt, (origMember, DB.Patch.Add(db, _))) =>
          val dbPatched = db.patch(db.top -> DB.Patch.Replace(origMember.getOwner, DB.Patch.Replace.Config.ChangeRefOnly))
          rt ++ dbPatched.refTable
        case (rt, (origMember, DB.Patch.Remove)) => memberTable.get(origMember) match {
          case Some(refs) => refs.foldLeft(rt)((rt2, r) => rt2 - r)
          case None => rt
        }
        case (rt, (_, DB.Patch.ChangeRef(origMember, refFunc, updatedRefMember))) =>
          val ref = refFunc(origMember)
          rt + (ref -> updatedRefMember)
      }
      DB(patchedMembers, patchedRefTable)
    }
    def patch(singlePatch : (DFMember, DB.Patch)) : DB = patch(List(singlePatch))
    @tailrec private def getGuards(currentOwner : DFBlock, targetOwner : DFBlock, currentGuards : List[DFAny]) : List[DFAny] =
      currentOwner match {
        case _ : DFDesign.Block => currentGuards //reached the design block
        case o : DFBlock if o == targetOwner => currentGuards //can't go past the target owner
        case cb : ConditionalBlock.IfBlock => getGuards(cb.getOwner, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseIfBlock => getGuards(cb.prevBlockRef, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseBlock => getGuards(cb.prevBlockRef, targetOwner, currentGuards)
        case cb : ConditionalBlock.CasePatternBlock[_] => getGuards(cb.getOwner, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
        case cb : ConditionalBlock.Case_Block => getGuards(cb.getOwner, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
      }

    //for a given consumer, we get a set of its producers
    lazy val consumerDependencyTable : Map[DFAny, Set[DFAny]] = {
      //first passing through all nets to get directionality of aliased values
      val netPass = members.foldLeft(Map.empty[DFAny, Set[DFAny]])((dt, m) => m match {
        case n : DFNet =>
          val toVal = n.toRef.get
          val fromVal = n.fromRef.get
          val depSet = dt.getOrElse(toVal, Set()) + fromVal
          val guards = n match {
            case _ : DFNet.Connection => List() //connections are insensitive to guards
            case a : DFNet.Assignment => getGuards(a.getOwner, toVal.getOwner, List())
          }
          dt + (toVal -> (depSet ++ guards))
        case _ => dt
      })
      members.foldLeft(netPass)((dt, m) => m match {
        case f : DFAny.Func2[_,_,_,_] => dt + (f-> Set(f.leftArgRef.get, f.rightArgRef.get))
        case a : DFAny.Alias[_,_,_] =>
          val relVal = a.relValRef.get
          (dt.get(a), dt.get(relVal)) match {
            //when alias is written to, then the relative value is also dependent on the alias
            case (Some(aDeps), Some(relDeps)) => dt + (a -> (aDeps + relVal)) + (relVal -> (relDeps + a))
            case (Some(aDeps), None) => dt + (a -> (aDeps + relVal)) + (relVal -> Set(a))
            //the alias is just read, and therefore it's dependent only on the rel
            case (None, _) => dt + (a -> Set(relVal))
          }
        case _ => dt
      })
    }

    //for a given producer, we get a set of its consumers
    lazy val producerDependencyTable : Map[DFAny, Set[DFAny]] = {
      consumerDependencyTable.foldLeft(Map.empty[DFAny, Set[DFAny]]){case (dt, (consumer, producerSet)) =>
        dt ++ producerSet.map(p => p -> (dt.getOrElse(p, Set()) + consumer))}
    }


    def printConsumerDependencyTable : DB = {
      println(consumerDependencyTable.map(op => s"${op._1.getFullName} <- ${op._2.map(e => e.getFullName)}").mkString("\n"))
      this
    }

    def printProducerDependencyTable : DB = {
      println(producerDependencyTable.map(op => s"${op._1.getFullName} -> ${op._2.map(e => e.getFullName)}").mkString("\n"))
      this
    }

    def printOwnership() : DB = {
      println(members.map(m => (m -> m.getOwner).toString()).mkString("\n"))
      this
    }
  }

  object DB {
    sealed trait Patch extends Product with Serializable
    object Patch {
      final case object Remove extends Patch
      final case class Replace(updatedMember : DFMember, config : Replace.Config) extends Patch
      object Replace {
        sealed trait Config extends Product with Serializable
        object Config {
          //only modifies the reference table so that all members currently referencing the original member will reference
          //the updated member.
          case object ChangeRefOnly extends Config
          //The updated member is replacing the original member in the member list and all members currently
          //referencing the existing member will reference the updated member.
          //If the updated member already exists in the member list (at a different position), then the original member is
          //removed from the list without being replaced in its position.
          case object FullReplacement extends Config
        }
      }
      final case class Add(db : DB, before : Boolean) extends Patch
      final case class ChangeRef[T <: DFMember](member : T, refAccess : T => DFMember.Ref[_ <: DFMember], updatedRefMember : DFMember) extends Patch
    }
    class Mutable {
      private val members : mutable.ListBuffer[DFMember] = mutable.ListBuffer()
      def addConditionalBlock[Ret, CB <: ConditionalBlock.Of[Ret]](cb : CB, block : => Ret)(implicit ctx : DFBlock.Context) : CB = {
        addMember(cb)
        cb.applyBlock(block)
        cb
      }
      def addMember[M <: DFMember](member : M) : M = {
        members += member
        member
      }
      private val refTable : mutable.Map[DFMember.Ref[_], DFMember] = mutable.Map()
      def getMember[T <: DFMember](ref : DFMember.Ref[T]) : T = refTable(ref).asInstanceOf[T]
      def setMember[T <: DFMember](originalMember : T, newMember : T) : T = {
        val idx = members.length - 1 - members.reverseIterator.indexOf(originalMember) //more likely to set latest members
        members.update(idx, newMember)
        newMember
      }
      def newRefFor[T <: DFMember, R <: DFMember.Ref[T]](ref : R, member : T) : R = {
        refTable += (ref -> member)
        ref
      }
      def immutable : DB = DB(members.toList, refTable.toMap)

      implicit val getset : MemberGetSet = new MemberGetSet {
        def apply[T <: DFMember](ref: DFMember.Ref[T]): T = getMember(ref)
        def set[T <: DFMember](originalMember : T, newMember: T): T = setMember(originalMember, newMember)
      }
    }
  }
}