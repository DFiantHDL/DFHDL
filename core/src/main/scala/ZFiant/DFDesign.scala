package ZFiant
import DFiant.internals._

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.mutable
import ZFiant.compiler.printer.Printer

abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFInterface {
  private[ZFiant] lazy val inlinedRep : Option[DFInlineComponent.Rep] = None
  private[ZFiant] final var block : DFDesign.Block = DFDesign.Block.Internal(typeName, inlinedRep)(ctx)
  private[ZFiant] final val __db: DFDesign.DB.Mutable = ctx.db
  private[ZFiant] final val ownerInjector : DFMember.OwnerInjector = new DFMember.OwnerInjector(block)
  final protected implicit val __getset : MemberGetSet = ctx.db.getSet

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
  ) : ConditionalBlock.NoRetVal.IfBlock = ConditionalBlock.NoRetVal.IfBlock(condConv(DFBool.Type(logical = true),cond))(block)(ctx)
  final protected def matchdf[MVType <: DFAny.Type](matchValue : DFAny.Of[MVType], matchConfig : MatchConfig = MatchConfig.NoOverlappingCases)(
    implicit ctx : DFBlock.Context
  ): ConditionalBlock.NoRetVal.MatchHeader[MVType] = ConditionalBlock.NoRetVal.MatchHeader[MVType](matchValue, matchConfig)(ctx)
  ///////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////
  // Ability to run construction at the owner's context
  ///////////////////////////////////////////////////////////////////
  final protected def atOwnerDo[T](block : => T) : T = ownerInjector.injectOwnerAndRun(ctx.owner)(block)
  ///////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////
  // Design block tags mutating methods
  ///////////////////////////////////////////////////////////////////
  implicit class DesignExtender[T <: DFDesign](design : T) {
    private def onBlock(b : DFDesign.Block => DFDesign.Block) : T = {design.block = b(design.block); design}
    def setName(value : String) : T = onBlock(_.setName(value))
    def keep : T = onBlock(_.keep)
    def !!(customTag : DFDesign.Block.CustomTag) : T = onBlock(_.!!(customTag))
  }
  ///////////////////////////////////////////////////////////////////
}

abstract class MetaDesign(lateConstruction : Boolean = false)(implicit ctx : ContextOf[MetaDesign]) extends DFDesign {
  final def addMember[T <: DFMember](member : T) : T = __db.addMember(member)
  final protected implicit val lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(lateConstruction)
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

  trait Implicits extends
    DFBits.Op.Implicits with
    DFUInt.Op.Implicits with
    DFSInt.Op.Implicits with
    DFEnum.Op.Implicits with
    DFBool.Op.Implicits


//  implicit class DesignExtender[T <: DFDesign](design : T) {
//    import design.__db.getSet
//    private def onBlock(b : Block => Unit) : T = {b(design.block); design}
//    def setName(value : String) : T = onBlock(_.setName(value))
//    def keep : T = onBlock(_.keep)
//    def !!(customTag : Block.CustomTag) : T = onBlock(_.!!(customTag))
//  }

  sealed trait Block extends DFBlock {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = Block.CustomTag
    val designType: String
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config): String = s"trait $designType extends DFDesign"
  }
  object Block {
    trait CustomTag extends DFMember.CustomTag
    final case class Internal(designType: String, ownerRef : DFBlock.Ref, tags : DFMember.Tags.Basic, inlinedRep : Option[DFInlineComponent.Rep]) extends Block {
      protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case Internal(designType, _, tags, inlinedRep) =>
          val inlineRepEq = (this.inlinedRep, inlinedRep) match {
            case (Some(l), Some(r)) => l =~ r
            case (None, None) => true
            case _ => false
          }
          this.designType == designType && this.tags =~ tags && inlineRepEq
        case _ => false
      }
      def setTags(tags : DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this, copy(tags = tags))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(designType : String, inlinedRep : Option[DFInlineComponent.Rep])(implicit ctx : Context) : Block = ctx.db.addMember(
        if (ctx.ownerInjector == null) Top(designType, ctx.meta)(ctx.db) else Internal(designType, ctx.owner, ctx.meta, inlinedRep))
    }
    final case class Top(designType: String, tags : DFMember.Tags.Basic)(db: DB.Mutable) extends Block {
      override lazy val ownerRef : DFBlock.Ref = ???
      override def getOwner(implicit getSet : MemberGetSet): DFBlock = this
      override val isTop: Boolean = true
      protected[ZFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case Top(designType, tags) =>
          this.designType == designType && this.tags =~ tags
        case _ => false
      }
      override lazy val typeName : String = designType
      override def getFullName(implicit getSet : MemberGetSet): String = name
      def setTags(tags : DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this, copy(tags = tags)(db))
    }
  }

  implicit class DevAccess(design : DFDesign) {
    def getDB : DB = design.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFMember.Ref, DFMember]) {self =>
    lazy val top : Block.Top = members.head match {
      case m : Block.Top => m
    }
    implicit val __getset : MemberGetSet = new MemberGetSet {
      def designDB : DFDesign.DB = self
      def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def set[M <: DFMember](originalMember : M, newMember: M): M = newMember
    }
    lazy val memberTable : Map[DFMember, Set[DFMember.Ref]] = refTable.invert

    //There can only be a single connection to a value (but multiple assignments are possible)
    //                              To    From
    lazy val connectionTable : Map[DFAny, DFAny] =
      members.collect{case n : DFNet.Connection => (n.toRef.get, n.fromRef.get)}.toMap

    lazy val connectionTableInverted : Map[DFAny, Set[DFAny]] =
      connectionTable.invert

    //                               To      From
    lazy val assignmentsTable : Map[DFAny, Set[DFAny]] =
      members.foldLeft(Map.empty[DFAny, Set[DFAny]]){
        case (at, DFNet.Assignment.Unref(toVal, fromVal, _, _)) =>
          at + (toVal -> (at.getOrElse(toVal, Set()) + fromVal))
        case (at, _) => at
      }

    //                                       From      To
    lazy val assignmentsTableInverted : Map[DFAny, Set[DFAny]] =
      members.foldLeft(Map.empty[DFAny, Set[DFAny]]){
        case (at, DFNet.Assignment.Unref(toVal, fromVal, _, _)) =>
          at + (fromVal -> (at.getOrElse(fromVal, Set()) + toVal))
        case (at, _) => at
      }

    def getConnectionTo(v : DFAny) : Option[DFAny] = connectionTable.get(v)
    def getConnectionFrom(v : DFAny) : Set[DFAny] = connectionTableInverted.getOrElse(v, Set())

    def getAssignmentsTo(v : DFAny) : Set[DFAny] = assignmentsTable.getOrElse(v, Set())
    def getAssignmentsFrom(v : DFAny) : Set[DFAny] = assignmentsTableInverted.getOrElse(v, Set())

//    def getAliasesTo(v : DFAny) : Option[DFAny] =
//      members.collectFirst{case n : DFNet.Connection if n.toRef.get == v => n.fromRef.get}

    //Owner-to-members list generation via a tail recursive function that topologically sorts the blocks according to dependency
    @tailrec private def OMLGen(
      oml : List[(DFBlock, List[DFMember])], globalMembers : List[DFMember], localStack : List[(DFBlock, List[DFMember])]
    ) : List[(DFBlock, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if m.getOwner == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
          m match {
            case o : DFBlock => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              OMLGen(oml, mList, updatedStack2)
            case _ => //Just a member
              OMLGen(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil if updatedStack0.nonEmpty =>
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          (localOwner -> localMembers.reverse) :: oml
      }
    }

    //holds the topological order of owner block dependency
    lazy val ownerMemberList : List[(DFBlock, List[DFMember])] =
      OMLGen(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP block
    def printOwnerMemberList() : Unit =
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val ownerMemberTable : Map[DFBlock, List[DFMember]] = Map(ownerMemberList : _*)

    //Owner-to-members list generation via a tail recursive function that topologically sorts the blocks according to dependency
    @tailrec private def DMLGen(
      oml : List[(DFDesign.Block, List[DFMember])], globalMembers : List[DFMember], localStack : List[(DFDesign.Block, List[DFMember])]
    ) : List[(DFDesign.Block, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if m.getOwnerDesign == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
          m match {
            case o : DFDesign.Block => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              DMLGen(oml, mList, updatedStack2)
            case _ => //Just a member
              DMLGen(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          DMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil if updatedStack0.nonEmpty =>
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          DMLGen(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          (localOwner -> localMembers.reverse) :: oml
      }
    }
    //holds the topological order of design block dependency
    lazy val designMemberList : List[(DFDesign.Block, List[DFMember])] =
      DMLGen(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP block

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val designMemberTable : Map[DFDesign.Block, List[DFMember]] = Map(designMemberList : _*)

    private implicit class RefTableOps(rt : Map[DFMember.Ref, DFMember]) {
      def replaceMember(origMember : DFMember, repMember : DFMember, scope : DB.Patch.Replace.Scope) : Map[DFMember.Ref, DFMember] =
        memberTable.get(origMember) match {
          case Some(refs) =>
            val scopeRefs = scope match {
              case DB.Patch.Replace.Scope.All => refs
              case DB.Patch.Replace.Scope.Outside(block) =>
                //for references that have owner references of their own, we check the owners location with respect
                //to the requested scope
                refs.collect{case r : DFMember.OwnedRef if r.owner.get.isOutsideDesign(block) => r}
              case DB.Patch.Replace.Scope.Inside(block) =>
                //for references that have owner references of their own, we check the owners location with respect
                //to the requested scope
                refs.collect{case r : DFMember.OwnedRef if r.owner.get.isInsideDesign(block) => r}
            }
            scopeRefs.foldLeft(rt)((rt2, r) => rt2.updated(r, repMember))
          case None =>
            rt
        }
    }

    //replaces all members and references according to the patch list
    def patch(patchList : List[(DFMember, DB.Patch)]) : DB = if (patchList.isEmpty) this else {
      val patchTable = patchList.map {
        //On change ref and remove replacement we setup the original member for removal here
        case (m, DB.Patch.Replace(_, DB.Patch.Replace.Config.ChangeRefAndRemove, _)) => (m, DB.Patch.Remove)
        //If we attempt to replace with an existing member, then we convert the patch to remove
        //the old member just for the member list (references are replaced).
        case (m, DB.Patch.Replace(r, DB.Patch.Replace.Config.FullReplacement, _)) if memberTable.contains(r) => (m, DB.Patch.Remove)
        //If we add after a design block, we need to actually place after the last member of the block
        case (block : DFDesign.Block.Internal, DB.Patch.Add(db, DB.Patch.Add.Config.After)) =>
          (designMemberTable(block).last, DB.Patch.Add(db, DB.Patch.Add.Config.After))
        //If we add inside a design block, we need to actually place after the last member of the block
        case (block : DFDesign.Block, DB.Patch.Add(db, DB.Patch.Add.Config.Inside)) =>
          designMemberTable(block).lastOption match {
            case Some(l) => (l, DB.Patch.Add(db, DB.Patch.Add.Config.After))
            case None => (block, DB.Patch.Add(db, DB.Patch.Add.Config.After))
          }
        case x => x
      }.toMap
      //Patching member list
      val patchedMembers = members.flatMap(m => patchTable.get(m) match {
        case Some(DB.Patch.Replace(r, config, _)) => config match {
          case DB.Patch.Replace.Config.ChangeRefOnly => Some(m)
          case DB.Patch.Replace.Config.ChangeRefAndRemove => Some(r)
          case DB.Patch.Replace.Config.FullReplacement => Some(r)
        }
        case Some(DB.Patch.Add(db, config)) =>
          val notTop = db.members.drop(1) //adding the members without the Top design block
          config match {
            case DB.Patch.Add.Config.After => m :: notTop
            case DB.Patch.Add.Config.Before => notTop :+ m
            case DB.Patch.Add.Config.ReplaceWithFirst(DB.Patch.Replace.Config.ChangeRefOnly, _) => m :: notTop
            case DB.Patch.Add.Config.ReplaceWithLast(DB.Patch.Replace.Config.ChangeRefOnly, _) => notTop :+ m
            case DB.Patch.Add.Config.ReplaceWithFirst(_, _) => notTop
            case DB.Patch.Add.Config.ReplaceWithLast(_, _) => notTop
            case DB.Patch.Add.Config.Via => m :: notTop
            case DB.Patch.Add.Config.Inside => ??? //Not possible since we replaced it to an `After`
          }
        case Some(DB.Patch.Remove) => None
        case Some(_ : DB.Patch.ChangeRef[_]) => Some(m)
        case None => Some(m) //not in the patch table, therefore remain as-is
      })
      //Patching reference table
      val patchedRefTable = patchList.foldLeft(refTable) {
        case (rt, (origMember, DB.Patch.Replace(repMember, _, scope))) => rt.replaceMember(origMember, repMember, scope)
        case (rt, (origMember, DB.Patch.Add(db, config))) =>
          val newOwner = config match {
            case DB.Patch.Add.Config.Inside => origMember
            case _ => origMember.getOwner
          }
          val dbPatched = db.patch(db.top -> DB.Patch.Replace(newOwner, DB.Patch.Replace.Config.ChangeRefOnly))
          val repRT = config match {
            case DB.Patch.Add.Config.ReplaceWithFirst(_, replacementScope) =>
              val repMember = db.members(1) //At index 0 we have the Top. We don't want that.
              rt.replaceMember(origMember, repMember, replacementScope)
            case DB.Patch.Add.Config.ReplaceWithLast(_, replacementScope) =>
              val repMember = db.members.last
              rt.replaceMember(origMember, repMember, replacementScope)
            case DB.Patch.Add.Config.Via =>
              val repMember = db.members.last //The last member is used for Via addition.
              rt.replaceMember(origMember, repMember, DB.Patch.Replace.Scope.All)
            case _ => rt
          }
          repRT ++ dbPatched.refTable
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
        case cb : ConditionalBlock.Case_Block[_] => getGuards(cb.getOwner, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
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
        case f : DFAny.Func2 => dt + (f-> Set(f.leftArgRef.get, f.rightArgRef.get))
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

    @tailrec private def mcf(remaining : List[DFMember], retList : List[DFMember]) : List[DFMember] =
      remaining match {
        case (block : DFBlock) :: mList =>
          val members = ownerMemberTable(block)
          val sortedMembers = block match {
            case _ : DFDesign.Block =>
              val split = members.partition {
                case _ : CanBeGuarded => false
                case _ => true
              }
              split._1 ++ split._2
            case _ => members
          }
          mcf(sortedMembers ++ mList, block :: retList)
        case m :: mList => mcf(mList, m :: retList)
        case Nil => retList.reverse
      }
    def moveConnectableFirst : DFDesign.DB = copy(members = mcf(List(top), List()))


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
      final case class Replace(updatedMember : DFMember, config : Replace.Config, scope : Replace.Scope = Replace.Scope.All) extends Patch
      object Replace {
        sealed trait Config extends Product with Serializable
        object Config {
          //Only modifies the reference table so that all members currently referencing the original member will reference
          //the updated member.
          case object ChangeRefOnly extends Config
          //Modifies the reference table so that all members currently referencing the original member will reference
          //the updated member, and removes the original member
          case object ChangeRefAndRemove extends Config
          //The updated member is replacing the original member in the member list and all members currently
          //referencing the existing member will reference the updated member.
          //If the updated member already exists in the member list (at a different position), then the original member is
          //removed from the list without being replaced in its position.
          case object FullReplacement extends Config
        }
        sealed trait Scope extends Product with Serializable
        object Scope {
          //All references are replaced
          case object All extends Scope
          //Only references from outside the given block are replaced
          case class Outside(block : DFDesign.Block.Internal) extends Scope
          //Only references from inside the given block are replaced
          case class Inside(block : DFDesign.Block) extends Scope
        }
      }
      final case class Add private (db : DB, config : Add.Config) extends Patch
      object Add {
        def apply(design : MetaDesign, config : Add.Config) : Add = Add(design.getDB, config)
        sealed trait Config extends Product with Serializable
        object Config {
          //adds members before the patched member
          case object Before extends Config
          //adds members after the patched member
          case object After extends Config
          //adds members inside the given block (appends elements at the end)
          case object Inside extends Config
          //adds members after the patched member, which will be replaced.
          //The FIRST (non-Top) member is considered the reference replacement member
          //Replacement is done as specified by the scope argument
          final case class ReplaceWithFirst(replacementConfig : Replace.Config = Replace.Config.ChangeRefAndRemove, replacementScope : Replace.Scope = Replace.Scope.All) extends Config
          //adds members before the patched member, which will be replaced.
          //The LAST member is considered the reference replacement member
          //Replacement is done as specified by the scope argument
          final case class ReplaceWithLast(replacementConfig : Replace.Config = Replace.Config.ChangeRefAndRemove, replacementScope : Replace.Scope = Replace.Scope.All) extends Config
          //adds members after the patched member.
          //The LAST member is considered the reference replacement member
          case object Via extends Config
        }
      }
      final case class ChangeRef[T <: DFMember](member : T, refAccess : T => DFMember.Ref, updatedRefMember : DFMember) extends Patch
    }
    class Mutable {
      private val members : mutable.ArrayBuffer[(DFMember, Set[DFMember.Ref])] = mutable.ArrayBuffer()
      def addConditionalBlock[Ret, CB <: ConditionalBlock.Of[Ret]](cb : CB, block : => Ret)(implicit ctx : DFBlock.Context) : CB = {
        addMember(cb)
        cb.applyBlock(block)
        cb
      }
      def addMember[M <: DFMember](member : M) : M = {
        memberTable += (member -> members.length)
        members += (member -> Set())
        member
      }
      private val memberTable : mutable.Map[DFMember, Int] = mutable.Map()
      private val refTable : mutable.Map[DFMember.Ref, DFMember] = mutable.Map()
      def hasToConnectionFor(dfVar : DFAny) : Boolean = {
        members(memberTable.getOrElse(dfVar, 0))._2.collectFirst {
          case DFNet.ToRef() => true
        }.nonEmpty
      }
      def getMember[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def setMember[M <: DFMember](originalMember : M, newMember : M) : M = {
        val idx = memberTable(originalMember)
        val (_, refSet) = members(idx)
        //update all references to the new member
        refSet.foreach(r => refTable.update(r, newMember))
        //add the member to the table with the position index
        //(we don't remove the old member since it might still be used as a user-reference in a mutable DB)
        memberTable.update(newMember, idx)
        //update the member in the member position array
        members.update(idx, (newMember, refSet))
        newMember
      }
      def newRefFor[M <: DFMember, T <: DFMember.Ref.Type, R <: DFMember.Ref.Of[T, M]](ref : R, member : M) : R = {
        memberTable.get(member) match {
          case Some(x) =>
            val (member, refSet) = members(x)
            members.update(x, (member, refSet + ref))
          case _ =>
          //In case where we do meta programming and planting one design into another,
          //we may not have the member available at the table. This is OK.
        }
        refTable += (ref -> member)
        ref
      }
      def immutable : DB = {
        var size = -1
        //Touching all lazy owner refs to force their addition.
        //During this procedure it is possible that new reference are added. If so, we re-iterate
        while (refTable.size != size) {
          size = refTable.size
          refTable.keys.foreach {
            case or : DFMember.OwnedRef => or.owner
            case _ => //do nothing
          }
        }
        DB(members.iterator.map(e => e._1).toList, refTable.toMap)
      }

      implicit val getSet : MemberGetSet = new MemberGetSet {
        def designDB : DFDesign.DB = immutable
        def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref: DFMember.Ref.Of[T, M]): M0 = getMember(ref)
        def set[M <: DFMember](originalMember : M, newMember: M): M = setMember(originalMember, newMember)
      }
    }
  }
}