package DFiant
import DFiant.DFDesign.DB.Patch
import DFiant.internals._

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import DFiant.compiler.printer.Printer
import DFiant.sim._

import scala.reflect.{ClassTag, classTag}
abstract class DFDesign(implicit ctx : DFDesign.Context) extends DFDesign.Abstract {
  private[DFiant] final lazy val __ctx : DFDesign.Context = ctx
  private[DFiant] final lazy val __portArgs : Map[DFAny, DFAny] = {
    val ret = Map(ctx.args.value.flatMap(l => l.flatMap {
      case (name, x : DFAny) => Some(x -> DFAny.Port.In(x.dfType).setName(name))
      case _ => None
    }) : _*)
    if (ret.nonEmpty) println(ret)
    ret
  }
  ///////////////////////////////////////////////////////////////////
  // Ability to run construction at the owner's context
  ///////////////////////////////////////////////////////////////////
  final protected def atOwnerDo[T](block : => T) : T = ownerInjector.injectOwnerAndRun(ctx.owner)(block)
  ///////////////////////////////////////////////////////////////////
}

abstract class MetaDesign(lateConstruction : Boolean = false)(implicit ctx : ContextOf[MetaDesign]) extends DFDesign {
  final def plantMember[T <: DFMember](member : T)(implicit ctx : DFMember.Context) : T = __db.addMember(member.updateOwner)
  final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(lateConstruction)
}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context

  trait Abstract extends DFOwner.Container {
    type Owner = DFDesign.Block
    private[DFiant] lazy val inlinedRep : Option[DFInlineComponent.Rep] = None
    private[DFiant] lazy val simMode : DFSimDesign.Mode = DFSimDesign.Mode.Off
    private[DFiant] val __ctx : DFDesign.Context
    private[DFiant] final lazy val __db : DFDesign.DB.Mutable = __ctx.db
    private[DFiant] final val owner : DFDesign.Block = DFDesign.Block.Internal(this)(typeName, inlinedRep, simMode)(__ctx)
    private[DFiant] final val ownerInjector : DFMember.OwnerInjector = new DFMember.OwnerInjector(owner)

    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    final protected implicit def __blockContext(
      implicit meta : Meta
    ) : DFBlock.Context = new DFBlock.Context(meta, ownerInjector, ASIS, __db, ClassArgs.empty)
    final protected implicit def __contextOfDesign[T <: DFDesign](
      implicit meta : Meta, args : ClassArgs[T]
    ) : ContextOf[T] = new ContextOf[T](meta, owner, ASIS, __db, args) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = ???
    }
    final protected implicit def __contextOfInterface[T <: DFInterface](
      implicit meta : Meta, cc : CloneClassWithContext[ContextOf[T]], args : ClassArgs[T]
    ) : ContextOf[T] = new ContextOf[T](meta, owner, ASIS, __db, args) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = cc(updatedCtx)
    }
    ///////////////////////////////////////////////////////////////////
  }


  trait Implicits extends
    DFBits.Op.Implicits with
    DFUInt.Op.Implicits with
    DFSInt.Op.Implicits with
    DFEnum.Op.Implicits with
    DFBool.Op.Implicits with
    DFString.Op.Implicits

  object Implicits extends Implicits

  implicit class DesignExtender[T <: DFDesign](design : T) {
    import design.__db.getSet
    private def onBlock(b : Block => Unit) : T = {b(design.owner); design}
    def setName(value : String) : T = onBlock(_.setName(value))
    def keep : T = onBlock(_.keep)
    def !!(customTag : Block.CustomTag) : T = onBlock(_.!!(customTag))
  }

  sealed trait Block extends DFBlock {
    type TTags = DFMember.Tags.Basic
    type TCustomTag = Block.CustomTag
    val designType: String
    def headerCodeString(implicit getSet : MemberGetSet, printConfig : Printer.Config): String = s"trait $designType extends DFDesign"
  }
  object Block {
    trait CustomTag extends DFMember.CustomTag
    final case class Internal(designType: String, ownerRef : DFOwner.Ref, tags : DFMember.Tags.Basic, inlinedRep : Option[DFInlineComponent.Rep]) extends Block {
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case Internal(designType, _, tags, inlinedRep) =>
          val inlineRepEq = (this.inlinedRep, inlinedRep) match {
            case (Some(l), Some(r)) => l =~ r
            case (None, None) => true
            case _ => false
          }
          this.designType == designType && this.tags =~ tags && inlineRepEq
        case _ => false
      }
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : DFMember = copy(ownerRef = ref)
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(container : DFOwner.Container)(designType : String, inlinedRep : Option[DFInlineComponent.Rep], simMode : DFSimDesign.Mode)(
        implicit ctx : Context
      ) : Block = ctx.db.addContainerOwner(container)(
        if (ctx.ownerInjector == null || ctx.owner == null) Top(designType, ctx.meta, simMode)(ctx.db)
        else Internal(designType, ctx.owner, ctx.meta, inlinedRep)
      )
    }
    final case class Top(designType: String, tags : DFMember.Tags.Basic, simMode : DFSimDesign.Mode)(db: DB.Mutable) extends Block {
      override lazy val ownerRef : DFOwner.Ref = ???
      override def getOwnerBlock(implicit getSet : MemberGetSet): DFBlock = this
      override val isTop: Boolean = true
      protected[DFiant] def =~(that : DFMember)(implicit getSet : MemberGetSet) : Boolean = that match {
        case Top(designType, tags, simMode) =>
          this.designType == designType && this.tags =~ tags && this.simMode == simMode
        case _ => false
      }
      override lazy val typeName : String = designType
      override def getFullName(implicit getSet : MemberGetSet): String = name
      private[DFiant] def setOwnerRef(ref : DFOwner.Ref) : this.type = ???
      def setTags(tagsFunc : DFMember.Tags.Basic => DFMember.Tags.Basic)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags))(db))
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
      def set[M <: DFMember](originalMember : M)(newMemberFunc: M => M): M = newMemberFunc(originalMember)
      def replace[M <: DFMember](originalMember : M)(newMember: M): M = newMember
      def remove[M <: DFMember](member : M) : M = member
      def getMembersOf(owner : DFOwner) : List[DFMember] = ownerMemberTable(owner)
    }
    lazy val memberTable : Map[DFMember, Set[DFMember.Ref]] = refTable.invert

    //concatenating design databases (dropping the top owner of the added database)
    def concat(db : DB) : DB = DB(this.members ++ db.members.drop(1), this.refTable ++ db.refTable)
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
    @tailrec private def OMLGen[O <: DFOwner : ClassTag](getOwnerFunc : DFMember => O)(
      oml : List[(O, List[DFMember])], globalMembers : List[DFMember], localStack : List[(O, List[DFMember])]
    ) : List[(O, List[DFMember])] = {
      val ((localOwner, localMembers), updatedStack0) = (localStack.head, localStack.drop(1))
      globalMembers match {
        case m :: mList if getOwnerFunc(m) == localOwner => //current member indeed belongs to current owner
          val updatedStack1 = (localOwner -> (m :: localMembers)) :: updatedStack0
          m match {
            case o : O if classTag[O].runtimeClass.isInstance(o) => //Deep borrowing into block as the new owner
              val updatedStack2 = (o -> List()) :: updatedStack1
              OMLGen[O](getOwnerFunc)(oml, mList, updatedStack2)
            case _ => //Just a member
              OMLGen[O](getOwnerFunc)(oml, mList, updatedStack1)
          }
        case x :: xs => //current member does not belong to current owner
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
        case Nil if updatedStack0.nonEmpty =>
          val updatedOML = (localOwner -> localMembers.reverse) :: oml
          OMLGen[O](getOwnerFunc)(updatedOML, globalMembers, updatedStack0)
        case Nil =>
          (localOwner -> localMembers.reverse) :: oml
      }
    }

    //holds the topological order of owner owner dependency
    lazy val ownerMemberList : List[(DFOwner, List[DFMember])] =
      OMLGen[DFOwner](_.getOwner)(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP owner
    def printOwnerMemberList() : Unit =
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))

    //holds a hash table that lists members of each owner owner. The member list order is maintained.
    lazy val ownerMemberTable : Map[DFOwner, List[DFMember]] = Map(ownerMemberList : _*)

    //holds the topological order of owner block dependency
    lazy val blockMemberList : List[(DFBlock, List[DFMember])] =
      OMLGen[DFBlock](_.getOwnerBlock)(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP block

    //holds a hash table that lists members of each owner block. The member list order is maintained.
    lazy val blockMemberTable : Map[DFBlock, List[DFMember]] = Map(blockMemberList : _*)

    //holds the topological order of design block dependency
    lazy val designMemberList : List[(DFDesign.Block, List[DFMember])] =
      OMLGen[DFDesign.Block](_.getOwnerDesign)(List(), members.drop(1), List(top -> List())).reverse //head will always be the TOP block

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
                refs.collect{case r : DFMember.OwnedRef if r.owner.get.isOutsideOwner(block) => r}
              case DB.Patch.Replace.Scope.Inside(block) =>
                //for references that have owner references of their own, we check the owners location with respect
                //to the requested scope
                refs.collect{case r : DFMember.OwnedRef if r.owner.get.isInsideOwner(block) => r}
            }
            scopeRefs.foldLeft(rt)((rt2, r) => rt2.updated(r, repMember))
          case None =>
            rt
        }
    }

    //replaces all members and references according to the patch list
    def patch(patchList : List[(DFMember, DB.Patch)]) : DB = if (patchList.isEmpty) this else {
      val patchTable = patchList.flatMap {
        //Replacing a member with the same member does nothing
        case (m, DB.Patch.Replace(m2, _, _)) if (m == m2) => None
        //On change ref and remove replacement we setup the original member for removal here
        case (m, DB.Patch.Replace(_, DB.Patch.Replace.Config.ChangeRefAndRemove, _)) => Some((m, DB.Patch.Remove))
        //If we attempt to replace with an existing member, then we convert the patch to remove
        //the old member just for the member list (references are replaced).
        case (m, DB.Patch.Replace(r, DB.Patch.Replace.Config.FullReplacement, _)) if memberTable.contains(r) => Some((m, DB.Patch.Remove))
        //If we add after a design block, we need to actually place after the last member of the block
        case (block : DFDesign.Block.Internal, DB.Patch.Add(db, DB.Patch.Add.Config.After)) =>
          Some((designMemberTable(block).last, DB.Patch.Add(db, DB.Patch.Add.Config.After)))
        //If we add inside a design block, we need to actually place after the last member of the block
        case (block : DFDesign.Block, DB.Patch.Add(db, DB.Patch.Add.Config.Inside)) =>
          designMemberTable(block).lastOption match {
            case Some(l) => Some((l, DB.Patch.Add(db, DB.Patch.Add.Config.After)))
            case None => Some((block, DB.Patch.Add(db, DB.Patch.Add.Config.After)))
          }
        case x => Some(x)
      }.foldLeft(Map.empty[DFMember, DB.Patch]){
        case (tbl, (m, p)) if tbl.contains(m) => (tbl(m), p) match {
          //concatenating additions with the same configuration
          case (Patch.Add(db1, config1), Patch.Add(db2, config2)) if (config1 == config2) =>
            tbl + (m -> Patch.Add(db1 concat db2, config1))
          //add followed by a replacement is allowed via a tandem patch execution
          case (add : Patch.Add, Patch.Remove) =>
            tbl + (m -> Patch.Add(add.db, Patch.Add.Config.ReplaceWithFirst()))
          //don't allow using the same member for patching if it's not an addition of the same configuration
          case _ => throw new IllegalArgumentException(
            s"Received two different patches for the same member: $m"
          )
        }
        case (tbl, pair) => tbl + pair
      }
      //Patching member list
      val patchedMembers = members.flatMap(m => patchTable.get(m) match {
        case Some(DB.Patch.Replace(r, config, _)) => config match {
          case DB.Patch.Replace.Config.ChangeRefOnly => Some(m)
          case DB.Patch.Replace.Config.ChangeRefAndRemove => None
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
        case (rt, (origMember, DB.Patch.Replace(repMember, _, scope))) if (origMember != repMember) => rt.replaceMember(origMember, repMember, scope)
        case (rt, (origMember, DB.Patch.Add(db, config))) =>
          val newOwner = config match {
            case DB.Patch.Add.Config.Inside => origMember
            case _ => origMember.getOwnerBlock
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
        case (rt, _) => rt
      }
      DB(patchedMembers, patchedRefTable)
    }
    def patch(singlePatch : (DFMember, DB.Patch)) : DB = patch(List(singlePatch))
    @tailrec private def getGuards(currentOwner : DFBlock, targetOwner : DFBlock, currentGuards : List[DFAny]) : List[DFAny] =
      currentOwner match {
        case _ : DFDesign.Block => currentGuards //reached the design block
        case o : DFBlock if o == targetOwner => currentGuards //can't go past the target owner
        case cb : ConditionalBlock.IfBlock => getGuards(cb.getOwnerBlock, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseIfBlock => getGuards(cb.prevBlockRef, targetOwner, cb.condRef.get :: currentGuards)
        case cb : ConditionalBlock.ElseBlock => getGuards(cb.prevBlockRef, targetOwner, currentGuards)
        case cb : ConditionalBlock.CasePatternBlock[_] => getGuards(cb.getOwnerBlock, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
        case cb : ConditionalBlock.Case_Block[_] => getGuards(cb.getOwnerBlock, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
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
            case a : DFNet.Assignment => getGuards(a.getOwnerBlock, toVal.getOwnerBlock, List())
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
          val members = blockMemberTable(block)
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
      println(members.map(m => (m.name -> m.getOwnerBlock.name).toString()).mkString("\n"))
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
        def apply(design : MetaDesign, config : Config) : Add = Add(design.getDB, config)
        def apply(addedMembers : List[DFMember], config: Config) : Add = {
          val dsn = new MetaDesign() {
            addedMembers.foreach(m => plantMember(m))
          }
          Add(dsn, config)
        }
        def apply(addedMember : DFMember, config: Config) : Add = Add(List(addedMember), config)
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
    class Mutable {self =>
      //                                          Member        RefSet        Ignore
      private val members : mutable.ArrayBuffer[(DFMember, Set[DFMember.Ref], Boolean)] = mutable.ArrayBuffer()
      def top : Block.Top = members.head._1 match {
        case m : Block.Top => m
      }
      ///////////////////////////////////////////////////////////////
      //Tracking FSMs
      ///////////////////////////////////////////////////////////////
      import fsm._
//      private var fsmMap : List[FSM] = List()
      private var fsmHistory : List[mutable.ListBuffer[FSM.Trackable]] = List()
      private var duringElaboration : Boolean = false
//      def getFSMHistory : List[FSM] = ??? //fsmHistory.toList
//      def enterStep(step : Step) : Step = {
//        stepStack = step :: stepStack
//        step
//      }
//      def exitStep() : Step = {
//        val step = stepStack.head
//        stepStack = stepStack.drop(1)
//        step
//      }

      private def pushFSMHistory() : Unit = {
        fsmHistory = mutable.ListBuffer.empty[FSM.Trackable] :: fsmHistory
      }
      private def elaborateFSMHistoryHead() : Unit = if (!duringElaboration) fsmHistory.headOption match {
        case Some(h) if h.nonEmpty =>
          duringElaboration = true
          FSM.Elaboration(h.toList)
          duringElaboration = false
          fsmHistory = fsmHistory.updated(0, mutable.ListBuffer.empty[FSM.Trackable])
        case _ =>
      }
      private def popFSMHistory() : Unit = {
        elaborateFSMHistoryHead()
        fsmHistory = fsmHistory.drop(1)
      }
      def trackFSM[T <: FSM.Trackable](fsm : T)(implicit ctx : DFAny.Context) : T = {
        fsmHistory = fsmHistory.updated(0, fsmHistory.head += fsm)
        fsm
      }
      def untrackFSM[T <: FSM.Trackable](fsm : T)(implicit ctx : DFAny.Context) : T = {
        fsmHistory = fsmHistory.updated(0, fsmHistory.head -= fsm)
        fsm
      }
      ///////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////
      //Tracking containers entrance and exit to run `OnCreate` as
      //soon as possible after a container is created.
      ///////////////////////////////////////////////////////////////
      private var containerStack = List.empty[DFOwner.Container]
      private def enterContainer(container : DFOwner.Container) : Unit = {
        containerStack = container :: containerStack
        pushFSMHistory()
      }
      private def exitContainer() : Unit = {
        popFSMHistory()
        containerStack.head.onCreate()
        containerStack = containerStack.drop(1)
      }
      private def checkContainers(currentMember : DFMember) : Unit =
        while (
          containerStack.nonEmpty &&
          !currentMember.isInsideOwner(containerStack.head.owner)
        ) exitContainer()
      private def exitAllContainers() : Unit = while (containerStack.nonEmpty) exitContainer()
      ///////////////////////////////////////////////////////////////

      def addConditionalBlock[Ret, CB <: ConditionalBlock.Of[Ret]](cb : CB, block : => Ret)(implicit ctx : DFBlock.Context) : CB = {
        addMember(cb)
        pushFSMHistory()
        cb.applyBlock(block)
        popFSMHistory()
        cb
      }
      def addContainerOwner[O <: DFOwner](container : DFOwner.Container)(owner : O) : O = {
        addMember(owner)
        enterContainer(container)
        owner
      }
      def addMember[M <: DFMember](member : M) : M = {
        elaborateFSMHistoryHead()
        checkContainers(member)
        memberTable += (member -> members.length)
        members += Tuple3(member, Set(), false)
        member
      }
      private val memberTable : mutable.Map[DFMember, Int] = mutable.Map()
      private val refTable : mutable.Map[DFMember.Ref, DFMember] = mutable.Map()
      def hasToConnectionFor(dfVar : DFAny) : Boolean = members(memberTable.getOrElse(dfVar, 0))._2.collectFirst {
        case DFNet.ToRef() => true
      }.nonEmpty
      def getMembersOf(owner : DFOwner) : List[DFMember] = {
        val ret = memberTable.get(owner) match {
          case Some(idx) =>
            var list = List.empty[DFMember]
            var i = idx + 1
            while (i < members.length) {
              val m = members(i)._1
              if (m.getOwner == owner) list = m :: list
              else if (m.isOutsideOwner(owner)) i = members.length
              i = i + 1
            }
            list
          case None => Nil
        }
        ret.reverse
      }
      def getMembers : Iterator[DFMember] = members.view.map(e => e._1).iterator
      def getMember[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def setMember[M <: DFMember](originalMember : M, newMemberFunc : M => M) : M = {
        val idx = memberTable(originalMember)
        //get the most updated member currently positioned at the index of the original member
        val newMember = newMemberFunc(members(idx)._1.asInstanceOf[M])
        val (_, refSet, ignore) = members(idx)
        //update all references to the new member
        refSet.foreach(r => refTable.update(r, newMember))
        //add the member to the table with the position index
        //(we don't remove the old member since it might still be used as a user-reference in a mutable DB)
        memberTable.update(newMember, idx)
        //update the member in the member position array
        members.update(idx, (newMember, refSet, ignore))
        newMember
      }
      def replaceMember[M <: DFMember](originalMember : M, newMember : M) : M = {
        ignoreMember(newMember) //marking the newMember slot as 'ignore' in case it exists
        setMember[M](originalMember, _ => newMember)
        newMember
      }
      def ignoreMember[M <: DFMember](member : M) : M = { //ignoring it means removing it for the immutable DB
        memberTable.get(member).foreach{idx =>
          members.update(idx, (member, members(idx)._2, true))
        }
        member
      }
      def newRefFor[M <: DFMember, T <: DFMember.Ref.Type, R <: DFMember.Ref.Of[T, M]](ref : R, member : M) : R = {
        memberTable.get(member) match {
          case Some(x) =>
            val (member, refSet, ignore) = members(x)
            members.update(x, (member, refSet + ref, ignore))
          case _ =>
          //In case where we do meta programming and planting one design into another,
          //we may not have the member available at the table. This is OK.
        }
        refTable += (ref -> member)
        ref
      }
      def immutable : DB = {
        exitAllContainers() //exiting all remaining containers (calling their OnCreate)
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
        val notIgnoredMembers = members.iterator.filterNot(e => e._3).map(e => e._1).toList
        DB(notIgnoredMembers, refTable.toMap)
      }

      implicit val getSet : MemberGetSet = new MemberGetSet {
        def designDB : DFDesign.DB = immutable
        def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref: DFMember.Ref.Of[T, M]): M0 = getMember(ref)
        def set[M <: DFMember](originalMember : M)(newMemberFunc: M => M): M = setMember(originalMember, newMemberFunc)
        def replace[M <: DFMember](originalMember : M)(newMember: M): M = replaceMember(originalMember, newMember)
        def remove[M <: DFMember](member : M) : M = ignoreMember(member)
        def getMembersOf(owner : DFOwner) : List[DFMember] = self.getMembersOf(owner)
      }
    }
  }
}