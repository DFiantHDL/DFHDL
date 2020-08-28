package DFiant
import DFiant.DFDesign.DB.Patch
import DFiant.compiler.evSanityCheck
import compiler.csprinter.CSPrinter
import DFiant.internals._

import scala.annotation.{nowarn, tailrec}
import scala.collection.{immutable, mutable}
import DFiant.fsm.FSMMember.Trackable
import DFiant.sim._

import scala.collection.immutable.ListSet
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
  final protected def atOwnerDo[T](block : => T) : T =
    __db.OwnershipContext.injectOwnerAndRun(this, owner.getOwner)(block)
  ///////////////////////////////////////////////////////////////////
}

abstract class MetaDesign(lateConstruction : Boolean = false)(
  implicit meta : Meta
) extends DFDesign()(new MetaDesign.Context(meta)) {
  final def plantMember[T <: DFMember](member : T) : T = __db.plantMember(this, member)
  final def applyBlock(owner : DFOwner)(block : => Unit) : Unit =
    __ctx.db.OwnershipContext.injectOwnerAndRun(this, owner)(block)

  final protected implicit val __lateConstructionConfig : LateConstructionConfig = LateConstructionConfig.Force(lateConstruction)
}

object MetaDesign {
  class Context(meta : Meta) extends DFBlock.Context(
    meta,implicitly[Meta.SymbolOf[MetaDesign]], null, ASIS, new DFDesign.DB.Mutable, ClassArgs.empty
  )
}

object DFDesign {
  protected[DFiant] type Context = DFBlock.Context

  trait Abstract extends DFOwner.Container {
    type Owner = DFDesign.Block
    private[DFiant] lazy val inlinedRep : Option[DFInlineComponent.Rep] = None
    private[DFiant] lazy val simMode : DFSimDesign.Mode = DFSimDesign.Mode.Off
    private[DFiant] val __ctx : DFDesign.Context
    protected[DFiant] final implicit lazy val __db : DFDesign.DB.Mutable = __ctx.db
    private lazy val designType : String = typeName.split("\\.").last
    private[DFiant] final val owner : DFDesign.Block = DFDesign.Block.Internal(this)(designType, inlinedRep, simMode)(__ctx)
    protected[DFiant] final implicit val __dir : DFDir = ASIS
    final protected val dsn : this.type = this

    ///////////////////////////////////////////////////////////////////
    // Context implicits
    ///////////////////////////////////////////////////////////////////
    final protected implicit def __contextOfDefs[T <: String with Singleton](
      implicit meta : Meta, v : ValueOf[T]
    ) : ContextOf[T] = new ContextOf[T](meta, Meta.SymbolOf(valueOf[T]), this, ASIS, __db, new ClassArgs[T]{val value = List()}) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = ???
    }
    final protected implicit def __contextOfDesign[T <: DFDesign](
      implicit meta : Meta, symbol : Meta.SymbolOf[T], args : ClassArgs[T]
    ) : ContextOf[T] = new ContextOf[T](meta, symbol, this, ASIS, __db, args) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = ???
    }
    final protected implicit def __contextOfInterface[T <: DFInterface](
      implicit meta : Meta, symbol : Meta.SymbolOf[T], cc : CloneClassWithContext[ContextOf[T]], args : ClassArgs[T]
    ) : ContextOf[T] = new ContextOf[T](meta, symbol, this, ASIS, __db, args) {
      def newInterface(updatedCtx : ContextOf[T]) : Any = cc(updatedCtx)
    }
    ///////////////////////////////////////////////////////////////////

    final implicit class __DesignExtender[D <: DFDesign](design : D) {
      private def onBlock(blockMod : Block => Block) : D = {
        blockMod(design.owner)
        design
      }
      def setName(value : String) : D = onBlock(_.setName(value))
      def keep : D = onBlock(_.keep)
      def !![CT <: DFMember.CustomTagOf[Block] : ClassTag](customTag : CT) : D = onBlock(_.!!(customTag))
      def !!(tags : TagsOf[D]) : D = {
        tags.attachDesign(design)
        tags.getTagMap.foreach{case (member, tagMap) => member !! tagMap}
        design
      }
      def getTagOf[CT <: DFMember.CustomTagOf[Block] : ClassTag] : Option[CT] = design.owner.getTagOf[CT]
    }

    override lazy val typeName : String = __ctx.symbol.value
  }

  trait Implicits extends
    DFBits.Op.Implicits with
    DFUInt.Op.Implicits with
    DFSInt.Op.Implicits with
    DFEnum.Op.Implicits with
    DFBool.Op.Implicits 
//    DFString.Op.Implicits

  object Implicits extends Implicits

  sealed trait Block extends DFBlock {
    val designType: String
    def headerCodeString(implicit printer: CSPrinter): String = s"trait $designType extends DFDesign"
  }
  object Block {
    final case class Internal(designType: String, ownerRef : DFOwner.Ref, tags : DFMember.Tags, inlinedRep : Option[DFInlineComponent.Rep]) extends Block {
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

      def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
      override lazy val typeName : String = designType
    }
    object Internal {
      def apply(container : DFOwner.Container)(designType : String, inlinedRep : Option[DFInlineComponent.Rep], simMode : DFSimDesign.Mode)(
        implicit ctx : Context
      ) : Block = ctx.db.addContainerOwner(container,
        if (ctx.container == null) Top(designType, ctx.meta, simMode)
        else Internal(designType, ctx.owner, ctx.meta, inlinedRep)
      )
    }
    final case class Top(designType: String, tags : DFMember.Tags, simMode : DFSimDesign.Mode) extends Block {
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

      def setTags(tagsFunc : DFMember.Tags => DFMember.Tags)(implicit getSet : MemberGetSet) : DFMember = getSet.set(this)(m => m.copy(tags = tagsFunc(m.tags)))
    }
  }

  implicit class DevAccess(design : DFDesign) {
    def getDB : DB = design.__db.immutable
  }
  final case class DB(members : List[DFMember], refTable : Map[DFMember.Ref, DFMember], globalTags : Map[(Any, ClassTag[_]), DFMember.CustomTag]) {self =>
    lazy val top : Block.Top = members.head match {
      case m : Block.Top => m
    }
    implicit val __getset : MemberGetSet = new MemberGetSet {
      val designDB : DFDesign.DB = self
      def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref : DFMember.Ref.Of[T, M]) : M0 = refTable(ref).asInstanceOf[M0]
      def set[M <: DFMember](originalMember : M)(newMemberFunc: M => M): M = newMemberFunc(originalMember)
      def replace[M <: DFMember](originalMember : M)(newMember: M): M = newMember
      def remove[M <: DFMember](member : M) : M = member
      def getMembersOf(owner : DFOwner) : List[DFMember] = ownerMemberTable(owner)
      def setGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any, tag : CT) : Unit = {}
      def getGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any) : Option[CT] =
        globalTags.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
    }
    lazy val memberTable : Map[DFMember, Set[DFMember.Ref]] = refTable.invert

    //concatenating design databases (dropping the top owner of the added database)
    def concat(db : DB) : DB = DB(this.members ++ db.members.drop(1), this.refTable ++ db.refTable, this.globalTags ++ db.globalTags)
    //There can only be a single connection to a value (but multiple assignments are possible)
    //                              To    From
    lazy val connectionTable : Map[DFAny, DFNet.Connection] =
      members.collect{case n : DFNet.Connection => (n.toRef.get, n)}.toMap

    lazy val connectionTableInverted : Map[DFAny, List[DFNet.Connection]] =
      members.collect{case n : DFNet.Connection => n}.groupBy(n => n.fromRef.get)

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

    def getConnectionTo(v : DFAny) : Option[DFNet.Connection] = connectionTable.get(v)
    def getConnectionFrom(v : DFAny) : List[DFNet.Connection] = connectionTableInverted.getOrElse(v, List())

    def getAssignmentsTo(v : DFAny) : Set[DFAny] = assignmentsTable.getOrElse(v, Set())
    def getAssignmentsFrom(v : DFAny) : Set[DFAny] = assignmentsTableInverted.getOrElse(v, Set())

    //Map of all enum types in the design with their design block owners.
    //If the enum type is global (used in IO or more than one design block),
    //then its owner is set to None.
    private lazy val enumTypes : Map[EnumType, Option[DFDesign.Block]] =
      members.foldLeft(Map.empty[EnumType, Option[DFDesign.Block]]) {
        case (enumMap, enumMember @ DFEnum(enumType)) => //an enum member
          if (enumMember.isPort) enumMap + (enumType -> None) //IO means a global enum type
          else enumMap.get(enumType) match {
            case Some(Some(owner)) => //enum type already found
              if (owner == enumMember.getOwnerDesign) enumMap //same design block -> nothing to do
              else enumMap + (enumType -> None) //used in more than one block -> global enum type
            case Some(None) => enumMap //known to be a global type
            case None => enumMap + (enumType -> Some(enumMember.getOwnerDesign)) //found new enum type
          }
        case (enumMap, _) => enumMap //not an enum member
      }

    private lazy val invertedEnumTypes = enumTypes.invert
    lazy val getGlobalEnumTypes : Set[EnumType] = invertedEnumTypes.getOrElse(None, Set())
    private lazy val localEnumTypes : Map[DFDesign.Block, Set[EnumType]] = invertedEnumTypes.flatMap{
      case (Some(b), set) => Some(b -> set)
      case _ => None
    }
    def getLocalEnumTypes(design : DFDesign.Block) : Set[EnumType] =
      localEnumTypes.getOrElse(design, Set())

//    def getAliasesTo(v : DFAny) : Option[DFAny] =
//      members.collectFirst{case n : DFNet.Connection if n.toRef.get == v => n.fromRef.get}

    //Owner-to-members list generation via a tail recursive function that topologically sorts the blocks according to dependency
    @tailrec private def OMLGen[O <: DFOwner : ClassTag](getOwnerFunc : DFMember => O)(
      oml : List[(O, List[DFMember])], globalMembers : List[DFMember], localStack : List[(O, List[DFMember])]
    ) : List[(O, List[DFMember])] = {
      if (localStack.isEmpty) this.sanityCheck
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
    def printOwnerMemberList(implicit printConfig : CSPrinter.Config) : DB = {
      implicit val printer : CSPrinter = new CSPrinter {
        val getSet : MemberGetSet = __getset
        val config : CSPrinter.Config = printConfig
      }
      println(ownerMemberList.map(e => (e._1.show, s"(${e._2.map(x => x.show).mkString(", ")})")).mkString("\n"))
      this
    }

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

    @nowarn("msg=The outer reference in this type test cannot be checked at run time")
    private final case class ReplacementContext(
      refTable : Map[DFMember.Ref, DFMember],
      memberRepTable : Map[DFMember, DFMember]
    ) {
//      def replaceRef(origRef : DFMember.Ref,)
      def replaceMember(origMember : DFMember, repMember : DFMember, refFilter : DB.Patch.Replace.RefFilter) : ReplacementContext = {
        if (origMember == repMember) this else memberTable.get(origMember) match {
          case Some(refs) =>
            //in case the replacement member already was replaced in the past, then we used the previous replacement
            //as the most updated member
            val actualReplacement = this.memberRepTable.getOrElse(repMember, repMember)
            ReplacementContext(
              refFilter(refs).foldLeft(refTable)((rt2, r) => rt2.updated(r, actualReplacement)),
              memberRepTable + (origMember -> actualReplacement)
            )
          case None =>
            this
        }
      }
    }
    private object ReplacementContext {
      def fromRefTable(refTable : Map[DFMember.Ref, DFMember]) : ReplacementContext =
        ReplacementContext(refTable, Map())
    }

    //replaces all members and references according to the patch list
    def patch(patchList : Iterable[(DFMember, DB.Patch)]) : DB = if (patchList.isEmpty) this else {
      val patchTable = patchList.flatMap {
        //Replacing a member with the same member does nothing
        case (m, DB.Patch.Replace(m2, _, _)) if (m == m2) => None
        //On change ref and remove replacement we setup the original member for removal here
        case (m, DB.Patch.Replace(_, DB.Patch.Replace.Config.ChangeRefAndRemove, _)) => Some((m, DB.Patch.Remove))
        //If we attempt to replace with an existing member, then we convert the patch to remove
        //the old member just for the member list (references are replaced).
        case (m, DB.Patch.Replace(r, DB.Patch.Replace.Config.FullReplacement, _)) if memberTable.contains(r) => Some((m, DB.Patch.Remove))
        //If we add after/inside an owner, we need to actually place after the last member of the owner
        case (owner : DFOwner, DB.Patch.Add(db, DB.Patch.Add.Config.After | DB.Patch.Add.Config.Inside)) =>
          owner.getVeryLastMember match {
            case Some(l) => Some((l, DB.Patch.Add(db, DB.Patch.Add.Config.After)))
            case None => Some((owner, DB.Patch.Add(db, DB.Patch.Add.Config.After)))
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
          //add before/after followed by a reference replacement
          case (
              add @ Patch.Add(addDB, Patch.Add.Config.After | Patch.Add.Config.Before),
              Patch.Replace(_,Patch.Replace.Config.ChangeRefOnly,_)
            ) => tbl + (m -> Patch.Add(addDB, add.config))
          //allow the same member to be removed more than once by getting rid of the redundant removals
          case (Patch.Remove, Patch.Remove) => tbl + (m -> Patch.Remove)
          //don't allow using the same member for patching if it's not an addition of the same configuration
          case (l, r) =>
            println(l)
            println(r)
            throw new IllegalArgumentException(
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
      val patchedRefTable = patchList.foldLeft(ReplacementContext.fromRefTable(refTable)) {
        case (rc, (origMember, DB.Patch.Replace(repMember, _, refFilter))) if (origMember != repMember) => rc.replaceMember(origMember, repMember, refFilter)
        case (rc, (origMember, DB.Patch.Add(db, config))) =>
          val newOwner = config match {
            case DB.Patch.Add.Config.Inside => origMember
            case _ => origMember.getOwnerBlock
          }
          val actualNewOwner = rc.memberRepTable.getOrElse(newOwner, newOwner) //owner may have been replaced before
          val dbPatched = db.patch(db.top -> DB.Patch.Replace(actualNewOwner, DB.Patch.Replace.Config.ChangeRefOnly))
          val repRT = config match {
            case DB.Patch.Add.Config.ReplaceWithFirst(_, refFilter) =>
              val repMember = db.members(1) //At index 0 we have the Top. We don't want that.
              rc.replaceMember(origMember, repMember, refFilter)
            case DB.Patch.Add.Config.ReplaceWithLast(_, refFilter) =>
              val repMember = db.members.last
              rc.replaceMember(origMember, repMember, refFilter)
            case DB.Patch.Add.Config.Via =>
              val repMember = db.members.last //The last member is used for Via addition.
              rc.replaceMember(origMember, repMember, DB.Patch.Replace.RefFilter.All)
            case _ => rc
          }
          repRT.copy(refTable = repRT.refTable ++ dbPatched.refTable)
        case (rc, (origMember, DB.Patch.Remove)) => memberTable.get(origMember) match {
          case Some(refs) => rc.copy(refTable = refs.foldLeft(rc.refTable)((rt2, r) => rt2 - r))
          case None => rc
        }
        case (rc, (_, DB.Patch.ChangeRef(origMember, refFunc, updatedRefMember))) =>
          val ref = refFunc(origMember)
          rc.copy(refTable = rc.refTable + (ref -> updatedRefMember))
        case (rc, _) => rc
      }.refTable
      DB(patchedMembers, patchedRefTable, globalTags)
    }
    def setGlobalTags(tagList : List[((Any, ClassTag[_]), DFMember.CustomTag)]) : DB =
      copy(globalTags = globalTags ++ tagList)
    def patch(singlePatch : (DFMember, DB.Patch)) : DB = patch(List(singlePatch))
    @tailrec private def getGuards(currentOwner : DFBlock, targetOwner : DFBlock, currentGuards : List[DFAny]) : List[DFAny] =
      currentOwner match {
        case _ : DFDesign.Block => currentGuards //reached the design block
        case o : DFBlock if o == targetOwner => currentGuards //can't go past the target owner
        case cb : ConditionalBlock.IfElseBlock => getGuards(cb.getOwnerBlock, targetOwner, cb.condRefOption.map(c => c.get).toList ++ currentGuards)
        case cb : ConditionalBlock.CaseBlock => getGuards(cb.getOwnerBlock, targetOwner, cb.matchHeaderRef.matchValRef.get :: currentGuards)
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
      println(members.map {
        case m : DFDesign.Block.Top => s"${m.name} <top>"
        case m =>
          val owner = m.getOwnerBlock
          s"${m.name}_${m.hashCode().toHexString} : ${m.typeName}   ->   ${owner.name}_${owner.hashCode().toHexString} : ${owner.typeName}"
      }.mkString("\n"))
      this
    }
  }

  object DB {
    sealed trait Patch extends Product with Serializable
    object Patch {
      final case object Remove extends Patch
      final case class Replace(updatedMember : DFMember, config : Replace.Config, refFilter : Replace.RefFilter = Replace.RefFilter.All) extends Patch
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
        trait RefFilter {
          def apply(refs : Set[DFMember.Ref])(implicit getSet: MemberGetSet) : Set[DFMember.Ref]
        }
        object RefFilter {
          //All references are replaced
          object All extends RefFilter {
            def apply(refs : Set[DFMember.Ref])(implicit getSet: MemberGetSet) : Set[DFMember.Ref] = refs
          }
          //Only references from outside the given block are replaced
          final case class Outside(block : DFDesign.Block.Internal) extends RefFilter {
            def apply(refs : Set[DFMember.Ref])(implicit getSet: MemberGetSet) : Set[DFMember.Ref] =
              refs.collect{case r : DFMember.OwnedRef if r.owner.get.isOutsideOwner(block) => r}
          }
          //Only references from inside the given block are replaced
          final case class Inside(block : DFDesign.Block) extends RefFilter {
            def apply(refs : Set[DFMember.Ref])(implicit getSet: MemberGetSet) : Set[DFMember.Ref] =
              refs.collect{case r : DFMember.OwnedRef if r.owner.get.isInsideOwner(block) => r}
          }
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
          final case class ReplaceWithFirst(replacementConfig : Replace.Config = Replace.Config.ChangeRefAndRemove, refFilter : Replace.RefFilter = Replace.RefFilter.All) extends Config
          //adds members before the patched member, which will be replaced.
          //The LAST member is considered the reference replacement member
          //Replacement is done as specified by the scope argument
          final case class ReplaceWithLast(replacementConfig : Replace.Config = Replace.Config.ChangeRefAndRemove, refFilter : Replace.RefFilter = Replace.RefFilter.All) extends Config
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
      private var fsmTrack : ListSet[FSMMember.Trackable] = ListSet()
      private var fsmDuringElaboration : Boolean = false

      private def elaborateFSMHistoryHead() : Unit = if (!fsmDuringElaboration && fsmTrack.nonEmpty) {
        fsmDuringElaboration = true
//        println("fsm elaboration")
        FSMMember.Elaboration(fsmTrack.toList)
        fsmDuringElaboration = false
        fsmTrack = ListSet()
      }
      def trackFSM[T <: FSMMember.Trackable](fsm : T)(implicit ctx : DFMember.Context) : T = {
//        println("track", fsm)
        OwnershipContext.checkContainerExits(ctx.container)
        fsmTrack = fsmTrack + fsm
        fsm
      }
      def untrackFSM[T <: FSMMember.Trackable](fsm : T) : T = {
        fsmTrack = fsmTrack - fsm
//        println("untrack", fsm)
        fsm
      }
      ///////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////
      //Tracking containers entrance and exit to run `OnCreate` as
      //soon as possible after a container is created.
      ///////////////////////////////////////////////////////////////
      @nowarn("msg=The outer reference in this type test cannot be checked at run time")
      protected final case class OwnershipContext(
        container : DFOwner.Container, owner : DFOwner, injectedContainer : Option[DFOwner.Container],
        fsmTrack : ListSet[FSMMember.Trackable], fsmDuringElaboration : Boolean
      )
      protected[DFiant] object OwnershipContext {
        private var stack : List[OwnershipContext] = List()
        private var injectedContainer : Option[DFOwner.Container] = None
        private var duringExitContainer : Boolean = false
        def injectOwner(newOwner : DFOwner) : Unit =
          stack = stack.updated(0, stack.head.copy(owner = newOwner))
        def injectContainer(newContainer : DFOwner.Container) : Unit = {
//          println(f"""${"injecting"}%-20s ${newContainer}""")
          injectedContainer = Some(newContainer)
        }
        def clearInjectedContainer() : Unit = injectedContainer = None
        private def enqContainerOwner(container : DFOwner.Container, owner : DFOwner) : Unit = {
          stack = OwnershipContext(container, owner, injectedContainer, fsmTrack, fsmDuringElaboration) :: stack
          fsmTrack = ListSet()
          fsmDuringElaboration = false
//          println(f"""${"enq"}%-20s ${stack.head.container.nameAndType}%-30s ${stack.head.owner.nameAndType}""")
        }
        private def deqContainerOwner() : Unit = {
          elaborateFSMHistoryHead()
//          println(f"""${"deq"}%-20s ${stack.head.container.nameAndType}%-30s ${stack.head.owner.nameAndType}""")
          fsmTrack = stack.head.fsmTrack
          fsmDuringElaboration = stack.head.fsmDuringElaboration
          stack = stack.drop(1)
        }
        def enterOwner(owner : DFOwner) : Unit = {
//          println(f"""${"enteringOwner"}%-20s ${owner.nameAndType}""")
          enqContainerOwner(stack.head.container, owner)
        }
        def enterContainer(container : DFOwner.Container, owner : DFOwner) : Unit  = {
//          println(f"""${"enteringContainer"}%-20s ${container.nameAndType}%-30s ${owner.nameAndType}""")
          enqContainerOwner(container, owner)
          injectedContainer = None
          container.onEnterContainer()
        }
        def exitOwner(owner : DFOwner) : Unit = {
          while (stack.head.owner != owner) deqContainerOwner()
          deqContainerOwner()
//          println(f"""${"exitOwner"}%-20s ${owner.nameAndType}""")
        }
        def exitContainer(container : DFOwner.Container) : Unit = {
          duringExitContainer = true
          while (stack.head.container != container) deqContainerOwner()
          injectedContainer = stack.head.injectedContainer
          deqContainerOwner()
          duringExitContainer = false
          val exitingContainer = stack.head.container
//          exitingContainer.onExitContainer()
//          exitingContainer.onCreateContainer()
//          println(f"""${"exitContainer"}%-20s ${exitingContainer.nameAndType}%-30s ${container.nameAndType}""")
        }

        def checkContainerExits(container : DFOwner.Container) : Unit = {
          val actualContainer = injectedContainer.getOrElse(container)
          while (
            !duringExitContainer &&
            stack.nonEmpty &&
            !actualContainer.isInsideParent(stack.head.container)
          ) exitContainer(stack.head.container)
        }

        def exitAllContainers() : Unit = while (!duringExitContainer && stack.nonEmpty) deqContainerOwner()

        def getCurrentOwner(container : DFOwner.Container) : DFOwner = {
          checkContainerExits(container)
          stack.head.owner
        }
        def injectOwnerAndRun[T](container : DFOwner.Container, injectedOwner : DFOwner)(block : => T) : T = {
//          println("injecting", injectedOwner)
          checkContainerExits(container)
          enterOwner(injectedOwner)
          val ret = block
          exitOwner(injectedOwner)
          ret
        }
      }
      ///////////////////////////////////////////////////////////////

      ///////////////////////////////////////////////////////////////
      //Extra Tags
      ///////////////////////////////////////////////////////////////
      object global_tags {
        private[Mutable] val tagMap : mutable.Map[(Any, ClassTag[_]), DFMember.CustomTag] = mutable.Map()
        def set[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any, tag : CT) : Unit =
          tagMap += ((taggedElement, classTag[CT]) -> tag)
        def get[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any) : Option[CT] =
          tagMap.get((taggedElement, classTag[CT])).asInstanceOf[Option[CT]]
      }

      def addContainerOwner[O <: DFOwner](container : DFOwner.Container, owner : O) : O = {
        addMember(container.__parent, owner)
        OwnershipContext.enterContainer(container, owner)
        owner
      }
      def addMember[M <: DFMember](container : DFOwner.Container, member : M) : M = {
        elaborateFSMHistoryHead()
        OwnershipContext.checkContainerExits(container)
        //        println(f"""${"addMember"}%-20s ${s"${member.name} : ${member.typeName}"}%-30s ${member.getOwner.nameAndType}""")
        memberTable += (member -> members.length)
        members += Tuple3(member, Set(), false)
        member
      }
      def addMember[M <: DFMember](member : M)(implicit ctx : DFMember.Context) : M =
        addMember(ctx.container, member)

      def addMemberOf[M <: DFMember](member : DFMember)(implicit ctx : DFMember.Context) : M with DFMember.RefOwner =
        addMember(ctx.container, member).asInstanceOf[M with DFMember.RefOwner]

      //same as addMember, but the ownerRef needs to be added, referring to the meta designer owner
      def plantMember[M <: DFMember](container : DFOwner.Container, member : M) : M = {
        newRefFor[DFOwner, DFOwner.Ref.Type, DFOwner.Ref](member.ownerRef, container.owner) //now this reference will refer to meta design owner
        addMember(container, member)
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
        //if the member is an owner, then we need to inject the new owner
        newMember match {
          case o : DFOwner => OwnershipContext.injectOwner(o)
          case _ =>
        }
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
        OwnershipContext.exitAllContainers() //exiting all remaining containers (calling their OnCreate)
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
        DB(notIgnoredMembers, refTable.toMap, global_tags.tagMap.toMap)
      }

      implicit val getSet : MemberGetSet = new MemberGetSet {
        val designDB : DFDesign.DB = immutable
        def apply[M <: DFMember, T <: DFMember.Ref.Type, M0 <: M](ref: DFMember.Ref.Of[T, M]): M0 = getMember(ref)
        def set[M <: DFMember](originalMember : M)(newMemberFunc: M => M): M = setMember(originalMember, newMemberFunc)
        def replace[M <: DFMember](originalMember : M)(newMember: M): M = replaceMember(originalMember, newMember)
        def remove[M <: DFMember](member : M) : M = ignoreMember(member)
        def getMembersOf(owner : DFOwner) : List[DFMember] = self.getMembersOf(owner)
        def setGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any, tag : CT) : Unit = global_tags.set(taggedElement, tag)
        def getGlobalTag[CT <: DFMember.CustomTag : ClassTag](taggedElement : Any) : Option[CT] = global_tags.get(taggedElement)
      }
    }
  }
}