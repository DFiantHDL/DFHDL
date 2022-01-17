package DFiant.compiler.patching
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*

import scala.reflect.ClassTag

sealed trait Patch extends Product with Serializable derives CanEqual
object Patch:
  case object Remove extends Patch
  final case class Replace(
      updatedMember: DFMember,
      config: Replace.Config,
      refFilter: Replace.RefFilter = Replace.RefFilter.All
  ) extends Patch
  object Replace:
    sealed trait Config extends Product with Serializable derives CanEqual
    object Config:
      // Only modifies the reference table so that all members currently referencing the original member will reference
      // the updated member.
      case object ChangeRefOnly extends Config
      // Modifies the reference table so that all members currently referencing the original member will reference
      // the updated member, and removes the original member
      case object ChangeRefAndRemove extends Config
      // The updated member is replacing the original member in the member list and all members currently
      // referencing the existing member will reference the updated member.
      // If the updated member already exists in the member list (at a different position), then the original member is
      // removed from the list without being replaced in its position.
      case object FullReplacement extends Config
    trait RefFilter derives CanEqual:
      def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny]
    object RefFilter:
      // All references are replaced
      object All extends RefFilter:
        def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] = refs
      // Only references from outside the given owner are replaced
      final case class Outside(block: DFOwner) extends RefFilter:
        def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
          refs.collect { case r: DFRef.TwoWayAny if r.originRef.get.isOutsideOwner(block) => r }
      // Only references from inside the given owner are replaced
      final case class Inside(block: DFOwner) extends RefFilter:
        def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
          refs.collect { case r: DFRef.TwoWayAny if r.originRef.get.isInsideOwner(block) => r }
  end Replace
  final case class Add private[patching] (db: DB, config: Add.Config) extends Patch
  object Add:
    def apply(design: MetaDesign, config: Config): Add = Add(design.getDB, config)
    def apply(addedMembers: List[DFMember], config: Config): Add =
      val dsn = new MetaDesign:
        addedMembers.foreach(m => plantMember(m))
      Add(dsn, config)
    def apply(addedMember: DFMember, config: Config): Add = Add(List(addedMember), config)

    sealed trait Config extends Product with Serializable derives CanEqual:
      def ==(moveConfig: Move.Config): Boolean = (this, moveConfig) match
        case (Config.Before, Move.Config.Before)           => true
        case (Config.After, Move.Config.After)             => true
        case (Config.InsideFirst, Move.Config.InsideFirst) => true
        case (Config.InsideLast, Move.Config.InsideLast)   => true
        case _                                             => false
    object Config:
      // adds members before the patched member
      case object Before extends Config
      // adds members after the patched member
      case object After extends Config
      // adds members inside the given block, at the beginning
      case object InsideFirst extends Config
      // adds members inside the given block, at the end
      case object InsideLast extends Config
      // adds members after the patched member, which will be replaced.
      // The FIRST (non-Top) member is considered the reference replacement member
      // Replacement is done as specified by the scope argument
      final case class ReplaceWithFirst(
          replacementConfig: Replace.Config = Replace.Config.ChangeRefAndRemove,
          refFilter: Replace.RefFilter = Replace.RefFilter.All
      ) extends Config
      // adds members before the patched member, which will be replaced.
      // The LAST member is considered the reference replacement member
      // Replacement is done as specified by the scope argument
      final case class ReplaceWithLast(
          replacementConfig: Replace.Config = Replace.Config.ChangeRefAndRemove,
          refFilter: Replace.RefFilter = Replace.RefFilter.All
      ) extends Config
      // adds members after the patched member.
      // The LAST member is considered the reference replacement member
      case object Via extends Config
    end Config
  end Add
  final case class Move private[patching] (movedMembers: List[DFMember], config: Move.Config)
      extends Patch
  object Move:
    def apply(owner: DFOwner, config: Config)(using MemberGetSet): Move =
      Move(owner.members(MemberView.Flattened), config)
    sealed trait Config extends Product with Serializable derives CanEqual:
      def ==(addConfig: Add.Config): Boolean = addConfig == this
    object Config:
      // moves members before the patched member
      case object Before extends Config
      // moves members after the patched member
      case object After extends Config
      // moves members inside the given block, at the beginning
      case object InsideFirst extends Config
      // moves members inside the given block, at the end
      case object InsideLast extends Config

  final case class ChangeRef[T <: DFMember](
      member: T,
      refAccess: T => DFRefAny,
      updatedRefMember: DFMember
  ) extends Patch
end Patch

extension (db: DB)
  def patch(patchList: Iterable[(DFMember, Patch)], debug: Boolean = false)(using
      MemberGetSet
  ): DB =
    import db.{members, refTable, memberTable, globalTags}
    if (patchList.isEmpty) return db
    def patchDebug(block: => Unit): Unit = if (debug) block
    val patchTable = patchList
      .flatMap {
        // Replacement of reference only does not require patching the member list, so we remove this from the table
        case (_, Patch.Replace(_, Patch.Replace.Config.ChangeRefOnly, _)) => None
        // Replacing a member with the same member does nothing
        case (m, Patch.Replace(m2, _, _)) if (m == m2) => None
        // On change ref and remove replacement we setup the original member for removal here
        case (m, Patch.Replace(_, Patch.Replace.Config.ChangeRefAndRemove, _)) =>
          Some((m, Patch.Remove))
        // If we attempt to replace with an existing member, then we convert the patch to remove
        // the old member just for the member list (references are replaced).
        case (m, Patch.Replace(r, Patch.Replace.Config.FullReplacement, _))
            if memberTable.contains(r) =>
          Some((m, Patch.Remove))
        // If we add insideFirst in an owner, we need to actually place after the owner head
        case (owner: DFOwner, Patch.Add(db, Patch.Add.Config.InsideFirst)) =>
          Some((owner, Patch.Add(db, Patch.Add.Config.After)))
        // If we add after/insideLast an owner, we need to actually place after the last member of the owner
        case (
              owner: DFOwner,
              Patch.Add(db, Patch.Add.Config.After | Patch.Add.Config.InsideLast)
            ) =>
          owner.getVeryLastMember match
            case Some(l) => Some((l, Patch.Add(db, Patch.Add.Config.After)))
            case None    => Some((owner, Patch.Add(db, Patch.Add.Config.After)))
        // A move patch operation adds a remove patch to all the moved members
        // If we move insideFirst in an owner, we need to actually place after the owner head
        // If we move after/insideLast an owner, we need to actually place after the last member of the owner
        case (m, Patch.Move(movedMembers, config)) =>
          val modMove = (m, config) match
            case (owner: DFOwner, Patch.Move.Config.InsideFirst) =>
              (owner, Patch.Move(movedMembers, Patch.Move.Config.After))
            case (owner: DFOwner, Patch.Move.Config.After | Patch.Move.Config.InsideLast) =>
              owner.getVeryLastMember match
                case Some(l) => (l, Patch.Move(movedMembers, Patch.Move.Config.After))
                case None    => ((owner, Patch.Move(movedMembers, Patch.Move.Config.After)))
            case (m, Patch.Move.Config.Before) => (m, Patch.Move(movedMembers, config))
            case _                             => ???
          modMove :: movedMembers.map((_, Patch.Remove))
        case x => Some(x)
      }
      .foldLeft(Map.empty[DFMember, Patch]) {
        case (tbl, (m, p)) if tbl.contains(m) =>
          (tbl(m), p) match
            // concatenating additions with the same configuration
            case (Patch.Add(db1, config1), Patch.Add(db2, config2)) if (config1 == config2) =>
              tbl + (m -> Patch.Add(db1 concat db2, config1))
            // concatenating addition and move with the same configuration
            case (Patch.Add(db, addConfig), Patch.Move(movedMembers, moveConfig))
                if (addConfig == moveConfig) =>
              tbl + (m -> Patch.Move(db.members.drop(1) ++ movedMembers, moveConfig))
            case (Patch.Move(movedMembers, moveConfig), Patch.Add(db, addConfig))
                if (addConfig == moveConfig) =>
              tbl + (m -> Patch.Move(movedMembers ++ db.members.drop(1), moveConfig))
            // removed followed an add replacement is allowed via a tandem patch execution
            case (Patch.Remove, add: Patch.Add) =>
              tbl + (m -> Patch.Add(add.db, Patch.Add.Config.ReplaceWithLast()))
            // add followed by a replacement is allowed via a tandem patch execution
            case (add: Patch.Add, Patch.Remove) =>
              tbl + (m -> Patch.Add(add.db, Patch.Add.Config.ReplaceWithFirst()))
            // replacement followed by an add via a tandem patch execution
            case (replace: Patch.Replace, add: Patch.Add) if add.config == Patch.Add.Config.After =>
              tbl + (m -> Patch.Add(
                add.db.copy(add.db.members.head :: replace.updatedMember :: add.db.members.drop(1)),
                Patch.Add.Config.ReplaceWithFirst()
              ))
            // allow the same member to be removed more than once by getting rid of the redundant removals
            case (Patch.Remove, Patch.Remove) => tbl + (m -> Patch.Remove)
            // don't allow using the same member for patching if it's not an addition of the same configuration
            case (l, r) =>
              println(l)
              println(r)
              throw new IllegalArgumentException(
                s"Received two different patches for the same member: $m"
              )
        case (tbl, pair) => tbl + pair
      }

    patchDebug {
      println("patchList:")
      println(patchList.mkString("\n"))
      println("----------------------------------------------------------------------------")
      println("patchTable:")
      println(patchTable.mkString("\n"))
      println("----------------------------------------------------------------------------")
    }
    // Patching member list
    val patchedMembers = members.flatMap(m =>
      patchTable.get(m) match
        case Some(Patch.Replace(r, config, _)) =>
          config match
            case Patch.Replace.Config.ChangeRefAndRemove => None
            case Patch.Replace.Config.FullReplacement    => Some(r)
            case Patch.Replace.Config.ChangeRefOnly =>
              ??? // Not possible since we filtered these out
        case Some(Patch.Add(db, config)) =>
          val notTop = db.members.drop(1) // adding the members without the Top design block
          config match
            case Patch.Add.Config.After  => m :: notTop
            case Patch.Add.Config.Before => notTop :+ m
            case Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.ChangeRefOnly, _) =>
              m :: notTop
            case Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefOnly, _) =>
              notTop :+ m
            case Patch.Add.Config.ReplaceWithFirst(_, _) => notTop
            case Patch.Add.Config.ReplaceWithLast(_, _)  => notTop
            case Patch.Add.Config.Via                    => m :: notTop
            case Patch.Add.Config.InsideFirst =>
              ??? // Not possible since we replaced it to an `After`
            case Patch.Add.Config.InsideLast =>
              ??? // Not possible since we replaced it to an `After`
        case Some(Patch.Move(movedMembers, config)) =>
          config match
            case Patch.Move.Config.After  => m :: movedMembers
            case Patch.Move.Config.Before => movedMembers :+ m
            case Patch.Move.Config.InsideFirst =>
              ??? // Not possible since we replaced it to an `After`
            case Patch.Move.Config.InsideLast =>
              ??? // Not possible since we replaced it to an `After`
        case Some(Patch.Remove)          => None
        case Some(_: Patch.ChangeRef[_]) => Some(m)
        case None => Some(m) // not in the patch table, therefore remain as-is
    )
    patchDebug {
      println("----------------------------------------------------------------------------")
      println("members:")
      println(members.mkString("\n"))
      println("----------------------------------------------------------------------------")
      println("refTable:")
      println(refTable.mkString("\n"))
      println("----------------------------------------------------------------------------")
      println("patchedMembers:")
      println(patchedMembers.mkString("\n"))
      println("----------------------------------------------------------------------------")
    }
    // Patching reference table
    val patchedRefTable = patchList
      .foldLeft(ReplacementContext.fromRefTable(refTable)) {
        case (rc, (origMember, Patch.Replace(repMember, _, refFilter)))
            if (origMember != repMember) => {
          val ret = rc.replaceMember(origMember, repMember, refFilter)
          patchDebug {
            println("rc.refTable:")
            println(ret.refTable.mkString("\n"))
          }
          ret
        }
        case (rc, (origMember, Patch.Add(db, config))) =>
          val newOwner = config match
            case Patch.Add.Config.InsideFirst => origMember
            case Patch.Add.Config.InsideLast  => origMember
            case _                            => origMember.getOwnerBlock
          val actualNewOwner = rc.getLatestRepOf(newOwner) // owner may have been replaced before
          val dbPatched =
            db.patchSingle(
              db.top -> Patch.Replace(actualNewOwner, Patch.Replace.Config.ChangeRefOnly)
            )
          // updating the patched DB reference table members with the newest members kept by the replacement context
          val updatedPatchRefTable = rc.getUpdatedRefTable(dbPatched.refTable)
          val repRT = config match
            case Patch.Add.Config.ReplaceWithFirst(_, refFilter) =>
              val repMember = db.members(1) // At index 0 we have the Top. We don't want that.
              rc.replaceMember(origMember, repMember, refFilter)
            case Patch.Add.Config.ReplaceWithLast(_, refFilter) =>
              val repMember = db.members.last
              rc.replaceMember(origMember, repMember, refFilter)
            case Patch.Add.Config.Via =>
              val repMember = db.members.last // The last member is used for Via addition.
              rc.replaceMember(origMember, repMember, Patch.Replace.RefFilter.All)
            case _ => rc
          //          patchDebug {
          //            println("repRT.refTable:")
          //            println(repRT.refTable.mkString("\n"))
          //          }
          //          patchDebug {
          //            println("dbPatched.refTable:")
          //            println(dbPatched.refTable.mkString("\n"))
          //          }
          //          patchDebug {
          //            println("updatedPatchRefTable:")
          //            println(updatedPatchRefTable.mkString("\n"))
          //          }
          val ret = repRT.copy(refTable = repRT.refTable ++ updatedPatchRefTable)
          //          patchDebug {
          //            println("rc.refTable:")
          //            println(ret.refTable.mkString("\n"))
          //          }
          ret
        // a move patch just requires change of the owner reference of the head moved members
        case (rc, (origMember, Patch.Move(movedMembers, config))) =>
          val newOwner = config match
            case Patch.Move.Config.InsideFirst => origMember
            case Patch.Move.Config.InsideLast  => origMember
            case _                             => origMember.getOwnerBlock
          val actualNewOwner = rc.getLatestRepOf(newOwner) // owner may have been replaced before
          val headRef = movedMembers.head.ownerRef
          rc.changeRef(headRef, actualNewOwner)
        case (rc, (origMember, Patch.Remove)) =>
          memberTable.get(origMember) match
            case Some(refs) => rc.copy(refTable = refs.foldLeft(rc.refTable)((rt2, r) => rt2 - r))
            case None       => rc
        case (rc, (_, Patch.ChangeRef(origMember, refFunc, updatedRefMember))) =>
          val ref = refFunc(origMember)
          rc.copy(refTable = rc.refTable + (ref -> updatedRefMember))
        case (rc, _) => rc
      }
      .refTable
    patchDebug {
      println("----------------------------------------------------------------------------")
      println("patchedRefTable:")
      println(patchedRefTable.mkString("\n"))
      println("----------------------------------------------------------------------------")
    }
    DB(patchedMembers, patchedRefTable, globalTags)
  end patch

  def patchSingle(singlePatch: (DFMember, Patch))(using MemberGetSet): DB =
    db.patch(List(singlePatch), debug = false)
  def concat(that: DB): DB = DB(
    db.members ++ that.members.drop(1),
    db.refTable ++ that.refTable,
    db.globalTags ++ that.globalTags
  )
  def setGlobalTags(tagList: List[((Any, ClassTag[_]), DFTag)]): DB =
    db.copy(globalTags = db.globalTags ++ tagList)

end extension