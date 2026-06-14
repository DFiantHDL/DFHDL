package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.immutable.ListMap

case object UniqueDesigns extends GlobalStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()

  private def groupDesigns(db: DB)(using MemberGetSet): List[List[DFDesignBlock]] =
    val eqDesign: ((DFDesignBlock, List[DFMember]), (DFDesignBlock, List[DFMember])) => Boolean =
      case ((thisBlock, theseMembers), (thatBlock, thoseMembers))
          if thisBlock.dclMeta == thatBlock.dclMeta =>
        (theseMembers lazyZip thoseMembers).forall { case (l, r) => l =~ r }
      case _ => false
    // we're grouping always according to case-insensitive design names because these affect
    // the eventual file names and we want these to be different across all operating systems.
    // the actual name case is preserved for design/entity/module generation.
    db.designMemberList.view
      .groupByCompare(eqDesign, _._1.dclName.toLowerCase().hashCode()).map(_.unzip._1).toList

  def transformGlobal(designDB: DB)(using co: CompilerOptions, refGen: RefGen): DB =
    // Cross-design structural comparison resolves refs from BOTH designs, so it
    // needs a single getSet covering every design. We use the flattened DB for
    // this — its design blocks are the SAME objects as the sub-DB tops, so the
    // grouping/decisions map straight back onto the hierarchy.
    val flatDB = designDB.newToOld
    val sameBlockLists: List[List[DFDesignBlock]] = flatDB.atGetSet(groupDesigns(flatDB))
    val uniqueTypeMap: Map[String, List[List[DFDesignBlock]]] =
      sameBlockLists.groupBy(g => g.head.dclName.toLowerCase())

    val topTop = designDB.top
    // canonical design -> its unique (possibly renamed) declaration name
    val canonicalRenames = collection.mutable.LinkedHashMap.empty[DFDesignBlock, String]
    // redundant duplicate design -> the canonical design it is shared into
    val dupToCanonical = collection.mutable.LinkedHashMap.empty[DFDesignBlock, DFDesignBlock]
    uniqueTypeMap.foreach { case (_, list) =>
      list.zipWithIndex.foreach {
        case (group, i) if group.length > 1 || list.length > 1 =>
          val canonical = group.head
          // using the actual name and not the lower-cased grouping key, to preserve
          // the original naming. we only lower-cased it for case-insensitive grouping.
          val updatedDclName =
            if (list.length > 1)
              if (canonical eq topTop) canonical.dclName // top name should not be mangled
              else s"${canonical.dclName}_${i.toPaddedString(list.length)}"
            else canonical.dclName
          if (updatedDclName != canonical.dclName) canonicalRenames(canonical) = updatedDclName
          // every other structurally-identical design is shared into the canonical
          group.drop(1).foreach(dup => dupToCanonical(dup) = canonical)
        case _ =>
      }
    }

    if (canonicalRenames.isEmpty && dupToCanonical.isEmpty) designDB
    else
      // Apply the sharing on the hierarchical sub-DBs.
      val dupKeys = dupToCanonical.keysIterator.map(_.ownerRef).toSet
      // duplicate design's sub-DB key -> the canonical design's sub-DB key. A parent
      // inst that targeted a duplicate must be retargeted to the canonical key (its
      // `designRef` IS that key under unification).
      val dupKeyToCanonicalKey: Map[DFOwner.Ref, DFOwner.Ref] =
        dupToCanonical.iterator.map((dup, canon) => dup.ownerRef -> canon.ownerRef).toMap
      // canonical design block -> its renamed copy. Built ONCE and reused so the
      // member-list head and EVERY refTable entry resolving to it stay the SAME
      // object. Otherwise a member's `ownerRef` would resolve (via refTable) to the
      // pre-rename block while the member list shows the renamed one — an
      // inconsistency `newToOld.canonicalize` used to mask but which now breaks
      // hierarchy navigation (e.g. OMLGen) since the root is threaded directly.
      val canonicalReplace: Map[DFDesignBlock, DFDesignBlock] =
        canonicalRenames.iterator.map { (canon, newName) =>
          canon -> canon.copy(meta = canon.meta.copy(nameOpt = Some(newName)))
        }.toMap
      val newSubDBs: ListMap[DFOwner.Ref, DB] = ListMap.from(
        designDB.subDBs.iterator.flatMap { (key, sub) =>
          // drop the redundant duplicate sub-DBs entirely
          if (dupKeys.contains(key)) None
          else
            val instReplace = collection.mutable.Map.empty[DFDesignInst, DFDesignInst]
            val newMembers = sub.members.map {
              // rename the sub-DB's top if it is a renamed canonical design
              case d: DFDesignBlock if canonicalReplace.contains(d) => canonicalReplace(d)
              // retarget a parent inst that targeted a duplicate onto the canonical
              // key (its `designRef` IS that key under unification)
              case inst: DFDesignInst if dupKeyToCanonicalKey.contains(inst.designRef) =>
                val newInst = inst.copy(designRef =
                  dupKeyToCanonicalKey(inst.designRef).asInstanceOf[DFDesignInst.DesignRef]
                )
                instReplace(inst) = newInst
                newInst
              case m => m
            }
            // keep refTable values consistent: point any ref that targeted a
            // retargeted inst at its replacement, a renamed canonical at its renamed
            // block (so members resolve their owner to the renamed design), and any
            // remaining duplicate design block at its canonical (renamed if so).
            // `designRef` itself no longer lives in refTable.
            val newRefTable =
              sub.refTable.view.mapValues {
                case inst: DFDesignInst if instReplace.contains(inst) => instReplace(inst)
                case d: DFDesignBlock if canonicalReplace.contains(d) => canonicalReplace(d)
                case d: DFDesignBlock if dupToCanonical.contains(d)   =>
                  val canon = dupToCanonical(d)
                  canonicalReplace.getOrElse(canon, canon)
                case t => t
              }.toMap
            Some(key -> sub.update(members = newMembers, refTable = newRefTable))
        }
      )
      designDB.update(subDBs = newSubDBs)
    end if
  end transformGlobal
end UniqueDesigns

extension [T: HasDB](t: T)
  def uniqueDesigns(using CompilerOptions): DB =
    StageRunner.run(UniqueDesigns)(t.db)
