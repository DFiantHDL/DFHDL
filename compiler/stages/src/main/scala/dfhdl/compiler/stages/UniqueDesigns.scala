package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.immutable.ListMap

case object UniqueDesigns extends GlobalStage:
  def dependencies: List[Stage] = List()
  def nullifies: Set[Stage] = Set()
  def transformGlobal(designDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val eqDesign: ((DFDesignBlock, List[DFMember]), (DFDesignBlock, List[DFMember])) => Boolean =
      case ((thisBlock, theseMembers), (thatBlock, thoseMembers))
          if thisBlock.dclMeta == thatBlock.dclMeta =>
        (theseMembers lazyZip thoseMembers).forall { case (l, r) => l =~ r }
      case _ => false
    end eqDesign
    // Each sub-DB owns one canonical DFDesignBlock (its `designBlock`) and that
    // design's own members. Walk `internalDBs` to enumerate every canonical
    // design across the hierarchy without flattening — the sub-DB's own
    // `designMemberTable` resolves the design's locals through that sub-DB's
    // getSet, which is exactly what `eqDesign` needs.
    val designsAndMembers: List[(DFDesignBlock, List[DFMember])] =
      designDB.internalDBs.values.toList.flatMap { subDB =>
        val block = subDB.designBlock.get
        if (block.isDuplicate) None
        else
          given MemberGetSet = subDB.getSet
          Some(block -> subDB.designMemberTable.getOrElse(block, Nil))
      }
    println(s"[UniqueDesigns DEBUG] designsAndMembers has ${designsAndMembers.size} entries:")
    designsAndMembers.foreach { case (b, _) => println(s"  - ${b.dclName} (${b.hashString}, ownerRef=${b.ownerRef.hashString}, isTop=${b.isTop})") }
    // we're grouping always according to case-insensitive design names because these affect
    // the eventual file names and we want these to be different across all operating systems.
    // the actual name case is preserved for design/entity/module generation.
    val sameBlockLists: List[List[DFDesignBlock]] =
      designsAndMembers.view
        .groupByCompare(eqDesign, _._1.dclName.toLowerCase().hashCode()).map(_.unzip._1).toList
    val uniqueTypeMap: Map[String, List[List[DFDesignBlock]]] =
      sameBlockLists.groupBy(g => g.head.dclName.toLowerCase())
    val perBlockPatches: List[(DFDesignBlock, Patch.Replace)] =
      uniqueTypeMap.toList.flatMap { case (designType, list) =>
        list.zipWithIndex.flatMap {
          case (group, i) if group.length > 1 || list.length > 1 =>
            val groupHead = group.head
            // using the actual name and not the `designType` grouping, to preserve the original
            // naming. we only lower-cased it for case-insensitive grouping.
            val updatedDclName =
              if (list.length > 1)
                if (groupHead.isTop) groupHead.dclName // top name should not be mangled
                else s"${groupHead.dclName}_${i.toPaddedString(list.length)}"
              else groupHead.dclName
            var first = true
            group.map(block =>
              val tags =
                if (first)
                  first = false
                  block.tags
                else block.tags.tag(DuplicateTag)
              block -> Patch.Replace(
                block.copy(meta = block.meta.copy(nameOpt = Some(updatedDclName)), tags = tags),
                Patch.Replace.Config.FullReplacement
              )
            )
          case _ => Nil
        }
      }
    if (perBlockPatches.isEmpty) designDB
    else
      // Each design block lives as `members.head`-after-globals of exactly one
      // sub-DB (the one keyed by its ownerRef). Apply each block's patch to
      // that sub-DB. Other sub-DBs that have the block as a local member
      // retain the stale copy; `newToOld` canonicalizes via ownerRef so the
      // flat output points at the patched block.
      val patchByKey: Map[DFOwner.Ref, List[(DFMember, Patch)]] =
        perBlockPatches.groupBy { case (block, _) => block.ownerRef }
      val updatedSubs = designDB.internalDBs.map { case (key, subDB) =>
        patchByKey.get(key) match
          case Some(patches) =>
            given MemberGetSet = subDB.getSet
            key -> subDB.patch(patches)
          case None => key -> subDB
      }
      designDB.copy(internalDBs = ListMap.from(updatedSubs))
  end transformGlobal
end UniqueDesigns

extension [T: HasDB](t: T)
  def uniqueDesigns(using CompilerOptions): DB =
    StageRunner.run(UniqueDesigns)(t.db)
