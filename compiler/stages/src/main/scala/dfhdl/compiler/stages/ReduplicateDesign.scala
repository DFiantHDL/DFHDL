package dfhdl.compiler.stages

import dfhdl.compiler.ir.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable

// Re-duplicates a shared design sub-DB into per-instance copies, reversing the
// natural sharing produced by elaboration / `oldToNew`. The criteria selects
// which DFDesignInsts need their own private DFDesignBlock so that downstream
// stages can transform them differently per instance.
//
// Per group of insts targeting the same DFDesignBlock D:
//   N = total insts of D, K = matching insts.
//   - if N <= 1 or K == 0: no-op
//   - if K == N: first matching keeps the original; K-1 clones for the rest
//   - if K  < N: all K matching get clones (non-matching keep the original)
//
// Clones are renamed to `${D.dclName}_${inst.getName}`. Today the entire sub-DB
// must be cloned (fresh members + fresh refs) because the old-style flat DB
// requires globally unique refs after `newToOld`; once that round-trip is
// dropped, only the DFDesignBlock header itself will need to differ.
abstract class ReduplicateDesign extends GlobalStage:
  def dependencies: List[Stage] = Nil
  def nullifies: Set[Stage] = Set(UniqueDesigns, DFHDLUniqueNames)
  def criteria(inst: DFDesignInst)(using MemberGetSet, CompilerOptions): Boolean

  def transformGlobal(designDB: DB)(using
      co: CompilerOptions,
      refGen: RefGen
  ): DB =
    // ----- Step 1: group all DFDesignInsts by their target design block's key -----
    // Key is `designBlock.ownerRef` (the stable identity used by `subDBs`).
    val instsByTarget =
      mutable.LinkedHashMap.empty[DFOwner.Ref, mutable.ListBuffer[(DFDesignInst, DB)]]
    designDB.subDBs.foreach { case (_, parentSubDB) =>
      parentSubDB.members.foreach {
        case inst: DFDesignInst =>
          val d = inst.getDesignBlock(using parentSubDB.getSet)
          instsByTarget
            .getOrElseUpdate(d.ownerRef, mutable.ListBuffer.empty) += ((inst, parentSubDB))
        case _ =>
      }
    }

    // ----- Step 2: per group, apply criteria and decide which insts get clones -----
    // When K == N (all matching), the first matching inst keeps the original
    // DFDesignBlock object, BUT that block also gets renamed with the first
    // inst's name suffix so every matching inst ends up referring to a name-
    // prefixed block (`originalRenames` carries that rename for later
    // application across all sub-DBs that still reference the original).
    val cloneRequests = mutable.ListBuffer.empty[(DFDesignInst, DB)]
    val originalRenames = mutable.Map.empty[DFDesignBlock, DFDesignBlock]
    instsByTarget.foreach { case (_, instList) =>
      val n = instList.size
      // Single-instance groups (N == 1) are skipped wholesale: there's no
      // sharing to break and no peer block to name-disambiguate against, so
      // even a matching inst (K == N == 1) gets neither a clone nor a rename.
      if (n > 1)
        val matching = instList.toList.filter { case (inst, parentSubDB) =>
          parentSubDB.atGetSet(criteria(inst))
        }
        val k = matching.size
        if (k > 0)
          if (k == n)
            val (firstInst, firstParent) = matching.head
            val firstName = firstParent.atGetSet(firstInst.getName)
            val d = firstInst.getDesignBlock(using firstParent.getSet)
            val renamed = d.copy(meta = d.meta.setName(s"${d.dclName}_$firstName"))
            originalRenames(d) = renamed
            cloneRequests ++= matching.drop(1)
          else cloneRequests ++= matching
    }

    if (cloneRequests.isEmpty && originalRenames.isEmpty) designDB
    else
      // ----- Step 3: clone the sub-tree for each request -----
      // Each request produces a fresh sub-tree of cloned sub-DBs. The cloned
      // design block becomes the new target for that request's inst.
      val newClonedSubDBs = mutable.LinkedHashMap.empty[DFOwner.Ref, DB]
      // (parent subDB key) -> (orig inst -> cloned design block target).
      val parentRewires =
        mutable.LinkedHashMap.empty[DFOwner.Ref, mutable.LinkedHashMap[DFDesignInst, DFDesignBlock]]

      cloneRequests.foreach { case (inst, parentSubDB) =>
        val origTargetBlock = inst.getDesignBlock(using parentSubDB.getSet)
        val instName = parentSubDB.atGetSet(inst.getName)
        val acc = CloneAcc(
          memberMap = mutable.Map.empty,
          refMap = mutable.Map.empty,
          newSubDBs = mutable.LinkedHashMap.empty
        )
        val clonedBlock = cloneSubTree(origTargetBlock, Some(instName), acc, designDB)
        newClonedSubDBs ++= acc.newSubDBs
        val parentKey = parentSubDB.members.collectFirst { case d: DFDesignBlock =>
          d.ownerRef
        }.get
        parentRewires
          .getOrElseUpdate(parentKey, mutable.LinkedHashMap.empty)
          .update(inst, clonedBlock)
      }

      // ----- Step 4: rewire each touched parent sub-DB -----
      val updatedParents = parentRewires.map { case (parentKey, rewires) =>
        val parentSubDB = designDB.subDBs(StaticRef(parentKey))
        parentKey -> rewireParentSubDB(parentSubDB, rewires.toMap)
      }.toMap

      // ----- Step 5: assemble the new root subDBs ListMap in elaboration order -----
      // Surviving original sub-DBs (and the parents we rewired) may still
      // reference DFDesignBlocks that we've renamed in-place (the K==N case);
      // walk their members + refTable and substitute. Cloned sub-DBs don't
      // need this — by construction their members reference only the clones.
      def applyOriginalRenames(sub: DB): DB =
        if (originalRenames.isEmpty) sub
        else
          val newMembers = sub.members.map {
            case d: DFDesignBlock => originalRenames.getOrElse(d, d)
            case m                => m
          }
          val newRefTable = sub.refTable.view.mapValues {
            case d: DFDesignBlock => originalRenames.getOrElse(d, d)
            case t                => t
          }.toMap
          sub.update(members = newMembers, refTable = newRefTable)

      val mergedByKey = mutable.LinkedHashMap.empty[StaticRef, DB]
      designDB.subDBs.foreach { case (key, sub) =>
        mergedByKey(key) = applyOriginalRenames(updatedParents.getOrElse(key.asRef, sub))
      }
      newClonedSubDBs.foreach { case (key, sub) => mergedByKey(StaticRef(key)) = sub }

      val emitted = mutable.LinkedHashMap.empty[StaticRef, DB]
      def emit(key: StaticRef): Unit =
        if (!emitted.contains(key))
          mergedByKey.get(key).foreach { sub =>
            emitted(key) = sub
            sub.members.foreach {
              // `designRef` IS the target design's sub-DB key under unification,
              // so it can be used directly (these merged sub-DBs are not yet wired
              // into a root, so structural `getDesignBlock` is not available here).
              case inst: DFDesignInst => emit(inst.designRef)
              case _                  =>
            }
          }
      val topKey = designDB.subDBs.head._1
      emit(topKey)
      // Tail: include any cloned sub-DB that the DFS didn't reach (defensive —
      // every cloned design should be reachable through its rewired parent inst).
      mergedByKey.foreach { case (key, sub) => if (!emitted.contains(key)) emitted(key) = sub }

      designDB.update(subDBs = ListMap.from(emitted))
    end if
  end transformGlobal

  // Mutable accumulators threaded through `cloneSubTree`.
  private case class CloneAcc(
      memberMap: mutable.Map[DFMember, DFMember],
      refMap: mutable.Map[DFRefAny, DFRefAny],
      newSubDBs: mutable.LinkedHashMap[DFOwner.Ref, DB]
  )

  // Recursively clones the sub-DB rooted at `d`. Cloned `DFDesignInst`s
  // encountered during the walk trigger recursion on their target so that the
  // cloned insts' fresh `designRef`s resolve to freshly cloned nested design
  // blocks in the new refTable.
  private def cloneSubTree(
      d: DFDesignBlock,
      renameWith: Option[String],
      acc: CloneAcc,
      designDB: DB
  )(using RefGen): DFDesignBlock =
    val origSubDB = designDB.subDBs(StaticRef(d.ownerRef))

    // Clone every member; record (oldMember -> newMember) and pairwise
    // (oldRef -> newRef) via the symmetric `getAllRefs` enumeration on
    // old/new copies (relies on `copyAsNewRef` being called in the same
    // field order as `getAllRefs` lists them).
    origSubDB.members.foreach { m =>
      val newM = m.copyWithNewRefs
      acc.memberMap(m) = newM
      m.getAllRefs.lazyZip(newM.getAllRefs).foreach { (o, n) => acc.refMap(o) = n }
    }

    // Recurse for nested designs. Skip if already cloned (when the same target
    // is reached by multiple peer insts inside this sub-DB).
    origSubDB.members.foreach {
      case inst: DFDesignInst =>
        val childBlock = inst.getDesignBlock(using origSubDB.getSet)
        if (!acc.memberMap.contains(childBlock))
          cloneSubTree(childBlock, None, acc, designDB)
      case _ =>
    }

    // Under unification a cloned inst's `designRef` must equal its cloned target
    // block's `ownerRef` (the `subDBs` key). `copyWithNewRefs` does not freshen
    // `designRef`, so set it explicitly to the cloned child block's fresh ownerRef.
    origSubDB.members.foreach {
      case inst: DFDesignInst =>
        val clonedInst = acc.memberMap(inst).asInstanceOf[DFDesignInst]
        val clonedChild =
          acc.memberMap(inst.getDesignBlock(using origSubDB.getSet)).asInstanceOf[DFDesignBlock]
        acc.memberMap(inst) =
          clonedInst.copy(designRef = clonedChild.ownerRef.asInstanceOf[DFDesignInst.DesignRef])
      case _ =>
    }

    // Rename the top-level clone (criteria-driven clones only; nested clones
    // keep the original dclName — `UniqueDesigns` re-runs downstream to
    // suffix-uniquify any colliding names).
    renameWith.foreach { suffix =>
      val origClone = acc.memberMap(d).asInstanceOf[DFDesignBlock]
      val newName = s"${d.dclName}_$suffix"
      acc.memberMap(d) = origClone.copy(meta = origClone.meta.setName(newName))
    }

    val clonedD = acc.memberMap(d).asInstanceOf[DFDesignBlock]
    val newMembers = origSubDB.members.map(acc.memberMap)
    val newRefTable: Map[DFRefAny, DFMember] = origSubDB.refTable.map { case (oR, target) =>
      val nR = acc.refMap(oR)
      val newTarget = target match
        case _: DFMember.Empty => target // sentinel — passthrough
        case t                 => acc.memberMap(t)
      nR -> newTarget
    }.toMap
    acc.newSubDBs(clonedD.ownerRef) = DB(
      members = newMembers,
      refTable = newRefTable,
      globalTags = designDB.globalTags,
      srcFiles = Nil
    )
    clonedD
  end cloneSubTree

  // Rebuilds a parent sub-DB with each touched inst replaced by a copy whose
  // `designRef` points at the cloned target. Refs in the parent's refTable
  // that targeted the old inst are retargeted to the new inst.
  private def rewireParentSubDB(
      parentSubDB: DB,
      rewires: Map[DFDesignInst, DFDesignBlock]
  ): DB =
    // Point each rewired inst's `designRef` at its cloned target block's `ownerRef`
    // (the cloned sub-DB's key). Under unification `designRef` IS that key and is
    // resolved structurally via `subDBs`, so it is not added to the parent refTable.
    val instMap: Map[DFMember, DFMember] =
      rewires.map { case (origInst, clonedBlock) =>
        origInst -> origInst.copy(designRef = StaticRef(clonedBlock.ownerRef))
      }
    val newMembers = parentSubDB.members.map(m => instMap.getOrElse(m, m))
    // Retarget any ref whose value was an old inst to the new inst.
    val newRefTable: Map[DFRefAny, DFMember] =
      parentSubDB.refTable.view.mapValues(t => instMap.getOrElse(t, t)).toMap
    parentSubDB.update(members = newMembers, refTable = newRefTable)
  end rewireParentSubDB
end ReduplicateDesign
