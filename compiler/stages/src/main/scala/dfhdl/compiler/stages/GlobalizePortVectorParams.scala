package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.core.{DFTypeAny, asFE, refTW}
import dfhdl.compiler.ir.DFVal.PortByNameSelect

/** This stage globalizes design parameters that set port vector lengths. This is needed only for
  * vhdl.v93 that does not support arrays with unconstrained ranges.
  */
case object GlobalizePortVectorParams extends Stage:
  def dependencies: List[Stage] = List(UniqueDesigns)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons, DFHDLUniqueNames)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet
    // ---------------------------------------------------------------------
    // Phase 1 — identify which canonical design blocks "need globalization":
    // any port whose dfType is a DFVector with dim/cell-type widths that
    // reach a DesignParam owned by that design.
    //
    // Run on the original DB. Canonical designs hold their members; legacy
    // duplicates (still tagged in the IR but unreferenced after MutableDB
    // rewired all DFDesignInst.designRef to canonicals) carry no members
    // and contribute nothing to the analysis.
    // ---------------------------------------------------------------------
    val preParams = mutable.LinkedHashSet.empty[DFVal]
    def preCheckRef(ref: DFRef.TwoWayAny): Int =
      ref.get match
        case dfVal: DFVal =>
          if (dfVal.isGlobal) 0
          else
            val ret = dfVal.getRefs.map(preCheckRef).sum
            if (dfVal.isAnonymous) ret
            else
              preParams += dfVal
              ret + 1
        case _ => 0
    def preCheckIntParamRef(ref: IntParamRef): Int =
      ref.getRef.map(preCheckRef).getOrElse(0)
    def preCheckVector(dfType: DFType): Int = dfType match
      case dt: DFVector =>
        val dimCnt = dt.cellDimParamRefs.map(preCheckIntParamRef).sum
        val cellCnt = dt.cellType match
          case DFBits(w)    => preCheckIntParamRef(w)
          case DFUInt(w)    => preCheckIntParamRef(w)
          case DFSInt(w)    => preCheckIntParamRef(w)
          case dt: DFVector => preCheckVector(dt.cellType)
          case _            => 0
        dimCnt + cellCnt
      case _ => 0
    designDB.members.foreach {
      case dcl @ DclPort() => preCheckVector(dcl.dfType)
      case _               =>
    }
    val designsNeedingGlob: Set[DFDesignBlock] = preParams.view.collect {
      case dp: DFVal.DesignParam => dp.getOwnerDesign
    }.toSet

    if (designsNeedingGlob.isEmpty) designDB
    else
      // -------------------------------------------------------------------
      // Phase 2 — for each DFDesignInst whose target design needs
      // globalization, allocate a fresh duplicated DFDesignBlock. The first
      // inst targeting a given canonical keeps the canonical as its target;
      // subsequent insts each get a freshly cloned design block (with
      // freshly cloned members and refs). Recurse for nested DFDesignInsts
      // inside the freshly cloned members.
      // -------------------------------------------------------------------
      val freshDupForInst = mutable.LinkedHashMap.empty[DFDesignInst, DFDesignBlock]
      val freshDupMembers = mutable.LinkedHashMap.empty[DFDesignBlock, List[DFMember]]
      val designToInst = mutable.Map.empty[DFDesignBlock, DFDesignInst]
      val newRefs = mutable.Map.empty[DFRefAny, DFMember]
      val targetUseCount = mutable.Map.empty[DFDesignBlock, Int]
      val workQueue = mutable.Queue.empty[DFDesignInst]
      designDB.members.foreach {
        case inst: DFDesignInst => workQueue.enqueue(inst)
        case _                  =>
      }
      // Resolve an inst's current target. For original insts the designRef
      // is in designDB.refTable (use the implicit getSet); for fresh insts
      // we recorded the mapping in `newRefs` at creation time.
      def currentTarget(inst: DFDesignInst): DFDesignBlock =
        newRefs.get(inst.designRef) match
          case Some(d: DFDesignBlock) => d
          case _                      => inst.designRef.get
      while (workQueue.nonEmpty)
        val inst = workQueue.dequeue()
        val canonical = currentTarget(inst)
        if (designsNeedingGlob.contains(canonical))
          val cnt = targetUseCount.getOrElse(canonical, 0) + 1
          targetUseCount(canonical) = cnt
          if (cnt == 1)
            // First inst — the canonical itself serves as this inst's design.
            designToInst.getOrElseUpdate(canonical, inst)
          else
            // Subsequent inst — allocate a fresh duplicate of canonical.
            val freshDup0 = canonical.copyWithNewRefs.asInstanceOf[DFDesignBlock]
            val freshDup = freshDup0.setTags(_.tag(DuplicateTag)).asInstanceOf[DFDesignBlock]
            // Map original member → cloned member, seeded with the design
            // block itself so internal refs to the design (e.g. ownerRef
            // chains of nested members) rebind to the fresh dup.
            val o2n = mutable.Map.empty[DFMember, DFMember]
            o2n += canonical -> freshDup
            val origMembers = designDB.designMemberTable.getOrElse(canonical, Nil)
            val clonedMembers = origMembers.map { om =>
              val nm = om.copyWithNewRefs
              o2n += om -> nm
              val origOwner = om.getOwner
              newRefs += nm.ownerRef -> o2n.getOrElse(origOwner, origOwner)
              om.getRefs.lazyZip(nm.getRefs).foreach { (oRef, nRef) =>
                val target = oRef.get
                newRefs += nRef -> o2n.getOrElse(target, target)
              }
              // DFDesignInst.designRef is OneWay and outside of getRefs;
              // register it explicitly so the freshly cloned inst initially
              // points at the same canonical as the original — recursion
              // below decides whether it gets its own fresh dup later.
              (om, nm) match
                case (oi: DFDesignInst, ni: DFDesignInst) =>
                  newRefs += ni.designRef -> oi.designRef.get
                case _ =>
              nm
            }
            // The fresh dup's ownerRef binds to canonical's owner so its
            // hierarchy and `getFullName` walk the same chain as canonical.
            newRefs += freshDup.ownerRef -> canonical.getOwner
            // Rewire the original inst's designRef to the fresh dup so
            // `designInstMap` in the augmented DB resolves cleanly.
            newRefs += inst.designRef -> freshDup
            freshDupForInst += inst -> freshDup
            freshDupMembers += freshDup -> clonedMembers
            designToInst += freshDup -> inst
            // Enqueue nested DFDesignInsts in the cloned members so they
            // contribute to the per-canonical use count and may receive
            // their own fresh dups.
            clonedMembers.foreach {
              case ni: DFDesignInst => workQueue.enqueue(ni)
              case _                =>
            }
          end if
        end if
      end while

      // Build the augmented members list. Inserting the fresh dup +
      // recursively-expanded fresh-dup members RIGHT BEFORE the inst that
      // targets it preserves the DFS pre-order layout that OMLGen requires
      // (canonical/dup design block followed by its body, then a non-design
      // sibling — the inst — that triggers the scope pop).
      def expandMembers(members: List[DFMember]): List[DFMember] =
        members.flatMap {
          case inst: DFDesignInst =>
            freshDupForInst.get(inst) match
              case Some(freshDup) =>
                (freshDup :: expandMembers(freshDupMembers(freshDup))) :+ inst
              case None => List(inst)
          case m => List(m)
        }
      val dupAugDB = designDB.update(
        members = designDB.membersGlobals ++ expandMembers(designDB.membersNoGlobals),
        refTable = designDB.refTable ++ newRefs
      )

      // -------------------------------------------------------------------
      // Phase 3 — analysis + patching on the augmented DB.
      // -------------------------------------------------------------------
      locally {
        given MemberGetSet = dupAugDB.getSet
        val designParams = mutable.LinkedHashSet.empty[DFVal]
        def checkRef(ref: DFRef.TwoWayAny): Int =
          ref.get match
            case dfVal: DFVal =>
              if (dfVal.isGlobal) 0
              else
                val ret = dfVal.getRefs.map(checkRef).sum
                if (dfVal.isAnonymous) ret
                else
                  designParams += dfVal
                  ret + 1
            case _ => 0
        def checkIntParamRef(intParamRef: IntParamRef): Int =
          intParamRef.getRef.map(checkRef).getOrElse(0)
        def checkVector(dfType: DFType): Int = dfType match
          case dt: DFVector =>
            val dimParamCnt = dt.cellDimParamRefs.map(checkIntParamRef).sum
            val cellTypeParamCnt = dt.cellType match
              case DFBits(widthParamRef) => checkIntParamRef(widthParamRef)
              case DFUInt(widthParamRef) => checkIntParamRef(widthParamRef)
              case DFSInt(widthParamRef) => checkIntParamRef(widthParamRef)
              case dt: DFVector          => checkVector(dt.cellType)
              case _                     => 0
            dimParamCnt + cellTypeParamCnt
          case _ => 0
        val vecTypeReplaceMap = mutable.Map.empty[DFVector, DFVector]
        // Index every port declaration by (target design, portNamePath) so a
        // PortByNameSelect can resolve its underlying Dcl directly from the
        // augmented DB without the legacy machinery.
        val portByDesign: Map[DFDesignBlock, Map[String, DFVal.Dcl]] =
          dupAugDB.members.iterator.collect {
            case dcl: DFVal.Dcl if dcl.isPort => dcl
          }.toList.groupBy(_.getOwnerDesign)
            .view.mapValues(_.view.map(d => d.getRelativeName(d.getOwnerDesign) -> d).toMap)
            .toMap
        def pbnsPortType(pbns: DFVal.PortByNameSelect): Option[DFVector] =
          val target = pbns.designInstRef.get.getDesignBlock
          portByDesign.get(target).flatMap(_.get(pbns.portNamePath)).map(_.dfType).collect {
            case v: DFVector => v
          }
        object VectorNetNodeType:
          def unapply(ref: DFRefAny): Option[DFVector] = ref.get match
            case pbns: DFVal.PortByNameSelect => pbnsPortType(pbns)
            case DFVector.Val(dfType)         => Some(dfType)
            case _                            => None
        dupAugDB.members.foreach {
          case dcl @ DclPort()              => checkVector(dcl.dfType)
          case pbns: DFVal.PortByNameSelect =>
            pbnsPortType(pbns).foreach { dclType =>
              if (pbns.dfType != dclType)
                vecTypeReplaceMap += pbns.dfType.asInstanceOf[DFVector] -> dclType
            }
          case net @ DFNet(lhsRef = VectorNetNodeType(lhsType), rhsRef = VectorNetNodeType(rhsType))
              if !(lhsType == rhsType) && lhsType.isSimilarTo(rhsType) =>
            val lhsCnt = checkVector(lhsType)
            val rhsCnt = checkVector(rhsType)
            // when a PBNS connects to a non-PBNS, always unify to the child design's port type
            // so that after globalization both sides share the same instance-specific globals
            val lhsIsPbns = net.lhsRef.get.isInstanceOf[DFVal.PortByNameSelect]
            val rhsIsPbns = net.rhsRef.get.isInstanceOf[DFVal.PortByNameSelect]
            if (lhsIsPbns && !rhsIsPbns) vecTypeReplaceMap += rhsType -> lhsType
            else if (rhsIsPbns && !lhsIsPbns) vecTypeReplaceMap += lhsType -> rhsType
            else if (lhsCnt < rhsCnt) vecTypeReplaceMap += lhsType -> rhsType
            else if (lhsCnt > rhsCnt) vecTypeReplaceMap += rhsType -> lhsType
          case _ =>
        }
        // Split vecTypeReplace into PBNS patches and non-PBNS patches.
        // PBNS replacements introduce TypeRefs from port declarations (shared TypeRefs).
        // If applied in the same patch batch as port replacements (which purge those same TypeRefs),
        // the TypeRefs get removed before the PBNS can reference them.
        // Apply PBNS replacements first so the shared TypeRefs have a higher repeat count.
        val (vecTypePbnsPatches, vecTypeOtherPatches) = dupAugDB.members.collect {
          case dfVal @ DFVector.Val(dfType) if vecTypeReplaceMap.contains(dfType) =>
            dfVal -> Patch.Replace(
              dfVal.updateDFType(vecTypeReplaceMap(dfType)),
              Patch.Replace.Config.FullReplacement
            )
        }.partition((m, _) => m.isInstanceOf[DFVal.PortByNameSelect])
        def movedMembers(namedParam: DFVal): List[DFVal] =
          namedParam match
            // for DesignParams, also collect anonymous deps of the actual param value
            // (from paramMap), since collectRelMembers only follows getRefs which
            // does not include the paramMap value reference
            case param: DFVal.DesignParam =>
              param.appliedOrDefaultVal.collectRelMembers(false).filterNot(_.isGlobal) ++
                List(param)
            case _ =>
              namedParam.collectRelMembers(true).filterNot(_.isGlobal)

        val addedGlobals = designParams.view.flatMap(movedMembers).toList.distinct
        // The set of designs whose params were marked for globalization.
        // Origin/dup grouping is by `dclName` and uses the DuplicateTag we
        // attached during Phase 2 — origin has no tag, fresh dups do.
        val dupDesignSet = designParams.view.map(_.getOwnerDesign).toSet
        val dupDesignMap = dupDesignSet.groupBy(_.dclName).values.map { grp =>
          val (dups, orig) = grp.partition(_.isDuplicate)
          assert(orig.size == 1)
          orig.head -> dups.toList
        }.toMap

        // patches to change the duplicated design declaration names to a unique identifier according
        // to their instance names.
        // Only remove globalized params from paramMap; keep unglobalized ones intact.
        val globalizedParamNamesPerDesign: Map[DFDesignBlock, Set[String]] =
          designParams.view.collect { case dp: DFVal.DesignParam => dp }
            .groupBy(_.getOwnerDesign)
            .view.mapValues(_.map(_.getName).toSet).toMap
        def prunedParamMap(inst: DFDesignInst): ListMap[String, DFDesignInst.ParamRef] =
          val target = currentTarget(inst)
          val toRemove = globalizedParamNamesPerDesign.getOrElse(target, Set.empty)
          if (toRemove.isEmpty) inst.paramMap
          else inst.paramMap.filterNot((name, _) => toRemove.contains(name))
        // Locate the inst tied to a design via the map we built in Phase 2
        // (canonical → first-seen inst, fresh dup → its rewired inst). This
        // intentionally avoids `design.getDesignInst`, which we want to
        // retire.
        def instPatchesFor(design: DFDesignBlock): Option[(DFDesignInst, Patch.Replace)] =
          if (design.isTop) None
          else
            designToInst.get(design).map { inst =>
              inst -> Patch.Replace(
                inst.copy(paramMap = prunedParamMap(inst)),
                Patch.Replace.Config.FullReplacement
              )
            }
        // Build identifiers from the inst-side hierarchy rather than
        // `design.getFullName`. After OMLGen stopped listing nested
        // DFDesignBlocks in their parent's member table and `getName`
        // stopped delegating to `getDesignInst` for immutable getSets,
        // a non-top design block's own `getFullName` no longer reflects
        // the per-use-site path. The DFDesignInst still does — its
        // `ownerRef` walks straight up the parent design chain.
        def designPath(design: DFDesignBlock): String =
          if (design.isTop) design.dclName
          else
            designToInst.get(design) match
              case Some(inst) => inst.getFullName
              case None       => design.dclName
        val designPatches = dupDesignMap.view.flatMap {
          case (orig, dups) if dups.length >= 1 =>
            (orig :: dups).flatMap { design =>
              val updatedMeta =
                design.meta.setName(
                  s"${design.dclName}_${designPath(design).replaceAll("\\.", "_")}"
                )
              val updatedDesign = design.removeTagOf[DuplicateTag].copy(
                meta = updatedMeta
              )
              val blockPatch =
                design -> Patch.Replace(updatedDesign, Patch.Replace.Config.FullReplacement)
              blockPatch :: instPatchesFor(design).toList
            }
          case (orig, _) => instPatchesFor(orig).toList
        }.toList
        val paramReplacementMap = mutable.Map.empty[DFVal, DFVal]
        def getUpdatedParamValue(param: DFVal.DesignParam): DFVal =
          var dfVal = param.appliedOrDefaultVal
          while (paramReplacementMap.contains(dfVal)) dfVal = paramReplacementMap(dfVal)
          dfVal
        // Compose a global identifier from the owning design's inst path
        // and the value's local name (joined by `_`).
        def globalNameFor(value: DFVal): String =
          val owner = value.getOwnerDesign
          s"${designPath(owner)}_${value.getName}".replaceAll("\\.", "_")
        // add global parameters before the design top
        val dsn = new MetaDesign(dupAugDB.top, Patch.Add.Config.Before):
          // patches to replace with properly named parameter or just move the anonymous members
          val replacePatches = addedGlobals.map {
            // design parameters are transformed into global as-is named aliases
            case param: DFVal.DesignParam =>
              val updatedMeta = param.meta.setName(globalNameFor(param))
              val updatedParamVal = getUpdatedParamValue(param)
              val globalParam = dfhdl.core.DFVal.Alias.AsIs.forced(
                param.dfType,
                updatedParamVal
              )(using dfc.setMeta(updatedMeta))
              paramReplacementMap += param -> globalParam
              param -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
            case m: DFVal if !m.isAnonymous =>
              val globalParam = m.setName(globalNameFor(m))
              plantMember(globalParam)
              paramReplacementMap += m -> globalParam
              m -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
            case m =>
              plantMember(m)
              m -> Patch.Remove(isMoved = true)
          }
        // TODO: when combined to a single patch, there is a bug that prevents some members to get a global ownership
        // Apply PBNS type patches first (they introduce shared TypeRefs), then other type patches
        // (which may purge those same TypeRefs from the originals).
        dupAugDB
          .patch(dsn.patch :: dsn.replacePatches ++ vecTypePbnsPatches)
          .patch(vecTypeOtherPatches)
          .patch(designPatches)
      }
    end if
  end transform
end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
