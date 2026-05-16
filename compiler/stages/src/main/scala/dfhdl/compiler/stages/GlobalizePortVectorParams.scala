package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import scala.collection.immutable.ListMap
import scala.collection.mutable
import dfhdl.compiler.stages.vhdl.VHDLDialect

/** Duplicates each `DFDesignInst` whose target `DFDesignBlock` has port `DFVector`s with
  * dim/cell-type widths that reach a `DFVal.DesignParam`. This is the criteria-side of the work
  * previously baked into `GlobalizePortVectorParams.Phase 2`; the actual sub-tree cloning is done
  * by [[ReduplicateDesign]].
  */
case object Duplicate4GlobalizePortVectorParams extends ReduplicateDesign:
  override def dependencies: List[Stage] = List(UniqueDesigns)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false

  def criteria(inst: DFDesignInst)(using MemberGetSet, CompilerOptions): Boolean =
    val design = inst.getDesignBlock
    val subDB = getSet.designDB.rootDB.subDBs(design.ownerRef)
    subDB.atGetSet {
      def preCheckRef(ref: DFRef.TwoWayAny): Boolean =
        ref.get match
          case dfVal: DFVal if !dfVal.isGlobal =>
            !dfVal.isAnonymous || dfVal.getRefs.exists(preCheckRef)
          case _ => false
      def preCheckIntParamRef(ref: IntParamRef): Boolean =
        ref.getRef.exists(preCheckRef)
      def preCheckVector(dfType: DFType): Boolean = dfType match
        case dt: DFVector =>
          dt.cellDimParamRefs.exists(preCheckIntParamRef) ||
          (
            dt.cellType match
              case DFBits(w)    => preCheckIntParamRef(w)
              case DFUInt(w)    => preCheckIntParamRef(w)
              case DFSInt(w)    => preCheckIntParamRef(w)
              case dt: DFVector => preCheckVector(dt.cellType)
              case _            => false
          )
        case _ => false
      subDB.members.exists {
        case dcl @ DclPort() => preCheckVector(dcl.dfType)
        case _               => false
      }
    }
  end criteria
end Duplicate4GlobalizePortVectorParams

/** Globalizes design parameters that set port vector lengths. Required only for vhdl.v93 (which
  * does not support arrays with unconstrained ranges).
  *
  * Sub-tree duplication is delegated to [[Duplicate4GlobalizePortVectorParams]]. By the time this
  * stage runs, every design needing globalization has its own unique `DFDesignBlock` and a single
  * `DFDesignInst` referencing it.
  *
  * All globalization is concentrated in the TOP sub-DB's `transformSubDB` call (the first DFS
  * iteration): a single `MetaDesign(top, Before)` block creates a new global `DFVal.Alias.AsIs` for
  * every `DesignParam` reachable from any design's port `DFVector` widths, in DFS order so
  * `${design.dclName}_${param.getName}` aliases can reference their ancestor counterparts
  * (`ID_id1_width.relValRef → IDTop_widthTop`). The same global objects are reused across
  * descendant sub-DBs via a per-`rootDB` cache, so `newToOld`'s object-identity dedup emits each
  * global exactly once at the start of the flat output.
  *
  * Per-sub-DB patches still run locally:
  *   - Each design's `DesignParam`s and moved anonymous deps are rewired (`ChangeRefAndRemove`
  *     replaced by the cached new globals) inside the sub-DB that owns them.
  *   - Vec-type replacements (PBNS unification) are applied per sub-DB.
  *   - `inst.paramMap` entries for globalized params are pruned from each parent sub-DB's
  *     `DFDesignInst`s.
  */
case object GlobalizePortVectorParams extends HierarchyStage:
  override def dependencies: List[Stage] = List(Duplicate4GlobalizePortVectorParams)
  override def nullifies: Set[Stage] = Set(DropUnreferencedAnons, DFHDLUniqueNames)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => false

  // Per-rootDB shared state, populated by the TOP sub-DB's `transformSubDB`
  // (the first DFS iteration). Subsequent sub-DBs read it.
  private case class GlobState(
      // Param-name set per design, used to prune `inst.paramMap` entries in
      // whichever parent sub-DB hosts the inst.
      globalizedNamesByDesign: Map[DFDesignBlock, Set[String]],
      // Patches grouped by the sub-DB they target (keyed by the design block
      // that anchors that sub-DB). Top's entry additionally includes the
      // MetaDesign `dsn.patch` that adds all new globals before the top.
      patchesByDesign: Map[DFDesignBlock, List[(DFMember, Patch)]]
  )

  private val stateCache = mutable.WeakHashMap.empty[DB, GlobState]

  // -------------------------------------------------------------------------
  // Predicate / walker helpers (shared between pre-analysis and the
  // MetaDesign body).
  // -------------------------------------------------------------------------

  // Collect named non-global DFVals reachable from this design's port
  // `DFVector` widths/dims and return just the `DesignParam`s owned by this
  // design. Intermediate named values are walked so the param set is
  // complete even across `a + 1`-style expressions.
  private def collectParamsFromPorts(subDB: DB, thisDesign: DFDesignBlock)(using
      MemberGetSet
  ): List[DFVal.DesignParam] =
    val collected = mutable.LinkedHashSet.empty[DFVal]
    def walkRef(ref: DFRef.TwoWayAny): Unit =
      ref.get match
        case dfVal: DFVal if !dfVal.isGlobal =>
          dfVal.getRefs.foreach(walkRef)
          if (!dfVal.isAnonymous) collected += dfVal
        case _ =>
    def walkIntParamRef(ref: IntParamRef): Unit = ref.getRef.foreach(walkRef)
    def walkVector(dfType: DFType): Unit = dfType match
      case dt: DFVector =>
        dt.cellDimParamRefs.foreach(walkIntParamRef)
        dt.cellType match
          case DFBits(w)    => walkIntParamRef(w)
          case DFUInt(w)    => walkIntParamRef(w)
          case DFSInt(w)    => walkIntParamRef(w)
          case dt: DFVector => walkVector(dt.cellType)
          case _            =>
      case _ =>
    subDB.members.foreach {
      case dcl @ DclPort() => walkVector(dcl.dfType)
      case _               =>
    }
    collected.view.collect {
      case dp: DFVal.DesignParam if dp.getOwnerDesign eq thisDesign => dp
    }.toList
  end collectParamsFromPorts

  // -------------------------------------------------------------------------
  // Per-sub-DB transform.
  // -------------------------------------------------------------------------
  def transformSubDB(rootDB: DB)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      refGen: RefGen
  ): DB =
    val sub = subDB
    val thisDesignOpt = sub.members.collectFirst { case d: DFDesignBlock => d }
    thisDesignOpt match
      case None             => sub
      case Some(thisDesign) =>
        // The very first transformSubDB call for a given `rootDB` is the
        // top design's call (DFS order). It populates the shared state by
        // building a single MetaDesign at the top that materialises every
        // new global across the whole hierarchy.
        val state = stateCache.getOrElseUpdate(
          rootDB,
          buildSharedState(rootDB, sub, thisDesign)
        )
        applyForSub(rootDB, sub, thisDesign, state)
  end transformSubDB

  // Construct the cross-sub-DB shared state inside TOP's transform context.
  // This is where all the new globals get created (anchored before TOP's
  // design block), and where per-design replacement patches are recorded.
  private def buildSharedState(rootDB: DB, topSub: DB, topDesign: DFDesignBlock)(using
      getSet: MemberGetSet,
      co: CompilerOptions,
      refGen: RefGen
  ): GlobState =
    // Collect per-design `DesignParam`s in DFS order — top first so that
    // descendant designs' replacement values can walk through
    // `paramReplacementMap` to ancestor globals.
    val paramsPerDesign: ListMap[DFDesignBlock, List[DFVal.DesignParam]] =
      ListMap.from(rootDB.subDBs.values.iterator.flatMap { sub =>
        sub.members.collectFirst { case d: DFDesignBlock => d }.flatMap { d =>
          val params = sub.atGetSet(collectParamsFromPorts(sub, d))
          if (params.isEmpty) None else Some(d -> params)
        }
      })

    val globalizedNamesByDesign: Map[DFDesignBlock, Set[String]] =
      paramsPerDesign.view.mapValues(_.map(p => topSub.atGetSet(p.getName)).toSet).toMap

    // `oldVal → newGlobal` map populated as MetaDesign creates globals.
    // Descendant designs walk it when wiring their globals' relValRef chain.
    val paramReplacementMap = mutable.Map.empty[DFVal, DFVal]
    // `oldVal → Patch` accumulator (target-design keyed).
    val patchesByDesign = mutable.Map.empty[DFDesignBlock, mutable.ListBuffer[(DFMember, Patch)]]
    def addPatch(design: DFDesignBlock, patch: (DFMember, Patch)): Unit =
      patchesByDesign.getOrElseUpdate(design, mutable.ListBuffer.empty) += patch

    val dsn = new MetaDesign(topDesign, Patch.Add.Config.Before):
      // Make every sub-DB's refTable visible to MetaDesign's mutableDB so
      // `plantMember` of moved-from-parent-sub-DB members can resolve their
      // `ownerRef` and ref chains (each lives in its parent design's
      // sub-DB, not in TOP's). MetaDesign already injects TOP's getSet at
      // construction; we layer the other sub-DBs on top.
      rootDB.subDBs.values.foreach { sub =>
        if (!(sub eq topSub))
          dfc.mutableDB.injectMetaGetSet(sub.getSet)
      }

      paramsPerDesign.foreach { case (design, params) =>
        // The inst targeting `design` lives in some PARENT sub-DB (TOP for
        // top-level designs, an outer nested design otherwise). Its
        // `paramMap[name]` holds the applied value for each param, and the
        // refTable that resolves those refs is the parent sub-DB's. The
        // applied values and their anonymous deps therefore live in the
        // PARENT sub-DB — NOT in `design`'s own sub-DB — and must be
        // collected under that parent's getSet.
        val instOpt = rootDB.designBlockInstMap.get(design).flatMap(_.headOption)
        val parentSubOpt = instOpt.flatMap { inst =>
          rootDB.subDBs.values.find(_.members.exists(_ eq inst))
        }
        val designSub = rootDB.subDBs.get(design.ownerRef).getOrElse(topSub)
        val parentDesignOpt = parentSubOpt.flatMap(
          _.members.collectFirst { case d: DFDesignBlock => d }
        )

        // Resolve each param's applied value under the right getSet:
        //   parent's getSet when there is an inst (applied value lives in
        //   parent), else design's own getSet (the absolute top — its
        //   params just use their declared default).
        // The `deps` list collects every member that must be brought along
        // with the new global. Two flavours of "applied value" need to feed
        // it: an anonymous expression (e.g. `lengthTop + 1`) contributes
        // itself + its anon operands; a *named* non-DesignParam (e.g.
        // `val length3 = length + 1`) contributes itself (so it can be
        // renamed and moved up) plus its anon operands. DesignParam
        // applieds are handled by their own design's processing — they're
        // already in paramReplacementMap by the time descendants run.
        case class ParamPlan(
            param: DFVal.DesignParam,
            applied: DFVal,
            deps: List[DFVal]
        )
        def depsFor(applied: DFVal)(using MemberGetSet): List[DFVal] =
          applied match
            case _: DFVal.DesignParam     => Nil
            case _ if applied.isAnonymous =>
              applied.collectRelMembers(false).filterNot(_.isGlobal)
            case _ =>
              applied.collectRelMembers(true).filterNot(_.isGlobal)
                .filterNot(_.isInstanceOf[DFVal.DesignParam])
        val plans: List[ParamPlan] =
          parentSubOpt match
            case Some(parentSub) =>
              parentSub.atGetSet {
                params.map { p =>
                  val applied = instOpt.flatMap(_.paramMap.get(p.getName))
                    .map(_.get.asInstanceOf[DFVal])
                    .getOrElse(p.defaultValRef.get.asInstanceOf[DFVal])
                  ParamPlan(p, applied, depsFor(applied))
                }
              }
            case None =>
              designSub.atGetSet {
                params.map { p =>
                  val applied = p.defaultValRef.get.asInstanceOf[DFVal]
                  ParamPlan(p, applied, depsFor(applied))
                }
              }

        plans.foreach { plan =>
          val param = plan.param
          val deps = plan.deps
          // First handle the deps (in elaboration order: anon deps before
          // their named referrers; `collectRelMembers` already returns this
          // bottom-up). These live in the parent's sub-DB.
          val depPatchTarget: DFDesignBlock =
            parentDesignOpt.getOrElse(design)
          deps.foreach { m =>
            // Skip if we already minted a global for this dep on an earlier
            // param's pass.
            if (!paramReplacementMap.contains(m))
              if (!m.isAnonymous)
                // Named non-anonymous (e.g., `val length3 = length + 1`):
                // rename with its OWNER design's dclName (the sub-DB it
                // lives in — `length3` is owned by `IDTop` so it becomes
                // `IDTop_length3`), plant in MetaDesign (TOP), and rewire
                // refs in the parent sub-DB.
                val ownerName = parentSubOpt match
                  case Some(parentSub) =>
                    parentSub.atGetSet(m.getOwnerDesign.dclName)
                  case None => design.dclName
                val renamed = parentSubOpt match
                  case Some(parentSub) =>
                    parentSub.atGetSet(m.setName(s"${ownerName}_${m.getName}"))
                  case None =>
                    designSub.atGetSet(m.setName(s"${ownerName}_${m.getName}"))
                plantMember(renamed)
                paramReplacementMap += m -> renamed
                addPatch(
                  depPatchTarget,
                  m -> Patch.Replace(renamed, Patch.Replace.Config.ChangeRefAndRemove)
                )
              else
                // Anonymous (e.g., the `+1` Func, its Const(1) operand):
                // move by planting in MetaDesign + removing from the
                // parent sub-DB.
                plantMember(m)
                addPatch(depPatchTarget, m -> Patch.Remove(isMoved = true))
          }

          // Now create the new global alias for the DesignParam itself. Its
          // `relValRef` walks `paramReplacementMap` so descendant designs
          // pick up ancestor globals (e.g., `ID_id1_width.relValRef →
          // IDTop_widthTop`).
          var updatedParamVal = plan.applied
          while (paramReplacementMap.contains(updatedParamVal))
            updatedParamVal = paramReplacementMap(updatedParamVal)
          val updatedMeta = designSub.atGetSet {
            param.meta.setName(s"${design.dclName}_${param.getName}")
          }
          val globalParam = dfhdl.core.DFVal.Alias.AsIs.forced(
            param.dfType,
            updatedParamVal
          )(using dfc.setMeta(updatedMeta))
          paramReplacementMap += param -> globalParam
          // The DesignParam itself lives in `design`'s sub-DB, so its
          // replacement patch targets `design`.
          addPatch(
            design,
            param -> Patch.Replace(globalParam, Patch.Replace.Config.ChangeRefAndRemove)
          )
        }
      }
    end dsn

    // The MetaDesign-add patch belongs to the top design's sub-DB.
    addPatch(topDesign, dsn.patch)

    GlobState(
      globalizedNamesByDesign = globalizedNamesByDesign,
      patchesByDesign = patchesByDesign.view.mapValues(_.toList).toMap
    )
  end buildSharedState

  // Apply the patches assembled for `thisDesign`'s sub-DB: the param
  // replacements + vec-type replacements + inst.paramMap pruning. The TOP
  // sub-DB also gets the MetaDesign add patch (already in its patch list).
  private def applyForSub(
      rootDB: DB,
      sub: DB,
      thisDesign: DFDesignBlock,
      state: GlobState
  )(using MemberGetSet, CompilerOptions, RefGen): DB =
    val replacePatches = state.patchesByDesign.getOrElse(thisDesign, Nil)
    val vecTypeReplaceMap = computeVecTypeReplaceMap(rootDB, sub)
    val (vecTypePbnsPatches, vecTypeOtherPatches) =
      computeVecTypePatches(sub, vecTypeReplaceMap)
    val instPatches = computeInstPatches(sub, state.globalizedNamesByDesign)

    if (
      replacePatches.isEmpty && vecTypePbnsPatches.isEmpty &&
      vecTypeOtherPatches.isEmpty && instPatches.isEmpty
    )
      sub
    else
      // PBNS type patches first (they introduce shared port-derived TypeRefs);
      // the non-PBNS replacements may purge those TypeRefs from the originals.
      sub
        .patch(replacePatches ++ vecTypePbnsPatches)
        .patch(vecTypeOtherPatches)
        .patch(instPatches)
  end applyForSub

  // -------------------------------------------------------------------------
  // vec-type replacement analysis (per sub-DB; cross-design lookup via rootDB).
  // -------------------------------------------------------------------------
  private def computeVecTypeReplaceMap(rootDB: DB, sub: DB)(using
      MemberGetSet
  ): Map[DFVector, DFVector] =
    val out = mutable.Map.empty[DFVector, DFVector]
    def pbnsPortType(pbns: DFVal.PortByNameSelect): Option[DFVector] =
      val target = pbns.designInstRef.get.getDesignBlock
      rootDB.subDBs.get(target.ownerRef).flatMap { tsub =>
        tsub.atGetSet {
          tsub.members.collectFirst {
            case dcl: DFVal.Dcl
                if dcl.isPort && dcl.getOwnerDesign.eq(target) &&
                  dcl.getRelativeName(target) == pbns.portNamePath =>
              dcl
          }
        }.map(_.dfType).collect { case v: DFVector => v }
      }
    // Index nets by their endpoints so a PBNS can find the non-PBNS member
    // connected to it without us needing `isSimilarTo` (which currently
    // relies on `appliedOrDefaultVal` and breaks under the new-style
    // `ownerRef → Empty` convention for non-top design blocks).
    val netsByEndpoint: Map[DFMember, List[DFNet]] =
      sub.members.iterator.collect { case net: DFNet => net }
        .toList.flatMap(n =>
          List[(DFMember, DFNet)](n.lhsRef.get -> n, n.rhsRef.get -> n)
        ).groupMap(_._1)(_._2)
    sub.members.foreach {
      case pbns: DFVal.PortByNameSelect =>
        pbnsPortType(pbns).foreach { dclType =>
          if (pbns.dfType != dclType)
            out += pbns.dfType.asInstanceOf[DFVector] -> dclType
          // For each net connecting this PBNS to a non-PBNS DFVector node,
          // propagate the target port's dfType to the other side too. This
          // is the per-design unification step that previously rode on top
          // of `isSimilarTo`; doing it directly off the PBNS sidesteps the
          // broken cross-design `isTop` check inside `compare/strip`.
          netsByEndpoint.getOrElse(pbns, Nil).foreach { net =>
            val otherRef = if (net.lhsRef.get eq pbns) net.rhsRef else net.lhsRef
            otherRef.get match
              case other: DFVal.PortByNameSelect => // skip — both PBNS, handled separately
              case other: DFVal                  =>
                other.dfType match
                  case otherVec: DFVector if otherVec != dclType =>
                    out += otherVec -> dclType
                  case _ =>
              case _ =>
          }
        }
      case _ =>
    }
    out.toMap
  end computeVecTypeReplaceMap

  private def computeVecTypePatches(
      sub: DB,
      vecTypeReplaceMap: Map[DFVector, DFVector]
  )(using MemberGetSet): (List[(DFMember, Patch)], List[(DFMember, Patch)]) =
    sub.members.collect {
      case dfVal @ DFVector.Val(dfType) if vecTypeReplaceMap.contains(dfType) =>
        val patch: (DFMember, Patch) = dfVal -> Patch.Replace(
          dfVal.updateDFType(vecTypeReplaceMap(dfType)),
          Patch.Replace.Config.FullReplacement
        )
        patch
    }.partition((m, _) => m.isInstanceOf[DFVal.PortByNameSelect])

  // -------------------------------------------------------------------------
  // Inst paramMap pruning (per sub-DB; uses the pre-computed names map).
  // -------------------------------------------------------------------------
  private def computeInstPatches(
      sub: DB,
      globalizedNamesByDesign: Map[DFDesignBlock, Set[String]]
  )(using MemberGetSet): List[(DFDesignInst, Patch.Replace)] =
    sub.members.collect {
      case inst: DFDesignInst =>
        val target = inst.getDesignBlock
        val toRemove = globalizedNamesByDesign.getOrElse(target, Set.empty)
        if (toRemove.isEmpty) None
        else
          val pruned = inst.paramMap.filterNot((name, _) => toRemove.contains(name))
          Some(inst -> Patch.Replace(
            inst.copy(paramMap = pruned),
            Patch.Replace.Config.FullReplacement
          ))
    }.flatten

end GlobalizePortVectorParams

extension [T: HasDB](t: T)
  def globalizePortVectorParams(using CompilerOptions): DB =
    StageRunner.run(GlobalizePortVectorParams)(t.db)
