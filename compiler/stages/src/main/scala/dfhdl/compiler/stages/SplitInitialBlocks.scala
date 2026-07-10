package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.annotation.tailrec
import scala.collection.mutable

//format: off
/** Lowers `initial` blocks (`ProcessBlock.Sensitivity.Initial`) into forms the downstream
  * stages and backends can consume. A block is transformed when either:
  *   - the block is under an RT domain whose resolved timing configuration (the
  *     `@timing.reset` annotation written by `ExplicitClkRstCfg`, resolved through any
  *     `@timing.related` chain) has a reset, or
  *   - the backend is VHDL (which has no `initial` construct).
  *
  * Blocks whose statements read a declaration that is assigned within the same block are not
  * split/converted (splitting would change their semantics); under VHDL+ED they are lowered
  * whole by Rule 3.
  *
  * ==Rule 1: Per-variable split==
  *
  * An initial block that assigns more than one declaration (or mixes assignments with
  * simulation-only statements) is split into one initial block per assigned declaration,
  * each keeping only that declaration's assignments along with their enclosing control flow,
  * plus a residual block for the remaining simulation-only statements (if any).
  * {{{
  * // Before
  * initial:
  *   a := 0
  *   for (i <- 0 until 4)
  *     vec(i) := 0
  *
  * // After
  * initial:
  *   a := 0
  * initial:
  *   for (i <- 0 until 4)
  *     vec(i) := 0
  * }}}
  *
  * ==Rule 2: Declaration-init conversion==
  *
  * An initial block reduced to a single full-width constant assignment is deleted and becomes
  * the declaration's `init`. After this, the existing init/reset machinery takes over (e.g.
  * a REG's init is applied on reset by `ToED`).
  * {{{
  * // Before
  * val a = SInt(16) <> VAR
  * initial:
  *   a := 0
  *
  * // After
  * val a = SInt(16) <> VAR init 0
  * }}}
  *
  * ==Rule 3: One-shot process conversion (VHDL)==
  *
  * Under the VHDL backend, any ED-domain initial block remaining after Rules 1/2 (sim-only
  * content, cross-reading blocks, non-convertible assignment blocks) becomes a `process`
  * (empty sensitivity) terminated by an endless `wait`, which VHDL prints as the classic
  * one-shot `process ... wait; end process;` form, preserving the block's sequential
  * execution at time zero.
  * {{{
  * // Before
  * initial:
  *   println("simulation started")
  *
  * // After
  * process:
  *   println("simulation started")
  *   wait
  * }}}
  */
//format: on
case object SplitInitialBlocks extends HierarchyStage:
  def dependencies: List[Stage] = List(ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  // The resolved reset presence of the block's domain: walk the `@timing.related` chain to
  // the timing owner and look for the resolved `@timing.reset` annotation (like `ToED` does).
  private def hasResolvedRst(pb: ProcessBlock)(using MemberGetSet): Boolean =
    @tailrec def resolveTimingOwner(owner: DFDomainOwner): DFDomainOwner =
      val relatedTarget = owner.meta.annotations.collectFirst {
        case rel: constraints.Timing.Related => rel.ref.get
      }
      relatedTarget match
        case Some(target) => resolveTimingOwner(target)
        case None         => owner
    resolveTimingOwner(pb.getOwnerDomain).meta.annotations.exists {
      case _: constraints.Timing.Reset => true
      case _                           => false
    }
  end hasResolvedRst

  // All declarations read by the given value's expression tree (dependencies included)
  private def readDcls(dfVal: DFVal)(using MemberGetSet): Set[DFVal.Dcl] = dfVal match
    case dcl: DFVal.Dcl => Set(dcl)
    case _              =>
      dfVal.getRefs.view
        .filterNot(_.isInstanceOf[DFRef.TypeRef])
        .map(_.get)
        .collect { case dfVal: DFVal => dfVal }
        .flatMap(readDcls)
        .toSet

  // The declarations assigned by the block's assignments, ordered by first assignment
  private def assignedDcls(blockMembers: List[DFMember])(using MemberGetSet): List[DFVal.Dcl] =
    val assigned = mutable.LinkedHashSet.empty[DFVal.Dcl]
    blockMembers.foreach {
      case DFNet.BAssignment(toVal, _) =>
        toVal.departialDcl.foreach { (dcl, _) => assigned += dcl }
      case _ =>
    }
    assigned.toList

  // Splitting/conversion is sound only when no statement reads a declaration that is
  // assigned within the same block (initialization order would be lost across the split
  // blocks). LHS partial-selection paths are excluded down to the assigned target, but
  // their selection indexes do count as reads.
  private def hasCrossReads(pb: ProcessBlock, blockMembers: List[DFMember])(using
      MemberGetSet
  ): Boolean =
    val assignedSet = assignedDcls(blockMembers).toSet
    if (assignedSet.isEmpty) false
    else
      blockMembers.exists {
        case net @ DFNet.BAssignment(toVal, fromVal) =>
          val lhsReads = toVal.departialDcl match
            case Some((dcl, _)) => readDcls(toVal) - dcl
            case None           => readDcls(toVal)
          ((readDcls(fromVal) ++ lhsReads) & assignedSet).nonEmpty
        case cb: DFConditional.Block =>
          cb.guardRef.get match
            case guard: DFVal => (readDcls(guard) & assignedSet).nonEmpty
            case _            => false
        case mh: DFConditional.DFMatchHeader =>
          (readDcls(mh.selectorRef.get) & assignedSet).nonEmpty
        case textOut: TextOut =>
          (textOut.msgArgs.view.map(_.get).flatMap(readDcls).toSet & assignedSet).nonEmpty
        case _ => false
      }
  end hasCrossReads

  private def transformsThisBlock(pb: ProcessBlock)(using
      MemberGetSet,
      CompilerOptions
  ): Boolean =
    pb.isInitial &&
      ((pb.isInRTDomain && hasResolvedRst(pb)) || summon[CompilerOptions].backend.isVHDL)

  // The ordered keep-list of one split group: the seed statements, their in-block value
  // dependencies (transitively), and their enclosing control-flow structure up to the block
  // (conditional chains are pulled in backwards via `prevBlockOrHeaderRef`).
  private def groupKeepList(
      pb: ProcessBlock,
      blockMembers: List[DFMember],
      seeds: List[DFMember]
  )(using MemberGetSet): List[DFMember] =
    val keep = mutable.Set.empty[DFMember]
    def isInBlock(m: DFMember): Boolean = m match
      case dfVal: DFVal if dfVal.isGlobal => false
      case _                              => m.isInsideOwner(pb)
    def add(m: DFMember): Unit =
      if (!keep.contains(m))
        keep += m
        m.getRefs.foreach { ref =>
          ref.get match
            case _: DFMember.Empty               =>
            case dep: DFMember if isInBlock(dep) => add(dep)
            case _                               =>
        }
        m.getOwner match
          case owner if owner == pb =>
          case owner                => add(owner)
    seeds.foreach(add)
    blockMembers.filter(keep)
  end groupKeepList

  // All rules are emitted as one bundled patch list per sub-DB — the patch system merges
  // same-member patches (e.g. a `Before`-anchored Add + a Remove on the same block become a
  // ReplaceWithFirst; a Replace + an InsideLast-Add compose), so no sequential `patch()`
  // phases are needed.
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val co = summon[CompilerOptions]

    def endlessWait()(using dfhdl.core.DFC): Unit =
      // an endless wait: `ir.Wait` with an anonymous const-`false` trigger
      // (printed by the VHDL backend as a bare `wait;`)
      dfhdl.core.Wait(dfhdl.core.DFVal.Const(dfhdl.core.DFBool, Some(false)))

    // Rule 3: convert the block itself, in place, into a one-shot process (empty sensitivity
    // list + endless wait). The Replace patch must precede the Add in the returned list so
    // the wait's owner reference is redirected to the updated block.
    def oneShotProcessPatches(pb: ProcessBlock): List[(DFMember, Patch)] =
      val updated = pb.copy(sensitivity = ProcessBlock.Sensitivity.List(Nil))
      val waitDsn = new MetaDesign(pb, Patch.Add.Config.InsideLast):
        endlessWait()(using dfc)
      List(pb -> Patch.Replace(updated, Patch.Replace.Config.FullReplacement), waitDsn.patch)

    // Rule 2: a group reduced to a single full-width constant assignment (plus its anonymous
    // dependencies) converts into the declaration's `init`; returns the dcl-anchored patch.
    def conversionPatch(keepList: List[DFMember]): Option[(DFMember, Patch)] =
      val nets = keepList.collect { case net: DFNet => net }
      val onlyNetAndAnons = keepList.forall {
        case _: DFNet                          => true
        case dfVal: DFVal if dfVal.isAnonymous => true
        case _                                 => false
      }
      nets match
        case (net @ DFNet.BAssignment(toVal, fromVal)) :: Nil if onlyNetAndAnons =>
          toVal.departialDcl match
            case Some((dcl, slice))
                if slice.isFullOf(dcl.dfType.widthIntOpt) == Tri.Yes && fromVal.isConst =>
              val dsn = new MetaDesign(
                dcl,
                Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
              ):
                import dfhdl.core.refTW
                val clonedInit = fromVal.cloneAnonValueAndDepsHere
                plantMember(
                  dcl.copy(initRefList = List(clonedInit.refTW[ir.DFVal.Dcl](using dfc)))
                )
              Some(dsn.patch)
            case _ => None
        case _ => None
      end match
    end conversionPatch

    val patchList: List[(DFMember, Patch)] = subDB.members.flatMap {
      case pb: ProcessBlock if transformsThisBlock(pb) =>
        val blockMembers = pb.members(MemberView.Flattened)
        // under VHDL, ED-domain groups/blocks that cannot become decl inits are lowered to
        // one-shot processes; RT-domain blocks are left for ToED's reset-branch planting
        val isVHDLED = co.backend.isVHDL && pb.isInEDDomain
        if (hasCrossReads(pb, blockMembers))
          // splitting/conversion would lose the intra-block initialization order — under
          // VHDL+ED the whole block is still lowered to a one-shot process (Rule 3)
          if (isVHDLED) oneShotProcessPatches(pb) else Nil
        else
          val dcls = assignedDcls(blockMembers)
          val dclSeeds: List[List[DFMember]] = dcls.map { dcl =>
            blockMembers.collect {
              case net @ DFNet.BAssignment(toVal, _)
                  if toVal.departialDcl.exists(_._1 == dcl) =>
                net
            }
          }
          val residualSeeds: List[DFMember] = blockMembers.collect { case textOut: TextOut =>
            textOut
          }
          val groupSeeds = dclSeeds ++ (if (residualSeeds.nonEmpty) List(residualSeeds) else Nil)
          if (groupSeeds.sizeIs <= 1)
            // no split needed — try converting the block itself
            conversionPatch(blockMembers) match
              case Some(convPatch) =>
                convPatch :: (pb :: blockMembers).map(_ -> Patch.Remove())
              case None if isVHDLED => oneShotProcessPatches(pb)
              case None             => Nil // stays initial (ToED plants it / Verilog prints it)
          else
            // Rule 1 split: convertible groups become decl inits; the rest are re-emitted as
            // fresh blocks (initial under RT, one-shot processes under VHDL+ED) before the
            // original block, which is removed along with all its members (the Add and the
            // Remove on the block merge into a single ReplaceWithFirst patch)
            val keepLists = groupSeeds.map(seeds => groupKeepList(pb, blockMembers, seeds))
            val (convPatches, reEmitLists) =
              keepLists.partitionMap { keepList =>
                conversionPatch(keepList).toLeft(keepList)
              }
            val reEmitPatchOption =
              if (reEmitLists.isEmpty) None
              else
                val dsn = new MetaDesign(pb, Patch.Add.Config.Before):
                  reEmitLists.foreach { keepList =>
                    val newPB =
                      if (isVHDLED)
                        dfhdl.core.Process.Block.list(Nil)(using dfc.setMeta(pb.meta))
                      else dfhdl.core.Process.Block.initial(using dfc.setMeta(pb.meta))
                    dfc.enterOwner(newPB)
                    plantClonedMembers(pb, keepList)
                    if (isVHDLED) endlessWait()(using dfc)
                    dfc.exitOwner()
                  }
                Some(dsn.patch)
            convPatches ++ reEmitPatchOption ++
              (pb :: blockMembers).map(_ -> Patch.Remove())
          end if
        end if
      case _ => Nil
    }
    subDB.patch(patchList)
  end transformSubDB
end SplitInitialBlocks

extension [T: HasDB](t: T)
  def splitInitialBlocks(using CompilerOptions): DB =
    StageRunner.run(SplitInitialBlocks)(t.db)
