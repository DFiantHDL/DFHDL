package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.collection.mutable

//format: off
/** Normalizes `initial` blocks (`ProcessBlock.Sensitivity.Initial`) into per-declaration
  * form and converts the trivial case into declaration inits. A block is transformed when
  * either:
  *   - the block is under an RT domain whose resolved timing configuration (the
  *     `@timing.reset` annotation written by `ExplicitClkRstCfg`, resolved through any
  *     `@timing.related` chain) has a reset, or
  *   - the backend is VHDL (which has no `initial` construct; the remaining per-declaration
  *     blocks are subsequently lowered by `DropInitialBlocks`).
  *
  * Blocks whose statements read a declaration that is assigned within the same block are not
  * split (splitting would lose the intra-block initialization order); they are left whole
  * for `DropInitialBlocks` (VHDL) or `ToED` (RT with reset) to lower.
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
  */
//format: on
case object SplitInitialBlocks extends HierarchyStage:
  def dependencies: List[Stage] = List(ExplicitClkRstCfg)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

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

  // Splitting is sound only when no statement reads a declaration that is assigned within
  // the same block (initialization order would be lost across the split blocks). LHS
  // partial-selection paths are excluded down to the assigned target, but their selection
  // indexes do count as reads.
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
      ((pb.isInRTDomain && pb.hasResolvedRstCfg) || summon[CompilerOptions].backend.isVHDL)

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

  // All rules are emitted as one bundled patch list per sub-DB (the patch system merges
  // same-member patches, so no sequential `patch()` phases are needed).
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
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
        if (hasCrossReads(pb, blockMembers))
          // splitting would lose the intra-block initialization order; the whole block is
          // left for `DropInitialBlocks` (VHDL) or `ToED` (RT with reset) to lower
          Nil
        else
          val dcls = assignedDcls(blockMembers)
          val dclSeeds: List[List[DFMember]] = dcls.map { dcl =>
            blockMembers.collect {
              case net @ DFNet.BAssignment(toVal, _) if toVal.departialDcl.exists(_._1 == dcl) =>
                net
            }
          }
          val residualSeeds: List[DFMember] = blockMembers.collect { case textOut: TextOut =>
            textOut
          }
          val groupSeeds = dclSeeds ++ (if (residualSeeds.nonEmpty) List(residualSeeds) else Nil)
          if (groupSeeds.sizeIs <= 1)
            // no split needed; try converting the block itself
            conversionPatch(blockMembers) match
              case Some(convPatch) =>
                convPatch :: (pb :: blockMembers).map(_ -> Patch.Remove())
              case None => Nil // stays initial (DropInitialBlocks/ToED lowers it)
          else
            // Rule 1 split: convertible groups become decl inits; the rest are re-emitted as
            // fresh per-declaration initial blocks before the original block, which is
            // removed along with all its members (the Add and the Remove on the block merge
            // into a single ReplaceWithFirst patch)
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
                      dfhdl.core.Process.Block.initial(using dfc.setMeta(pb.meta))
                    dfc.enterOwner(newPB)
                    plantClonedMembers(pb, keepList)
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
