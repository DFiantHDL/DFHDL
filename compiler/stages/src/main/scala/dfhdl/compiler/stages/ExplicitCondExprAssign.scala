package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DomainType.ED

//format: off
/** This stage transforms an assignment from a conditional expression to a statement.
  *
  * ==Rule 1: Conditional expression to statement==
  *
  * Converts assignments from conditional expressions into conditional statements.
  * {{{
  * // Before
  * val z = SInt(16) <> VAR
  * z := (if (x > 0) 5 else x)
  *
  * // After
  * val z = SInt(16) <> VAR
  * if (x > sd"16'0") z := sd"16'5"
  * else z := x
  * }}}
  *
  * ==Rule 2: ED domain process wrapping==
  *
  * When the conditional statement is in an ED domain and outside a process,
  * it is wrapped in a `process(all)` block.
  * {{{
  * // Before (EDDesign, outside process)
  * val y = SInt(16) <> OUT
  * y <>(if (x > sd"16'0") sd"16'5" else x)
  *
  * // After
  * val y = SInt(16) <> OUT
  * process(all):
  *   if (x > sd"16'0") y := sd"16'5"
  *   else y := x
  * }}}
  */
//format: on
case object ExplicitCondExprAssign extends HierarchyStage:
  def dependencies: List[Stage] = List(ExplicitNamedVars)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)

  private def phase2(phase1DB: DB)(using CompilerOptions): DB =
    given MemberGetSet = phase1DB.getSet
    given RefGen = RefGen.fromGetSet
    val wrapPatches: List[(DFMember, Patch)] = phase1DB.members.view.flatMap {
      case ch: DFConditional.Header
          if ch.dfType == DFUnit && ch.isInEDDomain && !ch.isInProcess
            && !ch.getOwnerBlock.isInstanceOf[DFConditional.Block] =>
        val chain = phase1DB.conditionalChainTable(ch)
        val chainBlocksAndMembers: List[DFMember] =
          chain.flatMap(cb => cb :: cb.members(MemberView.Flattened))
        val allToWrap: List[DFMember] = ch :: chainBlocksAndMembers
        val owner = ch.getOwnerBlock
        val wrapDsn = new MetaDesign(
          ch,
          Patch.Add.Config.Before,
          domainType = ED
        ):
          process(all):
            plantMembers(owner, allToWrap)
        val removals = allToWrap.map(_ -> Patch.Remove(isMoved = true))
        wrapDsn.patch :: removals
      case _ => None
    }.toList
    phase1DB.patch(wrapPatches)
  end phase2

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    extension (ch: DFConditional.Header)
      // recursive call to patch conditional block chains
      private def patchChains(headerVar: DFVal, op: DFNet.Op): List[(DFMember, Patch)] =
        // changing type of header to unit, since the expression is now a statement
        val headerPatch = ch -> Patch.Replace(
          ch.updateDFType(DFUnit),
          Patch.Replace.Config.FullReplacement
        )
        val cbChain = getSet.designDB.conditionalChainTable(ch)
        val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
        headerPatch :: lastMembers.flatMap {
          case ident @ Ident(underlying: DFConditional.Header) =>
            ident -> Patch.Remove() :: underlying.patchChains(headerVar, op)
          case ident @ Ident(underlying) =>
            val assignDsn = new MetaDesign(
              ident,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
            ):
              op.runtimeChecked match
                case DFNet.Op.Assignment =>
                  headerVar.asVarAny.:=(underlying.asValAny)(using
                    dfc.setMetaAnon(ident.meta.position)
                  )
                case DFNet.Op.NBAssignment =>
                  import dfhdl.core.nbassign
                  headerVar.asVarAny.nbassign(underlying.asValAny)(using
                    dfc.setMetaAnon(ident.meta.position)
                  )
            Some(assignDsn.patch)
          case _ => ??? // not possible
        }
      end patchChains
      private def patchChainsNet(
          headerVar: DFVal,
          net: DFNet,
          op: DFNet.Op
      ): List[(DFMember, Patch)] =
        val removeNetPatch = net -> Patch.Remove()
        removeNetPatch :: ch.patchChains(headerVar, op)
    end extension
    // Phase 1: transform conditional expressions to statements
    val patches = subDB.members.view
      // collect all the assignments from anonymous conditionals
      .flatMap {
        case net @ DFNet.Assignment(toVal, header: DFConditional.Header) if header.isAnonymous =>
          header.patchChainsNet(toVal, net, net.op)
        case net @ DFNet.Connection(toVal: DFVal, header: DFConditional.Header, _)
            if !net.isViaConnection && header.isAnonymous && (toVal.isPortOutPBNS || toVal.isVar) =>
          header.patchChainsNet(toVal, net, DFNet.Op.Assignment)
        case _ => Nil
      }.toList
    phase2(subDB.patch(patches))
  end transformSubDB
end ExplicitCondExprAssign

extension [T: HasDB](t: T)
  def explicitCondExprAssign(using CompilerOptions): DB =
    StageRunner.run(ExplicitCondExprAssign)(t.db)
