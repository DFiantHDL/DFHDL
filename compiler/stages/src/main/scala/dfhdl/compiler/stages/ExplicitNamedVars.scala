package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.options.CompilerOptions

/** This stage turns all named values to variables that get assigned. As a result, conditional
  * expressions (if/match) are converted to statements.
  *
  * Additionally, this stage creates explicit register and variable declarations for named value
  * expressions under RT process blocks. The named expression becomes a register when the
  * declaration and usage are in different steps. Otherwise, it is a variable declaration. If the
  * named expression is accessed both in the same step and in another step, then both variable and
  * register declarations are created. Examples:
  * {{{
  *   val x = UInt(16) <> IN
  *   val y = UInt(16) <> OUT.REG init 0
  *   process:
  *     def S0: Step =
  *       val v = x // becomes a variable declaration
  *       y.din := v
  *       NextStep
  *     val vr = x // becomes a register declaration
  *     def S1: Step =
  *       y.din := vr + 1
  *       NextStep
  * }}}
  */
case object ExplicitNamedVars extends HierarchyStage:
  def dependencies: List[Stage] = List(NamedAnonCondExpr)
  def nullifies: Set[Stage] = Set(DropLocalDcls)
  // `getRegOrVarDeps` -> `getReadDeps` -> `connectionTable.getValAccess`
  // calls `getOwnerDesign` on Dcls that may live in a sibling/child sub-DB,
  // so we need the outer flat-DB getSet.
  override def rebindGetSet: Boolean = false

  object WhenHeader extends Patch.Replace.RefFilter:
    def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] =
      refs.filter {
        case r: DFRef.TwoWayAny =>
          r.get match
            case header: DFConditional.Header =>
              header.originMembers.view
                .collect { case b: DFConditional.Block => b }
                .exists(_.getRefs.exists(_ equals r))
            case _ => false
        case _ => false
      }

  extension (ch: DFConditional.Header)
    // recursive call to patch conditional block chains
    private def patchChains(headerVar: DFVal)(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
      val cbChain = getSet.designDB.conditionalChainTable(ch)
      val lastMembers = cbChain.map(_.members(MemberView.Folded).last)
      lastMembers.flatMap {
        case Ident(underlying: DFConditional.Header) =>
          underlying.patchChains(headerVar)
        case m @ Ident(underlying) =>
          val assignDsn = new MetaDesign(
            m,
            Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
          ):
            headerVar.asVarAny.:=(underlying.asValAny)(using dfc.setMetaAnon(m.meta.position))
          Some(assignDsn.patch)
        case _ => ??? // not possible
      }
  end extension

  extension (member: DFMember)
    def getProcessOrStepBlockOwner(using MemberGetSet): Option[ProcessBlock | StepBlock] =
      @scala.annotation.tailrec
      def recur(current: DFMember): Option[ProcessBlock | StepBlock] = current match
        case pb: ProcessBlock => Some(pb)
        case sb: StepBlock    => Some(sb)
        case _: DFDesignBlock => None
        case m                => recur(m.getOwner)
      recur(member.getOwner)
  extension (dfVal: DFVal)
    def getRegOrVarDeps(using MemberGetSet): (regs: Set[DFValReadDep], vars: Set[DFValReadDep]) =
      val ownerOpt = dfVal.getProcessOrStepBlockOwner
      ownerOpt match
        case Some(owner) =>
          dfVal.getReadDeps.partition { _.getProcessOrStepBlockOwner.get != owner }
        case None => (regs = Set(), vars = dfVal.getReadDeps)
  end extension

  def transformSubDB(subDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patches = subDB.members.view
        // just named values
        .collect { case dv: DFVal if !dv.isAnonymous => dv }
        .flatMap {
          // ignoring port and variable declarations
          case _: DFVal.Dcl => None
          // ignoring constant declarations (named constants or derived constants)
          case DclConst() => None
          // all other named values
          case named =>
            // anonymized value
            val anonValueIR = named match
              // named ident requires removal of the ident itself
              case Ident(value) => value
              // removing name and type from header
              case mh: DFConditional.DFMatchHeader => mh.copy(dfType = DFUnit).anonymize
              // removing name and type from header
              case ih: DFConditional.DFIfHeader => ih.copy(dfType = DFUnit).anonymize
              case _                            => named.anonymize
            val readDeps = named.getRegOrVarDeps
            // println(s"regs: ${readDeps.regs}")
            // println(s"vars: ${readDeps.vars}")
            val regUse = readDeps.regs.nonEmpty
            // var is also used if there are no dependencies at all
            val varUse = readDeps.vars.nonEmpty || readDeps.regs.isEmpty && readDeps.vars.isEmpty
            val dsn = new MetaDesign(
              named,
              Patch.Add.Config.Before,
              dfhdl.core.DomainType.RT
            ):
              val regDFC = dfc.setMeta(named.meta)
              lazy val varDFC = if (regUse) regDFC.setName(s"${named.getName}_din") else regDFC
              lazy val plantedNewVar = named.asValAny.dfType.<>(VAR)(using varDFC)
              if (varUse) plantedNewVar // touch to trigger the lazy val
              lazy val plantedNewReg = named.asValAny.dfType.<>(VAR.REG)(using regDFC)
              if (regUse) plantedNewReg // touch to trigger the lazy val
              val anonValue = named match
                case Ident(_) => anonValueIR.asValAny
                case _        => plantMember(anonValueIR).asValAny
              named match
                case _: DFConditional.Header => // do nothing
                case _                       =>
                  if (varUse && regUse)
                    plantedNewVar := anonValue
                    plantedNewReg.din := plantedNewVar
                  else if (varUse)
                    if (named.isInEDDomain && !named.isInProcess)
                      plantedNewVar <> anonValue
                    else
                      plantedNewVar := anonValue
                  else if (regUse)
                    plantedNewReg.din := anonValue
            val varPatchOpt =
              if (varUse)
                Some(named ->
                  Patch.Replace(
                    dsn.plantedNewVar.asIR,
                    Patch.Replace.Config.ChangeRefOnly,
                    Patch.Replace.RefFilter.OfMembers(readDeps.vars.asInstanceOf[Set[DFMember]])
                  ))
              else None
            val regPatchOpt =
              if (regUse)
                Some(named ->
                  Patch.Replace(
                    dsn.plantedNewReg.asIR,
                    Patch.Replace.Config.ChangeRefOnly,
                    Patch.Replace.RefFilter.OfMembers(readDeps.regs.asInstanceOf[Set[DFMember]])
                  ))
              else None
            // final named value handling according to the specialized use-cases
            val finalizePatches = named match
              // only ident values are removed completely, along with their references
              case Ident(_)                 => Some(named -> Patch.Remove())
              case ch: DFConditional.Header =>
                val headerVar = if (varUse) dsn.plantedNewVar.asIR else dsn.plantedNewReg.asIR
                // replacing all the references of header as a conditional header
                val headerPatch = ch -> Patch.Replace(
                  anonValueIR,
                  Patch.Replace.Config.ChangeRefOnly,
                  WhenHeader
                )
                // named header removal while preserving the references
                ch -> Patch.Remove(true) ::
                  // setting the conditional blocks to reference the new anonymous header
                  headerPatch ::
                  // patching chains
                  ch.patchChains(headerVar)
              // remove the member, but preserve the references
              case _ => Some(named -> Patch.Remove(isMoved = true))
            Iterable(
              Some(dsn.patch),
              varPatchOpt,
              regPatchOpt,
              finalizePatches
            ).flatten
      }
      .toList
    subDB.patch(patches)
  end transformSubDB
end ExplicitNamedVars

extension [T: HasDB](t: T)
  def explicitNamedVars(using CompilerOptions): DB =
    StageRunner.run(ExplicitNamedVars)(t.db)
