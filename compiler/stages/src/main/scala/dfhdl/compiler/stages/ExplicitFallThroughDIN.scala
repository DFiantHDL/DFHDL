package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import scala.collection.mutable

//format: off
/** This stage makes explicit what a `fallThrough` condition already means: every register it reads
  * is read as `.din`, the pending next-cycle value.
  *
  * ==Rule 1: register reads in a `fallThrough` block become `.din` reads==
  *
  * A `fallThrough` condition is a decision taken on the transition *into* its step, in the very
  * cycle in which entering that step already assigns registers: an entered step runs its `onEntry`
  * block, a `FALL_THROUGH` for-loop resets the iterator [[SimplifyRTOps]] gave it, and a
  * forever-process wrap-around re-runs the prologue. Reading the registered values there decides
  * the skip on the values the entering state is about to replace, which is a cycle behind what the
  * condition names.
  * {{{
  * // Before
  * def Armed: Step =
  *   def onEntry =
  *     armed.din := x
  *   def fallThrough =
  *     !armed
  *   end fallThrough
  *   NextStep
  * end Armed
  *
  * // After
  * def Armed: Step =
  *   def onEntry =
  *     armed.din := x
  *   def fallThrough =
  *     !armed.din
  *   end fallThrough
  *   NextStep
  * end Armed
  * }}}
  *
  * The rule is uniform over both spellings of the construct. A user writes the condition directly;
  * a `FALL_THROUGH` loop has none to write, since [[DropRTWaits]] synthesizes its `fallThrough`
  * block as the negated loop guard. Running here rather than in [[DropRTWaits]] is what makes that
  * uniformity possible: by this point both forms exist, in one shape.
  *
  * The block is rebuilt rather than edited in place, so a named intermediate inside it is rewritten
  * along with the condition proper:
  * {{{
  * // Before
  * def fallThrough =
  *   val edge = x && !armed
  *   edge || armed
  * end fallThrough
  *
  * // After
  * def fallThrough =
  *   val edge = x && !armed.din
  *   edge || armed.din
  * end fallThrough
  * }}}
  *
  * A register read reached through a partial selection is wrapped at its outermost point, so the
  * result is the canonical `r(3, 0).din` and not the unspellable `r.din(3, 0)`.
  */
//format: on
case object ExplicitFallThroughDIN extends HierarchyStage:
  def dependencies: List[Stage] = List(DropRTWaits)
  def nullifies: Set[Stage] = Set()

  /** A read whose root declaration is a register: the declaration itself, or a partial selection
    * chain into it. `Alias.RegDIN` is an `Alias.Consumer` rather than an `Alias.Partial`, so an
    * already-rewritten read is excluded here and the rewrite is a fix-point.
    */
  private def regRootedRead(v: DFVal)(using MemberGetSet): Boolean = v match
    case dcl: DFVal.Dcl         => dcl.modifier.isReg
    case p: DFVal.Alias.Partial => regRootedRead(p.relValRef.get)
    case _                      => false

  private def readsRegister(m: DFMember)(using MemberGetSet): Boolean =
    m.getRefs.exists { ref =>
      ref.get match
        case t: DFVal => regRootedRead(t)
        case _        => false
    }

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patches = subDB.members.view.collect {
      case ft: StepBlock if ft.isFallThrough =>
        val ftMembers = ft.members(MemberView.Flattened)
        // the block's last member is its condition marker, which `DropRTProcess` and
        // `FirstStepFusion` both read positionally: the rebuild must keep it last
        ftMembers.lastOption match
          case Some(marker) if ftMembers.exists(readsRegister) =>
            val dsn = new MetaDesign(ft, Patch.Add.Config.InsideFirst, dfhdl.core.DomainType.RT):
              // the clones' references are registered in this meta design's DB, which also
              // resolves through to the stage's DB for everything they read
              given MemberGetSet = dfc.getSet
              val clonedOf = mutable.Map.empty[ir.DFMember, ir.DFMember]
              // one per reading reference, never shared: an anonymous value may only be read once
              def dinRead(v: ir.DFVal): ir.DFVal =
                dfhdl.core.DFVal.Alias.RegDIN(v.asValAny)(using dfc.anonymize).asIR
              ftMembers.foreach { m =>
                // a partial selection into a register is a link in the read's chain, not a reader
                // of it: the wrap belongs at the outermost point, which is its own consumer. The
                // marker is a reader even though it aliases the condition.
                val isChainLink = m != marker &&
                  (m match
                    case v: ir.DFVal => regRootedRead(v)
                    case _           => false)
                // resolved before the clone is added, so a `.din` read is emitted ahead of the
                // member that reads it
                val targets = m.getRefs.map { ref =>
                  ref.get match
                    case t: ir.DFVal =>
                      val resolved = clonedOf.getOrElse(t, t).asInstanceOf[ir.DFVal]
                      if (!isChainLink && regRootedRead(resolved)) dinRead(resolved) else resolved
                    case other => other
                }
                val cloned = m.copyWithNewRefs
                dfc.mutableDB.addMember(cloned)
                dfc.mutableDB.newRefFor(cloned.ownerRef, dfc.owner.asIR)
                cloned.getRefs.lazyZip(targets).foreach(dfc.mutableDB.newRefFor(_, _))
                clonedOf += m -> cloned
              }
            // the rebuilt block supersedes every member it was given
            dsn.patch :: ftMembers.map(_ -> Patch.Remove())
          case _ => Nil
        end match
    }.flatten.toList
    subDB.patch(patches)
  end transformSubDB
end ExplicitFallThroughDIN

extension [T: HasDB](t: T)
  def explicitFallThroughDIN(using CompilerOptions): DB =
    StageRunner.run(ExplicitFallThroughDIN)(t.db)
