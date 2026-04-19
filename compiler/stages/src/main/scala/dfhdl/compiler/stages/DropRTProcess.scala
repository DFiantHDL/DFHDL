package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
import scala.collection.immutable.ListMap
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFValAny, DFOwnerAny, asValAny, DFC}
//format: off
/** This stage drops RT processes by creating an explicit enumerated state machine.
  *
  * Each RT `process` block containing `StepBlock` definitions (produced by the preceding
  * `FlattenStepBlocks` stage) is replaced by:
  *   - An `enum` type whose entries correspond 1-to-1 with the step names.
  *   - A `VAR.REG` state register of that enum type, initialised to the first step.
  *   - A `match` expression that dispatches on the current state value and contains one
  *     case per step.
  *
  * All `Goto` members are already explicit `StepBlock` references at this point
  * (relative forms — `NextStep`, `ThisStep`, `FirstStep` — were resolved by
  * `FlattenStepBlocks`). Each `Goto` is replaced by an assignment to `stateReg.din`.
  *
  * ==Rules==
  *
  * ===Rule 1: Basic FSM transformation===
  * Each step becomes a match case; each `Goto` inside it becomes `stateReg.din := EnumEntry`.
  * {{{
  * // Before
  * process:
  *   def S0: Step =
  *     y.din := 0
  *     if (x) S1 else S0
  *   def S1: Step =
  *     y.din := 1
  *     if (x) S2 else S0
  *   def S2: Step =
  *     y.din := 0
  *     if (x) S2 else S0
  *
  * // After
  * enum State(...) extends Encoded.Manual(2):
  *   case S0 extends State(d"2'0")
  *   case S1 extends State(d"2'1")
  *   case S2 extends State(d"2'2")
  * val state = State <> VAR.REG init State.S0
  * state match
  *   case State.S0 =>
  *     y.din := 0
  *     if (x) state.din := State.S1
  *     else state.din := State.S0
  *   case State.S1 =>
  *     y.din := 1
  *     if (x) state.din := State.S2
  *     else state.din := State.S0
  *   case State.S2 =>
  *     y.din := 0
  *     if (x) state.din := State.S2
  *     else state.din := State.S0
  * end match
  * }}}
  *
  * ===Rule 2: `onEntry` block inlining===
  * If a step `S` has an `onEntry` child block, its body is inlined at every site that
  * transitions *into* `S` from a *different* step (i.e., wherever a `Goto` targets `S`
  * and the source step differs from `S`), immediately before the `stateReg.din` assignment.
  * The `onEntry` block itself is removed from the case body. Self-transitions (`S -> S`)
  * do NOT trigger `onEntry`.
  * {{{
  * // Before
  * def S0: Step =
  *   if (x) S1 else S0
  * def S1: Step =
  *   def onEntry =
  *     y.din := 1   // run only when entering S1 from a different step
  *   if (x) S1 else S0  // self-transition: onEntry NOT fired
  *
  * // After
  * case State.S0 =>
  *   if (x)
  *     y.din := 1           // S1.onEntry inlined (S0 -> S1: different step)
  *     state.din := State.S1
  *   else state.din := State.S0
  * case State.S1 =>
  *   if (x) state.din := State.S1   // S1 -> S1: onEntry NOT inlined
  *   else state.din := State.S0
  * }}}
  *
  * ===Rule 3: `onExit` block inlining===
  * If a step `S` has an `onExit` child block, its body is inlined at every site that
  * transitions *out of* `S` to a *different* step, immediately before any `onEntry`
  * inlining and the `stateReg.din` assignment. Self-transitions do not trigger `onExit`.
  * {{{
  * // Before
  * def S2: Step =
  *   def onExit =
  *     y.din := 0   // run whenever we leave S2 to another step
  *   if (x) S2 else S0
  *
  * // After
  * case State.S2 =>
  *   if (x) state.din := State.S2        // self-loop: onExit NOT triggered
  *   else
  *     y.din := 0                        // S2.onExit inlined here (transition to S0)
  *     state.din := State.S0
  *   end if
  * }}}
  *
  * ===Rule 4: `fallThrough` block — conditional cascading===
  * If a step `S` has a `fallThrough` child block, it defines a condition under which
  * control *immediately falls through* to the next step in the same clock cycle (without
  * waiting). When transitioning into `S`:
  *   1. Inline `S.onEntry` (if any) and assign `stateReg.din := S`.
  *   2. Emit a conditional `if (fallThroughCondition)` block.
  *   3. Inside that block, recursively apply steps 1–2 for the next step (the one `S`
  *      itself eventually transitions to via its own `Goto`).
  * The `fallThrough` condition represents the *exit* predicate — when true, control passes
  * to the subsequent step without registering in `S`.
  * {{{
  * // Before
  * def S0: Step =
  *   y.din := 0
  *   S1
  * def S1: Step =
  *   def fallThrough = x          // if x, skip straight to S2
  *   def onEntry = y.din := 1
  *   S2
  * def S2: Step =
  *   def fallThrough = !x         // if !x, skip straight to S3
  *   def onEntry = y.din := !y
  *   S3
  *
  * // After (case for S0)
  * case State.S0 =>
  *   y.din := 0
  *   y.din := 1                   // S1.onEntry
  *   state.din := State.S1
  *   if (x)                       // S1.fallThrough: skip to S2
  *     y.din := !y                // S2.onEntry
  *     state.din := State.S2
  *     if (!x)                    // S2.fallThrough: skip to S3
  *       ...
  *     end if
  *   end if
  * }}}
  *
  * ===Rule 5: Circular fall-through protection===
  * The recursive `fallThrough` chain stops as soon as the next step to handle equals the
  * step that contains the original `Goto`. This prevents infinite expansion when the fall-
  * through cycle loops back to the current case.
  * {{{
  * // Before
  * def S0: Step =
  *   def fallThrough = x
  *   def onEntry = y.din := y
  *   S1
  * def S1: Step =
  *   def fallThrough = !x
  *   def onEntry = y.din := !y
  *   S2
  * def S2: Step =
  *   def fallThrough = x ^ x.reg(1, init = 0)
  *   def onEntry = y.din := y ^ y.reg
  *   S0
  *
  * // After (case for S2)
  * case State.S2 =>
  *   y.din := y                   // S0.onEntry
  *   state.din := State.S0
  *   if (x)                       // S0.fallThrough: cascade to S1
  *     y.din := !y                // S1.onEntry
  *     state.din := State.S1
  *     if (!x)                    // S1.fallThrough: cascade to S2
  *       y.din := y ^ y.reg       // S2.onEntry
  *       state.din := State.S2
  *       // S2.fallThrough would cascade to S0, but S0 == currentStep => stop
  *     end if
  *   end if
  * }}}
  */
//format: on
case object DropRTProcess extends HierarchyStage:
  // rebind off: `getOwnerStepBlock` / `collectRelMembers` etc walk refs across designs.
  override def rebindGetSet: Boolean = false
  def dependencies: List[Stage] = List(FlattenStepBlocks)
  def nullifies: Set[Stage] = Set(DFHDLUniqueNames)

  def transformSubDB(subDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patchList = subDB.members.view.collect {
      case pb: ProcessBlock if pb.isInRTDomain =>
        val stateBlocks = pb.members(MemberView.Folded).collect {
          case sb: StepBlock if sb.isRegular =>
            sb
        }
        val gotos = pb.members(MemberView.Flattened).collect { case g: Goto => g }
        val pbPatch = pb -> Patch.Replace(pb.getOwner, Patch.Replace.Config.ChangeRefAndRemove)
        if (stateBlocks.isEmpty) List(pbPatch)
        else
          // assuming flat step blocks structure
          val nextBlocks = stateBlocks.lazyZip(stateBlocks.tail :+ stateBlocks.head).toMap
          val enumName = if (pb.isAnonymous) s"State" else s"${pb.getName}_State"
          val stateRegName = if (pb.isAnonymous) s"state" else s"${pb.getName}_state"
          val entries = ListMap.from(stateBlocks.view.zipWithIndex.map { case (sb, idx) =>
            sb.getName -> BigInt(idx)
          })
          val stateEnumIR = DFEnum(enumName, (stateBlocks.length - 1).bitsWidth(false), entries)
          type StateEnum = dfhdl.core.DFEnum[dfhdl.core.DFEncoding]
          val stateEnumFE = stateEnumIR.asFE[StateEnum]
          def enumEntry(value: BigInt)(using DFC) = dfhdl.core.DFVal.Const(stateEnumFE, Some(value))
          val dsn = new MetaDesign(
            pb,
            Patch.Add.Config.Before,
            dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
          ):
            val stateInit = dfhdl.core.DFVal.Const(stateEnumFE, Some(entries.head._2))
            val patterns = entries.map { case (_, value) =>
              dfhdl.core.DFMatch.Pattern.Singleton(enumEntry(value))
            }
            val stateReg = stateEnumFE.<>(VAR.REG).init(stateInit)(using dfc.setName(stateRegName))
            val header = dfhdl.core.DFMatch.Header(dfhdl.core.DFUnit, stateReg).asIR
          val stateReg = dsn.stateReg
          var prevBlockOrHeader: DFOwnerAny | DFValAny = dsn.header.asValAny
          val caseDsns = stateBlocks.lazyZip(dsn.patterns).map { (sb, pattern) =>
            new MetaDesign(
              sb,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
              dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
            ):
              val block = dfhdl.core.DFMatch.Block(pattern, None, prevBlockOrHeader)(using
                dfc.setMeta(sb.meta)
              )
              prevBlockOrHeader = block
          }
          val removedOnEntryExitFallThroughMembers = mutable.Set.empty[DFMember]
          val gotoDsns = gotos.map { g =>
            new MetaDesign(
              g,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
              dfhdl.core.DomainType.RT(dfhdl.core.RTDomainCfg.Derived)
            ):
              import dfhdl.core.{DFIf, DFUnit, DFBool}
              val currentStepBlock = g.getOwnerStepBlock
              // the stage `FlattenStepBlocks` guarantees that the goto always references a regular StepBlock
              val nextStepBlock: StepBlock = g.stepRef.get.asInstanceOf[StepBlock]
              def setState(nextStepBlock: StepBlock): Unit =
                stateReg.din.:=(enumEntry(entries(nextStepBlock.getName)))(using
                  dfc.setMeta(g.meta)
                )
              if (currentStepBlock != nextStepBlock)
                // add currentStepBlock-onExit members
                currentStepBlock.members(MemberView.Folded).collectFirst {
                  case onExit: StepBlock if onExit.isOnExit => onExit
                }.foreach { onExit =>
                  val onExitMembers = onExit.members(MemberView.Flattened)
                  plantClonedMembers(onExit, onExitMembers)
                  removedOnEntryExitFallThroughMembers += onExit
                  removedOnEntryExitFallThroughMembers ++= onExitMembers
                }
                def handleNextStep(nextStepBlock: StepBlock): Unit =
                  // onEntry members will always activated, even if we fall-through the next step block
                  val nextStepBlockMembers = nextStepBlock.members(MemberView.Flattened)
                  nextStepBlockMembers.collectFirst {
                    case onEntry: StepBlock if onEntry.isOnEntry => onEntry
                  }.foreach { onEntry =>
                    val onEntryMembers = onEntry.members(MemberView.Flattened)
                    plantClonedMembers(onEntry, onEntryMembers)
                    removedOnEntryExitFallThroughMembers += onEntry
                    removedOnEntryExitFallThroughMembers ++= onEntryMembers
                  }
                  setState(nextStepBlock)
                  // in case of circular fall-through, we stop
                  if (nextStepBlock != currentStepBlock)
                    val fallThroughCond = nextStepBlockMembers.collectFirst {
                      case fallThrough: StepBlock if fallThrough.isFallThrough =>
                        val fallThroughMembers = fallThrough.members(MemberView.Flattened)
                        val fallThroughDFC = dfc.setMeta(fallThrough.meta.anonymize)
                        removedOnEntryExitFallThroughMembers += fallThrough
                        removedOnEntryExitFallThroughMembers ++= fallThroughMembers
                        // planting all members except the last one (the ident of the condition)
                        val clonedMemberMap =
                          plantClonedMembers(fallThrough, fallThroughMembers.dropRight(1))
                        val Ident(origCond) = fallThroughMembers.last.runtimeChecked
                        val cond = clonedMemberMap.getOrElse(origCond, origCond).asInstanceOf[DFVal]
                        val ifBlock = DFIf.Block(
                          Some(cond.asValOf[DFBool]),
                          DFIf.Header(DFUnit)(using fallThroughDFC)
                        )(using fallThroughDFC)
                        dfc.enterOwner(ifBlock)
                        val fallThroughStepBlock = nextBlocks(nextStepBlock)
                        handleNextStep(fallThroughStepBlock)
                        dfc.exitOwner()
                    }
                  end if
                end handleNextStep
                handleNextStep(nextStepBlock)
              else
                setState(nextStepBlock)
              end if
          }
          Iterator(
            List(dsn.patch, pbPatch),
            removedOnEntryExitFallThroughMembers.map(_ -> Patch.Remove()),
            caseDsns.map(_.patch),
            gotoDsns.map(_.patch)
          ).flatten
        end if
    }.flatten.toList

    subDB.patch(patchList)
  end transformSubDB
end DropRTProcess

extension [T: HasDB](t: T)
  def dropRTProcess(using CompilerOptions): DB =
    StageRunner.run(DropRTProcess)(t.db)
