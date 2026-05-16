package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import DFVal.Func.Op as FuncOp
import scala.collection.mutable
//format: off
/** This stage drops all RT waits (and loops depending on them) and replaces them with explicit step `def`s.
 *  rules:
 *  1. Single cycle wait (`1.cy.wait`) is replaced with:
 *     ```scala
 *     def S_N: Step = 
 *       NextStep
 *     end S_N
 *     ```
 *     where N is the step number.
 *  2. Multiple single cycle waits are replaced with sequential step definitions (S_0, S_1, S_2, ...)
 *     ```scala
 *     def S_0: Step =
 *       NextStep
 *     end S_0
 *     def S_1: Step =
 *       NextStep
 *     end S_1
 *     def S_2: Step =
 *       NextStep
 *     end S_2
 *     ```
 *     where 0, 1, 2 are the step numbers.
 *  3. While loop `while (condition) { body }` is replaced with:
 *     ```scala
 *     def S_N: Step = 
 *       if (condition)
 *         body
 *         ThisStep
 *       else
 *         NextStep
 *       end if
 *     end S_N
 *     ```
 *     where N is the step number.
 *  4. While loops that are tagged with `FALL_THROUGH` are replaced with:
 *     ```scala
 *     def S_N: Step =
 *       def fallThrough = !condition
 *       if (condition)
 *         body
 *         ThisStep
 *       else
 *         NextStep
 *       end if
 *     end S_N
 *     ```
 *     where N is the step number.
 *  5. While loops with nested waits: waits inside while loops become step definitions with nested naming
 *     (S_0_0, S_0_1, etc.), and the while loop itself becomes a step definition (S_0)
 *  6. If the process does not start with a step, a wait statement or while loop, we add an empty step definition for the first 
 *     step (with a NextStep return value) before the first member. Internally, there could be anonymous value
 *     members before the step/while/wait members, but these are ignored for the check of what the first member is.
 *     For example:
 *     ```scala
 *     process:
 *       x.din := 0
 *     end process
 *     ```
 *     will be transformed to:
 *     ```scala
 *     process:
 *       def S_0: Step =
 *         NextStep
 *       end S_0
 *       x.din := 0
 *     end process
 *     ```
 *  7. The user can define explicit naming by setting a value name for a wait statement or a while block,
 *     or by defining a step block. Nested steps are named by appending the parent step name and an underscore,
 *     unless the nested step already includes the parent step name with an underscore. 
 *     Unnamed steps are enumerated with the parent step name and an underscore. The enumeration starts from 0,
 *     and increments while counting the named steps as well.
 *     For example:
 *     ```scala
 *     process:
 *       def MyStep: Step =
 *         1.cy.wait //no name, so enumerate as MyStep_0
 *         val MyWait = 1.cy.wait //named, so use as MyStep_MyWait
 *         1.cy.wait //no name, so enumerate as MyStep_2
 *         def Internal: Step = //nested step, so change name to MyStep_Internal
 *           NextStep
 *         end Internal
 *         def MyStep_Internal2: Step = //nested step, but already includes the parent step name with an underscore,
 *           NextStep                   //so use as MyStep_Internal2
 *         end MyStep_Internal2
 *         NextStep
 *       end MyStep
 *       x.din := y
 *       val MyStepB = 1.cy.wait //named, so use as MyStepB
 *       x.din := 1
 *       1.cy.wait //no name, so enumerate as MyStep_2
 *       val MyWhile = while (y) //named, so use as MyWhile
 *         x.din := 1
 *         def GoGo: Step = //nested step, so change name to MyWhile_GoGo
 *           NextStep
 *         end GoGo
 *       end MyWhile
 *       x.din := 1
 *     end process
 *     ```
 *     will be transformed to:
 *     ```scala
 *     process:
 *       def MyStep: Step =
 *         def MyStep_0: Step =
 *           NextStep
 *         end MyStep_0
 *         def MyStep_MyWait: Step =
 *           NextStep
 *         end MyStep_MyWait
 *         def MyStep_2: Step =
 *           NextStep
 *         end MyStep_2
 *         def MyStep_Internal: Step =
 *           NextStep
 *         end MyStep_Internal
 *         def MyStep_Internal2: Step =
 *           NextStep
 *         end MyStep_Internal2
 *         NextStep
 *       end MyStep
 *       x.din := y
 *       def MyStepB: Step =
 *         NextStep
 *       end MyStepB
 *       x.din := 1
 *       def S_2: Step = 
 *         NextStep
 *       end S_2
 *       def MyWhile: Step =
 *         if (y)
 *           x.din := 1
 *           def MyWhile_GoGo: Step =
 *             NextStep
 *           end MyWhile_GoGo
 *           ThisStep
 *         else
 *           NextStep
 *         end if
 *       end MyWhile
 *       x.din := 1
 *     end process
 *     ```
 */
//format: on
case object DropRTWaits extends HierarchyStage:
  def dependencies: List[Stage] = List(DropTimedRTWaits, SimplifyRTOps)
  def nullifies: Set[Stage] = Set()

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patches = subDB.members.view.collect {
      // each process block has its own step enumeration
      case pb: ProcessBlock if pb.isInRTDomain =>
        val pbMembers = pb.members(MemberView.Flattened)
        // Rule 7: step scope (prefix, counter).
        case class StepScope(prefix: String, counter: Int)
        var stepNameStack = List(StepScope("", 0))
        // for nested while loops, we need to keep track of the exit members.
        // an exit member may have multiple patches that need to be applied in the LIFO order they are stacked.
        var exitMemberPatches = mutable.Map.empty[DFMember, List[Patch]]
        // increment the counter for the current (top) step scope
        def nextStepBlock(): Unit =
          val h = stepNameStack.head
          stepNameStack = StepScope(h.prefix, h.counter + 1) :: stepNameStack.tail
        // entering a step block (e.g. while body) starts a deeper nesting level and memoizes an exit member patch
        def enterStepBlock(stepMember: DFMember, exitMember: DFMember, patch: Option[Patch]): Unit =
          // stacking the patches for the exit member
          exitMemberPatches.get(exitMember) match
            case Some(patches) => exitMemberPatches += exitMember -> (patch.toList ++ patches)
            case None          => exitMemberPatches += exitMember -> patch.toList
          stepNameStack = StepScope(getStepName(stepMember), 0) :: stepNameStack
        // checking if the step block has an exit member and returning the patches that need to be applied.
        def checkAndExitStepBlock(lastMember: DFMember): List[(DFMember, Patch)] =
          exitMemberPatches.get(lastMember) match
            case Some(patches) =>
              stepNameStack = stepNameStack.tail
              nextStepBlock()
              patches.map(lastMember -> _)
            case None => Nil

        def getStepName(stepMember: DFMember): String =
          val prefix = stepNameStack.head.prefix
          stepMember.meta.nameOpt match
            case Some(name) =>
              if (prefix.isEmpty) name
              else if (name.startsWith(prefix + "_")) name
              else s"${prefix}_${name}"
            case None =>
              if (prefix.isEmpty) s"S_${stepNameStack.head.counter}"
              else s"${prefix}_${stepNameStack.head.counter}"

        // Rule 6: If process does not start with step, wait, or while, add empty S_0 before first member
        val needsInitialStep =
          pbMembers
            .dropWhile {
              case v: DFVal => v.isAnonymous
              case _        => false
            }.headOption match
            case None                          => true
            case Some(_: Wait)                 => false
            case Some(wb: DFLoop.DFWhileBlock) => wb.isCombinational
            case Some(_: StepBlock)            => false
            case _                             => true

        val initialStepPatches = if needsInitialStep then
          val stepName = "S_0"
          nextStepBlock()
          val dsn = new MetaDesign(pb, Patch.Add.Config.InsideFirst):
            import dfhdl.core.StepBlock
            val step = StepBlock.forced(using dfc.setName(stepName))
            dfc.enterOwner(step)
            NextStep
            dfc.exitOwner()
          List(dsn.patch)
        else Nil

        // transforming the process block members.
        // all members except the while loops can be an exit member, and need to be handled with `checkAndExitStepBlock`.
        // waits and while loops are handled specially.
        initialStepPatches ++ pbMembers.flatMap {
          // transform a wait statement into a step block (assuming the wait is a single cycle wait, due to previous stages)
          case wait: Wait =>
            val stepName = getStepName(wait)
            nextStepBlock()
            val dsn = new MetaDesign(
              wait,
              Patch.Add.Config.ReplaceWithFirst(Patch.Replace.Config.FullReplacement)
            ):
              import dfhdl.core.StepBlock
              val step = StepBlock.forced(using dfc.setName(stepName))
              dfc.enterOwner(step)
              NextStep
              dfc.exitOwner()
            dsn.patch :: checkAndExitStepBlock(wait)
          // transform a while loop into a step block.
          case wb: DFLoop.DFWhileBlock if !wb.isCombinational =>
            val stepName = getStepName(wb)
            wb.getVeryLastMember match
              // Empty loop body: generate the complete step+if/else structure in one shot.
              case None =>
                nextStepBlock()
                val dsn = new MetaDesign(
                  wb,
                  Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
                ):
                  import dfhdl.core.{StepBlock, DFIf, DFBool, DFUnit}
                  val step = StepBlock.forced(using dfc.setName(stepName))
                  dfc.enterOwner(step)
                  val wbGuard = wb.guardRef.get
                  val cond = wbGuard.asValOf[DFBool]
                  val ifBlock = DFIf.Block(Some(cond), DFIf.Header(DFUnit))
                  // Stay inside `step` while building the if/else structure so that both
                  // the if-true-branch and the else-branch are owned by `step`, not by the
                  // enclosing ProcessBlock.  (Previously `dfc.exitOwner()` was called here,
                  // which caused elseBlock and its Goto(NextStep) to be created at process
                  // level, breaking getOwnerStepBlock in FlattenStepBlocks.)
                  dfc.enterOwner(ifBlock)
                  ThisStep
                  dfc.exitOwner()
                  val elseBlock = DFIf.Block(None, ifBlock)
                  dfc.enterOwner(elseBlock)
                  NextStep
                  dfc.exitOwner()
                  dfc.exitOwner()
                Some(dsn.patch)
              // Non-empty loop body: the last member of the while loop is the exit member.
              case Some(lastLoopMember) =>
                // creating the if part of the while loop step block.
                val stepAndIfDsn = new MetaDesign(
                  wb,
                  Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
                ):
                  import dfhdl.core.{StepBlock, DFIf, DFBool, DFUnit}
                  val step = StepBlock.forced(using dfc.setName(stepName))
                  dfc.enterOwner(step)
                  val wbGuard = wb.guardRef.get
                  if (wb.isFallThrough)
                    val fallThrough = StepBlock.forced(using dfc.setName("fallThrough"))
                    dfc.enterOwner(fallThrough)
                    val clonedCond = !wbGuard.cloneAnonValueAndDepsHere.asValOf[DFBool]
                    dfhdl.core.DFVal.Alias.AsIs.ident(clonedCond)(using dfc.anonymize)
                    dfc.exitOwner()
                  end if
                  val cond = wbGuard.asValOf[DFBool]
                  val ifBlock = DFIf.Block(Some(cond), DFIf.Header(DFUnit))
                  dfc.exitOwner()
                // creating the else part of the while loop step block, to be applied when the while loop exits.
                val elseDsn = new MetaDesign(
                  lastLoopMember,
                  Patch.Add.Config.After
                ):
                  import dfhdl.core.DFIf
                  dfc.enterOwner(stepAndIfDsn.ifBlock)
                  ThisStep
                  dfc.exitOwner()
                  // Enter `step` explicitly so that elseBlock is owned by `step` (sibling of
                  // ifBlock), not by `ifBlock`. Without this, FullReplacement(wb → ifBlock)
                  // would redirect elseBlock's ownerRef to ifBlock, nesting it inside the
                  // if-true branch instead of at the same level.
                  dfc.enterOwner(stepAndIfDsn.step)
                  val elseBlock = DFIf.Block(None, stepAndIfDsn.ifBlock)
                  dfc.enterOwner(elseBlock)
                  NextStep
                  dfc.exitOwner()
                  dfc.exitOwner()
                enterStepBlock(wb, lastLoopMember, Some(elseDsn.patch._2))
                Some(stepAndIfDsn.patch)
            end match
          // onEntry/onExit/fallThrough blocks must NOT be renamed: DropRTProcess identifies them
          // by exact names "onEntry"/"onExit"/"fallThrough" via isOnEntry/isOnExit/isFallThrough.
          // They also must NOT affect the step-name counter of their enclosing scope.
          case stepBlock: StepBlock if !stepBlock.isRegular => None
          case stepBlock: StepBlock                         =>
            val stepName = getStepName(stepBlock)
            val lastStepBlockMember = stepBlock.getVeryLastMember.get
            enterStepBlock(stepBlock, lastStepBlockMember, None)
            if (stepBlock.getName != stepName)
              Some(
                stepBlock -> Patch.Replace(
                  stepBlock.setName(stepName),
                  Patch.Replace.Config.FullReplacement
                )
              )
            else None
          case member => checkAndExitStepBlock(member)
        }
    }.flatten.toList
    subDB.patch(patches)
  end transformSubDB
end DropRTWaits

extension [T: HasDB](t: T)
  def dropRTWaits(using CompilerOptions): DB =
    StageRunner.run(DropRTWaits)(t.db)
