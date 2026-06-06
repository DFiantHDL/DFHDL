package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.internals.*
import scala.annotation.tailrec
//format: off
/** This stage removes the gratuitous clock cycle that every loop iteration costs in an RT process.
  *
  * == Background ==
  *
  * This stage runs on the '''nested''' step-block form produced by [[DropRTWaits]] — before
  * [[FlattenStepBlocks]] flattens it — where relative gotos (`ThisStep`/`NextStep`) are still in
  * place and a loop's control step '''directly owns''' its wait step as a nested child. A loop in
  * the source lowers to two kinds of steps:
  *   - A '''wait step''' — the innermost step whose terminus loops back to itself (`ThisStep` in the
  *     counting branch). It is a multi-cycle wait counter, or a single-cycle pass-through delay. It
  *     legitimately consumes ≥1 clock cycle and must stay.
  *   - A '''control step''' — the '''owner''' step that wraps a nested wait step together with the
  *     per-iteration loop bookkeeping (index update, wait-counter reset, condition test). Its
  *     dedicated FSM state burns one extra clock cycle per iteration that the source never asked for.
  *
  * Working on the nested form makes the control↔wait pairing trivial: the control step is the
  * '''owner''' and its wait is the '''nested child''' — no flat-goto reverse-engineering needed. The
  * fold dissolves the '''owner''' control step into its '''internal''' wait step.
  *
  * == The transform — entry-folding ==
  *
  * For each foldable control step `C` whose iteration enters wait step `W` (and `W`'s exit loops
  * back to `C`), the control step is dissolved into the wait:
  *
  *   1. '''Allocate a guard reg''' `C_entered : Bit <> VAR.REG init 1`, named after the control
  *      step. It is `1` on exactly the '''first cycle of each iteration''' and gates `C`'s one-shot
  *      work.
  *   2. '''Fold `C`'s index update into `W`'s first cycle''', guarded — prepend to `W`'s counting
  *      branch: `if (C_entered) { <C's index update>; C_entered.din := 0 }`.
  *   3. '''Keep `C`'s wait-counter reset + condition at `W`'s exit''' (where `W` decides continue
  *      vs. leave): `if (C.cond) { <reset W's counter>; C_entered.din := 1; -> W } else
  *      { <C's else-body> }`. The `C_entered.din := 1` re-arms the guard for the next iteration.
  *   4. '''Re-point `C`'s other predecessors''' (e.g. the loop preamble) directly to `W`, and
  *      '''remove `C`'''.
  *
  * The split matters: `C`'s '''index update''' (`i.din := i + 1`) folds into the wait's ''entry''
  * (guarded); `C`'s write to '''`W`'s own counter''' (`waitCnt.din := 0`) stays at the
  * ''exit-continue'' (a separate cycle), since putting both in one cycle would double-write the
  * counter. `W`'s counter is identifiable structurally (the reg `W` tests/increments); everything
  * else `C` writes is iteration work that folds at entry.
  *
  * '''Worked example (single loop).''' Nested form after [[DropRTWaits]] — control step `S_1` owns
  * its wait step `S_1_0`:
  * {{{
  * def S_1: Step =                          // control step (owner)
  *   if (i < 100)
  *     def S_1_0: Step =                     // wait step (nested child)
  *       if (waitCnt != 49)
  *         waitCnt.din := waitCnt + 1
  *         ThisStep
  *       else NextStep
  *     waitCnt.din := 0                      // reset W's counter
  *     i.din := i + 1                        // index update
  *     ThisStep
  *   else
  *     finish()
  *     NextStep
  * }}}
  * Entry-folded — `S_1` dissolved into `S_1_0` (now a direct process child), `i.din := i + 1`
  * guarded by `S_1_entered`, condition `i < 100` left exactly as written:
  * {{{
  * // + reg:  val S_1_entered = Bit <> VAR.REG init 1
  * def S_1_0: Step =
  *   if (waitCnt != 49)
  *     if (S_1_entered)                      // S_1's increment, folded at entry
  *       i.din := i + 1
  *       S_1_entered.din := 0
  *     waitCnt.din := waitCnt + 1
  *     ThisStep
  *   else if (i < 100)                       // S_1's condition -> continue (re-arm)
  *     waitCnt.din := 0
  *     S_1_entered.din := 1
  *     ThisStep
  *   else                                    // S_1's else
  *     finish()
  *     NextStep
  * }}}
  *
  * == Why a guard reg ==
  *
  * The guard is a general "first cycle of this iteration" marker that works for any wait (including
  * single-cycle/pass-through waits with no counter). It is the '''self-loop-surviving analog of an
  * `onEntry` action''': an `onEntry` block runs once on entry but is skipped on self-transitions
  * (see [[DropRTProcess]] Rule 2), and the folded wait self-loops, so an explicit reg is needed to
  * fire the folded work once per iteration. Folding the index update at the wait's '''entry'''
  * (rather than its exit) keeps each loop condition exactly as written — the increment (entry) and
  * the condition test (exit) land in different cycles, so there is no off-by-one and no condition
  * rewrite.
  *
  * == Nesting ==
  *
  * One guard per control level, named after that level's step. In the nested form the levels are
  * literally the ownership chain: outer control `S_1` owns inner control `S_1_0` owns the wait
  * `S_1_0_0`. For a 2-deep loop, the outer guard `S_1_entered` gates the outer index update
  * (`i.din := i + 1`) and the inner guard `S_1_0_entered` gates the inner one (`j.din := j + 1`);
  * both control steps fold into the single innermost wait `S_1_0_0`. The outer guard is re-armed when
  * a new outer pass begins (the inner loop exhausts and the outer continues); the inner guard is
  * re-armed on every inner continue. The rule is uniform for any nesting depth.
  *
  * == Precondition (hard rule) ==
  *
  * A control/wait pair is '''not foldable''' — left as-is — if either the control step `C` or its
  * wait `W` carries any non-regular child block (`onEntry`, `onExit`, or `fallThrough`). For
  * `fallThrough` the fold semantics aren't worked out yet; for `onEntry`/`onExit` no real input
  * actually places them on a foldable control step (a `while`-generated control step can't carry
  * them, and a hand-written step that does drops its nested wait), so folding such a step would be
  * dead code. An `onEntry`/`onExit` on an '''enclosing''' step (one that merely contains a foldable
  * inner loop) is neither `C` nor `W`, so it is '''preserved untouched''' — the fold only rewrites
  * the inner loop. This precondition can be relaxed in the future.
  *
  * == Placement ==
  *
  * The control↔wait pairing is read directly from the step '''ownership''' nesting, which is present
  * only before flattening. So this stage runs '''between [[DropRTWaits]] and [[FlattenStepBlocks]]''':
  * [[FlattenStepBlocks]] depends on this stage, which depends on [[DropRTWaits]] (plus the
  * [[ExplicitNamedVars]] / [[DropLocalDcls]] prerequisites that [[FlattenStepBlocks]] previously
  * required directly).
  */
//format: on
case object FoldControlSteps extends HierarchyStage:
  def dependencies: List[Stage] = List(DropRTWaits, ExplicitNamedVars, DropLocalDcls)
  def nullifies: Set[Stage] = Set()

  // One control level `C_m` of a (possibly nested) loop chain: its `if (cCond)` then-block, the loop
  // condition, the index-update net (writes the reg the condition tests — folds at entry) and the
  // remaining bookkeeping nets (reset of the level below — replayed on continue / exhaust).
  private case class Level(
      c: StepBlock,
      cThen: DFConditional.DFIfElseBlock,
      cCond: DFVal,
      idxNet: DFNet,
      resetNets: List[DFNet]
  )
  // A foldable control chain `C_1` ⊃ … ⊃ `C_k` ⊃ `W` (outer→inner), down to the leaf wait `W`.
  private case class Chain(
      levels: List[Level],
      w: StepBlock,
      wThen: DFConditional.DFIfElseBlock,
      wCond: DFVal,
      wCountNets: List[DFNet],
      c1Else: DFConditional.DFIfElseBlock // the outermost control step's else-body (the loop exit)
  )

  // direct (non-nested) gotos of a block, in order
  private def directGotos(block: DFOwner)(using MemberGetSet): List[Goto] =
    block.members(MemberView.Folded).collect { case g: Goto => g }
  private def lastGotoIs(block: DFOwner, target: DFMember)(using MemberGetSet): Boolean =
    directGotos(block).lastOption.exists(_.stepRef.get == target)
  private def hasNonRegularChild(s: StepBlock)(using MemberGetSet): Boolean =
    s.members(MemberView.Folded).exists { case sb: StepBlock => !sb.isRegular; case _ => false }
  private def directRegularSteps(block: DFOwner)(using MemberGetSet): List[StepBlock] =
    block.members(MemberView.Folded).collect { case sb: StepBlock if sb.isRegular => sb }
  private def singleIfHeader(s: StepBlock)(using MemberGetSet): Option[DFConditional.DFIfHeader] =
    s.members(MemberView.Folded).collect { case h: DFConditional.DFIfHeader => h } match
      case List(h) => Some(h)
      case _       => None
  // Forward conditional-block navigation by `prevBlockOrHeaderRef` over the step's own children.
  // (Deliberately avoids the `getFirstCB`/`getNextCB` analysis helpers, which build the whole-DB
  // `originMemberTable` — that trips over the orphaned `for`-loop `DFRange` still present at this
  // pipeline stage, which is only cleaned later by `DropUnreferenced`.)
  private def thenBlockOf(step: StepBlock, h: DFConditional.DFIfHeader)(
      using MemberGetSet
  ): DFConditional.DFIfElseBlock =
    step.members(MemberView.Folded).collectFirst {
      case b: DFConditional.DFIfElseBlock if b.prevBlockOrHeaderRef.get == h => b
    }.get
  private def elseBlockOf(step: StepBlock, thenB: DFConditional.DFIfElseBlock)(
      using MemberGetSet
  ): Option[DFConditional.DFIfElseBlock] =
    step.members(MemberView.Folded).collectFirst {
      case b: DFConditional.DFIfElseBlock if b.prevBlockOrHeaderRef.get == thenB => b
    }
  private def lhsDcl(n: DFNet)(using MemberGetSet): Option[DFVal.Dcl] =
    n.lhsRef.get match
      case v: DFVal => v.departialDcl.map(_._1)
      case _        => None
  // the declarations read by a (condition) value
  private def condDcls(cond: DFVal)(using MemberGetSet): Set[DFVal.Dcl] =
    (cond :: cond.collectRelMembers(false))
      .flatMap(_.getRefs.map(_.get))
      .collect { case d: DFVal.Dcl => d }
      .toSet

  // `W` is a leaf multi-cycle wait: one `if`, then-branch self-loops (ThisStep) with a counting
  // assignment and no nested step, else-branch exits (NextStep).
  private def isWaitStep(w: StepBlock)(using MemberGetSet): Boolean =
    w.isRegular && !hasNonRegularChild(w) && singleIfHeader(w).exists { h =>
      val wThen = thenBlockOf(w, h)
      elseBlockOf(w, wThen) match
        case Some(wElse) =>
          directRegularSteps(wThen).isEmpty &&
          lastGotoIs(wThen, Goto.ThisStep) && lastGotoIs(wElse, Goto.NextStep) &&
          wThen.members(MemberView.Folded).exists { case _: DFNet => true; case _ => false }
        case None => false
    }

  // Build a `Level` from a control-step shell: one `if`, then-branch owns exactly one nested regular
  // step and loops back (ThisStep), else-branch exists, no non-regular children, and exactly one
  // then-net updates the loop's condition variable (the index update). Returns the level + the
  // nested step + the else-block, or None if `c` is not a foldable control shell.
  private def controlLevel(c: StepBlock)(
      using MemberGetSet
  ): Option[(Level, StepBlock, DFConditional.DFIfElseBlock)] =
    if (!c.isRegular || hasNonRegularChild(c)) None
    else
      singleIfHeader(c).flatMap { h =>
        val cThen = thenBlockOf(c, h)
        elseBlockOf(c, cThen).flatMap { cElse =>
          directRegularSteps(cThen) match
            case List(nested) if lastGotoIs(cThen, Goto.ThisStep) =>
              val cCond = cThen.getGuardOption.get
              val thenNets = cThen.members(MemberView.Folded).collect { case n: DFNet => n }
              val dcls = condDcls(cCond)
              thenNets.find(n => lhsDcl(n).exists(dcls.contains)).map { idxNet =>
                (Level(c, cThen, cCond, idxNet, thenNets.filterNot(_ == idxNet)), nested, cElse)
              }
            case _ => None
        }
      }

  // The maximal foldable control chain rooted at `c` (recursing through nested control steps down to
  // a leaf wait). None if `c` is not a control shell or never reaches a wait.
  private def chainOf(c: StepBlock)(using MemberGetSet): Option[Chain] =
    controlLevel(c).flatMap { case (level, nested, cElse) =>
      if (isWaitStep(nested))
        val wThen = thenBlockOf(nested, singleIfHeader(nested).get)
        Some(Chain(
          List(level), nested, wThen, wThen.getGuardOption.get,
          wThen.members(MemberView.Folded).collect { case n: DFNet => n }, cElse
        ))
      else
        chainOf(nested).map(inner => inner.copy(levels = level :: inner.levels, c1Else = cElse))
    }

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    foldToFixpoint(subDB)

  @tailrec private def foldToFixpoint(db: DB)(using CompilerOptions, RefGen): DB =
    given MemberGetSet = db.getSet
    // pre-order traversal => the first foldable step is the OUTERMOST of its chain (so the whole
    // chain folds at once)
    db.members.iterator
      .collect { case c: StepBlock if c.isInRTDomain => c }
      .flatMap(chainOf)
      .nextOption() match
      case None        => db
      case Some(chain) => foldToFixpoint(db.patch(foldPatches(chain)))

  private def foldPatches(chain: Chain)(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
    val levels = chain.levels // outer (index 0) -> inner (index k-1)
    val k = levels.size
    val innermost = levels.last
    val c1 = levels.head.c

    // one guard register per control level, named after that level's step (outer first)
    val regDsn =
      new MetaDesign(enclosingProcess(c1), Patch.Add.Config.Before, dfhdl.core.DomainType.RT):
        import dfhdl.core.*
        val regs = levels.map(L => (Bit <> VAR.REG).init(1)(using dfc.setName(s"${L.c.getName}_entered")))
    val enteredRegs = regDsn.regs

    def cloneNets(md: MetaDesign[?], baseOwner: DFOwner, nets: List[DFNet]): Unit =
      nets.foreach { n =>
        md.plantClonedMembers(baseOwner, n.collectRelMembers.asInstanceOf[List[DFMember]] :+ n)
      }

    val stepDsn =
      new MetaDesign(
        c1,
        Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
        dfhdl.core.DomainType.RT
      ):
        import dfhdl.core.{StepBlock, DFIf, DFBool, DFUnit}
        // re-arm the guard regs for control levels `from..k-1` (inner-first ordering)
        def reArm(from: Int): Unit =
          (k - 1 to from by -1).foreach(p => enteredRegs(p).din := 1)
        // clone the wait-counter reset that re-initialises the wait for the next iteration
        def resetWaitCounter(): Unit = cloneNets(this, innermost.cThen, innermost.resetNets)
        def cloneElse(): Unit = plantClonedMembers(chain.c1Else, chain.c1Else.members(MemberView.Flattened))
        // the leave path for the outer control levels (k-2..0): each resets the level below's index,
        // then tests its own condition (continue vs. recurse outward); the outermost else is C_1's
        // else-body. For a single loop (k==1) this is just C_1's else-body.
        def buildOuterExit(mi: Int): Unit =
          if (mi < 0) cloneElse()
          else
            cloneNets(this, levels(mi).cThen, levels(mi).resetNets)
            val nh = DFIf.Header(DFUnit)
            val cont = DFIf.Block(Some(levels(mi).cCond.cloneAnonValueAndDepsHere.asValOf[DFBool]), nh)
            dfc.enterOwner(cont)
            resetWaitCounter()
            reArm(mi)
            ThisStep
            dfc.exitOwner()
            val els = DFIf.Block(None, cont)
            dfc.enterOwner(els)
            if (mi == 0) cloneElse() else buildOuterExit(mi - 1)
            dfc.exitOwner()

        val step = StepBlock.forced(using dfc.setName(chain.w.getName))
        dfc.enterOwner(step)
        val header = DFIf.Header(DFUnit)
        // count branch: guarded per-level index updates (inner-first), then W's counter increment
        val countBlock = DFIf.Block(Some(chain.wCond.cloneAnonValueAndDepsHere.asValOf[DFBool]), header)
        dfc.enterOwner(countBlock)
        levels.zipWithIndex.reverse.foreach { (lvl, idx) =>
          val gIf = DFIf.Block(Some(enteredRegs(idx).asValOf[DFBool]), DFIf.Header(DFUnit))
          dfc.enterOwner(gIf)
          cloneNets(this, lvl.cThen, List(lvl.idxNet))
          enteredRegs(idx).din := 0
          dfc.exitOwner()
        }
        cloneNets(this, chain.wThen, chain.wCountNets)
        ThisStep
        dfc.exitOwner()
        // innermost continue (part of the main chain, rendered as `else if`)
        val contInner =
          DFIf.Block(Some(innermost.cCond.cloneAnonValueAndDepsHere.asValOf[DFBool]), countBlock)
        dfc.enterOwner(contInner)
        resetWaitCounter()
        reArm(k - 1)
        ThisStep
        dfc.exitOwner()
        // leave (innermost loop exhausted): outer levels, or just C_1's else-body for a single loop
        val leaveBlock = DFIf.Block(None, contInner)
        dfc.enterOwner(leaveBlock)
        buildOuterExit(k - 2)
        dfc.exitOwner()
        dfc.exitOwner()

    // Remove the whole chain. C_1 is removed by the ReplaceWithLast above; its descendants — every
    // inner level, W, nets, gotos — are removed here. The original (now-cloned) conditions of
    // while/for-lowered loops are owned by the *enclosing* block, not C_1, so they are added
    // explicitly. BUT a condition's operands may be SHARED with surviving members — e.g. a `for`
    // loop's bound const is referenced by both the loop condition and the (not-yet-cleaned) `DFRange`
    // — so an external cond-member is removed only if no surviving member still references it.
    val baseRemove = c1.members(MemberView.Flattened)
    val baseSet = baseRemove.toSet
    val condCands =
      (chain.wCond :: levels.map(_.cCond)).flatMap(c => c :: c.collectRelMembers(false)).distinct
    val extraConds = condCands.filterNot(baseSet.contains)
    val tentativeRemove = baseSet ++ extraConds + c1
    // ref targets of everything that will survive (resolved on the intact pre-patch DB)
    val externalTargets: Set[DFMember] =
      getSet.designDB.members.iterator
        .filterNot(tentativeRemove.contains)
        .flatMap(_.getRefs.iterator.map(_.get))
        .toSet
    val removeSet = (baseRemove ++ extraConds.filterNot(externalTargets.contains)).distinct
    val removeChain = removeSet.map(_ -> Patch.Remove())

    regDsn.patch :: stepDsn.patch :: removeChain
  end foldPatches

  @tailrec private def enclosingProcess(m: DFMember)(using MemberGetSet): ProcessBlock =
    m.getOwner match
      case pb: ProcessBlock => pb
      case owner            => enclosingProcess(owner)
end FoldControlSteps

extension [T: HasDB](t: T)
  def foldControlSteps(using CompilerOptions): DB =
    StageRunner.run(FoldControlSteps)(t.db)
