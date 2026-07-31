package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.internals.*
import dfhdl.compiler.ir
import scala.annotation.tailrec

//format: off
/** First-step fusion for RT process step blocks — the implementation of the fusion phase of
  * [[FlattenStepBlocks]] (see its ScalaDoc for the user-facing rules).
  *
  * A step block whose first time-consuming action — scanning through prologue statements
  * (assignments, prints, anonymous values) and through conditional branch guards — is entering a
  * nested step block shares its entry cycle with that nested step ("same label"). Such a step is a
  * fusion candidate: its body is pure *dispatch* (prologue + guard tree + gotos), and instead of
  * occupying an FSM state for one cycle, the dispatch is inlined combinationally at every goto site
  * that targets it.
  *
  * ==Value forwarding==
  *
  * The dispatch conceptually executes one cycle *later* than the site's own statements (it belonged
  * to the following FSM state). Inlining it into the site's cycle therefore forwards register
  * reads: a register with a pending unconditional full assignment on the site's execution path is
  * read as that assignment's right-hand-side value. For example, a loop-back site that increments
  * `i.din := i + 1` evaluates the inlined loop guard as `(i + 1) < 100`. Registers without pending
  * assignments (and external signals such as ports) are sampled directly in the transition cycle.
  * Non-register variable reads follow blocking-assignment semantics within the expansion and become
  * unreadable (fusion aborts) across a conceptual cycle boundary.
  *
  * ==Constant pruning==
  *
  * A substituted guard that becomes statically known selects its branch at compile time: a `true`
  * guard inlines the branch without the conditional, and a `false` guard drops the branch. This is
  * what terminates the recursive expansion of mutually-referencing loop dispatches: a loop
  * re-entry with a constant range resets the iterator to a constant, so the re-entered loop's
  * guard folds and the expansion cycle breaks.
  *
  * ==Step hooks==
  *
  * `onEntry`/`onExit` bodies run on real FSM edges, so a step carrying either keeps its state. A
  * `fallThrough` holding nothing but its condition does not: a fused step consumes no cycle at
  * all, which is strictly more than the conditional zero-cycle skip the hook asks for. Its
  * condition is therefore materialized at every site as the dispatch's first decision —
  * `if (cond) <default exit> else <dispatch>` — and, being part of the dispatch, it is forwarded
  * like the step's own guards. Two shapes keep the hook (and hence the state): one carrying
  * statements of its own, and the process's first step, which survives as the reset bootstrap
  * state where the hook has no edge left to run on.
  *
  * When the condition is the negation of the dispatch's own leading guard, the hook is dropped
  * instead of materialized (`fallThroughSubsumed`) — otherwise the guard-false path, which is
  * where [[FlattenStepBlocks]] relocates whatever follows the construct, becomes dead code. This
  * is the shape [[DropRTWaits]] gives a `FALL_THROUGH` loop, so such a loop lowers to exactly what
  * the same loop without the marker lowers to.
  *
  * ==Fallback==
  *
  * Fusion of a step silently falls back to keeping the step as a real FSM state (consuming its
  * entry cycle) when its dispatch cannot be soundly inlined: unresolvable forwarding (conditional
  * or partial pending assignments on a site path), reads of values that cannot be re-evaluated at
  * the site (e.g. `.reg`/history aliases), match-based dispatch, a self-goto in the dispatch, or a
  * dynamic dispatch cycle (e.g. nested loops with non-constant re-entry guards, detected via a
  * visit limit during expansion).
  *
  * ==Reset bootstrap==
  *
  * If the process's first step is fused, it is kept as a state solely for the reset entry (there
  * is no jump site to inline its dispatch into at reset); all real jump sites still inline it, so
  * the bootstrap costs at most one cycle at process start and none per iteration. When the
  * bootstrap's dispatch additionally const-folds under the values the prologue assigns (the
  * reset/initial values), even that state is dropped: the folded assignments join the prologue
  * (and thus the generated `initial` block) and the fold's target step becomes the FSM entry
  * state, so the process starts with zero bootstrap cycles.
  */
//format: on
private[stages] object FirstStepFusion:

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Candidate analysis — runs on the NESTED step form (before flattening), since only nesting
  // provenance distinguishes a parent step whose first action is its nested child (fusable) from
  // an ordinary sequential step (not fusable) — the two are structurally identical once flat.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  private enum Scan derives CanEqual:
    case FoundStep, NoStep, Blocked

  private def hasNonRegularChild(s: StepBlock)(using MemberGetSet): Boolean =
    s.members(MemberView.Folded).exists {
      case sb: StepBlock => !sb.isRegular
      case _             => false
    }

  // A `fallThrough` hook holding nothing but its condition value. Such a hook does not force its
  // step to keep a state: a fused step consumes no cycle at all, so its entry condition simply
  // becomes the first decision of the dispatch inlined at every site (see `expandGoto`).
  // `onEntry`/`onExit` bodies, and a `fallThrough` carrying statements of its own, must run on a
  // real FSM edge, so they still keep their step a state.
  private def isPureFallThrough(sb: StepBlock)(using MemberGetSet): Boolean =
    sb.isFallThrough && sb.members(MemberView.Flattened).forall {
      case _: DFVal => true
      case _        => false
    }

  private def hasBlockingHook(s: StepBlock)(using MemberGetSet): Boolean =
    s.members(MemberView.Folded).exists {
      case sb: StepBlock => !sb.isRegular && !isPureFallThrough(sb)
      case _             => false
    }

  private def fallThroughOf(s: StepBlock)(using MemberGetSet): Option[StepBlock] =
    s.members(MemberView.Folded).collectFirst { case sb: StepBlock if sb.isFallThrough => sb }

  // the hook's condition is the value its trailing `Ident` wraps (the same shape
  // [[DropRTProcess]] reads when it plants the cascade)
  private def fallThroughCondOf(hook: StepBlock, victim: StepBlock)(using
      MemberGetSet
  ): DFVal =
    hook.members(MemberView.Flattened).lastOption match
      case Some(Ident(cond)) => cond
      case _                 => throw new AbortFusion(victim)

  /** Is `m` inside one of `root`'s hook blocks rather than on its dispatch path? */
  private def isInHook(m: DFMember, root: StepBlock)(using MemberGetSet): Boolean =
    m.getOwner match
      case owner if owner == root => false
      case owner: StepBlock       => !owner.isRegular || isInHook(owner, root)
      case owner: DFMember        => isInHook(owner, root)

  /** A step's default exit — the target of the last `Goto` on its dispatch path — mirroring
    * [[DropRTProcess]]'s `defaultExitOf`. This is where a `fallThrough` sends control, so a
    * candidate carrying one fuses only when it resolves to another step.
    */
  private def defaultExitOf(s: StepBlock)(using MemberGetSet): Option[StepBlock] =
    s.members(MemberView.Flattened).reverseIterator.collectFirst {
      case g: Goto if !isInHook(g, s) => g.stepRef.get
    }.collect { case target: StepBlock if target != s => target }

  /** Does the step's own dispatch already do what its `fallThrough` asks for?
    *
    * The hook is materialized as `if (cond) <default exit> else <dispatch>`, so when `cond` is the
    * negation of the dispatch's leading guard the two are exact complements of one value in one
    * cycle: the guard-false path can never be taken, and everything on it becomes **dead code**.
    * That path is where [[FlattenStepBlocks]] relocates whatever follows the construct — trailing
    * statements, and the forever-rotation's prologue clone — so killing it silently drops work the
    * skip is supposed to continue into ("falls through ... continuing at whatever follows the
    * loop"). Dropping the hook is therefore the only reading under which the guard-false path means
    * anything, and it is what `DropRTWaits` gives a `FALL_THROUGH` loop: such a loop fuses to
    * exactly what the same loop without the marker fuses to.
    *
    * The dispatch must *start* with that conditional — a leading statement is the step's own, and
    * the hook is entitled to skip it — and the guard-false path must reach the default exit
    * unconditionally, so that "guard false" and "fall through" name the same edge.
    *
    * The hook reads registers through `.din` and the dispatch reads them directly, but here the two
    * still name one value: fusion inlines the dispatch into the same cycle as the hook, and nothing
    * runs between them, so both resolve to the same forwarded value ([[substDin]] falls back to
    * exactly what [[substDcl]] returns when the region has not written the register yet). The
    * comparison therefore looks through the `.din` reads.
    */
  private def fallThroughSubsumed(s: StepBlock, cond: DFVal, exit: StepBlock)(using
      MemberGetSet
  ): Boolean =
    val negatedOpt = cond match
      case DFVal.Func(op = DFVal.Func.Op.unary_!, args = List(argRef)) => Some(argRef.get)
      case _                                                           => None
    negatedOpt.exists { negated =>
      val dispatch = s.members(MemberView.Folded).dropWhile {
        case _: DFConditional.Header => false
        case sb: StepBlock           => sb.isFallThrough
        case _: DFVal | _: DFRange   => true
        case _                       => false
      }
      dispatch match
        case (h: DFConditional.DFIfHeader) :: rest =>
          gatherChain(h, rest) match
            case (List(ifBlock, elseBlock), Nil) =>
              (ifBlock.guardRef.get, elseBlock.guardRef.get) match
                case (guard: DFVal, _: DFMember.Empty.type) =>
                  sameAtFusedEntry(negated, guard) &&
                  // statements on the guard-false path are fine (they are the continuation the
                  // skip must run); a further goto or step on it is not — the path would no
                  // longer be the single edge the hook names
                  (elseBlock.members(MemberView.Flattened).reverse match
                    case (g: Goto) :: leading =>
                      g.stepRef.get == exit && leading.forall {
                        case _: Goto | _: StepBlock => false
                        case _                      => true
                      }
                    case _ => false)
                case _ => false
            case _ => false
        case _ => false
      end match
    }
  end fallThroughSubsumed

  /** Structural comparison of two values as they read at a fused entry, where a `.din` read and the
    * plain read of the same register resolve to one forwarded value (see [[fallThroughSubsumed]]).
    * A `.din` read through a partial selection is not compared through: such a hook aborts fusion
    * in [[substDin]] and keeps the step's state, so it never reaches this test.
    */
  private def sameAtFusedEntry(a: DFVal, b: DFVal)(using MemberGetSet): Boolean =
    def stripDin(v: DFVal): DFVal = v match
      case d: DFVal.Alias.RegDIN =>
        d.relValRef.get match
          case dcl: DFVal.Dcl => dcl
          case _              => v
      case _ => v
    (stripDin(a), stripDin(b)) match
      case (fa: DFVal.Func, fb: DFVal.Func) if fa.op == fb.op && fa.args.length == fb.args.length =>
        fa.args.lazyZip(fb.args).forall((ra, rb) => sameAtFusedEntry(ra.get, rb.get))
      case (va, vb) => va =~ vb

  // Gathers the conditional chain blocks that follow header `h` among `rest` (skipping the guard
  // values interleaved between chain blocks), returning the chain and the members after it.
  private def gatherChain(h: DFConditional.Header, rest: List[DFMember])(using
      MemberGetSet
  ): (List[DFConditional.Block], List[DFMember]) =
    @tailrec def go(
        remaining: List[DFMember],
        prev: DFMember,
        chain: List[DFConditional.Block],
        skipped: List[DFMember]
    ): (List[DFConditional.Block], List[DFMember]) =
      remaining match
        case (cb: DFConditional.Block) :: rest if cb.prevBlockOrHeaderRef.get == prev =>
          go(rest, cb, cb :: chain, skipped)
        // guard values (and their dependencies) may be interleaved between chain blocks
        case (v: DFVal) :: rest if !v.isInstanceOf[DFConditional.Header] =>
          go(rest, prev, chain, v :: skipped)
        case _ => (chain.reverse, skipped.reverse ::: remaining)
    go(rest, h, Nil, Nil)
  end gatherChain

  // Scans a region (step body or conditional branch) in order for its first time-consuming action.
  private def scanRegion(members: List[DFMember])(using MemberGetSet): Scan =
    members match
      case Nil       => Scan.NoStep
      case m :: rest =>
        m match
          case h: DFConditional.Header =>
            val (chain, afterChain) = gatherChain(h, rest)
            val hasTime = chain.exists {
              _.members(MemberView.Flattened).exists {
                case _: StepBlock => true
                case _: Goto      => true
                case _            => false
              }
            }
            if (!hasTime) scanRegion(afterChain) // purely combinational conditional — prologue
            else
              val results = chain.map(cb => scanRegion(cb.members(MemberView.Folded)))
              if (results.contains(Scan.Blocked)) Scan.Blocked
              else if (results.contains(Scan.FoundStep)) Scan.FoundStep
              else scanRegion(afterChain) // dispatch gotos only — a fall-through path may follow
          case _: DFConditional.Block        => Scan.Blocked // out-of-chain block — unexpected
          case sb: StepBlock if sb.isRegular =>
            if (hasNonRegularChild(sb)) Scan.Blocked else Scan.FoundStep
          // the step's own `fallThrough` hook is a pure entry condition, not a time-consuming
          // action: the scan continues into the dispatch that follows it
          case sb: StepBlock if isPureFallThrough(sb) => scanRegion(rest)
          case _: StepBlock => Scan.Blocked // onEntry/onExit — excluded from fusion
          case _: Goto      => Scan.NoStep // dispatch leaf; subsequent members are unreachable
          case _: Wait      => Scan.Blocked
          case lb: DFLoop.Block if lb.isCombinational        => scanRegion(rest)
          case _: DFLoop.Block                               => Scan.Blocked
          case _: DFVal | _: DFNet | _: TextOut | _: DFRange => scanRegion(rest) // prologue
          case _                                             => Scan.Blocked
  end scanRegion

  private def isCandidate(s: StepBlock)(using MemberGetSet): Boolean =
    s.isRegular && !hasBlockingHook(s) &&
      scanRegion(s.members(MemberView.Folded)) == Scan.FoundStep

  /** Collects fusion candidates from the nested (pre-flattening) sub-DB, in member order. */
  def collectCandidates(subDB: DB)(using MemberGetSet): List[StepBlock] =
    subDB.members.collect {
      case sb: StepBlock if sb.isInRTDomain && sb.isInProcess && isCandidate(sb) => sb
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Fusion — runs on the FLAT form (after goto resolution), where candidates' bodies are pure
  // dispatch and all gotos carry explicit StepBlock references.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  // Thrown while building a site expansion when a candidate's dispatch cannot be soundly inlined.
  // The whole fusion pass restarts without the victim (silent fallback to a real FSM state).
  private final class AbortFusion(val victim: StepBlock) extends Exception

  private def fullAssignDcl(net: DFNet)(using MemberGetSet): Option[DFVal.Dcl] =
    if (net.op != DFNet.Op.Assignment) None
    else
      net.lhsRef.get.departialDcl.collect {
        case (dcl, slice) if slice.isFullOf(dcl.dfType.widthIntOpt) == Tri.Yes => dcl
      }

  // Post-flattening validation: a fused body must be pure if-dispatch with no self-goto.
  // Exception: the process's first step may self-goto — the forever-loop rotation wraps the
  // process end back to it, planting the re-initializing prologue clone right before the wrap
  // goto, so expansion resolves the re-entry by constant pruning on the re-initialized values
  // (a genuinely dynamic re-entry still falls back via the expansion visit limit).
  private def validCandidate(s: StepBlock)(using MemberGetSet): Boolean =
    lazy val isProcessFirstStep = s.getOwnerBlock match
      case pb: ProcessBlock =>
        pb.members(MemberView.Folded).collectFirst {
          case sb: StepBlock if sb.isRegular => sb
        }.contains(s)
      case _ => false
    // a `fallThrough` hook is materialized at the fusion sites, so it fuses away only together
    // with its step: it needs a default exit to redirect to, and the process's first step is kept
    // as the reset bootstrap state, where the hook would have no edge left to run on
    val fallThroughFusable =
      fallThroughOf(s).forall(_ => defaultExitOf(s).isDefined && !isProcessFirstStep)
    fallThroughFusable && s.members(MemberView.Flattened).forall {
      case _: DFVal.Dcl            => false
      case h: DFConditional.Header => h.isInstanceOf[DFConditional.DFIfHeader]
      case cb: DFConditional.Block => cb.isInstanceOf[DFConditional.DFIfElseBlock]
      case v: DFVal                => v.isAnonymous
      case net: DFNet              => fullAssignDcl(net).nonEmpty
      case _: TextOut              => true
      // orphaned for-loop range leftovers (cleaned later by DropUnreferenced) — dead members
      case _: DFRange => true
      case g: Goto    =>
        g.stepRef.get match
          case target: StepBlock => target != s || isProcessFirstStep
          case _                 => false // relative gotos must not remain at this point
      case sb: StepBlock => isPureFallThrough(sb)
      case _             => false
    }
  end validCandidate

  // Walks backward from an anchor member to its owning step's (or process's) boundary,
  // collecting the value each register will hold in the next cycle (pending unconditional full
  // assignments) and the declarations whose next-cycle value is not statically resolvable
  // (conditional or partial pending assignments, and same-cycle non-register blocking
  // assignments). Anchored at a goto site this yields the site's forwarding state; anchored at
  // the process's first step it yields the prologue's reset-entry state.
  private def walkBack(anchor: DFMember)(using
      MemberGetSet
  ): (Map[DFVal.Dcl, DFVal], Set[DFVal.Dcl]) =
    def assignedDclsOf(m: DFMember): Iterator[DFVal.Dcl] = m match
      case owner: DFOwner =>
        owner.members(MemberView.Flattened).iterator.flatMap {
          case net: DFNet if net.op == DFNet.Op.Assignment =>
            net.lhsRef.get.departialDcl.map(_._1)
          case _ => None
        }
      case _ => Iterator.empty
    @tailrec def walk(
        cur: DFMember,
        regs: Map[DFVal.Dcl, DFVal],
        dirty: Set[DFVal.Dcl]
    ): (Map[DFVal.Dcl, DFVal], Set[DFVal.Dcl]) =
      val block = cur.getOwnerBlock
      val preceding = block.members(MemberView.Folded).takeWhile(_ != cur).reverse
      var newRegs = regs
      var newDirty = dirty
      preceding.foreach {
        case net: DFNet if net.op == DFNet.Op.Assignment =>
          fullAssignDcl(net) match
            case Some(dcl) if !newRegs.contains(dcl) && !newDirty.contains(dcl) =>
              // the last assignment (first in reverse) determines the next-cycle value; only
              // registers carry a value across the removed cycle boundary — a blocking (non-reg)
              // variable assigned in the site's cycle is unreadable by the inlined dispatch
              if (dcl.modifier.isReg) newRegs = newRegs.updated(dcl, net.rhsRef.get)
              else newDirty = newDirty + dcl
            case Some(_) => // already resolved by a later assignment on the path
            case None    => // partial assignment — the next-cycle value is not a single expression
              net.lhsRef.get.departialDcl.foreach { (dcl, _) =>
                if (!newRegs.contains(dcl)) newDirty = newDirty + dcl
              }
        case owner: DFOwner =>
          // a preceding sibling block (conditional branch, combinational loop) executes its
          // assignments conditionally relative to the site
          assignedDclsOf(owner).foreach { dcl =>
            if (!newRegs.contains(dcl)) newDirty = newDirty + dcl
          }
        case _ => // values, prints, gotos — no assignment effect
      }
      block match
        case _: StepBlock            => (newRegs, newDirty)
        case cb: DFConditional.Block =>
          // ascend to the chain header: preceding sibling chain blocks did NOT execute on this
          // path, so the walk continues from the header's position
          @tailrec def chainHeader(b: DFConditional.Block): DFConditional.Header =
            b.prevBlockOrHeaderRef.get match
              case h: DFConditional.Header   => h
              case prev: DFConditional.Block => chainHeader(prev)
          walk(chainHeader(cb), newRegs, newDirty)
        case _ => (newRegs, newDirty) // process level — stop
      end match
    end walk
    walk(anchor, Map(), Set())
  end walkBack

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Dispatch expansion machinery, shared by goto-site inlining and reset-site folding: value
  // forwarding through pending register assignments, constant guard resolution, and statement
  // cloning into the expansion's meta design.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  private abstract class DispatchExpansion(
      anchor: DFMember,
      addCfg: Patch.Add.Config
  )(using MemberGetSet, RefGen)
      extends MetaDesign(anchor, addCfg, dfhdl.core.DomainType.RT):
    import dfhdl.core.{refTW, addMember}

    // the constant data of a Bool/Bit guard, if statically known (handles both the raw and the
    // Option-wrapped (bubble-capable) data representations)
    def guardConstData(guard: ir.DFVal): Option[Boolean] =
      // the guard may be a fresh clone known only to this meta design's mutable DB
      given ir.MemberGetSet = dfc.getSet
      guard.getConstDataThroughParams[Any] match
        case Some(Some(b: Boolean)) => Some(b)
        case Some(b: Boolean)       => Some(b)
        case _                      => None

    type Regs = Map[ir.DFVal.Dcl, ir.DFVal]
    // per-execution-path forwarding state within one region (one conceptual cycle):
    // `pendingRegs` — register assignments made in this region (committed at the next goto);
    // `blocking` — non-register variable values (blocking semantics, same cycle only);
    // `dirty` — declarations whose value is not statically resolvable on this path
    case class PathState(
        pendingRegs: Regs,
        blocking: Regs,
        dirty: Set[ir.DFVal.Dcl]
    )

    def emitGoto(target: ir.StepBlock): Unit =
      ir.Goto(target.refTW[ir.Goto], dfc.ownerOrEmptyRef, dfc.getMeta.anonymize, dfc.tags)
        .addMember

    def treeDcls(v: ir.DFVal): Set[ir.DFVal.Dcl] = v match
      case dcl: ir.DFVal.Dcl => Set(dcl)
      case _                 =>
        v.getRefs.view.map(_.get).foldLeft(Set.empty[ir.DFVal.Dcl]) {
          case (acc, dep: ir.DFVal) => acc ++ treeDcls(dep)
          case (acc, _)             => acc
        }

    def forbiddenRead(v: ir.DFVal, entryRegs: Regs, st: PathState): Boolean =
      treeDcls(v).exists(d =>
        entryRegs.contains(d) || st.blocking.contains(d) || st.dirty.contains(d)
      )

    // the forwarded value for a read: re-emit anonymous values here (fresh, single-referenced),
    // reference declarations/named values directly
    def emitForward(fwd: ir.DFVal, victim: ir.StepBlock): ir.DFVal =
      if (fwd.isAnonymous)
        try fwd.cloneAnonValueAndDepsHere
        catch case _: IllegalArgumentException => throw new AbortFusion(victim)
      else fwd

    def substDcl(
        dcl: ir.DFVal.Dcl,
        entryRegs: Regs,
        st: PathState,
        victim: ir.StepBlock
    ): ir.DFVal =
      val fwdOpt = if (dcl.modifier.isReg) entryRegs.get(dcl) else st.blocking.get(dcl)
      fwdOpt match
        case Some(fwd)             => emitForward(fwd, victim)
        case None if st.dirty(dcl) => throw new AbortFusion(victim)
        case None                  => dcl

    /** A `.din` read resolves to the register's pending value *at this point of the expansion*: a
      * write this region has already made, else the value forwarded across the boundary the
      * expansion removed, else the register itself. That is one step ahead of the plain register
      * read above, which crosses only the boundary.
      *
      * A partial `.din` read would need the pending value sliced, which the forwarding state does
      * not carry, so it falls back to keeping the step's own state.
      */
    def substDin(
        din: ir.DFVal.Alias.RegDIN,
        entryRegs: Regs,
        st: PathState,
        victim: ir.StepBlock
    ): ir.DFVal =
      din.relValRef.get match
        case dcl: ir.DFVal.Dcl =>
          st.pendingRegs.get(dcl).orElse(entryRegs.get(dcl)) match
            case Some(fwd)             => emitForward(fwd, victim)
            case None if st.dirty(dcl) => throw new AbortFusion(victim)
            case None                  => dcl
        case _ => throw new AbortFusion(victim)

    // clones an anonymous expression tree here and retargets its declaration reads through the
    // forwarding state
    def substValue(
        v: ir.DFVal,
        entryRegs: Regs,
        st: PathState,
        victim: ir.StepBlock
    ): ir.DFVal =
      // fresh clone refs are registered only in this meta design's mutable DB
      given ir.MemberGetSet = dfc.getSet
      def rewire(root: ir.DFVal): Unit =
        root.getRefs.foreach { ref =>
          ref.get match
            case dcl: ir.DFVal.Dcl =>
              val r = substDcl(dcl, entryRegs, st, victim)
              if (r ne dcl)
                dfc.mutableDB.newRefFor(ref.asInstanceOf[ir.DFRef[ir.DFVal]], r)
            case din: ir.DFVal.Alias.RegDIN =>
              val r = substDin(din, entryRegs, st, victim)
              if (r ne din)
                dfc.mutableDB.newRefFor(ref.asInstanceOf[ir.DFRef[ir.DFVal]], r)
            case dep: ir.DFVal if dep.isAnonymous => rewire(dep)
            case dep: ir.DFVal                    =>
              if (forbiddenRead(dep, entryRegs, st)) throw new AbortFusion(victim)
            case _ =>
        }
      v match
        case dcl: ir.DFVal.Dcl          => substDcl(dcl, entryRegs, st, victim)
        case din: ir.DFVal.Alias.RegDIN => substDin(din, entryRegs, st, victim)
        case _ if !v.isAnonymous        =>
          if (forbiddenRead(v, entryRegs, st)) throw new AbortFusion(victim)
          v
        case _ =>
          val cloned =
            try v.cloneAnonValueAndDepsHere
            catch case _: IllegalArgumentException => throw new AbortFusion(victim)
          rewire(cloned)
          cloned
    end substValue

    // clones a single statement member here with fresh registered refs, remapping value reads
    // through the forwarding state (used for prints/asserts)
    def plantSubstClone(
        m: ir.DFMember,
        entryRegs: Regs,
        st: PathState,
        victim: ir.StepBlock
    ): Unit =
      val cloned = m.copyWithNewRefs
      dfc.mutableDB.addMember(cloned)
      dfc.mutableDB.newRefFor(cloned.ownerRef, dfc.owner.asIR)
      m.getRefs.lazyZip(cloned.getRefs).foreach { (ref, clonedRef) =>
        val target: ir.DFMember = ref.get match
          case v: ir.DFVal => substValue(v, entryRegs, st, victim)
          case other       => other
        dfc.mutableDB.newRefFor(clonedRef.asInstanceOf[ir.DFRef[ir.DFMember]], target)
      }
    end plantSubstClone

    // clones a full-width left-hand-side access chain (a Dcl or full-width alias wrappers)
    def cloneLhs(v: ir.DFVal, victim: ir.StepBlock): ir.DFVal = v match
      case dcl: ir.DFVal.Dcl             => dcl
      case alias: ir.DFVal.Alias.Partial =>
        val rel = cloneLhs(alias.relValRef.get, victim)
        val cloned = alias.copyWithNewRefs
        dfc.mutableDB.addMember(cloned)
        dfc.mutableDB.newRefFor(cloned.ownerRef, dfc.owner.asIR)
        alias.getRefs.lazyZip(cloned.getRefs).foreach { (ref, clonedRef) =>
          val target: ir.DFMember = if (ref.get == alias.relValRef.get) rel else ref.get
          dfc.mutableDB.newRefFor(clonedRef.asInstanceOf[ir.DFRef[ir.DFMember]], target)
        }
        cloned
      case _ => throw new AbortFusion(victim)

    // clones an assignment net here, forwarding its right-hand-side reads through the path state
    def emitAssign(
        net: ir.DFNet,
        entryRegs: Regs,
        st: PathState,
        victim: ir.StepBlock
    ): (ir.DFVal.Dcl, ir.DFVal) =
      val dcl = fullAssignDcl(net).getOrElse(throw new AbortFusion(victim))
      val rhs = substValue(net.rhsRef.get, entryRegs, st, victim)
      val lhs = cloneLhs(net.lhsRef.get, victim)
      ir.DFNet(
        lhs.refTW[ir.DFNet],
        ir.DFNet.Op.Assignment,
        rhs.refTW[ir.DFNet],
        dfc.ownerOrEmptyRef,
        net.meta,
        net.tags
      ).addMember
      (dcl, rhs)
    end emitAssign
  end DispatchExpansion

  // Builds the site expansion: replaces the site goto with the (recursively expanded) dispatch of
  // its fused target, applying value forwarding and constant pruning.
  private def expandSite(site: Goto, fusedSet: Set[StepBlock])(using
      MemberGetSet,
      RefGen
  ): (DFMember, Patch) =
    val rootTarget = site.stepRef.get.asInstanceOf[StepBlock]
    val (initRegs, initDirty) = walkBack(site)
    val dsn = new DispatchExpansion(
      site,
      Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
    ):
      import dfhdl.core.{DFIf, DFBool, DFUnit, DFOwnerAny, DFValAny}

      def expandGoto(
          target: ir.StepBlock,
          entryRegs: Regs,
          dirty: Set[ir.DFVal.Dcl],
          visits: Map[ir.StepBlock, Int]
      ): Unit =
        if (!fusedSet.contains(target)) emitGoto(target)
        else
          val count = visits.getOrElse(target, 0)
          // a fused step revisited on the same expansion path beyond the constant-pruned loop
          // re-entry indicates a dynamic dispatch cycle — not inlinable
          if (count >= 2) throw new AbortFusion(target)
          else
            val visited = visits.updated(target, count + 1)
            val st = PathState(Map(), Map(), dirty)
            def dispatch(): Unit =
              buildRegion(target.members(MemberView.Folded), entryRegs, st, visited, target)
            // A fused step's `fallThrough` hook is no longer an edge hook: the step consumes no
            // cycle of its own, so its entry condition becomes the first decision of the dispatch
            // inlined here — evaluated with the same forwarded values as the step's own guards,
            // and sending control to the step's default exit exactly as the cascade would.
            fallThroughOf(target) match
              case None       => dispatch()
              case Some(hook) =>
                val exit = defaultExitOf(target).getOrElse(throw new AbortFusion(target))
                val hookCond = fallThroughCondOf(hook, target)
                if (fallThroughSubsumed(target, hookCond, exit)) dispatch()
                else
                  val cond = substValue(hookCond, entryRegs, st, target)
                  guardConstData(cond) match
                    case Some(true)  => expandGoto(exit, entryRegs, dirty, visited)
                    case Some(false) => dispatch()
                    case None        =>
                      val block = DFIf.Block(Some(cond.asValOf[DFBool]), DFIf.Header(DFUnit))
                      dfc.enterOwner(block)
                      expandGoto(exit, entryRegs, dirty, visited)
                      dfc.exitOwner()
                      val elseBlock = DFIf.Block(None, block)
                      dfc.enterOwner(elseBlock)
                      dispatch()
                      dfc.exitOwner()
                end if
            end match
          end if

      def buildRegion(
          members: List[ir.DFMember],
          entryRegs: Regs,
          st: PathState,
          visits: Map[ir.StepBlock, Int],
          victim: ir.StepBlock
      ): Unit =
        members match
          case Nil =>
            // fell off a region end without reaching a goto — cannot preserve semantics
            throw new AbortFusion(victim)
          case m :: rest =>
            m match
              case h: ir.DFConditional.DFIfHeader =>
                val (chain, afterChain) = gatherChain(h, rest)
                buildChain(chain, afterChain, entryRegs, st, visits, victim, None)
              case _: ir.DFConditional.Header => throw new AbortFusion(victim) // match dispatch
              case _: ir.DFConditional.Block  => throw new AbortFusion(victim) // out-of-chain
              case net: ir.DFNet              =>
                val (dcl, rhs) = emitAssign(net, entryRegs, st, victim)
                val newSt =
                  if (dcl.modifier.isReg)
                    st.copy(pendingRegs = st.pendingRegs.updated(dcl, rhs))
                  else st.copy(blocking = st.blocking.updated(dcl, rhs))
                buildRegion(rest, entryRegs, newSt, visits, victim)
              case t: ir.TextOut =>
                plantSubstClone(t, entryRegs, st, victim)
                buildRegion(rest, entryRegs, st, visits, victim)
              case g: ir.Goto =>
                val target = g.stepRef.get match
                  case sb: ir.StepBlock => sb
                  case _                => throw new AbortFusion(victim)
                // crossing to the next region = crossing the removed conceptual cycle boundary:
                // commit this region's register writes; blocking values do not survive it
                expandGoto(
                  target,
                  entryRegs ++ st.pendingRegs,
                  (st.dirty ++ st.blocking.keySet) -- st.pendingRegs.keySet,
                  visits
                )
              // anonymous values are re-emitted on demand by substValue
              case _: ir.DFVal => buildRegion(rest, entryRegs, st, visits, victim)
              // orphaned for-loop range leftovers — dead members, not part of the dispatch
              case _: ir.DFRange => buildRegion(rest, entryRegs, st, visits, victim)
              // the step's `fallThrough` hook was already materialized at the entry
              case sb: ir.StepBlock if sb.isFallThrough =>
                buildRegion(rest, entryRegs, st, visits, victim)
              case _ => throw new AbortFusion(victim)
      end buildRegion

      // emits a conditional dispatch chain, pruning statically resolved guards. Every branch is
      // continued with the after-chain members: branches ending in a goto never reach them (the
      // region build stops at gotos), and non-terminal branches duplicate the continuation —
      // which is the price of eliminating the join state.
      def buildChain(
          chain: List[ir.DFConditional.Block],
          afterChain: List[ir.DFMember],
          entryRegs: Regs,
          st: PathState,
          visits: Map[ir.StepBlock, Int],
          victim: ir.StepBlock,
          prevEmitted: Option[DFOwnerAny | DFValAny]
      ): Unit =
        // emits `body` inside a conditional block (an else block when `guard` is None), or inline
        // when nothing was emitted for this chain yet and the branch is statically selected
        def emitBranch(guard: Option[ir.DFVal], branchMembers: List[ir.DFMember])(
            wrap: Boolean
        ): Option[DFOwnerAny] =
          if (!wrap)
            buildRegion(branchMembers ::: afterChain, entryRegs, st, visits, victim)
            None
          else
            val prevBlockOrHeader: DFOwnerAny | DFValAny =
              prevEmitted.getOrElse(DFIf.Header(DFUnit))
            val block = DFIf.Block(guard.map(_.asValOf[DFBool]), prevBlockOrHeader)
            dfc.enterOwner(block)
            buildRegion(branchMembers ::: afterChain, entryRegs, st, visits, victim)
            dfc.exitOwner()
            Some(block)
        chain match
          case Nil =>
            // no chain blocks left: all emitted guards may be false at runtime — continue with
            // the fall-through path (synthesized as the else of the emitted chain)
            emitBranch(None, Nil)(wrap = prevEmitted.nonEmpty)
          case cb :: restChain =>
            cb.guardRef.get match
              case _: ir.DFMember.Empty.type => // else branch — always selected at this point
                emitBranch(None, cb.members(MemberView.Folded))(wrap = prevEmitted.nonEmpty)
              case guard: ir.DFVal =>
                val guardIR = substValue(guard, entryRegs, st, victim)
                guardConstData(guardIR) match
                  case Some(true) =>
                    // statically taken — the rest of the chain is unreachable
                    emitBranch(None, cb.members(MemberView.Folded))(wrap = prevEmitted.nonEmpty)
                  case Some(false) => // statically dropped
                    buildChain(restChain, afterChain, entryRegs, st, visits, victim, prevEmitted)
                  case None =>
                    val blockOpt =
                      emitBranch(Some(guardIR), cb.members(MemberView.Folded))(wrap = true)
                    buildChain(
                      restChain, afterChain, entryRegs, st, visits, victim,
                      blockOpt.orElse(prevEmitted)
                    )
              case _ => throw new AbortFusion(victim)
        end match
      end buildChain

      expandGoto(rootTarget, initRegs, initDirty, Map())
    dsn.patch
  end expandSite

  // Local unreferenced-anonymous sweep (mirrors DropUnreferencedAnons) — fusion orphans the
  // original dispatch guard expressions that were parked outside the removed steps, plus guard
  // clones emitted for branches that were then statically pruned.
  @tailrec private def sweepUnreferenced(db: DB): DB =
    given MemberGetSet = db.getSet
    val patchList = db.members.flatMap {
      case _: DFConditional.Header                              => None
      case Ident(_)                                             => None
      case m: DFVal if m.isAnonymous && m.originMembers.isEmpty => Some(m -> Patch.Remove())
      case m: DFRange if m.originMembers.isEmpty                => Some(m -> Patch.Remove())
      case _                                                    => None
    }
    if (patchList.isEmpty) db
    else sweepUnreferenced(db.patch(patchList))

  private def collectFusionPatches(
      fusedList: List[StepBlock]
  )(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
    val fusedSet = fusedList.toSet
    getSet.designDB.members.flatMap {
      case pb: ProcessBlock if pb.isInRTDomain =>
        val pbFused = fusedList.filter(_.getOwnerBlock == pb)
        if (pbFused.isEmpty) Nil
        else
          // the process's first step, if fused, is kept as the one-time reset bootstrap state:
          // there is no jump site to inline its dispatch into at reset
          val bootstrapOpt = pb.members(MemberView.Folded).collectFirst {
            case sb: StepBlock if sb.isRegular => sb
          }.filter(fusedSet.contains)
          def isRealRegion(s: StepBlock): Boolean =
            !fusedSet.contains(s) || bootstrapOpt.contains(s)
          val sites = pb.members(MemberView.Flattened).flatMap {
            case g: Goto if isRealRegion(g.getOwnerStepBlock) =>
              g.stepRef.get match
                case target: StepBlock if fusedSet.contains(target) => Some(g)
                case _                                              => None
            case _ => None
          }
          val sitePatches = sites.map(expandSite(_, fusedSet))
          val removedSteps = pbFused.filterNot(bootstrapOpt.contains)
          sitePatches ++ subtreeRemovalPatches(removedSteps, sites.toSet)
        end if
      case _ => Nil
    }
  end collectFusionPatches

  // Removes the given steps with their whole subtrees. A member of a removed subtree that is
  // still referenced from a survivor must be relocated, not removed — DropRTWaits parks a nested
  // step's dispatch guard in the *parent* step's branch, so a surviving wait step may reference
  // values owned by the fused control step being removed. Relocate each such value to the top of
  // the first surviving step that references it (preserving relative order via patch
  // concatenation). `alsoRemoved` lists additional members that vanish in the same patch (e.g.
  // the replaced site gotos), so references from them do not count as survivor references.
  private def subtreeRemovalPatches(
      removedSteps: List[StepBlock],
      alsoRemoved: Set[DFMember]
  )(using MemberGetSet): List[(DFMember, Patch)] =
    val subtrees = removedSteps.map(s => s -> s.members(MemberView.Flattened))
    val removedSet: Set[DFMember] =
      subtrees.flatMap((s, subtree) => s :: subtree).toSet ++ alsoRemoved
    val exemptTargets: Map[DFMember, StepBlock] =
      val subtreeSet = subtrees.flatMap(_._2).toSet
      val builder = scala.collection.mutable.Map.empty[DFMember, StepBlock]
      getSet.designDB.members.foreach { m =>
        if (!removedSet.contains(m))
          m.getRefs.foreach { ref =>
            val target = ref.get
            if (subtreeSet.contains(target) && !builder.contains(target))
              builder += target -> m.getThisOrOwnerStepBlock
          }
      }
      // transitive closure: an exempt member's own dependencies within the subtrees are
      // referenced by a survivor-to-be and must be relocated along with it
      var worklist = builder.keys.toList
      while (worklist.nonEmpty)
        val e = worklist.head
        worklist = worklist.tail
        val target = builder(e)
        e.getRefs.foreach { ref =>
          val dep = ref.get
          if (subtreeSet.contains(dep) && !builder.contains(dep))
            builder += dep -> target
            worklist = dep :: worklist
        }
      builder.toMap
    end exemptTargets
    val movePatches = subtrees.flatMap { (s, subtree) =>
      subtree.collect {
        case v: DFVal if exemptTargets.contains(v) =>
          exemptTargets(v) -> Patch.Move(List(v), v.getOwner, Patch.Move.Config.InsideFirst)
      }
    }
    // a non-value subtree member referenced from a survivor would be structural breakage
    if (subtrees.exists(_._2.exists(m => exemptTargets.contains(m) && !m.isInstanceOf[DFVal])))
      throw new AbortFusion(removedSteps.head)
    val removePatches = subtrees.flatMap { (s, subtree) =>
      (s :: subtree.filterNot(exemptTargets.contains)).map(_ -> Patch.Remove())
    }
    movePatches ++ removePatches
  end subtreeRemovalPatches

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Reset-site folding: after every jump site is inlined, a fused first step survives only as the
  // one-time reset bootstrap state. When its dispatch fully const-folds under the values the
  // prologue assigns (the reset/initial values), even that state is dropped: the folded dispatch
  // assignments are appended to the prologue (and thus lower into the generated `initial` block)
  // and the fold's target step becomes the FSM entry state, so the process starts with zero
  // bootstrap cycles.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  private def collectResetSitePatches(
      pb: ProcessBlock,
      bootstrap: StepBlock
  )(using MemberGetSet, RefGen): List[(DFMember, Patch)] =
    // the bootstrap must be unreachable except through reset (every jump site was inlined)
    val targeted = pb.members(MemberView.Flattened).exists {
      case g: Goto => g.stepRef.get == bootstrap
      case _       => false
    }
    if (targeted) throw new AbortFusion(bootstrap)
    // the prologue's pending register assignments are the values reset/initial will provide
    val (entryRegs, entryDirty) = walkBack(bootstrap)
    val dsn = new DispatchExpansion(bootstrap, Patch.Add.Config.Before):
      // single-path fold: every guard must resolve statically, and every emitted statement must
      // remain initial-convertible (a full-width constant assignment to a register) so the
      // extended prologue still lowers into the generated `initial` block
      @tailrec def foldRegion(members: List[ir.DFMember], st: PathState): ir.StepBlock =
        members match
          case Nil       => throw new AbortFusion(bootstrap)
          case m :: rest =>
            m match
              case h: ir.DFConditional.DFIfHeader =>
                val (chain, afterChain) = gatherChain(h, rest)
                @tailrec def select(chain: List[ir.DFConditional.Block]): List[ir.DFMember] =
                  chain match
                    case Nil => Nil // all guards statically false: continue past the chain
                    case cb :: restChain =>
                      cb.guardRef.get match
                        case _: ir.DFMember.Empty.type => cb.members(MemberView.Folded)
                        case guard: ir.DFVal           =>
                          guardConstData(substValue(guard, entryRegs, st, bootstrap)) match
                            case Some(true)  => cb.members(MemberView.Folded)
                            case Some(false) => select(restChain)
                            case None        => throw new AbortFusion(bootstrap)
                        case _ => throw new AbortFusion(bootstrap)
                foldRegion(select(chain) ::: afterChain, st)
              case net: ir.DFNet =>
                val (dcl, rhs) = emitAssign(net, entryRegs, st, bootstrap)
                given ir.MemberGetSet = dfc.getSet
                if (!dcl.modifier.isReg || !rhs.isConst) throw new AbortFusion(bootstrap)
                // register assignments are not readable on the fold path (non-blocking
                // semantics): reads keep resolving through the reset-entry values
                foldRegion(rest, st)
              case g: ir.Goto =>
                g.stepRef.get match
                  case target: ir.StepBlock if target != bootstrap => target
                  case _ => throw new AbortFusion(bootstrap)
              case _: ir.DFVal   => foldRegion(rest, st)
              case _: ir.DFRange => foldRegion(rest, st)
              // text output and anything else cannot move into an RT `initial` block
              case _ => throw new AbortFusion(bootstrap)
      end foldRegion
      val terminal = foldRegion(
        bootstrap.members(MemberView.Folded),
        PathState(Map(), Map(), entryDirty)
      )
    // DropRTProcess initializes the state register to the process's first remaining step, so
    // the fold target must be exactly that step
    val nextFirstStep = pb.members(MemberView.Folded).collectFirst {
      case sb: StepBlock if sb.isRegular && sb != bootstrap => sb
    }
    if (!nextFirstStep.contains(dsn.terminal)) throw new AbortFusion(bootstrap)
    dsn.patch :: subtreeRemovalPatches(List(bootstrap), Set())
  end collectResetSitePatches

  // Drops reset bootstrap states whose dispatch const-folds at the reset entry. A fold that
  // cannot complete leaves the bootstrap state as a real (one-cycle) FSM state.
  private def fuseResetSites(db: DB, fusedSet: Set[StepBlock]): DB =
    if (fusedSet.isEmpty) db
    else
      given MemberGetSet = db.getSet
      given RefGen = RefGen.fromGetSet
      val patchList = db.members.flatMap {
        case pb: ProcessBlock if pb.isInRTDomain =>
          pb.members(MemberView.Folded).collectFirst {
            case sb: StepBlock if sb.isRegular => sb
          }.filter(fusedSet.contains).toList.flatMap { bootstrap =>
            try collectResetSitePatches(pb, bootstrap)
            catch case _: AbortFusion => Nil
          }
        case _ => Nil
      }
      if (patchList.isEmpty) db
      else sweepUnreferenced(db.patch(patchList))
  end fuseResetSites

  /** Fuses the given candidate steps (computed on the nested form) into their jump sites on the
    * flat DB. A candidate whose dispatch cannot be soundly inlined silently falls back to remaining
    * a real FSM state.
    */
  def fuse(flatDB: DB, candidates: List[StepBlock]): DB =
    @tailrec def loop(db: DB, remaining: List[StepBlock]): (DB, List[StepBlock]) =
      given MemberGetSet = db.getSet
      given RefGen = RefGen.fromGetSet
      val valid = remaining.filter(validCandidate)
      if (valid.isEmpty) (db, Nil)
      else
        var victimOpt: Option[StepBlock] = None
        val patchedOpt =
          try Some(db.patch(collectFusionPatches(valid)))
          catch
            case abort: AbortFusion =>
              victimOpt = Some(abort.victim)
              None
        patchedOpt match
          case Some(patched) => (sweepUnreferenced(patched), valid)
          case None          => loop(db, valid.filterNot(_ == victimOpt.get))
    end loop
    val (fusedDB, fused) = loop(flatDB, candidates)
    fuseResetSites(fusedDB, fused.toSet)
  end fuse
end FirstStepFusion
