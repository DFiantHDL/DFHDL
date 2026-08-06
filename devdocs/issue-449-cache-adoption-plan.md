# Issue #449 v2 Plan: Immutable Anonymous Members + Snapshot Sweep

Status: IMPLEMENTED (2026-08-06), with two scope adjustments made during implementation:

- The `DFVal.Alias.AsIs` conversion sites were converted and then REVERTED to their original
  in-place form by user decision, on the strength of this analysis: an in-place REVISION
  (`setMember`), unlike a REMOVAL, cannot leave a ghost refTable value, because same-context
  bindings are re-pointed by `setMember`, cross-context bindings to anonymous members never
  exist (`cloneUnreachable` clones instead), and the revised slot stays in `memberTable` so
  later binds through a stale handle redirect to the current object. The one true ghost
  producer in the whole inventory was `MergeAssocFunc`'s REMOVE. The in-place arms carry an
  audit note saying so. (By the same analysis the `SimplifyFunc` `setMember` arms and the
  DFDecimal carry revisions could not ghost either; their build-new conversion is kept as
  landed, principle hygiene at no cost, with the carry conversion also fixing a real
  token-sharing wart: the old `func.copy` shared its `ownerRef` token between two members.)
- One intended print change: a named binding over a collapsed simplification result now prints
  as an ident line (`val p1: Int <> CONST = p0`) where it previously vanished from the
  printout; `PrintCodeStringSpec."simplify function"` was updated accordingly.

The v1 mechanism (resurrectable removal + gate validation) remains parked in the git stash as
"449 v1 mechanism...". `SubDesignEntry.isSelfContained` is a sanity-level contract for tests and
debugging only. The `ClassDesignCacheSpec` "issue #449" round-trip test is a live regression test
(the `.fail` pin mark was removed), extended with the debris assert. The diagnosis at the bottom
of this file is unchanged and remains the ground truth.

Why v1 was retired: its resurrection machinery (an ignored-slot set, un-ignore-on-bind in
`newRefFor`, a cross-context sweep on every ref bind) is compensation for a decision made at the
wrong time. `MergeAssocFunc` answers the global question "is this value read?" with local,
premature evidence ("is it read RIGHT NOW?"), and everything else exists to repair the cases where
the answer changes later. Deciding once, at the snapshot boundary where the answer is final,
dissolves the problem instead of patching it, and removes a per-bind hot-path lookup.

## The principle, scoped

In OPERATION SIMPLIFICATIONS (arithmetic/logic/casting/conversion), anonymous members are
immutable expression-graph nodes: a simplification never revises (`setMember`/`replaceMember`)
and never removes a member; it builds a new member and returns it, leaving the superseded one as
debris for the snapshot sweep. Revision-in-place is only sound when every binding to the member
is tracked, and bindings are tracked only within one design context (`newRefFor`'s fallback
branch binds cross-context refs without updating the owning context's `refSet`); a revision or
removal can therefore leave refTable values pointing at objects that are no longer (current)
members: ghosts, benign in their own run and fatal across the cache-adoption boundary.

DELIBERATELY OUT OF SCOPE (revision semantics stay): construction protocols and naming flows,
which are pervasive and legitimate: `initForced` revising a `Dcl` during its own construction
window, `setName`/`tag`/`anonymize`, the conditional-expression HEADER retyping in
`DFIf`/`DFMatch` (`replaceMemberWith` as branches unify), `DFRange`'s `stepRef` replacement, and
`ResourceOwner`'s design-block update. A blanket non-anonymous-target guard on
`setMember`/`replaceMember` was considered and REJECTED: the fallout is too broad. Should one of
these flows ever produce a ghost, it surfaces loudly (a dangling-ref crash at `DB.check` /
`SanityCheck.refCheck`'s "Ref exists for a removed member" in debug and spec runs, and the
test-level `isSelfContained` contract); revisit that flow then.

## Phase 1: the endDesign sweep

An elaboration-level `DropUnreferencedAnons`, run in `DesignContext.endDesign` before the
`designMembers`/`getImmutableMemberList` snapshot is taken:

- Drop every anonymous `DFVal` not transitively reachable from the design's roots (the
  non-anonymous members, the statements, the owners). Transitive: dropping a reader orphans its
  own dependency cone, so iterate to a fixpoint (or walk reachability once from the roots, which
  is the same thing stated forward).
- Reachability is computed over `getRefs` of the members, NOT over `refSet`: `refSet` misses
  binds that landed in a nested context's table, reachability does not depend on bind bookkeeping
  at all. Cross-context refs INTO a design's non-global anons do not exist (the
  `getReachableMember`/`cloneUnreachable` invariant clones instead), and globals are already
  reachability-filtered by `buildSubDB.globalsClosure`, so the design-local walk is complete.
- The keep-rules must be THE SAME predicate the `DropUnreferencedAnons` stage uses
  (`DropUnreferenced.scala`). DECIDED: the shared predicate lives in compiler/ir ANALYSIS
  (`DFValAnalysis.scala`, next to the `Ident` extractor it uses), and the stage is refactored to
  consume it so the two can never drift. The stage's criteria, read off the source, are exactly:
  KEEP `DFConditional.Header` (headers can be values), KEEP `Ident(_)` (idents are always kept,
  even unreferenced ones, which also guarantees the `rebindMeta` Idents survive the sweep), KEEP
  a Unit-returning `Func.Call` (a procedural statement, referenced by nothing); DROPPABLE kinds
  when unread: an anonymous `DFVal`, and a `DFRange` (do not forget ranges: a dangling loop
  `DFRange` is a known pre-existing debris species). The "is it read" half is deliberately NOT
  shared: the stage asks `originMembers.isEmpty` on the immutable DB, the sweep computes
  reachability on the snapshot (next bullet); paired comments on both sides.
- Skip duplicate designs (their snapshots are never read). The global context needs no sweep.
- SKIP ENTIRELY when `inMetaProgramming` is true (user decision). This is essential, not merely
  prudent: a stage's MetaDesign builds members that are often unreferenced WITHIN the meta
  context and only gain their readers after the patch lands in the target DB (exactly what the
  `stageCreatesUnrefAnons` StageSpec flag acknowledges), and its refs resolve through the
  injected outer getSet, so a reachability sweep there would delete live work. `SimplifyFunc`
  already sits out meta-programming for the same class of reason.
- Dropped members' refs stay behind in the mutable refTable; that is harmless (same as today's
  ignored members) because `buildSubDB.refsFor` records only the refs that surviving members
  emit, so sub-DBs and cache entries stay clean.

## Phase 2: the troublemakers become purely additive (complete site inventory)

The grep inventory of `setMember`/`replaceMember`/`remove` inside operation simplifications
found exactly three files. Convert each site to build-new (fresh member, FRESH refs to the same
arg targets: reusing the superseded member's ref objects entangles tokens and origin tracking):

**`SimplifyFunc.scala` (DFInt32 arithmetic/logic):**

- `MergeAssocFunc`: mint fresh refs for the absorbed args; DELETE the removal, the
  referenced-elsewhere guard, and the `cloneAnonValueAndDepsHere` path. (The fresh-refs edit is
  reusable from the v1 stash.) This is the #449 trigger.
- `ConstFoldAddSubChain` arm 1: clone-then-`setMember` x2; rebuild instead: fresh Const with the
  folded data + fresh Func referencing `prevLHSArg`. The clone existed only to avoid destructive
  mutation; build-new makes it moot.
- `ConstFoldAddSubChain` arm 2 (Const+Const): `setMember`s the lhs const IN PLACE; build a fresh
  Const (`mkInt32Const` pattern).
- `NegateDecimalConst`: `setMember`s the const's data in place (knowingly risking shared nodes,
  hence its `isAnonymous || inDFCPosition` guard); build a fresh negated Const, drop the guard's
  mutation arm.
- `IdentityOps` `x*0`/`0*x`: `setMember`s the zero const's meta; use `mkInt32Const(0)` as
  `SelfCancelling` already does.
- `rebindMeta` (meta-restamp-by-mutation; used by `MaxMinChainAbsorb`, `IdentityOps` identity
  arms, `SelfCancelling`, `MaxMinWithOffset`, `AdditiveCancellation`). DECIDED (user): never
  restamp; naming goes through an `Ident` wrap. The new rule: when `dfc.getMeta` carries a name,
  wrap the returned value in a named `Ident` (the identity `Alias.AsIs`, as conditional-branch
  wiring already uses); when it is anonymous, return the value AS-IS, no meta update at all
  (prior art: the redundant-cast collapse already returns the inner value without a restamp).
  Implementation notes: the as-is arm means returned members keep their inner positions, so
  check `DFDecimalSpec`'s position tests; the Ident arm also covers a NAMED returned value
  (`val W = max(a, 5)` collapsing to named `a` today silently drops `W` from the printout,
  while an Ident prints `val W = a`, aligning simplified ops with plain value aliasing), so
  audit the print specs and treat any `ref/` delta as intended-or-not case by case. Use the
  existing creation helper `DFVal.Alias.AsIs.ident` (`forceNewAlias = true` + `IdentTag`, which
  also bypasses the AsIs collapse arms by construction).
- **Ident transparency in the simplifications themselves** (VERIFIED: `SimplifyFunc` does NOT
  currently know `Ident(a) == a`, except where it routes through `IntExprCalc`):
  - `IntExprCalc.Calc.strip` already dereferences ANY type-preserving `AsIs`
    (`dt == relVal.dfType`, recursive), so `MaxMinWithOffset` (`constDiff`),
    `IntParamRef.compare`, and the symbolic slice machinery are ident-transparent as-is.
  - Plain `=~` is ident-blind (`Alias.AsIs.prot_=~` matches only same-class members, and named
    idents also differ in `meta`), so THREE sites must strip idents on BOTH sides before
    comparing: `MaxMinChainAbsorb.chainAbsorbs` (`chainFunc.args.exists(_.get =~ other)`; the
    chain args can themselves be named idents, e.g. `max(W, a)` with `val W = max(a, 5)`),
    `SelfCancelling` (both the `-` and the max/min arms; `W - a` must still cancel), and
    `AdditiveCancellation` (the leaf-term `t1 =~ t2` pair search; `collectChain` treats a named
    ident as a leaf, which is fine once leaves are compared stripped).
  - Do it with ONE shared helper: extract `IntExprCalc.strip`'s AsIs rule into compiler/ir
    analysis (type-preserving-alias dereference) and consume it from both `IntExprCalc` and
    `SimplifyFunc`, so the two definitions can never drift.
  - The structural chain matchers (`MergeAssocFunc`'s and `ConstFoldAddSubChain`'s
    `prevFunc: Func` / anonymous-Const patterns) deliberately do NOT strip: a NAMED ident is a
    chain boundary exactly like any named value today (the `isAnonymous` guards), the new
    `rebindMeta` never creates anonymous idents (the as-is arm), and an anonymous ident from any
    other producer would at worst cost a missed fold, never wrong output.
  - Verification additions: `val W = max(a, 5)` followed by `max(W, a)` still absorbs to one
    max, and `W - a` still cancels to 0.

**`DFDecimal.scala` carry promotion (arithmetic):**

- The multi-arg peel (`setMember(func, _.copy(args = func.args.dropRight(1)))`): shrinks the
  merged Func in place to keep member order (inner before carry). Build-new preserves order just
  as well: append a fresh inner Func (fresh refs to the first N-1 arg targets), then a fresh
  binary carry Func referencing it; the original N-arg Func becomes debris. Fold the next site
  into this construction:
- The carry retype (`setMember(carryFunc, _.updateDFType(newDT))`): construct the carry Func
  with the promoted dfType FROM THE START in the peel path; in the no-peel (2-arg) path, build a
  fresh promoted Func with fresh arg refs, original becomes debris. Note the peel is gated
  `!dfc.inMetaProgramming` today ("MutableDB ref tracking is limited"); build-new does not rely
  on ref tracking, but keep the gate initially and revisit separately.

**`DFVal.scala` `Alias.AsIs` (casting/conversion): REVERTED, kept in place (see the status
note).** These sites revise without removing, so they cannot ghost; converting them forced a
naming-protocol addition (`anonymizeInDFCPosition` on the superseded literal, since a NAMED
original is not sweepable debris) for no safety gain. The in-place arms now carry the issue #449
audit note. (The redundant-cast collapse arm was always additive, returning `asIs.relValRef.get`
and abandoning the outer alias as debris, which the sweep now cleans.)

**Phase 1/2 coupling is an empirical question, not a certainty.** The v1 plan assumed
`SanityCheck` rejects unreferenced anons (the `stageCreatesUnrefAnons` StageSpec flag suggests
it), which would force the sweep to land with the producers. But `ConstFoldAddSubChain` and the
redundant-cast collapse ALREADY leave unreferenced-anon debris today and the suite is green, so
either no default-flag spec elaborates those shapes or the sanity check tolerates them. Resolve
at implementation time: land the producers, run `StagesSpec.*`; if debris trips sanity, the
sweep is a prerequisite; either way the sweep is wanted for DB/entry hygiene.

## Phase 3: cache-layer hardening. DESCOPED to sanity level

Always-on gate validation (v1's store-refuse + lookup-miss wiring of `isSelfContained`) is
REJECTED as redundant computation: an O(members + refs) walk plus member hashing on every cache
interaction, defending against states only a DFHDL bug or a dirty dev loop can produce. Post-fix
elaboration cannot create ghosts; entries from other DFHDL builds retire through the code
digest's version fold; uncommitted-edit dev loops are `clearDFHDL` territory; a truncated file is
already a miss via the parse failure ("a corrupt entry is just a miss" stays scoped to parse
failures). `isSelfContained` remains a test/debug-level sanity check, enforced where the project
enforces internal invariants: the `ClassDesignCacheSpec` round-trip test asserts it on every
stored entry of the repro shape.

Two crumbs kept from v1, both zero-cost:

- `cloneForAdoption`'s strict value remap (explicit `Empty` case, then `memberMap(t)` instead of
  the silent keep-as-stored fallback): same lookup, no extra work, and a hypothetical future
  ghost fails AT adoption with a clear provenance instead of as a deferred dangling-ref crash
  three consumers later. Optional but recommended.
- Revisit always-on validation only if shared caches ever land (the "own reproducibility across
  machines" improvement in devdocs/elaboration-caching.md): entries produced by foreign builds
  would make corrupt-entry rejection a genuine user-facing need rather than a dev-loop one.

## Phase 4: verification

- Un-`.fail` the `ClassDesignCacheSpec` pin; it becomes the regression test (it was proven to
  fail against the unfixed code, on the entry `isSelfContained` assert). The v1 "corrupt entry
  self-heals" test was removed with the Phase 3 descoping: the behavior it pinned no longer
  exists by design.
- New sweep tests: a `+`-chain design's stored entry holds no unreferenced anons; an anonymous
  Unit-returning method call survives the sweep; `p + 1 + 1 + 1` folds AND its design passes a
  default-flag StageSpec sanity check (pinning that ConstFold debris no longer reaches the DB).
- Ladder: cache specs alone, then `StagesSpec.*`, then `clearDFHDL` + full suite. The clear is
  mandatory: stale cache entries stored by the pre-change build stay digest-valid under
  uncommitted DFHDL edits, and mixed-era adoption shifts the dclName enumeration (the v1 blast
  radius run demonstrated exactly this via the AES `FullCompileSpec` file-name mismatch).
- Print-output audit: the `rebindMeta` decision may shift positions or naming; run the print
  specs first and treat any `ref/` delta as a decision point (`docExamplesRefUpdate` only for
  intended changes).
- Perf sanity: the sweep is O(members + refs) once per design end (and it removes v1's per-bind
  lookup); compare a StagesSpec wall-clock before/after.

## Open questions

None. All resolved:

- The `rebindMeta` arm choice: DECIDED, see Phase 2 (never restamp; named `dfc` wraps in an
  `Ident`, anonymous `dfc` returns the value as-is).
- The sweep is skipped entirely under `inMetaProgramming`: DECIDED, see Phase 1.
- The shared keep-predicate location: DECIDED, compiler/ir analysis (`DFValAnalysis.scala`),
  consumed by both the sweep and a refactored `DropUnreferencedAnons`; see Phase 1 for the
  extracted criteria.
- Mid-elaboration member-window readers: AUDITED, no action needed up front. The accessors
  (`getMembersNum`/`getMembers`/`getLastMembers`) have test-only consumers (`DFSpec`'s window
  capture, `DFBitsSpec`'s last-consts assert) plus ONE main-source use:
  `r__For_Plugin.designFromDefImpl` scans the body's member window for auto-created
  `DesignParam` members to decide loadability, which anonymous Func/Const debris cannot match,
  so its outcome is unaffected. The tests are expected to keep working with redundant anonymous
  members present; at most, position/count checks over anonymous members may trip and get
  adjusted as they surface during the verification phase.

## Diagnosis (v1 findings; unchanged, the ground truth for this plan)

The crash: `NoSuchElementException: key not found: "TW_..."` from `DB.blockScopeCheck` at
`Design.onCreateEnd`, on a sub-design cache HIT. Not cache invalidation: the hit is legitimate,
and adoption of the entry always crashes. The field's parameter edit only changes the top's code
digest so the DFApp step cache misses and elaboration actually runs; identical re-runs replay the
whole design and never exercise adoption, which is why the bug hid. "Two files required" is
digest separation (one file would invalidate the child too, forcing a live child elaboration).

Verified mechanism chain, for `partial_histogram.lsbitsAt(i * INPUT_BIN_WIDTH + off, INPUT_BIN_WIDTH)`:

1. The offset expression `i*W + off` is built as an anonymous 2-arg `+` Func.
2. Inside `lsbitsAt` (DFBits.scala), `val idxHigh = baseIdx + selWidth - 1` runs FIRST: building
   `(i*W+off) + W` fires `MergeAssocFunc`, whose referenced-elsewhere guard is (correctly, at
   that instant) empty, so it reuses the intermediate's arg refs into a 3-arg Func and removes
   the intermediate (`ignoreMember`: the slot keeps its `memberTable` index).
3. `DFVal.Alias.ApplyRange(lhs, idxHigh, baseIdx)` then references `baseIdx`, the SAME handle (a
   method parameter is a handle; anonymity is about naming, not about reachability from Scala
   code): `newRefFor` binds the alias's relIdx ref and the type's `IntParamRef` TypeRef to the
   removed object. These are the ghost bindings. `List(0)` is immune because `x + 0` folds via
   `IdentityOps`, so no intermediate exists.
4. At store, `buildSubDB.refsFor` resolves those refs (the mutable table holds them) and records
   the ghost as a binding VALUE; the entry is ref-closed over keys but not value-reunited.
5. At adoption, `cloneForAdoption` re-mints tokens pairwise for members-list objects only; its
   `memberMap.getOrElse(t, t)` fallback keeps the ghost, which still emits the STORING run's
   tokens; after re-minting those resolve against nothing, and the first deep ref walk crashes.

Why live runs never notice: the ghost's arg tokens are the very tokens the absorbed 3-arg Func
reuses, so in the storing run's own namespace they resolve fine. Only the adoption boundary is
strict enough to expose the ghost; `SanityCheck.refCheck` reports the same defect as "Ref exists
for a removed member" in debug/spec runs.

Repro seam (deterministic, in-repo): elaborate the issue's two designs twice in one JVM through a
`MapSubDesignCache` (the `SubDesignCacheSpec` pattern); the second elaboration crashes with the
exact field signature. Token forensics: `TW_<posHashHex>_<counterHex>_<id>` with
`grpId = (position.hashCode, per-position JVM counter)`, so in-JVM double elaboration separates
the storing (counter 0) from the loading (counter 1) namespace, which is what proved a stored
token had survived re-minting.
