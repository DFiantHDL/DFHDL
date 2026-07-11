# Sub-Design Caching and the Generalized Design Load Harness

Status (2026-07-11, branch `pure-checks`):

- Phase 1 (pure def cache fix: harness-created params, extended key, capture handling)
  IMPLEMENTED and committed (`0101e0211`).
- Pure-by-default + `PureCheckPhase` (see the Evolution section, which is the authoritative
  current model) IMPLEMENTED and committed (`fa05842af`).
- Param-level data-impurity attribution IMPLEMENTED (see the "Forcing attribution" section
  below for what actually landed, including the two elaboration-dedup fixes it forced).
- Annotation shape (user decision, 2026-07-11): `pure(isPure: Boolean = true,
  impureParams: String*)`. Data-impure params are recorded BY NAME on the DEF's own
  annotation (synthesized `pure(true, "const")`, PRINTED as
  `@hw.annotation.pure(impureParams = "const")`), not on param symbols: it prints at the
  declaration (valuable, self-documenting output), and it covers PHANTOM parameters that
  have no source symbol. A user-written `pure(true, names*)` is a per-param escape hatch:
  declares data-dependence the detection cannot see while keeping the def cacheable
  (detection is then off, as with any explicit marking). IMPLEMENTED.
- `"*"` wildcard (user decision, 2026-07-11): the `toScalaXYZ` family is marked
  `@pure(true, "*")` ("pure given its receiver's data; ALL params data-impure"), replacing
  the interim `forcesConstData` marker entirely: the forcers are now simply the BASE CASE
  of the generic marked-param propagation (their receiver is the extension's first term
  argument, attributed at every application like any marked param; bare references escalate
  through the same generic unapplied-param path). The wildcard also works for users (all
  const params keyed) and at the runtime gate. CONVENTION: a data forcer's forced value
  must be a PARAMETER (extension receiver); a `this`-qualified forcer would escape
  call-site attribution. IMPLEMENTED (forcesConstData deleted).
- Known pre-existing wart (surfaced by the multi-param test): a compound forced expression
  used in place, e.g. `(a + b).toScalaInt` without naming the sum, leaves the derivation
  member as a dangling anonymous value that fails the sanity check (same before
  attribution, under design-level impurity). Fold into the planned forcing errors/warnings
  work: either drop force-only derivations or report them properly.
- LOCKED (user decisions, 2026-07-11), for the next increment:
  - Phantom params/ports creation (making design defs self-contained, as on the `ed-defs`
    branch) belongs in `DesignDefsPhase` (post-Pickler rigging), NOT in PureCheck: rigging
    must not be pickled into TASTy, and DesignDefs already owns the harness. The two phases
    share a capture-discovery helper (ordered out-of-scope value list + deterministic
    phantom naming) so PureCheck can attribute forcings to phantom param names before the
    params exist.
  - Plain Scala captures do NOT become phantom values; they join the cache key through the
    `scalaArgs` list (the rigging appends captured Scala value idents, evaluated in the
    def's rhs scope per call). This closes the per-instance-Scala-data soundness hole for
    design DEFS; design CLASSES get the same treatment in Phase 4's instantiation-gate
    rigging. Residual for detection: captures of METHODS whose results depend on instance
    state cannot be keyed by value and must escalate (PureCheck per-instance-data reads).
- NEXT increments, in order:
  1. Shared capture-discovery helper + phantom params/ports + Scala-capture `scalaArgs`
     extension in DesignDefs (retires the runtime auto-param/`cloneUnreachable` path and
     unhittable entries for def designs; flips the per-instance PureCheckSpec test from
     design-level impurity to a keyed phantom param).
  2. Phase 2: extract the `DesignLoadGate` abstraction from `runFuncWithInputs`.
  3. Phase 3: disk tier for pure def designs (factum CodeRef keys, sub-DB bundles).
  4. Phase 4: class designs (instantiation-gate + body-extraction rigging); includes
     class-ctor-param attribution (currently a forced root at a class param accessor
     conservatively escalates to design-level impurity) and Scala-capture keying for
     class bodies.
  5. Recovery tiers for impure sub-design poison (tracked-effect manifests first).
  6. User documentation for the purity model (docs/), including the "unmarked effects are
     the user's responsibility" contract, the `@pure` overrides (with and without named
     impure params), and the static-dispatch approximation (the analysis never models
     subclass overrides).

## Goal

Two related goals, in increasing order of ambition:

1. Fix `@hw.pure` for def designs: applied design parameters are broken on a cache hit because
   the parameter members are created by running the method body, which a hit skips.
2. Leverage disk caching at the sub-design level, not just the top design as DFApp does today.
   Since applied design parameters are fed through actual class instantiation, a cache hit
   (which must skip the instantiation) needs rigging, like def designs have, with the right
   care for applied parameters. Generalize the mechanism so def designs and class designs
   share one "design load harness".

## Background: how things work today

### Design parameters

- IR node: `ir.DFVal.DesignParam` (`compiler/ir/.../DFMember.scala:467`). Owned by the child
  design. Holds `dfType`, `defaultValRef`, and a secondary-parameter-list `appliedData`
  snapshot that is excluded from equality and serialization, so instances with different
  applied values still unify structurally.
- The applied value lives at the instantiation site: `ir.DFDesignInst.paramMap`
  (`DFMember.scala:1770`), a `ListMap[name -> ref to parent-scope value]`.
- Runtime factory: `core.DFVal.DesignParam.apply` (`core/.../DFVal.scala:705`), which snapshots
  the applied constant data via `getConstDataThroughParams` at creation time.
- Plugin, class designs: `MetaContextPlacerPhase` splits the class template into `paramBody`
  (constructor params turned into `genContainerParam` val defs) and `nonParamBody`, and rewires
  body references from the raw constructor args to the generated params
  (`MetaContextPlacerPhase.scala:87-110`, `218-268`; `CommonPhase.scala:203-213`). It also
  injects `__clsAppliedArgs` (name -> applied value pairs).
- Plugin, def designs: `DesignDefsPhase` turns non-const `<> VAL` args into inputs fetched via
  `designFromDefGetInput(idx)` and prepends `genContainerParam` val defs INSIDE the body for
  `<> CONST` args, wrapping everything in `r__For_Plugin.designFromDef(args, constArgs, meta)(body)`
  (`DesignDefsPhase.scala:40-127`).
- Instance binding: `Design.Inst.collectParamEntries` (`core/.../Design.scala:178`) scans the
  child context's `DesignParam` members (so it must run before `exitOwner`) and matches applied
  values by name from `clsAppliedArgs`, falling back to `unreachableNamedValues` reverse lookup
  for auto-created params (captured outer values, `cloneUnreachable`, `DFVal.scala:1999`).
  `Design.Inst.apply` (`Design.scala:208`) then builds the `DFDesignInst` in the parent context.

### Intra-run deduplication

- `DesignContext.endDesign` (`core/.../MutableDB.scala:188-242`) structurally compares each
  ended design against previously seen designs of the same `dclName` (`=~` over members) and,
  for duplicates, keeps only the public members (ports, params) in the mutable space; these are
  dropped at immutable-DB time. The stage-level `UniqueDesigns` re-establishes this invariant
  after any stage splits designs.
- Applied parameter values are excluded from `=~`, so instances with different params unify
  into one parametrized module; each instance keeps its own `paramMap` and emits its own
  generic/parameter override.

### `@hw.pure` def cache

- `DesignContext.runFuncWithInputs` (`MutableDB.scala:243-262`):
  key = `(dclMeta.position, input DFTypes)`. On hit: sets `current.isDuplicate = true`, returns
  the cached ret, and NEVER runs the body.

### DFApp top-design disk cache

- `DFApp.elaborate` step (`lib/.../app/DFApp.scala:115-139`) over `DiskCache`
  (`internals/.../DiskCache.scala`) backed by factum. Key: `factum.CodeRef(topClass)` (bytecode
  closure digest), `dfhdlVersion`, `defaultRTDomainCfgTag`, `designArgs`. Value: the full
  hierarchical `ir.DB` as JSON (`DB.scala:2076-2108`). On a hit the top constructor thunk is
  never forced.
- The hierarchical DB (`DB.oldToNew`, `DB.scala:1663-1809`) partitions members into
  self-contained per-design sub-DBs (each with its own refTable and a copied globals closure),
  keyed by `StaticRef(designBlock.ownerRef)`. Ref tokens are only meaningful within a sub-DB's
  refTable, which is the property that makes per-sub-design cache artifacts feasible.

## The concrete bugs in `@hw.pure` today

On a pure cache hit in `designFromDef` (`r__For_Plugin.scala:136-178`):

- B1: No `DesignParam` members exist in the fresh shell context (they are created by the
  `genContainerParam` val defs inside the skipped body), so `collectParamEntries` returns
  nothing for the const args. The new `DFDesignInst.paramMap` silently drops the call-site
  applied values; the emitted instance has no generic overrides.
- B2: The cache key excludes plain Scala args, whose values may legitimately shape the
  elaborated structure (e.g. a loop bound); two calls differing only in a Scala arg wrongly
  share one body.
- B3: Auto-created params (non-global captured outer values) cannot be rebound on a hit; the
  reverse lookup in `unreachableNamedValues` finds nothing in the shell context.

Note on applied const values (LOCKED DECISION): they are deliberately NOT a bug in the key and
will stay OUT of it. Under the purity contract, forcing a design parameter's data into
elaboration (the `.toScalaXYZ` family and anything equivalent) is impure by definition, so a
pure body's structure cannot depend on applied values; the body is shared across all
applications and only the per-instance `paramMap` binding differs. For now we assume pure
designs are truly pure; proper errors/warnings for violations are follow-up work (see
Phase 1's impurity-detection note).

UPDATE: the pure-by-default evolution (see the dedicated section below) refines this: applied
values of params STATICALLY MARKED `@impure` (data-forced) DO join the key; all other params
stay out of it, as decided here.

## Design: the generalized load harness

Target invariant, shared by def designs and class designs:

> The public interface of a design (its design parameters, with their fresh applied values,
> and its ports) is created by the HARNESS, outside the body. The body is a skippable thunk.
> A load gate keyed by (code identity, input types, plain Scala args) decides per
> instantiation whether to run the body, reuse an intra-run canonical, or splice a
> disk-cached sub-DB. Code identity is the full `dclMeta` intra-run, plus the class
> `factum.CodeRef` digest for the disk tier. Applied parameter VALUES are never part of the key: purity guarantees
> the body is identical for any application. Instance binding (`DFDesignInst` + `paramMap`)
> always happens with the fresh call-site applied values, regardless of which path produced
> the body.

This is exactly what def designs almost do today (inputs are harness-created via
`designFromDefGetInput`), minus the params (created in-body, hence B1). The phases below first
complete the def-design shape, then port class designs onto it.

### Phase 1: fix the pure def cache (correctness, small scope) — IMPLEMENTED

Implementation notes (2026-07-11): all three bugs were first proven by failing tests in
`compiler/stages/src/test/scala/StagesSpec/PureDesignDefSpec.scala` (each pure test asserts
output identical to its non-pure baseline twin, plus the printed `@hw.annotation.pure` line).
The fix landed as: `designFromDefGetParam(idx)` in `r__For_Plugin` (mirroring
`designFromDefGetInput`) with `DesignDefsPhase` rewiring const-arg references to it and passing
`(name, applied, meta)` const tuples plus the plain Scala arg values; `designFromDef` creates
the `DesignParam` members itself before `runFuncWithInputs`; the cache key became
`(dclMeta, input DFTypes, scalaArgs)`; the cache value became `PureDefEntry(ret,
autoParamEntries, hittable)` where `hittable = autoParamEntries.forall(_.isGlobal)`, an
unhittable entry re-runs the body (structural dedup unifies afterwards), and a hit appends the
stored (global) auto entries to the fresh explicit param entries. `paramEntriesOf` is passed
by-name into `runFuncWithInputs` so collection happens at the right moment on both paths.

1. Hoist param creation out of the body (plugin + core):
   - `DesignDefsPhase`: stop prepending `genContainerParam` val defs into the body. Instead
     rewire const-arg references to a new `designFromDefGetParam(idx)` (mirroring
     `designFromDefGetInput`), and pass the const-arg info (name, applied ident, meta) to
     `designFromDef`.
   - `designFromDef`: after creating the input Dcls, create the `DesignParam` members itself
     (same `genContainerParam` logic, preserving the `getReachableNamedValue` memoization) and
     store them in `current.defParams`. This runs on hit AND miss, so the shell context always
     has the params as public named members with FRESH applied values and fresh `appliedData`
     snapshots. `collectParamEntries` then works unchanged; the `endDesign` duplicate path
     already retains public members and transfers their refs.
2. Extend the cache key: `(dclMeta, input DFTypes, scalaArgs)`. The full `dclMeta` replaces
   today's `dclMeta.position`, aligning the key with the identity that structural dedup
   already uses (`endDesign`'s `sameDesignAs` and `DFDesignBlock.prot_=~` compare the whole
   `dclMeta`). `scalaArgs` are the values of non-DFHDL method parameters (plugin passes them
   through; value equality). Applied const param values are deliberately excluded (see the
   locked decision above): purity guarantees the body is identical for any application, so
   different applied values hit the same entry and share the body, and only the fresh
   `paramMap` binding differs.
3. Captures (B3): record in the cache entry the canonical design's auto-param list with their
   applied values. On lookup, if any auto-param's applied value is non-global, treat as a miss
   (re-run; correctness first). If all are global, rebind them directly into `paramEntries`
   (same elaboration run, object identity is valid).
4. Tests (StagesSpec, new `PureDesignDefSpec`): same-params double call (one design, both
   instances with full paramMaps), different-const-params call (cache HIT, one shared body,
   correct per-instance generics in the printed HDL), plain-Scala-arg variation (cache MISS,
   separate entries), global and non-global capture cases. Follow the verification ladder.

Follow-up (impurity detection, not part of Phase 1): flag when a pure body forces a design
parameter's data during elaboration, e.g. by recording forcing in `protGetConstData` (or at
the `.toScalaXYZ` conversion ops) while a pure body is running, and raise a proper
error/warning. Until then we assume pure designs honor the contract.

### Phase 2: extract the load-gate abstraction

Refactor without behavior change:

- Pull the hit/miss decision, key construction, capture policy, and the "shell context +
  public interface + instance binding" choreography out of `runFuncWithInputs`/`designFromDef`
  into a `DesignLoadGate` in `MutableDB` (or a sibling), with the in-memory pure cache as its
  first backing tier.
- Define the `@hw.pure` contract explicitly (docs + plugin validation): elaboration must be a
  pure function of (code, applied const params, input types, plain Scala args); no file/env
  reads, no reliance on non-global captured state. For class designs (Phase 4), the plugin
  additionally validates that all constructor DFHDL params are `<> CONST` (already enforced)
  and warns about patterns that break the contract where detectable.

### Phase 3: disk tier for pure def designs

Def designs first because the harness already controls the entire Scala surface (no user
object needs to survive a skipped body).

1. Cache service: a small trait in core, e.g.
   `trait SubDesignCache { def lookup(key: SubDesignKey): Option[SubDesignBundle]; def store(...): Unit }`,
   injected through `ElaborationOptions` (default: none, feature off). `lib`/DFApp provides the
   implementation on top of `DiskCache`/factum, adding the cross-run key parts:
   `factum.CodeRef(owner class)` and `dfhdlVersion`. The intra-run key parts are those from
   Phase 1.

   LOCKED DECISION: `ElaborationOptions` are NOT part of the sub-design key. A pure design's
   elaborated content must be option-independent by definition. This holds mechanically for
   the option the top-level cache keys on: `defaultRTDomainCfgTag` is a GLOBAL tag on the root
   DB (`Design.scala:20`) consumed at stage time (`AddClkRst`, `ExplicitClkRstCfg`), so cached
   sub-DB members never embed it and a spliced sub-DB naturally resolves against the current
   run's tag (the top-level steps keep keying on it, unchanged). Any elaboration option found
   to leak into a pure design's elaborated members is a purity violation to surface with a
   proper error/warning (follow-up work), not a key component.

   Leveraging `factum.CodeRef` (verified against the factum sources):
   - `CodeRef(cls)` is per-class already; anchoring it at the sub-design class (instead of the
     top, as DFApp does) gives exactly the wanted granularity: directory-classpath classes are
     digested individually with references followed, so a parent-only change does not
     invalidate an unchanged sub-design.
   - TASTy sibling files are folded in (nested/companion classes share the outermost TASTy),
     which covers DFHDL's inline/macro-expanded code that is invisible in bytecode.
   - Def designs: the runtime thunk is a `LambdaMetafactory` class with no resolvable class
     file (CodeRef degrades to a "missing" identity entry), so the plugin must pass
     `classOf[<method owner>]`; the body compiles into the owner's class file and TASTy. One
     CodeRef cannot distinguish two defs in the same class, so `dclMeta` stays in the key:
     CodeRef identifies the code VERSION, dclMeta identifies WHICH declaration.
   - Why the other key parts are NOT subsumed by CodeRef: CodeRef digests code only and sees
     no runtime information. Input DFTypes and scalaArgs are per-call runtime values that
     legitimately shape the elaborated structure (input Dcls take the APPLIED args' dfTypes,
     so `Bits(8)` vs `Bits(16)` calls share one CodeRef but need distinct entries; a
     param-value-driven input width therefore also misses correctly through the type, without
     param values ever entering the key). `dclMeta`'s content is source-derived, so against
     CodeRef it is a DISAMBIGUATOR (which declaration), not an invalidator; it is required for
     defs sharing an owner class and is the sole code-identity part of the intra-run tier
     (which computes no CodeRefs). For class designs on the disk tier it is redundant but
     free, kept for a uniform key shape.
   - Cost: each CodeRef call re-reads the closure's class files and fully hashes non-repository
     jars. The service must memoize `Class -> CodeRef` per process; a factum-side digest memo
     keyed by (path, mtime, size) would additionally dedup snapshot-jar hashing and shared
     closure prefixes across sub-design classes (small factum enhancement, worth doing there).
   - Re-entrancy: sub-design lookups happen INSIDE the top `elaborate` factum task. Use
     factum's `CodeRef`/`Digest` for keying, but let `SubDesignCache` do plain
     content-addressed file get/put under the same cache directory; nest factum tasks only if
     the evaluator is verified re-entrant.
2. Artifact (`SubDesignBundle`): the design's own sub-DB plus, transitively, its child designs'
   sub-DBs (self-contained, since sub-DBs carry their globals closure and their own refTables),
   plus a public-interface manifest: ordered params (name, dfType, default), ordered input
   ports, output port, and the auto-param/captures record from Phase 1.3 (with global captures
   stored structurally, to be unified by `=~` against the current run's globals).
3. Hit path (elaboration time): the harness creates a fresh block, the params (fresh applied
   values, per Phase 1), the IN Dcls and the out port from the manifest, marks the context as
   "duplicate with external body", and builds the `DFDesignInst` normally. The bundle is
   registered against the block. No member-level ref freshening is needed because the cached
   body is attached as a sub-DB unit, not spliced into the flat mutable space.
4. Attachment (immutable time): `MutableDB.immutable`/`oldToNew` attaches the bundle's sub-DBs
   into the hierarchical DB, rekeying the bundle's top block to the fresh block's `ownerRef`
   (and rebinding the fresh `DFDesignInst.designRef` accordingly; note
   `DFDesignInst.copyWithNewRefs` already leaves `designRef` to the caller by design). The
   fresh public members are dropped exactly like today's duplicate-design public members. Care:
   the fresh block must be `=~` to the bundle's top block (same `dclMeta`, `instMode`) so
   `UniqueDesigns` and `endDesign` grouping keep working; simplest is to reuse the deserialized
   top block with a fresh `ownerRef` and instance meta.
5. Write path: after a successful elaboration, for every pure design that ran live, serialize
   its bundle under its key (the design -> key mapping is recorded at the load gate). Store
   via the cache service; DFApp wires it to the same cache folder as the top-level steps.
6. Mixed intra-run/disk interplay: first instantiation may be a disk hit while a later one is
   an intra-run duplicate of it, and vice versa; the load gate resolves in order: intra-run
   canonical, then disk, then run live.
7. Checks: the post-elaboration `check` still runs over the full DB including spliced parts
   (correctness first; skipping re-checks of cached sub-trees is a later optimization).

### Phase 4: class designs (the real treat)

The blocker: `new SubDesign(args)` runs the whole Scala constructor, and the constructor IS the
elaboration; the parent also needs the resulting object for `sub.port` access. The rigging
brings class designs to the def-design shape:

1. Instantiation-site rewrite: `OnCreateEventsPhase` already rewrites `new C(args)` into
   `(new C(args)).onCreate(...)`. Extend it (or a sibling phase) to lift the argument
   expressions into locals and route the instantiation through
   `r__For_Plugin.instantiateDesign(classOf[C], appliedConstArgs, scalaArgs, () => new C(<locals>))`
   so the load gate can compute the key BEFORE deciding whether to construct.
2. Body extraction (the key mechanism, recommended over `Unsafe.allocateInstance`):
   for `@hw.pure` design classes, `MetaContextPlacerPhase` already separates `paramBody` from
   `nonParamBody`. Move `nonParamBody` statements into a generated `__body(): Unit` method
   (DelayedInit-style): field val defs split into a mutable field declaration in the template
   plus an assignment inside `__body`. The template becomes:
   `paramBody; if (dfc.shouldRunBody) __body()`.
   - On a MISS the constructor behaves as today (params created by `paramBody` with fresh
     applied values, then the body runs).
   - On a HIT the constructor still runs `paramBody` (exactly the "care for applied
     parameters": params and their `DFDesignInst` bindings are always fresh), skips `__body`,
     and a generated `__bindPublic()` assigns every public DFHDL-typed field from the replayed
     public members (looked up by name in the manifest/context). Non-DFHDL fields remain
     default-initialized; the `@hw.pure` contract states the parent may only rely on the
     design's DFHDL surface (document, and have the plugin warn where it can detect
     violations).
3. Scope limits for the first iteration (validated by the plugin, falling back to
   "never disk-hit, always elaborate" rather than erroring where possible):
   - All constructor params are `<> CONST` DFHDL params or key-hashable plain Scala values.
   - No interface/domain container fields initially (containers need the same treatment
     recursively; add in a follow-up).
   - Inheritance: each design class in the hierarchy gets its own `__body` extraction; the
     skip flag is consulted per template. Design traits with member-creating template
     statements are the riskiest corner; if extraction is not feasible for a given shape, the
     class is simply not disk-cacheable.
4. The intra-run benefit falls out for free: a second `new C(sameArgs)` in the same run
   currently re-runs the whole body only for `endDesign` to throw it away; with the gate, a
   pure class skips the body and replays the public interface, which is also a compile-time
   elaboration performance win independent of the disk tier.

### Rollout and safety

- Everything behind an opt-in: `@hw.pure` on the design (existing opt-in semantics) plus an
  elaboration/app option (e.g. `cacheSubDesigns`, default off) for the disk tier.
- Any doubt resolves to "run live": missing key data, non-global captures, unsupported class
  shapes. Running live is always correct because structural dedup handles unification.
- Regression risk concentrates in three places: `DesignDefsPhase`/`designFromDef` reshuffle
  (Phase 1), `oldToNew` attachment (Phase 3.4), and the class body extraction (Phase 4.2).
  Phase 4.2 deserves a standalone spike before committing to the approach.

## Evolution: pure by default and PureCheckPhase

Direction set 2026-07-11 (supersedes the opt-in `@hw.pure` model; Phase 1's harness mechanics
carry over unchanged, only the gate's polarity and key composition evolve).

STATUS: the base model is IMPLEMENTED (2026-07-11). What landed:

- SINGLE annotation `hw.annotation.pure(isPure: Boolean)` (user decision: no separate
  `@impure`): `@pure(false)` marks impure (user-written or PureCheck-synthesized), `@pure` /
  `@pure(true)` is the explicit trust override, absence means pure by default. IR:
  `ir.annotation.Pure(isPure)`; the runtime def-design cache gate skips caching only when
  `Pure(false)` is present in `dclMeta`.
- `PureCheckPhase` (plugin, runsAfter TopAnnot, runsBefore MetaContextPlacer so synthesized
  annotations land in `__clsMeta` and TASTy): global `runOn` fixpoint over all units of the
  run (roots = defs, classes, class/module-owned vals; worklist propagation), synthesizing
  `@pure(false)`. Detection: references to `@pure(false)`-marked symbols (incl. from
  dependencies' TASTy), an FQN blacklist (random/IO/net/time/sys), and outer `var` access.
- The `toScalaXYZ` family carries EXPLICIT `@pure(false)` source annotations in core.
  Empirically verified: export forwarders carry the annotation to the user-facing call
  sites, so annotating the defining methods suffices. (An initial cyclic compile error that
  seemed to forbid these annotations turned out to be the pre-existing zinc incremental
  artifact, not a real typing cycle; clean compilation always works and no code
  restructuring is warranted for it.)
- Implementation pitfalls encoded in the phase (each found by a failing test):
  `This`/`Super` references must not create impurity edges (every member references the
  enclosing class through the implicit `this.dfc`); container-typed instance vals are not
  analysis roots and are never annotated (the child's class marking plus the owner's template
  scan carry the poison; the template scan descends into such vals' rhs directly); the
  immediate-parents inheritance check consults MARKINGS ONLY (user decision: transitivity is
  guaranteed by induction, and symbol-level parents include compiler-added ones like
  `java.io.Serializable` on case classes, which would falsely match the IO blacklist).
- Tests: `PureCheckSpec` (toScala detection through forwarders, outer var, blacklist,
  transitive helpers, `@pure` override wrong-sharing contract, class-inheritance impurity,
  and instance-hierarchy poison propagation).

NOT yet implemented from this section: tracked-effect manifests and user documentation.

### Forcing attribution (param-level data impurity) - IMPLEMENTED

Implementation notes (2026-07-11). The `toScalaXYZ` family carries a new internal marker,
`dfhdl.internals.forcesConstData` (export forwarders carry it to user call sites, like the
`pure` annotation). `PureCheckPhase` no longer treats a marked forcer reference as impure by
itself; instead it attributes the forcing to the forced expression's dataflow roots.
Data-impure params are recorded BY NAME on their def's own annotation,
`pure(true, impureParams*)` (see the Status block for why), which prints at the declaration:

- CONST param of an enclosing design def: the def gets a synthesized `pure(true, <name>)`;
  the design def stays pure. The runtime harness (`designFromDef`) matches `constArgs` by
  name against the dclMeta annotation and keys the cache on the named params' applied
  (dfType, data), read from the created `DesignParam.appliedData` snapshot (the dfType
  covers width-generic params whose TYPE would otherwise escape the key). Unknown applied
  data makes that call uncacheable (runs live; structural dedup unifies).
- `<> VAL` input or plain Scala arg of a design def: fully pure (types/values already keyed;
  forcing a non-constant's data is impossible, so a VAL root implies a type-derived
  constant, e.g. `value.width.toScalaInt` - this made both `prioEnc` `@pure` overrides
  removable, and they were removed).
- Param of any OTHER same-run def (helpers, `@inline`-hinted defs like `prioEnc`, local
  defs): the param is recorded the same way on its def, and every APPLICATION of it
  re-attributes the applied argument in the caller's context, so forcing propagates to its
  true root across helper layers and across compilations (the def annotation persists to
  TASTy; call sites match applied args by param name). Same-run ordering is handled by
  recorded edges (param -> pending call-site attribution thunks) fired when a param becomes
  marked. Anonymous functions are excluded (their call sites are not attributable); bare
  method references (closures/eta-expansion) and partial applications of markable params
  escalate conservatively.
- Immutable vals (locals, sibling class members, globals): traced through their definitions
  (a global `sym -> rhs` map collected over all units), so code-determined captures (e.g. a
  literal-initialized design-local constant) attribute as PURE. NOTE: resolution is STATIC,
  like the rest of the phase; subclass val overrides are not modeled.
- Trusted library symbols (fields included, e.g. the implicit `dfc`), the dfhdl root
  package's export forwarders, Scala core value ops, and literals are data-transparent;
  `this.x` defers to `x`'s own rule (the instance link is not data). Implicit/contextual
  arguments are skipped unless DFHDL-valued.
- Everything else (per-instance data such as Scala ctor params via captured vals, mutable
  state, opaque user methods, unknown shapes): design-level impurity, as before.
- An explicit design-def marking wins: `@pure` (trust) suppresses param attribution
  entirely (documented wrong-sharing contract preserved), `@pure(false)` makes it moot.

Two elaboration-dedup fixes were forced by data-keyed entries (both in `MutableDB`):

1. `endDesign` on a pure cache hit used to join the FIRST group of the design's dclName
   unconditionally; with per-data groups it must join the CANONICAL entry's group, so
   `PureDefEntry` now records the canonical `DFDesignBlock` and the hit context records
   `duplicateOf` for `endDesign` to match groups by identity.
2. `sameDesignAs` compared member lists in which child designs appear only as HEADERS
   (equal `dclMeta`), so parents of same-dclMeta-but-diverged children (different forced
   data) wrongly unified; corresponding child design blocks must now also belong to the
   same body group. (The stage-level `UniqueDesigns` is immune: its `DFDesignInst.designRef`
   is a `StaticRef` sub-DB key, distinct for diverged children.)

Tests: `PureCheckSpec` covers param-rooted forcing (separate bodies per applied value plus
a repeated-value cache hit), per-instance-data-rooted forcing (design-level impurity),
input-type-rooted forcing (fully pure, shared body), and nested design-def propagation
(`outer` passing its own param into `inner`'s forced param, both pure, both keyed).
`PrioEncSpec` passes with no `prioEnc` annotations.

1. Model: designs and design defs are PURE BY DEFAULT. A single argument-less `hw.impure`
   annotation marks impurity, written by users or synthesized by the compiler. Documented
   stance: undetectable effects left unmarked are the user's responsibility (future work may
   adopt Scala Capture Checking). A manual `@pure` override wins locally, as the trusted
   escape hatch for over-approximation false positives.
2. `PureCheckPhase` (new plugin phase): transitively synthesizes `@impure` on
   designs/methods/params from direct dependencies' saved annotations only, so there is no
   deep nesting exploration; compiled sources already carry their summaries. HARD CONSTRAINT:
   the phase must run BEFORE Pickler so synthesized annotations persist to TASTy/bytecode,
   which is what makes the transitive check incremental.
3. Impurity sources: an FQN blacklist for known-impure code we cannot annotate (java.io,
   java.nio.file, scala.io, scala.util.Random, java.util.Random, System, java.time, ...);
   the `toScalaXYZ` family (see taint attribution below); reads of outer-scope `var`s and
   mutable collections (syntactically detectable, the most common accidental impurity);
   explicit user `@impure`.
4. Two taints under one annotation, treated differently:
   - DATA-IMPURITY (`toScalaXYZ` as a taint PROPAGATOR, attributed to its root):
     design-param root: mark THAT param `@impure`; the design stays cacheable and the key
     gains that param's applied data. Input/type-derived root (e.g. prioEnc's
     `value.width.toScalaInt`): already covered by the input-DFTypes key component, stays
     pure. Global-const root: code-determined (CodeRef/dclMeta), stays pure. Unattributable
     (taint escaped through a var, complex flow): conservatively mark ALL params impure,
     which keeps the design cacheable with a fat key rather than poisoning it.
   - EFFECT-IMPURITY (blacklist, var reads, user-marked): design-level `@impure`; the body
     always runs (no cache skip); structural dedup still applies as today for non-pure.
5. Key becomes: (dclMeta, input DFTypes, scalaArgs, impure-params applied data). When an
   impure param's data is unknown during elaboration (symbolic in the parent's params), that
   instance misses; the parent's own cache entry contains the cost, so nothing is redundantly
   recached. Param impurity stops at its hierarchy level: upward, params stay pure unless the
   parent itself forces.
6. Poison containment and recovery (effect-impure sub-design poisons ancestors' cacheability):
   - Cost containment first: with per-level caching, a re-running ancestor's pure children
     still hit their own entries, so poison costs only the ancestors' own glue code.
   - Tier 1, tracked effects: blessed elaboration APIs (file reads for ROM init, config)
     register their reads; cache entries carry a dependency manifest (path + content hash,
     factum FileRef/Task.source style), re-validated at lookup and aggregated upward through
     ancestors' entries. A declared effect then poisons nothing: the design is pure given D.
   - Tier 2, refactor guidance (documented): hoist effects upward and pass results as
     parameters, converting effect-impurity into keyable param data-impurity.
   - Tier 3, hole punching (later): cache the parent with the impure child as a hole,
     re-instantiating the child from stored (class, applied param data) with an
     interface-stability check; mismatch invalidates the parent entry. Requires
     data-reconstructible ctor args; hold until tiers 1-2 prove insufficient.
   - A design whose structure is genuinely nondeterministic is correctly uncacheable.
7. Migration: `hw.pure` deprecated/removed; lib `prioEnc` drops its annotations; printers emit
   `@hw.annotation.impure`; PrioEncSpec and reference outputs updated; the pure-def cache gate
   applies to ALL unannotated defs by default.
8. Open design detail: attribution across class hierarchies (a subclass body forcing a
   base-class ctor param). Possibly synthesize a design-level annotation listing impure param
   names (e.g. `@impureParams("p")`) on the forcing class instead of cross-class param-symbol
   surgery.

## Decision points

RESOLVED: applied const param values stay out of the cache key. Purity means the body cannot
depend on them (`.toScalaXYZ` and equivalents are impure by definition); violations get proper
errors/warnings as follow-up work.

1. Capture policy (Phase 1.3): non-global captures always miss (recommended) vs including
   capture identities in the key.
2. Class rigging mechanism (Phase 4.2): body extraction into `__body()` (recommended; keeps
   param creation live and mirrors the def harness) vs `Unsafe.allocateInstance` + full field
   binding (no template surgery, but bypasses constructor invariants entirely).
3. Where the disk tier hooks in: `ElaborationOptions`-injected service (recommended; core stays
   free of factum/DiskCache dependencies) vs a core-level dependency.

## Test plan

- Phase 1: new `PureDesignDefSpec` in StagesSpec covering B1/B2/B3 scenarios; run the
  verification ladder (individual spec, then `StagesSpec.*`, then full `test`).
- Phase 3: round-trip test that elaborates a hierarchy, stores bundles, re-elaborates with a
  fabricated cache hit, and asserts the final DB is `=~`-identical to a live elaboration
  (including paramMaps and generics in printed HDL). Cross-run test through DFApp with
  `--cache`, mirroring the existing top-level cache tests if any.
- Phase 4: same DB-equivalence test for class designs; plus parent-access patterns (port
  connect via by-name select, param read) on a hit; plus a negative test that an unsupported
  shape falls back to live elaboration.
