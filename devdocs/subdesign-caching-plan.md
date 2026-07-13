# Sub-Design Caching and the Generalized Design Load Harness

## INCREMENT 3 LANDED (2026-07-13): COMPILE-TIME CODE DIGEST, AND THE TIER IS ON BY DEFAULT

`ElaborationOptions.CacheEnable` now defaults to TRUE. Keying an entry was the blocker (a
`factum.CodeRef` digest walked the declaring class's whole code closure at runtime: 7s -> 107s on
StagesSpec, 26s even with factum memoized). The compiler now records what the digest needs, and a
run only folds those records: StagesSpec is back to 5s, the tier-off baseline, with the cross-run
tier ON.

- `CodeDigestPhase` (plugin, right after `PureCheck`, before the DFHDL rewrites) writes, beside each
  top-level class it compiles, a `<pkg>/<Cls>.dfdigest` record: `own` (a hash of the class's TYPED
  TREE, so it is position-insensitive and free of the absolute paths the meta-context phases plant
  later) and `dep` lines (the top-level classes the code actually REACHES; typed trees, so not every
  class the bytecode mentions). Synthetic top-level classes are included deliberately: a file's
  top-level design defs live in `<file>$package$`, which is the anchor of their entries.
- `dfhdl.internals.CodeDigest` (runtime) composes a design's digest by folding those records over
  the transitive closure. COMPOSITION STAYS AT RUNTIME: a compile-time composed digest would go
  stale exactly where zinc does not recompile (a helper changes, its dependents keep their class
  files, their composed digests keep describing the old helper).
- THE PLUGIN IS PART OF THE KEY (`pluginStamp`, folded into every `own`). The plugin is what a
  design's code MEANS, and it is invisible to the class closure: a plugin change need not recompile
  a single DFHDL runtime class. Without this an entry produced by an older plugin stays "valid" and
  is adopted (this actually happened: the domain fix below was masked for hours by poisoned entries
  that no key retired). Stamped BY CONTENT (the jar's class entries, not its path/mtime): this build
  republishes the plugin jar under a fresh timestamped name every sbt session.
- TWO BOUNDARIES keep the closure small. A jar folds to its own identity (jar code cannot reach
  build output). And DFHDL folds to its VERSION and is never scanned: that is what the library is to
  every real user (a released jar), and a development build now gets the same treatment, so editing
  DFHDL's own modules invalidates entries through the version rather than through a class-file walk.
- The DFHDL VERSION is no longer a separate key component; it IS the library's identity inside the
  digest.

TWO REAL BUGS the tier's first genuine cross-run hits exposed (both were invisible while the tier
was off, and both are now covered by tests):

1. ADOPTION KEPT THE STORING RUN'S TOKENS (the `No owner found` / StackOverflow failures).
   `cloneForAdoption` re-anchored the design block on a token minted by the LOADING run while the
   entry still held the STORING run's tokens. Ref generators restart per run and both runs elaborate
   the same code, so the minted token routinely EQUALS one the entry already uses internally: the two
   bindings (the block's owner, which resolves to nothing, and a port's owner, which resolves to the
   design) collapse onto one map key and the port loses its owner. Re-minting and re-anchoring are
   now ONE pass, so no stored token ever shares the table with a minted one. This replaces the
   flat-view collision machinery (`freshenLocalRefs` / `collisionFreeSubDBs`), which papered over the
   corruption downstream and is deleted: sub-DBs are self-contained, and the flat view merges them
   with no collisions to resolve.
2. DOMAIN VALS WERE GUARDED BY THE CLASS BODY-SKIP. A domain is public surface just like an
   interface (`child.dmn.o <> ...`), so a skipped body left a null field. Domains are now unguarded
   interface statements. (Latent since the class rigging landed; only a cache hit on a design's FIRST
   instantiation could expose it.)

Cross-run adoption is now exercised end to end: the whole suite passes twice in a row, the second
pass adopting every entry the first stored (core 89, compiler_stages 561, lib 167, platforms 1;
ips's GHDL-127 vga case is a known local env failure).

## INCREMENT 2 LANDED (2026-07-13): CLASS-DESIGN BODY SKIP + SERVICE CACHING

Class designs now go through the gate the same way design defs do, body skip included.

- `DesignClsSkipPhase` (plugin, LAST phase: `runsAfter` OnCreateEvents AND MetaContextDelegate,
  since a meta-context pass reads a val's rhs to name the value it creates and would not look
  through a guard). Per named, non-abstract design class it:
  - LIFTS the applied design parameters out of the body: the `genContainerParam` val defs
    `MetaContextPlacer` generates become `__clsGetParam(cls, idx)` fetches, and their (applied,
    default, meta) arguments are passed to the gate call, which creates the parameter members
    itself. This is the plan's target invariant (the harness owns the public interface, the body is
    a skippable thunk) and it is REQUIRED: parameter creation plants members of its own, so a gate
    that merely observed the body's parameters could not tell its context apart from one where a
    base class's body had already run.
  - guards every body statement with the gate's decision (`if (__clsSkipBody)`), EXCEPT the
    public-interface declarations, which always run: port and constant declarations, interface
    vals, and plain Scala vals (widths and the like). A val's guarded rhs becomes its type's zero.
  - refuses to guard a class that captures a DFHDL value from an enclosing design (such a capture
    materializes an auto-created parameter INSIDE the body, `cloneUnreachable`, which a skipped body
    would not create) or whose interface declarations read a guarded val.
- `Design.__clsBodyGate(bodyClass, params, skippable, hasBody)` (core): creates the parameters, then
  decides. It stands down for a top design, for a BASE class's body (`Design.dclClassOf` resolves
  the leaf declaring class through the plugin's anonymous instantiation wrapper), and once any class
  in the chain has run its body live (`clsBodyRanLive`), so a design never ends up holding half a
  body. The key is the same `DesignLoadKey.designClsKeyWith` used at design end, computed once and
  reused there (`DesignContext.clsLoadKey`).
- A body that creates design parameters of its own (a capture the static analysis missed) is
  detected at design end (`clsGateParamNum`) and makes the design unstorable, so no later run's gate
  can skip a body that has parameters to create.
- INTRA-RUN skip is now the default behavior for every design class and needs no cache service: two
  instantiations of the same key elaborate ONE body. This is the bulk of the win.
- `StagesSpec.ClassDesignCacheSpec`: forest adoption through a class hierarchy, one-body-per-key
  intra-run, and per-instance applied values on a cached parametrized class. Bodies are counted
  through a Java atomic: a Scala `var` write the purity analysis can see (even a few calls deep)
  makes the design impure and unkeyable.
- `CacheEnable` STAYS OFF BY DEFAULT (the intra-run tier is unconditional and free). The blocker is
  the CACHE KEY, not the cache: keying an entry needs the declaring class's code digest, and
  `factum.CodeRef` walks that class's whole code closure. Every design class is its own anchor, so
  the suite pays it per class. MEASURED on StagesSpec (503 tests): 7s with the tier off, ~107s with
  it on.
- FACTUM WAS MADE MUCH CHEAPER (kept — it is strictly better — but NOT enough to default the tier
  on). Three memos, all correctness-preserving, since the digest still composes at traversal time
  from CURRENT file stamps (so an upstream class rebuilt without its dependents — zinc's normal
  behavior — still invalidates the dependents' entries):
  - per-file content digest + reference list, keyed by file identity (path, size, mtime): a file's
    bytes are read and hashed ONCE per process, a rebuilt file is re-read. This alone barely helped
    (1.5s -> 1.4s per class): the bytes were never the bottleneck.
  - per-class-loader resource LOCATION memo: the real cost was thousands of `loader.getResource`
    classpath scans per traversal (1.4s -> 210ms per class).
  - `Config.epoch` (opt-in token): a window in which the caller guarantees no recompilation, so a
    class is stamped once per window instead of once per `CodeRef`. `SubDesignDiskCache` is created
    per elaboration and passes ITSELF as the epoch.
  Net: StagesSpec with the tier on went 107s -> 26s. STILL 4x the 7s baseline, and rejected as a
  default (user, 2026-07-13): ~200ms per elaboration + ~2.6s cold is fine for a DFApp run and
  unacceptable for a suite of hundreds of tiny elaborations. `CodeRef` was built for DFApp's
  one-digest-per-process use and does not survive being called per design class.
  STATUS: the factum memos are IMPLEMENTED AND TESTED but NOT RELEASED, and `build.sbt` stays on
  factum `0.2.0`. They are parked in the factum working tree (`CodeRef.scala`, `CodeRefSpec.scala`)
  until a release is cut; nothing here depends on them, since the tier is off by default.
- NEXT (the only path to defaulting the tier on): COMPILE-TIME RIGGING of the code digest. The
  plugin emits, per class, a small record `{own: <hash of this class's own code>, deps: [class
  names]}` beside the class file; the runtime composes a design's digest by folding `own` hashes
  over the transitive `deps`, reading only records. Composition MUST stay at runtime — emitting a
  COMPOSED digest at compile time reintroduces the zinc hole (a helper's body changes, zinc does not
  recompile its dependents, their composed digests stay stale). Wins beyond speed: the plugin reads
  typed trees, so `deps` is what the design BODY actually reaches (not every class the bytecode
  mentions) and everything outside plugin-compiled code folds coarsely, collapsing the closure from
  ~15k classes to the user's own handful; and `own` can be a position-insensitive structural hash,
  so reformatting a file stops invalidating every cached sub-design. Risks: `own` must be
  byte-reproducible across machines/JVMs (needs a compile-twice determinism test), non-plugin code
  needs the existing coarse fallback, and reflection/dynamic dispatch stay blind spots exactly as
  today.

## OPEN ISSUES AND REMAINING WORK (2026-07-12, AUTHORITATIVE; supersedes every
## scattered NEXT/REMAINING list below)

State: steps 1-3 committed or ready (def designs fully cached: intra-run + disk
service); step 4 (class designs keyed through the gate, key-based unification primary,
structural `=~` scoped to a keyless fallback) implemented and green, uncommitted.

OPEN ISSUES (correctness/design gaps, largest first):

1. CONSERVATIVE IMPURITY ESCALATION ON BODY-LOCALS (the `mulByte` case, pre-existing):
   forcing whose dataflow passes through a value the analysis cannot trace to a
   param/capture/static (a lambda parameter, a pattern binding, an anonfun-computed
   local) escalates to design-level `pure(false)` instead of a param marking. Since
   anonymous functions cannot carry param markings (their application sites are
   unknowable), any forced expression mixing a keyable root WITH such a local kills the
   whole def. Consequence: such designs are keyless, never unify (one enumerated
   design per instantiation), and can never disk-cache. The escape hatch is a
   user-written `@pure(true, <names>)` declaring the data-impure params explicitly:
   APPLIED to AES's `mulByte` (`@pure(true, "lhs")`, the canonical example; user
   decision 2026-07-12). Automatic-attribution fix directions, in increasing power:
   (a) pattern-bind tracing: attribute a `Case` binding through its match selector;
   (b) trusted-HOF data-flow: for scala-collection combinators (foldLeft/map/...) over
       code-determined collections (literal ranges), treat the lambda's element/acc
       params as code-determined.
2. RESOLVED BY USER DECISION (2026-07-12): NO runtime structural deduplication AT ALL.
   Designs unify ONLY through the design load gate's key (the key information
   differentiates designs even when caching is disabled); keyless (impure/unloadable)
   designs emit one dclName-enumerated design per instantiation, and only the dclName
   RENAMING iterates over same-name groups. The `=~` fallback briefly added after the
   AES explosion (14 -> 595 sub-designs when mulByte was keyless) is DELETED; the AES
   fix is the explicit `@pure(true, "lhs")` marking on `mulByte` (issue 1). The cost:
   an unannotated impure design instantiated N times emits N identical designs; the
   remedy is declaring purity/impure-params, not structural comparison.
3. ADOPTED-CHILD DCLNAME CLASHES (service tier): an adopted (disk-loaded) forest's
   CHILD designs keep their stored dclNames; a native same-name design in the loading
   run is not uniquified against them. Fix: seed `uniqueDesigns` with adopted children
   or re-uniquify at assembly.
4. KEY OVER-APPROXIMATION (accepted, document): two pure instantiations whose keys
   differ but whose bodies happen to be identical (e.g. a Scala arg that does not shape
   structure) now emit two designs where `=~` previously unified them.
5. RESIDUAL KEYING HOLES (accepted, document): (a) abstract Scala vals overridden by
   anonymous subclasses are keyed only when declared in the leading paramBody section;
   (b) an INTERFACE template's Scala captures do not reach the instantiating design's
   key; (c) a forced-only class capture (never materialized as an auto-param) makes the
   class unloadable (strict name resolution: safe, conservative).
6. DEF SERVICE-HIT RETURN TYPES REFERENCING GLOBALS (untested gap): a cached
   `subDesignRetDFType` carrying refs to the cached run's globals would embed
   unresolvable tokens in the fresh out port.
7. PRE-EXISTING PLUGIN LIMITS (surfaced by ClassDesignKeySpec work, independent of the
   gate): instantiating a design inside a design-level `for` comprehension crashes the
   loop transform (ExplicitOuter path assertion); `Range.foreach` at design level is
   DFRange-reserved. Both deserve proper compiler errors (or support).
8. PRE-EXISTING WART: in-place compound forced expressions (`(a + b).toScalaInt`
   unnamed) leave a dangling anonymous member that fails the sanity check; fold into
   the forcing errors/warnings work.

REMAINING WORK, in order:

1. Commit step 4 (class keying + keyless fallback + PureCheck class attribution + key
   normalization + ClassDesignKeySpec).
2. Increment 2 - CLASS BODY-SKIP + SERVICE CACHING (design recorded in step 4 notes
   below): call-site gate decision fed by the plugin from `transformApply`'s lifted
   ctor args (mid-constructor keying is impossible: leaf fields are uninitialized
   during a base template's run, so instance methods are unusable on a hit);
   plugin-guarded template statements (DFVal port/const val decls always run,
   re-creating the public interface against fresh params; container-typed vals or
   unsafe port dependencies make a class non-skippable; capture-carrying classes never
   skip); service adoption at design end as today; the store side already works
   (`completed`/`buildDesignForestDB` are design-agnostic).
3. Cache rollout: default `ElaborationOptions.CacheEnable` from `AppOptions.cacheEnable`
   in DFApp flows; enable the service tier for classes; flip the global default once
   proven.
4. PureCheck attribution strengthening (issue 1) -> retire the structural fallback.
5. Recovery tiers for impure sub-design poison (tracked-effect manifests).
6. User documentation: the purity model, `@pure` overrides (with and without named
   impure params), the unmarked-effects contract, the static-dispatch approximation,
   and the key over-approximation semantics.

## RESEQUENCED ROADMAP (user decision, 2026-07-12; supersedes the NEXT list below)

The caching mechanism is REMOVED for now and returns only after the DB substrate is
restructured. The three steps:

1. DONE: drop ALL elaboration caching (intra-run pure-def cache AND the sub-design cache
   service tiers) and stay green. Deleted: `DesignLoadGate` (Key/Entry/intraRunCache/
   externalShells/finalize), `DesignLoadResult`, `MutableDB.subDesignCache`,
   `DesignContext.isDuplicate`/`duplicateOf` (cache-hit group joining),
   `Design.finalizeCachedSubDesigns`. STASHED for step 3 (git stash "subdesign-cache
   service layer (resurrect at step 3)"): `SubDesignDiskCache.scala` (trait + disk
   service), `SubDesignCacheSpec.scala`, the `ElaborationOptions.CacheEnable` diff, and
   the `DB.scala` diff (extractSubDesignDB/attachExternalSubDesigns/subDesignRetDFType).
   KEPT: all purity/self-containment rigging (PureCheckPhase attribution, phantom
   capture rigging, PhantomTag, local def-dcl printing, harness-created params/inputs),
   and `designFromDef` keeps its full signature (`scalaArgs`/`ownerClass` inert) so the
   plugin stays untouched. Every def call now elaborates live; the pre-existing
   structural dedup unifies identical bodies, so printed output is unchanged (one test
   updated: PureCheckSpec's @pure-override test now expects two distinct folded designs
   instead of trusted-cache body sharing).
2. Hierarchical DB by construction (no caching involved): at `endDesign` each
   DesignContext finalizes into its own immutable SELF-CONTAINED sub-DB (own members via
   the same materialization structural dedup already performs, child sub-DBs stashed at
   their ends, globals closure) and LOCKS (post-end mutation asserts). First landed
   alongside the flat model with an equality test (stashed sub-DBs == oldToNew's
   sub-DBs) across the suite; then `getDB` assembles from the stashed sub-DBs, dclName
   uniquification moves to assembly time, and the flat->hierarchical `oldToNew`
   conversion retires. Known post-end mutator to relocate: dclName dedup renaming
   (revises child design headers inside ended parents at `immutable` time).
   - STEP 2 VALIDATION LANDED (2026-07-12): the mutable model is ALREADY
     semi-hierarchical: `endDesign` freezes each design's member list into
     `DesignContext.designMembers` (post-end member mutation through the normal context
     path throws, since the ended context's tables are discarded), and the flat
     `immutable` merely flattens that per-design structure for `oldToNew` to
     re-partition. `MutableDB.hierarchical` now assembles the root DB DIRECTLY from the
     `designMembers` snapshots: per-design locals (child blocks and globals excluded),
     parent-to-children claims from a snapshot-order scan (identical to the flat
     first-inst order), whole-run fixes applied per member (`designDedupMaps` extracted
     and shared with `immutable`: dclName renaming + duplicate canonicalization;
     constrained domain owners/dcls; inst unification; global-ctx cleanup), and the
     `oldToNew`-mirrored globals closure + refsFor + orphan-global anchoring over the
     fixed refTable (sourced from the memoized `immutable` during validation).
     `MutableDB.verifyHierarchicalConstruction()` compares against `immutable.oldToNew`
     piecewise and is SOAKED SUITE-WIDE: wired into `StageSpec.assertCodeString(dsn,*)`
     and lib `DesignSpec.assertCodeString` (removed at flip time).
     HierarchicalConstructionSpec adds targeted cases (dedup + renamed groups, design
     defs with phantoms/globals, nested grandchild claims, design insts inside RT
     domains). StagesSpec 495/495 green with the soak active.
   - HARD CONSTRAINT (user, 2026-07-12): `hierarchical` MUST NOT source ANYTHING from
     the flat `immutable` DB. Corrected: it now derives everything from the mutable
     model directly: the merged run refTable (`DesignContext.current`'s refTable, with
     the whole-run fixes applied PER REF TARGET at resolution: inst unification,
     dclName-renamed/constrained owners, constrained dcls, sub-DB-top ownerRef ->
     Empty), and the globals order from the top-level context list (globals are only
     ever injected there; global-ctx cleanup applies at emission). The flat refTable's
     sweeps (unused TypeRefs, redundant dup refs, orphan OneWay.Gen) need no
     counterpart: `refsFor` only collects refs EMITTED by live members, so swept
     entries are never queried. The flat path remains as the soak REFERENCE ONLY.
   - END STATE (user, 2026-07-12): `immutable` ITSELF becomes the hierarchical DB (the
     by-construction assembly), retiring the flat form entirely.
   - FLIP LANDED (2026-07-12): `immutable` = meta-programming flat context view (the
     patch system consumes a meta-design's DB as a flat member container, unchanged) OR
     the hierarchical assembly (now `private def hierarchical`, with the
     `clearDesignInstCache` side effect relocated into its `build`). The flat
     build (`getFlattenedMemberList` + the fix passes over the flat list) and
     `verifyHierarchicalConstruction` + the test-base soak hooks are DELETED. `getDB` =
     `immutable` directly; the top-end check is `designDB.check`; `latchesCheck` takes
     the root as-is. All other `oldToNew` call sites are naturally identity on a root
     DB (kept where they defend old-style deserialized DBs). `getDBOld` semantics
     unchanged (its callers are meta contexts). One genuine flat consumer rewritten:
     `SimulationAPI.instSegment` now enumerates sibling instances per sub-DB (within a
     sub-DB all instances share the parent design, so the same-parent check is
     implied). HierarchicalConstructionSpec repurposed: asserts `getDB.isRoot` +
     `sanityCheck` over the representative shapes. Commits: `3a7ebb264` = the
     validation-phase assembly (MutableDB only); flip uncommitted pending green +
     review.
   - MID-RUN `designDB` SEMANTICS (found by CoreSpec DFSpec failures): the hierarchical
     DB only exists by construction once the design tree is COMPLETE (all contexts
     ended: `DesignContext.stack.isEmpty`). DURING elaboration (open contexts, e.g. the
     DFSpec test base that enters an owner and prints mid-run, and mid-run error
     printing), `immutable` serves the current subtree's FLAT snapshot
     (`currentContextDB(flatten = true)`: current context member list with ended child
     designs expanded through their snapshots + context refTable + cleanup, NO
     whole-run fixes); meta-programming serves the same view unflattened. Only a
     complete tree yields the hierarchical root.
3. Reintroduce caching on the self-sustained substrate (pop the stash): the agreed
   simplified design; a single per-run map `Key -> design` for intra-run unification
   (which then replaces the structural dedup mechanism), store = `service.store(key,
   subDB)` right at `endDesign` (sub-DBs now exist at design end), hit = the shell
   context ADOPTS the cached forest as its sub-DB inline (no deferred attach/finalize
   choreography), return DFType always read from the out port of the DB/design at hand.
   - STEP 3 IMPLEMENTED (2026-07-12, uncommitted). The gate (`MutableDB.DesignLoadGate`)
     is now: `Key(dclMeta, inputTypes, scalaArgs, impureParamsKey)`; `canonicalOf:
     Map[Key, DFDesignBlock]` (the ONLY intra-run state: no Entry, no DesignLoadResult;
     retDFType read from the canonical's snapshot out port, or from the cached DB's out
     port via `subDesignRetDFType`); `designFromDef` orchestrates directly (keyOpt ->
     lookup -> skip or run -> completed). Intra-run hit = ctx.duplicateOf joining (the
     restored group-join mechanics). Service hit = shell ctx.isDuplicate
     (self-canonical), forest token-freshened AT HIT TIME (`DB.freshenSubDesignForest`,
     using dfc.refGen; top adopts the shell token, children get genOneWay tokens,
     designRefs remapped; top HEADER stays a placeholder), recorded in `adopted`
     (StaticRef-keyed; localKey doubles as the =~-dedup guard identity via
     `adoptionIdOf`), and seeds `canonicalOf`. FINAL ASSEMBLY (`hierarchical.build`)
     dispatches: adopted forest emitted as the shell's content with the placeholder
     header replaced BY EQUALITY (JSON round trips deserialize equal-but-distinct
     instances; `eq` misses them) by the shell's final dclName-uniquified block +
     loading-run globalTags; native designs build from snapshots as before.
     STORE AT END: `completed` (clean live runs only; body-created DesignParams are
     unloadable) builds the artifact via `buildDesignForestDB`: the design's
     self-contained forest from end-of-design snapshots, NATURAL names, no whole-run
     fixes; content canonicalized (a duplicate's body is embedded from its canonical's
     snapshot, even one OUTSIDE the subtree, keeping self-containment); insts unified;
     refs resolved through the mutable run state (`getMemberOption` walks the context
     stack); globals closure from the GLOBAL context list; children discovered from RAW
     insts (a unified designRef resolves only structurally). PITFALL: do NOT
     clearDesignInstCache on live blocks in the artifact builder (the design is still
     live in the run; PBNS resolution needs the cache; the transient cache is not
     serialized anyway). SubDesignCacheSpec 3/3 (map service round trip, real disk
     service, no-cache regression), StagesSpec 494/494, FULL TEST GREEN (modulo the
     known-ignorable local ips VgaMonitorSimSpec environment failure).
   - REMAINING (later increments): dclName-clash proofing for adopted child designs vs
     native same-name designs (seed uniqueDesigns with adopted children or re-uniquify
     at assembly); recovery tiers; user docs.
4. CLASS DESIGNS THROUGH THE GATE + KEY-BASED UNIFICATION PRIMARY (2026-07-12,
   uncommitted): class designs are keyed at their END (body always runs live for now)
   in `Design.onCreateStartLate`: `gate.designClsKeyOf(__clsScalaArgs)` (dclMeta = leaf
   class meta; no inputTypes; plain Scala ctor params + template captures via the
   plugin-injected `__clsScalaArgs` chain, chained like `__clsMeta` so base-class
   captures are covered; impure params matched BY NAME against the class `pure`
   annotation over the design's DesignParam members, STRICT: an unresolved name or
   unknown applied data = keyless) -> `joinCanonicalOf` (sets ctx.duplicateOf) or
   `completed` after exit (cacheEnable=false for classes for now: intra-run tier only).
   A vendor IP blackbox keys ALL its params' applied data (it bakes values into the
   emitted IP instance; mirrors the old defaultValRef=applied `=~` discrimination).
   - KEY NORMALIZATION (`normalizedKeyPart`; user decision 2026-07-12): a DFType in
     any key part (def inputTypes, scala args like `new IDGen(SInt(w))`, impure-param
     dfTypes) keys by its DEFAULT-printer `codeString`: unique, repetitive across
     constructions AND across runs (so it serves the service's cross-run localKey
     as-is; raw equality would wrongly split on per-construction TypeRef tokens).
     Printing needs every type reference to resolve with a member ORIGIN; an ad-hoc
     type built at the instantiation site (never landing in a member) has none and
     falls back to (token-erased copyWithNewRefs(RefGen.initial) copy, resolved
     ref-target members): sound intra-run, miss-only cross-run. Containers decompose
     recursively ONLY when they hold a DFType (tuples of DFTypes); every other value
     keeps its OWN equality. PITFALL that forced this rule: decomposing foreign values
     Product-wise destroys custom equality semantics - a BitVector inside applied
     constant data holds rope-internal Array[Byte] fields that compare BY REFERENCE,
     which silently keyed every `h"02"` application uniquely (AES: 64 mulByte designs
     out of 3 logical ones). localKeyOf toStrings inputTypes (normalized strings/pairs,
     not writable DFTypes).
   - PURECHECK CLASS ATTRIBUTION: forcing rooted at a design class's `<> CONST` ctor
     param (via param accessor or ctor param symbol) marks the param on the CLASS
     annotation (`pure(true, name)`) instead of design-level escalation; plain Scala
     ctor params are Pure roots (keyed by value). SOUND ONLY when the analyzed root's
     nearest design boundary IS that class (`nearestDesignBoundary`): inside a nested
     design def the def's own key must cover the data, so the param escalates there
     and the def-boundary capture path records a phantom name instead. Class-template
     captured constants attribute like def phantom captures (recorded name = the
     auto-created cloneUnreachable param's name; `discoverClsCaptures` shared by
     PureCheck and MetaContextPlacer). Ctor params normalize accessor->ctor-param
     symbol; ctor callees resolve to their class for markings/roots/synthesis.
   - CRITICAL FINDING + FINAL RESOLUTION: AES's `mulByte` (and transitively
     mixColumns/cipher/Cipher) is design-level Pure(false) EVEN AT HEAD (forcing traced
     through a foldLeft case-lambda pattern binding escalates: pre-existing
     conservatism), yet HEAD produced 14 sub-designs because the =~ dedup silently
     unified the identical impure bodies. Key-based-only unification exploded AES to
     595 sub-designs (576 mulByte copies) and broke tool compilation. An interim
     `DesignContext.keyed` + keyless structural fallback was added, then DELETED by
     user decision (2026-07-12): NO structural dedup at all - designs unify only by
     key, keyless designs enumerate, and `mulByte` carries an explicit
     `@pure(true, "lhs")` (so its applied multiplicand data keys it: 3 designs).
     `endDesign` is now trivially key-driven (duplicateOf join or new group); the
     adoptionIdOf/sameExternalIdAs guard is retired (adopted shells are keyed by
     definition).
   - `StagesSpec.ClassDesignKeySpec` (4 tests): param-parametric unification, Scala
     ctor arg keying (split/unify), template capture keying (local class in a
     List-foreach lambda capturing the loop value), class-param forcing
     (pure(impureParams="amount") on the class, folded-data splits + repeat unify).
   - PRE-EXISTING LIMITATIONS surfaced (unrelated to the gate): instantiating a design
     class inside a design-level `for` comprehension crashes the plugin loop transform
     (ExplicitOuter "failure to construct path"); a Range `foreach` at design level
     resolves to DFRange.foreach (plugin-reserved). ClassDesignKeySpec uses
     `List(...).foreach`.
   - NEXT (increment 2): class body-SKIP + service caching. Design sketch: the gate
     decision must happen at the INSTANTIATION SITE (plugin transformApply already
     lifts ctor args to locals) because a mid-constructor decision cannot compute the
     leaf key from a base template (leaf fields uninitialized during super ctor). The
     call-site key uses dclMeta + ctor-derived parts only; classes with template
     captures never body-skip (start-key/end-key mismatch is conservative-safe). On a
     hit the decision is PUSHED onto the gate before construction; every template
     statement is guarded by the plugin (DFVal-typed port/const val decls always run,
     re-creating the public interface against fresh params; other statements skip;
     classes with container-typed vals or unsafe port dependencies are not skippable);
     service adoption happens at design end like today. Store-side for classes is
     nearly free (`completed` + buildDesignForestDB already work for any design).

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
- CORRECTIONS + additional locked decisions (user, 2026-07-12):
  - `cloneUnreachable` is STILL REQUIRED even with phantom params/ports: ports may
    introduce parameters they depend on (e.g. through their type) that the design does not
    explicitly declare, so the harness must still clone such unreachable dependencies. The
    earlier "retires the cloneUnreachable path" expectation in NEXT item 1 is wrong.
  - The whole (ugly) MutableDB elaboration-dedup mechanism (uniqueDesigns grouping,
    `duplicateOf`/canonical-group joining, `sameDesignAs` child-group pairing) is expected
    to become unnecessary once the proper caching rigging is in place; RETIRE it only
    AFTER the caching mechanism proves it works.
  - Realistically only design DEFS get phantom parameters and ports (class designs declare
    theirs in source). Hiding applies to the design-def VIEW form ONLY: once a def is
    dropped to a regular design block (`DropDesignDefs`), its phantom ports/params are
    visible just like any other port/parameter.
- `PhantomTag` IMPLEMENTED (2026-07-12): `compiler_ir` `PhantomTag` marks
  compiler-synthesized phantom members. The DFHDL printer (`hidePhantoms` hook, true only
  for `DFPrinter`) hides them in the DEF VIEW form only: phantom def args and params at
  the def dcl (arg list, param list, and the def-return detection skips phantom OUT
  nets), phantom positional args and param applications at the def call site
  (`DB.phantomParamNamesOf(design)`: hierarchical-aware, navigates to the design's own
  sub-DB since the instantiation scope only holds the design header), and call-site
  wiring nets whose lhs/rhs endpoint is a phantom-tagged `PortByNameSelect` of a DEF
  instance (input wiring is already generically excluded for def instances, so this
  effectively covers phantom output captures). Def-body references print the phantom
  NAMES, which (with deterministic phantom naming after the captured values) read exactly
  like the host values the def captures. Design-BLOCK printing paths and backend printers
  are untouched: post-drop, phantoms print like any port/parameter. CONTRACT for the
  future creation code: tag the phantom port Dcl, the phantom DesignParam, AND every PBNS
  targeting a phantom port. THROWAWAY `PhantomTagSpec` (StagesSpec) simulates creation by
  tagging `ph*`-named members via a test-local HierarchyStage: a def within a host design
  takes its "captured" host locals as same-named args/params; one test asserts the hidden
  def view, another asserts full visibility after `dropDesignDefs`. Replace with
  end-to-end tests once real phantom creation lands.
- PHANTOM CAPTURE RIGGING IMPLEMENTED (2026-07-12), the former NEXT item 1:
  - Shared capture discovery (`CommonPhase.discoverDesignDefCaptures`, used by BOTH
    PureCheck and DesignDefs): free stable references (Ident/Select paths, keyed by the
    FULL symbol path — the same member through different instance paths must not unify) in
    a design def's body that are not static, not the def's own params/locals, and not
    MetaContext. Classified: DFHDL constant -> phantom design parameter; DFHDL
    non-constant value -> phantom input port; anything else -> plain Scala capture.
  - `DesignDefsPhase`: phantom const/val captures are passed to `designFromDef` as
    `(value, fallback meta)` tuples evaluated in the def's rhs scope per call; body
    occurrences are path-key-replaced by `designFromDefGetParam`/`designFromDefGetInput`
    accessors sharing the explicit args' index spaces (phantoms appended after explicit).
    Scala captures append to `scalaArgs` (closes the per-instance-Scala-data soundness
    hole for defs). Phantom leaf-name clashes (with explicit args or each other) are a
    compiler error.
  - Runtime harness (`designFromDef`): creates phantom IN Dcls and phantom DesignParams
    on hit and miss alike, tagged `PhantomTag` through the DFC (`dfc.tag(ir.PhantomTag)`),
    NAMED after the captured value's own meta (exactly like `cloneUnreachable`
    auto-params; the plugin meta with leaf name + declaration position is only the
    fallback for anonymous applied values). Phantom input call-site connections carry the
    tag so the created PBNS (and net) are tagged. Phantom const entries join
    `collectParamEntries` matching and the impure-params key matching by name.
  - `runFuncWithInputs`: unhittable-entry machinery RETIRED. `PureDefEntry(design, ret)`
    only; a hit collects fresh param entries (the whole interface is harness-created). If
    a body run still auto-creates DesignParams (a capture path the rigging cannot see,
    via `cloneUnreachable` — which REMAINS), that call is simply never cached (checked by
    a members-length snapshot around `func`).
  - PureCheck: forcing rooted at a captured constant of a design def attributes to the
    phantom's predicted name, recorded on the def (`pure(true, <name>)`) and keyed at the
    runtime gate like an explicit data-impure param. CRITICAL DETAIL: attribution runs
    under whatever root is being scanned — for a design-def body that is the def's own
    CONTEXT LAMBDA root (`$anonfun`), not the def root — so `phantomCaptureRes` resolves
    the nearest enclosing design-def root via `rootSym.ownersIterator` and records the
    name DIRECTLY on that def (returning Pure); guarding on `rootSym` membership silently
    never fires. The per-instance PureCheckSpec test flipped from design-level impurity
    to `pure(true, "localConst")` with a pure design.
  - Printing (user decision, 2026-07-12): a design def WITH phantoms must not print its
    declaration at file level (its body references host values by name); it prints
    LOCALLY in the host design's body, just before the def's first instance.
    `DB.designHasPhantoms` (hierarchical-aware, shares `fromDesignMembers` navigation
    with `phantomParamNamesOf`), `printDesignDefDclInline` hook (AbstractOwnerPrinter,
    overridden only by the DFHDL printer), `designPrinters` filters such defs out of
    file-level emission, and `csDFMembers` injects the dcl (via `printerForDesign`)
    before the first `DFDesignInst` of that design — the inst member always precedes the
    statement consuming the def's output, so the anchor also covers inline-printed
    instances.
  - Tests: PhantomTagSpec is now END-TO-END (throwaway test-stage deleted): a def within
    a host design capturing a host port + host constant; asserts the def view prints
    exactly like the source (local def dcl, hidden phantoms) and full phantom visibility
    after `dropDesignDefs`. PureDesignDefSpec capture tests updated (captures cache-hit
    now; printed like source). Verified: StagesSpec 491/491, full test green except the
    known-ignorable ips VgaMonitor env failure and munit "0 total" anomaly lines.
- Phase 2 `DesignLoadGate` extraction IMPLEMENTED (2026-07-12): `runFuncWithInputs` (and
  the `PureDefEntry`/`pureDesignDefOutCache` machinery) moved out of `DesignContext` into
  a sibling `MutableDB.DesignLoadGate` with named pieces: `Key(dclMeta, inputTypes,
  scalaArgs, impureParamsKey)`, `Entry(design, ret)`, the `intraRunCache` map (the first
  backing tier), `isPure(design)`, `keyOf(...): Option[Key]` (None = uncacheable), and
  `loadDesignDef(func, inputs, params, scalaArgs, impureParamsKeyOpt)(paramEntriesOf)`.
  Behavior-identical (full test matches the pre-refactor baseline); `designFromDef` is the
  only client. The disk tier (Phase 3) joins behind the same gate; class designs (Phase 4)
  route their instantiation through it. The `@hw.pure` contract documentation remains part
  of the user-docs item below.
- Phase 3 GROUNDWORK (2026-07-12): the plugin now passes the def's nearest enclosing
  class (`classOf`, the future `factum.CodeRef` anchor) through `designFromDef` into
  `DesignLoadGate.loadDesignDef(ownerClass)`; unused by the intra-run tier (dclMeta
  already identifies the declaration within a run). All remaining Phase 3 work is
  core/ir/lib-side (no more plugin rebuilds needed).
- Phase 3 DESIGN RISKS discovered while mapping the attachment path (must be resolved by
  the disk-tier implementation):
  - REF-TOKEN COLLISION on attach: a cached bundle's sub-DB keys (`StaticRef` tokens) and
    the `DFDesignInst.designRef` tokens inside its members come from the CACHED run's
    RefGen counters and can collide with the fresh run's tokens once spliced into one
    `subDBs` map. Attachment must FRESHEN the bundle's design-block ownerRef tokens (the
    subDBs keys) and the matching `designRef`s inside bundle members. Per-sub-DB
    refTables are self-contained, so only these cross-sub-DB tokens need rewriting;
    `db.patch` per bundle sub-DB keeps refTable value-consistency for the replaced
    `DFDesignInst`s (`designRef` itself is deliberately absent from refTables and from
    `copyWithNewRefs`).
  - DCLNAME DEDUP RENAMING: `MutableDB.immutable` renames same-dclName design groups
    (`${designType}_NN`), so a cached bundle may embed a suffixed dclName that clashes
    with (or diverges from) the fresh run's naming. Attachment must re-uniquify names
    (the stage-level `UniqueDesigns` invariant) rather than trust cached names.
  - EXTRACTION POINT: bundles must be extracted from the post-`oldToNew` hierarchical DB
    (self-contained sub-DBs) right after elaboration (pre-stages), and the recorded
    design -> key mapping must survive `immutable`'s duplicate unification (the shell
    block of a disk hit has no live canonical to join; it needs a "duplicate with
    external body" marking so its non-public members drop like today's duplicates).
  - Return-type manifest: on a disk hit the harness needs the def's RETURN DFType (for
    the out port and the Unit-return check) before any body exists; store it in the
    bundle alongside the sub-DB forest (the rest of the public interface is already
    harness-created from call-site information).
- Phase 3 CORE MECHANISM IMPLEMENTED (2026-07-12), everything except the DFApp/lib disk
  implementation:
  - `ir.SubDesignBundle(retDFType, forest: List[(StaticRef, DB)])` derives ReadWriter
    (sub-DBs serialize through the existing DB RW); `ir.SubDesignCache` trait with
    `lookup/store(ownerClass, localKey)`; `DB.extractSubDesignForest(key)` (transitive,
    top first) and `DB.attachExternalSubDesigns(bundles)(using RefGen)` which freshens
    cross-sub-DB tokens (bundle top adopts the SHELL's token and the shell's fresh block
    header wholesale, resolving dclName renaming; children get newly generated tokens;
    `DFDesignInst.designRef`s remapped; per-sub-DB refTables need only the top ownerRef
    key swap since designRef is absent from refTables), with an instMode/domainType/
    dclMeta-position guard against key collisions.
  - `DesignLoadResult` (Live(ret)/Cached(retDFType)): the harness only ever needs the
    RETURN DFTYPE on any cached path (the user-facing return value is always the fresh
    out port), so intra-run entries store retDFType and a disk hit needs no live object.
  - Gate: resolution order intra-run -> disk -> live. A disk hit marks the shell context
    duplicate WITHOUT `duplicateOf` (it becomes its own group's canonical with an
    external body), registers the bundle for attachment, records a shell cache identity
    (`externalShellIdOf`) consulted by `sameDesignAs` so different-body shells never
    structurally unify (their in-run members are just the public interface), and seeds
    the intra-run tier so same-run repeats join as regular duplicates. A live cacheable
    run records a write-back entry; `Design.finalizeCachedSubDesigns` (used by BOTH
    `getDB` and the top `onCreateEnd` check path, which otherwise checks a pre-attach DB
    and reports dangling shell ports) attaches bundles and stores live ones, skipping
    designs dropped by duplicate unification.
  - `localKey` = SHA-256 over (upickle(dclMeta) | upickle(inputTypes) | scalaArgs
    toStrings | impureParamsKey toStrings); unstable parts (identity toStrings,
    ref-carrying dependent DFTypes) can only cause misses, never false hits, since the
    service anchors by the owner class's code digest and dclMeta is in the digest.
  - `ElaborationOptions.SubDesignCacheOption` (default None) injects the service;
    NOTE for tests: option givens do NOT flow into a plain `new Top` (no @top), use the
    `DFC.empty(summon[ElaborationOptions])` + `def gen(using DFC)` pattern.
  - `StagesSpec.SubDesignCacheSpec`: a JSON-round-tripping map-backed service; asserts
    live-store, fabricated hit prints identically, hit count, and that the attached DB
    behaves through `dropDesignDefs`; plus a no-service regression test. Verified:
    StagesSpec 493/493, full test green modulo known ignorables.
- Phase 3 SIMPLIFIED + DISK SERVICE IMPLEMENTED (user corrections, 2026-07-12):
  - `SubDesignBundle` DELETED: the cache artifact is a PLAIN hierarchical DB (an
    empty-members root whose subDBs are the design's forest, `DB.extractSubDesignDB`),
    serialized with the existing DB ReadWriter exactly like the top-design cache. The
    return DFType is derived from the cached top's (non-phantom) out port
    (`DB.subDesignRetDFType`; no out port = Unit return). `ir.SubDesignCache` now
    traffics in `DB`.
  - Service injection DELETED from the options surface: `ElaborationOptions.CacheEnable`
    is a plain Boolean (default false), mirroring AppOptions. The premise that core must
    stay factum-free was void: core depends on internals, which owns DiskCache and
    factum, so core owns the whole disk service (`dfhdl.core.SubDesignDiskCache`) and
    DFApp needs no special wiring.
  - Disk location (user decision): entries live BESIDE the def's owner class build
    output (`<scala target dir>/dfhdl-cache/`, via the class's code source), so `clean`
    drops them with the classes and multi-module projects get per-module locality.
    Top-level defs are covered via their synthetic `<file>$package` owner class. The
    `factum.CodeRef` digest stays in the key regardless (incremental recompilation
    rewrites classes without a clean). Jar-located owner classes (library-shipped defs)
    skip the DISK tier only.
  - In-memory store tier (user request): the service memoizes the DESERIALIZED DB
    process-wide, keyed by the full content key (digest + dfhdlVersion + localKey), in
    front of the disk: repeat elaborations in one JVM session skip file IO and JSON
    parsing, a run's write-back serves later runs from memory, and jar-located defs
    still cache in-process. NOTE: factum already provides byte-level layering
    (`MemoryStore` + `AggregateStore(MemoryStore(), DiskStore(dir))`) — a candidate
    upgrade for DFApp's TOP-level DiskCache, but the deserialized-DB memo above is the
    bigger win for sub-designs, so the sub-design service keeps its own.
  - Write-back is ONE-SHOT per elaboration (`storeLiveSubDesigns` clears its records):
    the finalized DB is derived at both the top `onCreateEnd` check and every `getDB`.
  - Gate/registry renames: `cachedSubDesignDBs`, `attachCachedSubDesigns`,
    `storeLiveSubDesigns` (no "bundle" naming).
  - `SubDesignCacheSpec` (3 tests): fabricated-hit round trip via a JSON-round-tripping
    map service (covers a phantom-capturing def AND a top-level def), the REAL disk
    service end-to-end (stores beside test-classes, hits on re-elaboration; clears the
    cache dir + in-memory store first for determinism), and a no-cache regression with
    the dropped view asserted equal on live and cached paths. StagesSpec 494/494, full
    test green modulo known ignorables.
- Thread-safety + placement corrections (user, 2026-07-12):
  - The `SubDesignCache` trait lives in CORE (`dfhdl.core`), not in `ir`: it is an
    elaboration-runtime service, while the DB extract/attach helpers remain `ir.DB`
    methods.
  - No global service `var`: `SubDesignDiskCache` is a CLASS instantiated per
    `MutableDB` (`MutableDB.subDesignCache`, the per-elaboration testing seam that
    cannot race other elaborations), with the shared process-wide stores (deserialized
    DB memStore + CodeRef/cache-dir memos) as thread-safe companion state
    (ConcurrentHashMap; `clearInMemoryStore()` remains the test/diagnostic reset).
  - Tests inject per-DFC: `DFC.empty(eo)` then `dfc.mutableDB.subDesignCache = fake`.
- Gate bookkeeping consolidation (user challenge, 2026-07-12): the three parallel
  registries (`cachedSubDesignDBs`, `externalShellIds`, `liveStores`) plus two finalize
  methods plus two `has*` probes collapsed into ONE `externalShells` map (keyed by the
  shell block's ownerRef `StaticRef`, values `ExternalShell.Attach(cachedDB, localKey)`
  / `ExternalShell.StoreBack(service, ownerClass, localKey)`) with a single
  `finalizeExternalShells(db)` method (attachments run on every DB derivation;
  store-backs are one-shot via `filterInPlace`). What remains is irreducible: the
  service is content-addressed and shared ACROSS elaborations, so it cannot know a
  run's shell tokens; the per-run shell-to-action association must live on the
  per-elaboration gate, and `Attach` entries keep the DB reference (no re-lookup at
  finalize) because the service contract does not promise a later hit. Bonus: keying by
  `StaticRef` instead of the block instance makes `externalShellIdOf` robust to block
  revisions.
- NEXT increments, in order:
  1. Phase 3 polish: multi-variant coverage tests (two shells of the same def with
     different keys in one run; disk hits coexisting with intra-run duplicates), and
     optionally defaulting `ElaborationOptions.CacheEnable` from `AppOptions.cacheEnable`
     in DFApp flows (flip the global default to true once proven).
  2. Phase 4: class designs (instantiation-gate + body-extraction rigging); includes
     class-ctor-param attribution (currently a forced root at a class param accessor
     conservatively escalates to design-level impurity) and Scala-capture keying for
     class bodies.
  3. Recovery tiers for impure sub-design poison (tracked-effect manifests first).
  4. User documentation for the purity model (docs/), including the "unmarked effects are
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

### Phase 2: extract the load-gate abstraction — IMPLEMENTED

Implementation notes (2026-07-12): landed as `MutableDB.DesignLoadGate` (sibling of
`DesignContext`) with `Key`/`Entry`/`intraRunCache`/`isPure`/`keyOf`/`loadDesignDef`;
`runFuncWithInputs` deleted; `designFromDef` is the only client. Verified as a
no-behavior-change refactor by the full test suite.

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
