# Elaboration Caching

How DFHDL avoids elaborating the same design twice: within one run, and across runs.

Elaboration is the execution of the user's Scala code that builds the IR. It is the dominant cost of
a DFHDL invocation for any design with repetition (an ALU instantiated per lane, an AES round built
from the same byte-level defs, a parametrized FIFO used everywhere), and it is pure work: running the
same code, in the same context, with the same data, builds the same design. The caching mechanism is
what turns that observation into reuse.

There are three layers, from innermost to outermost:

| Layer | Scope | Granularity | Enabled by |
|---|---|---|---|
| Design load gate, intra-run tier | one elaboration | one sub-design | always on |
| Sub-design cache service | across runs (and JVM sessions) | one sub-design | `ElaborationOptions.CacheEnable` (default `true`) |
| DFApp step cache | across runs | the whole top design | `AppOptions.cacheEnable` |

The first two share one key and one gate; the third is a separate, coarser cache in the app flow. All
three key on the same notion of code identity, described under [The code digest](#the-code-digest).

## The design load gate

Every design instantiation, whether a design `def` or a design class, goes through the gate
(`MutableDB.DesignLoadGate`). The gate answers one question: has this exact design already been
loaded, here or in an earlier run? Its three outcomes are:

- **Intra-run hit.** This run already elaborated a design with the same key. The instantiation joins
  that design's group as a duplicate: its own body does not run (a def) or does not run past the
  public interface (a class), and it drops out of the final assembly, leaving one design and several
  instances of it.
- **Service hit.** A cross-run cache entry exists for the key. The entry is adopted as a design of
  this run (see [Adoption](#adoption)), seeded into the intra-run tier so later repeats join it, and
  the body is skipped exactly as on an intra-run hit.
- **Miss.** The body runs live. At the design's end the gate records it as the key's canonical design
  and, when caching is enabled, stores its cache entry.

A design the gate CANNOT key (see [Purity](#purity-the-precondition)) never unifies: each
instantiation emits its own `dclName`-enumerated design, and none of them is cacheable. There is no
structural deduplication anywhere; the key is the only thing that unifies designs. That is a
deliberate trade (a keyless design instantiated N times emits N identical copies) whose remedy is
declaring purity, not comparing bodies.

### Purity: the precondition

A design is loadable only if its body is *pure*: its structure is a function of its key and nothing
else. Elaboration-time reads of mutable state, of the wall clock, or of a design parameter's *data*
all make the body depend on something the key does not carry.

`PureCheckPhase` (compiler plugin) analyzes every design and records the verdict on the design's
`dclMeta` as `@hw.annotation.pure`. Designs are pure by default; the phase escalates to `pure(false)`
when it sees an effect it cannot attribute. The interesting middle case is *data impurity*: a body
that forces a constant parameter's value (`toScalaInt` and friends) and lets it shape the structure
is still perfectly cacheable, provided the applied data joins the key. The phase attributes such
forcing back to the parameter it flows from and records that parameter BY NAME on the design's own
annotation, printed as `@hw.annotation.pure(impureParams = "lhs")`. A user can write the same
annotation by hand as an escape hatch, declaring a data dependence the analysis is too conservative
to see (AES's `mulByte` does exactly this, and is the canonical example). `"*"` marks every parameter
data-impure, which is what the `toScalaXYZ` forcers themselves carry, and what a vendor IP blackbox
gets implicitly (it bakes its applied values into the emitted instance).

### The key

`DesignLoadKey` is the identity of one instantiation:

| Part | Def designs | Class designs |
|---|---|---|
| `dclMeta` | the declaration's meta (name, position, annotations) | the same, of the LEAF class |
| `inputTypes` | the call-site input DFTypes | none (ports are body-declared) |
| `scalaArgs` | plain Scala arguments and Scala captures | plain Scala constructor params and template captures, through the plugin-injected `__clsScalaArgs` chain |
| `impureParamsKey` | applied data of the parameters named data-impure | the same |

Applied design parameter values are deliberately NOT in the key. A pure body cannot depend on them
(depending on a parameter's data is what data-impurity *means*), so every application shares one
loaded body and differs only in its instance parameter bindings, which the harness constructs afresh
on hit and miss alike. This is what keeps a parametrized module one module.

Every IR-bearing key part is normalized to a default-printer `codeString`: DFTypes, and applied
constant data together with its type. That makes the key plain data comparable by value, stable
across constructions and across runs (raw IR equality would split on per-construction reference
tokens), and directly reusable as the cross-run content key. Note the pitfall the rule exists for:
key parts are never decomposed structurally, because a `BitVector` inside applied constant data holds
arrays that compare by reference, which would key every identical constant uniquely.

`DesignLoadKey.localKey` is a SHA-256 over those parts, and is what the cache service sees. It is
best effort in one direction only: an unstable string form can cause a MISS, never a false hit,
because the service anchors every entry by the declaring class's code digest, and `dclMeta` is inside
that digest.

### The two design forms

**Methods.** The plugin's `MethodsPhase` wraps the body in `r__For_Plugin.designFromDef`. The
harness owns the design's whole public interface: it creates the input ports and the design parameters
(bound to this call's applied values) OUTSIDE the body, and the body fetches them by index
(`designFromDefGetInput` / `designFromDefGetParam`). The body is therefore a skippable thunk, and a
hit needs nothing from it except the return DFType, which is read off the loaded design's out port.
Values the def body captures from its enclosing design become PHANTOM ports and parameters (constant
captures become parameters, non-constant ones become input ports), which is what makes a def's design
self-contained and cacheable at all. They are tagged `PhantomTag` and hidden in the method
printed view, so the def still reads like its source.

**Design classes.** A class declares its own ports in its body, so the interface cannot be lifted out
wholesale, and the decision cannot wait for the body to finish either. `DesignClsSkipPhase` (the
plugin's LAST phase) rewrites every skippable design class so that:

- the applied `<> CONST` parameters are LIFTED out of the body and into the gate call
  (`Design.__clsBodyGate`), which creates the parameter members itself; the body's parameter
  declarations become `__clsGetParam` fetches;
- every body statement is guarded by the gate's decision (`if (__clsSkipBody)`), EXCEPT the public
  interface declarations, which always run: port and constant declarations, interface vals, domains,
  and plain Scala vals.

`__clsBodyGate` runs at the head of the body, after the parameters exist and before any statement. It
stands down for a top design, for a BASE class's body (the leaf's constructor arguments are not yet
initialized during a base template's run, so nothing keyed there would describe the design), and once
any class in the chain has already run its body live, so a design never ends up holding half a body.
A class that captures a DFHDL value from an enclosing design is refused outright: such a capture
materializes an auto-created parameter INSIDE the body, which a skipped body would not create.

Intra-run body skipping is the bulk of the win, and it needs no cache service at all: two
instantiations of the same key elaborate ONE body, always.

## The sub-design cache service

`SubDesignCache` is a content-addressed get/put store consulted by the gate when
`ElaborationOptions.CacheEnable` is set (it is, by default). Each `MutableDB` (one per elaboration)
owns its service instance, which is also the injection seam for tests; the default implementation is
`SubDesignDiskCache`, whose stores are shared process-wide through thread-safe companion state.

### The entry

`ir.SubDesignEntry(db, children)` is the artifact of ONE design: that design's own self-contained
sub-DB, plus, per instantiated child design, the child's own cache key (`SubDesignRef`: the declaring
class name and the `localKey`).

Children are REFERENCED, not embedded. The loading run resolves each child through the gate exactly
like a live instantiation, so a design used by several parents is loaded once and unifies with a live
elaboration of the same key. Embedding child bodies would duplicate every shared descendant, once per
adopting parent.

Storing requires every child to be a stored entry itself: a keyless child (an impure design, or a
class the plugin could not guard) cannot be referenced, so its parent is not storable either. Children
end before their parents, so this simply propagates up the tree.

### Adoption

A stored ref token means nothing to the loading run. The storing run minted it from its own generator,
and generators restart per run, so an entry arrives holding tokens THIS run will mint again for its
own members. That is not a remote coincidence but the norm, since both runs elaborate the same code.

`SubDesignEntry.cloneForAdoption` therefore re-mints every token the entry holds from the loading
run's generator, and threads the design onto this run's hierarchy in the SAME pass: the design block
takes the token that will be its `subDBs` key, and each child instance's `designRef` is retargeted at
the design this run resolved for that child. Doing both together is not a stylistic choice. Doing them
in sequence would leave stored tokens in the table while a minted one is added, and a minted token
equal to a stored one silently merges two bindings (the design block's owner, which resolves to
nothing, and a port's owner, which resolves to the design) onto one key, losing the port's owner.

Globals keep their stored identity, so the same global reached through two different entries stays ONE
member (globals unify by value across sub-DBs).

An adopted design is a design of this run like any other: it joins the `dclName` enumeration, and the
final assembly emits its cloned sub-DB as the design's content.

### Storage

Entries live BESIDE the declaring class's build output, in `<scala target dir>/dfhdl-cache/`, as
`<sha256 of the full key>.dfdb.json` (the entry serialized through the existing DB `ReadWriter`).
Locating them there means a build `clean` drops the cache along with the classes, and a multi-module
project gets per-module locality. Top-level methods are covered like anything else: Scala places
them in the synthetic `<file>$package` class, whose class file sits in the same output. A declaring
class with no directory code source (a def shipped inside a library jar) skips the DISK tier only.

In front of the disk sits a process-wide in-memory store keyed by the full content key: repeat
elaborations in one JVM session (an sbt server, a test suite) skip the file read and the JSON parse, a
run's write-back serves later runs in the same session, and jar-shipped defs still cache in-process.
It memoizes the JSON and NOT the deserialized DB, and every hit deserializes afresh: IR members carry
per-run mutable caches, so two elaborations must never adopt the same member objects.

Writes go through a temp file and an atomic move, so parallel test forks writing the same key stay
consistent. A store that fails is not an error (the run simply stays live), and a corrupt entry is
just a miss.

The full content key is `<code digest of the declaring class>|<localKey>`.

## The code digest

The `localKey` says which instantiation this is. The code digest says whether the code behind it is
still the code that produced the entry. It must change whenever the design's own code, or anything the
design reaches, changes, and must not change otherwise.

The obvious implementation, walking the declaring class's whole reference closure at runtime and
hashing every class file behind it, costs more per design class than elaborating the design (measured
at roughly 1.5s per class, against a whole StagesSpec run of a few seconds). The compiler already
knows the answer, so it writes it down.

**Compile time.** `CodeDigestPhase` (right after `PureCheck`, before the DFHDL rewrites) writes,
beside each top-level class it compiles, a `<pkg>/<Cls>.dfdigest` record:

```
dfhdl-digest 1
own <sha-256 of the plugin stamp and the class's typed tree>
dep some.pkg.OtherClass
dep some.pkg.Helper$
```

`own` hashes the TYPED TREE, so it is insensitive to formatting and free of the absolute source paths
the meta-context phases plant later. `dep` lists the top-level classes the code actually REACHES
(typed trees, so not every class the bytecode happens to mention). Synthetic top-level classes are
recorded deliberately: a file's top-level methods live in `<file>$package$`, which anchors their
entries.

**Runtime.** `dfhdl.internals.CodeDigest.of` composes a class's digest by folding those records over
the transitive closure, sorted by name. Composition MUST stay at runtime. A digest composed at compile
time would go stale exactly where zinc does not recompile: change a helper's body, and its dependents
keep their class files, so their composed digests would keep describing the old helper. Folding over
the CURRENT records sees the rebuilt helper and invalidates every design that reaches it.

**The plugin is part of the key.** Its content hash (the class entries of the plugin jar, by content
and not by path or mtime, since the build republishes the jar under a fresh name each session) is
folded into every `own` the phase writes. The plugin is what a design's code MEANS, and it is
invisible to the class closure: a plugin change need not recompile a single DFHDL runtime class, and
without this an entry produced by an older plugin would stay "valid" and be adopted by a run whose
plugin no longer agrees with it. A plugin change does force every plugin-compiled source to
recompile, so restamping the records as they are rewritten retires every entry keyed on the old
plugin.

**The boundaries** are what keep the fold cheap, and their order matters:

1. A class in a JAR that is not the development jar folds to that JAR's identity, and the scan STOPS.
   A dependency jar is a versioned artifact whose code cannot reach the build output, so nothing
   inside it is worth scanning.
2. A class WITH a record contributes its `own` hash, and the scan continues into its `dep` list.
3. A class with no record under the `dfhdl.` package folds to `dfhdl@<version>`. DFHDL's own modules
   are not plugin-compiled, so they carry no records, and they fold to the library VERSION rather than
   being scanned: that is what the library is to every real user (a released jar), and a development
   build gets the same treatment deliberately. Editing DFHDL's own sources invalidates entries through
   the version, not through a class-file walk.
4. Anything else with no record folds to its class file's stamp, and names no dependencies of its own.

Rule 2 outranking rule 3 is essential, not incidental: the `dfhdl` package namespace is SHARED with
user code (a design in `dfhdl.AES` is no more library code than one in `com.acme`), so a package name
cannot decide what is library and what is under development. Having been compiled by the plugin can,
and does.

The one jar that is not a versioned dependency is the one holding the design itself: under sbt, a
`runMain` runs off a jar repackaged from the build output, under a fresh `bg-jobs/job-N/` path, on
every single run. That jar is the code under development, so the scan reads the records INSIDE it
(they are packaged with the classes) rather than folding it whole, which would key every design on a
throwaway artifact and retire the entire cache once a run.

Blind spots, all shared with any static approach: reflection and dynamic dispatch are invisible, and a
class compiled without the plugin names no dependencies of its own. A class with no record at all
yields no digest (`of` returns `None`), which callers read as "not cacheable" rather than "unchanged",
so the failure mode is a live elaboration.

## The DFApp step cache

Independently of the gate, `DFApp` caches its whole `elaborate` and `compile` steps on disk
(`internals.DiskCache`). The elaboration step is keyed by the entry-point class's code digest (the
same `CodeDigest`, falling back to a runtime `factum.CodeRef` walk for an entry point the plugin never
saw), the DFHDL version, the default RT domain config, and the design's arguments. A hit prints
`Loading elaborated design from cache...` and never forces the top constructor thunk. It is enabled by
`AppOptions.cacheEnable`, and is strictly coarser than the gate: it replays a whole design, or
nothing.

## Working with the cache

- **Disable the cross-run tier**: `given ElaborationOptions.CacheEnable = false`. The intra-run tier
  stays on and unconditional, since it needs no cache at all.
- **Test seams**: `dfc.mutableDB.DesignLoadGate.subDesignCache = <fake>` injects a service per
  elaboration; `SubDesignDiskCache.clearInMemoryStore()` drops the process-wide memory tier;
  `CodeDigest.clearMemos()` forgets every stamp taken so far.
- **Tests**: `StagesSpec.SubDesignCacheSpec` (round trip through a JSON map service, the real disk
  service end to end, adoption keeping no stored token, a no-cache regression) and
  `StagesSpec.ClassDesignCacheSpec` (forest adoption through a class hierarchy, one body per key
  intra-run, per-instance applied values on a cached parametrized class). Body runs are counted
  through a Java atomic on purpose: a Scala `var` write is an effect the purity analysis sees, and it
  would make the design under test impure and unkeyable.
- **Options in tests**: option givens do not flow into a plain `new Top` (no `@top`); use
  `DFC.empty(summon[ElaborationOptions])` with a `def gen(using DFC)`.

## Open issues and potential improvements

### Correctness gaps

1. **Adopted-child `dclName` clashes.** An adopted forest's CHILD designs keep their stored
   `dclName`s, and a native same-name design in the loading run is not uniquified against them. Fix:
   seed `uniqueDesigns` with the adopted children, or re-uniquify at assembly. This is the one open
   item that can produce wrong output through a path that is on by default.
2. **Def service-hit return types referencing globals** (untested gap): a cached `subDesignRetDFType`
   carrying refs to the cached run's globals would embed unresolvable tokens in the fresh out port.
3. **Conservative impurity escalation on body-locals.** Forcing whose dataflow passes through a value
   the analysis cannot trace to a parameter, capture or static (a lambda parameter, a pattern binding,
   an anonfun-computed local) escalates to design-level `pure(false)` instead of a parameter marking,
   since anonymous functions cannot carry parameter markings (their application sites are
   unknowable). Any forced expression mixing a keyable root with such a local therefore kills the
   whole design: it becomes keyless, never unifies, and can never cache. The escape hatch is the
   explicit `@pure(true, <names>)`, which is what AES's `mulByte` carries. Fix directions, in
   increasing power: (a) attribute a `Case` binding through its match selector; (b) for trusted
   collection combinators (`foldLeft`, `map`, ...) over code-determined collections (literal ranges),
   treat the lambda's element and accumulator parameters as code-determined.

### Accepted approximations (to document rather than fix)

4. **Key over-approximation**: two pure instantiations whose keys differ but whose bodies happen to be
   identical (a Scala argument that does not shape structure) emit two designs. The alternative,
   structural comparison, was deliberately removed.
5. **Residual keying holes**: abstract Scala vals overridden by anonymous subclasses are keyed only
   when declared in the leading paramBody section; an INTERFACE template's Scala captures do not reach
   the instantiating design's key; a forced-only class capture (never materialized as an auto-created
   parameter) makes the class unloadable (strict name resolution: conservative and safe).

### Improvements worth making

6. **Eviction.** Nothing ever removes a cache entry: a directory grows one file per key ever used, and
   only `clean` reclaims it. Wanted: a size or age bound, or a sweep of entries whose code digest no
   longer resolves.
7. **`own` reproducibility across machines.** The digest of a typed tree ought to be byte-identical on
   two machines compiling the same source with the same toolchain, which would let a cache be SHARED
   (checked in, or served from CI). It is untested, and `tree.show` is not contractually stable. A
   compile-twice determinism test is the prerequisite for any shared-cache ambition.
8. **An artifact-based library boundary.** Rule 3 above tests a package-name prefix. Testing the class
   SOURCE instead (does this class come from a DFHDL artifact?) would state the same intent more
   strongly, and would stop relying on a namespace that user code legitimately shares.
9. **Recovery tiers for impure designs.** A design escalated to `pure(false)` poisons its whole
   subtree for caching. Tracked-effect manifests (recording the effects a body performed, and
   replaying or re-checking them on a hit) would let some of those designs cache anyway.
10. **User documentation** of the purity model in `docs/`: the `@pure` overrides with and without named
    impure parameters, the "unmarked effects are the user's responsibility" contract, the
    static-dispatch approximation (the analysis never models subclass overrides), and the key
    over-approximation semantics.
