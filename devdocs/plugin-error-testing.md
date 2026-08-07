# Plugin-Error Testing (`assertPluginError`)

How munit specs assert on diagnostics that only the DFHDL compiler-plugin phases emit.

`assertCompileError` is built on `compiletime.testing.typeCheckErrors`, which compiles its
snippet through the typer plus a small fixed set of reconstructed standard transforms. DFHDL's
plugin phases never run on that snippet, so every `report.error` they emit (the Methods
phase's HDL-method rules, for example) is invisible to it. The intrinsics cannot be extended from a
plugin either: the Inliner consumes `typeCheckErrors` calls during typer, before any plugin
phase sees the tree.

The mechanism here replicates the intrinsic inside the plugin: a marker function whose calls a
dedicated plugin phase intercepts, nested-compiles, and replaces with the literal answer.

| Piece | Where | Role |
|---|---|---|
| `PluginErrCheck.pluginCheckErrors(code)` | `internals/src/test/.../PluginErrCheck.scala` | marker; throwing body, never published |
| `PluginTestPhase` (pipeline name `PluginErrCheck`) | `plugin/.../PluginTestPhase.scala` | intercepts marker calls, runs the nested compile |
| `assertPluginError(expectedErr)(code)` | `NoDFCSpec` | munit-facing helper |
| `pluginErrorTestSettings` + `internals % "test->test;compile->compile"` | build.sbt (core only) | gating and marker visibility |
| `CoreSpec` | core tests |

## How a call is replaced

`PluginTestPhase.transformApply` matches calls whose symbol is the marker. The marker symbol is
resolved optionally (`getModuleIfDefined`, never `requiredMethod`): it lives in internals TEST
sources, so any compilation that enables the phase without having the marker on its classpath
is simply inert.

The snippet argument must be a statically known string, exactly as for `typeCheckErrors`: the
phase strips `Typed`/`Inlined` wrappers and `ConstFold`s the argument, and anything that is not
a constant is a call-site error. One deliberate exception: the retained rhs of the inline
`assertPluginError` helper itself contains a marker call whose argument is the inline
parameter; marker calls with a non-constant argument under an `Inline` owner are left
untouched, since each call site is replaced on its inlined, constant-argument copy.

The call is rewritten into a `List[String]` literal of the error messages the nested compile
produced. The helper asserts `errs.lastOption` against the expected text, which is the FIRST
reported error, the same convention as `assertCompileError`.

## The nested compilation pipeline

`snippetErrors` mirrors the compiler intrinsic's implementation
(`Inlines.Intrinsics.compileForErrors`), extended with this plugin's phases:

1. A virtual source is built as `"import dfhdl.*\n" + code` (the wildcard import every DFHDL
   test file opens with) and parsed with `Parser.block()`. A block parse is what keeps
   snippets isolated: block-local classes are owned by a throwaway symbol and never enter a
   package scope, so snippets cannot collide with each other or leak into the real
   compilation.
2. `PreTyperPhase.rewriteParsed` applies the two `<>` precedence fixers to the parse tree,
   giving snippets the same parse-level fidelity as regular units (the auto-`@top` rewrite
   never applies inside a block and is skipped). Notably, `typeCheckErrors` skips untyped
   rewrites entirely; owning the pipeline is what makes this fidelity possible.
3. A fresh nested context is created: a fresh typer state (whose buffering reporter is what
   isolates the snippet's diagnostics from the real run), a nested `Typer`, a dummy owner (as
   in the intrinsic: the real owner may be inspected by a transform phase, causing cyclic
   errors), and
   `-rewrite` disabled. Everything from here on happens under `atPhase(typerPhase)`: a plugin
   phase's own period forbids implicit search (`Phase.allowsImplicitSearch` asserts on the
   first implicit lookup otherwise), and symbols created at typer stay valid for the forward
   phase runs.
4. The typed tree is run through the phases the real pipeline would apply, each wrapped as a
   `(phaseId, run)` pair and executed in ascending id order, i.e. the real schedule's order:
   - `PostTyper` and `Inlining`, via the public `Phase.runOn` on synthetic
     `CompilationUnit`s (their `newTransformer` is `protected` and inaccessible to a plugin).
   - FRESH instances of the plugin's typed phases: all of them except `PreTyper` (already
     applied, untyped), `CodeDigest` (elaboration-caching bookkeeping, no diagnostics), and
     the interceptor itself (no recursion). Fresh instances are mandatory because the phases
     keep per-unit mutable state (`prepareForUnit` symbol caches, `collectDFHDLMethods`); the
     prepare chain runs via `MegaPhase.transformUnit`. Each instance is wrapped in a
     single-phase `MegaPhase` pinned to its installed counterpart's phase id so denotation
     lookups match the real pipeline. The installed ids come from walking the public
     `Phase.next` chain (`ContextBase.phases` is `private[dotc]`).
   - Skipped: `Pickler` (no diagnostics), `SetRootTree` (exists only under
     `-Yretain-trees`), and the `InlineVals`/`ElimRepeated`/`RefChecks` group the intrinsic
     reconstructs (irrelevant to plugin diagnostics).
5. The first error short-circuits all remaining runs; the collected messages become the
   replacement literal. A snippet that fails the typer therefore reports the typer error and
   the plugin phases never run: typer errors mask plugin errors (e.g. a `Unit <> EDRET` body
   using `:=` hits the `Scope.Procedural` typer error before the plugin's procedural error).

## Writing tests

```scala
test("ED method direct recursion"):
  assertPluginError(
    "Recursion is not allowed for ED methods."
  )(
    """
    class Foo extends EDDesign:
      val y = UInt(8) <> OUT
      def rec(): UInt[8] <> EDRET = rec()
      y <> rec()
    """
  )
```

- **Snippets are self-contained blocks.** They see the classpath, not the call site's lexical
  scope: no file imports beyond the auto-prepended `import dfhdl.*`, no spec members, no
  ambient `DFC` given. This is the one real regression against the intrinsic (which runs at
  typer where the call-site scope is live), and it is why `assertPluginError` complements
  `assertCompileError` rather than replacing it: use the intrinsic-based helper for
  typer-level errors (scope-aware, cheaper), this one only for plugin diagnostics.
- **...but the enclosing CLASS'S members do leak in**, through the dummy owner's owner chain
  (as in the intrinsic). Two observed consequences: a spec member can make a snippet
  ambiguous (PluginSpec defines a `<>` extension, so snippets using `<>` fail there; its
  plugin-error tests live in other specs), and an owner-chain base can change plugin behavior
  (the spec's `NoTopAnnotIsRequired` suppresses the missing-`@top` instantiation error, which
  is therefore untestable this way).
- **Snippets must be statically known strings**: a `"""..."""` literal, no `stripMargin` (a
  runtime call). Uniform extra indentation is harmless to the block parse, so indent the
  snippet naturally. The expected-error argument is compared at runtime and may use
  `stripMargin` freely.
- Local design classes need no `@top` and nothing is elaborated at runtime; the diagnostics
  fire on the trees during the nested compile.
- Each snippet is a nested compile during `core/Test` compilation, the same cost class as a
  `typeCheckErrors` site plus the plugin phases. Keep snippets minimal.

## Gating and footprint in the wild

The interceptor phase is appended by `Plugin.initialize` only when the `testing` plugin option
is present. Only `pluginErrorTestSettings` passes `-P:dfhdl.plugin:testing`, and only `core`
uses it (`compiler_stages` keeps the plain `pluginTestUseSettings`, so the phase does not
exist in its pipeline at all; lib deliberately does not enable it either). Production
compilation and any downstream user of the published plugin never pass the option, so the
phase is never instantiated: zero pipeline presence, zero per-node dispatch cost. The dormant
class in the plugin jar is the entire footprint.

The marker is test-only source reached through core's `test->test` dependency on internals; it
is never published, so downstream code cannot even reference it. If a stage-side plugin-error
spec is ever needed, give compiler_stages `pluginErrorTestSettings` plus the same `test->test`
mapping. Metals/BSP export `Test / scalacOptions`, so the gating applies in the IDE unchanged.

## Sharp edges (for maintainers)

- **The nested typing must stay under `atPhase(typerPhase)`.** Moving it to the phase's own
  period trips the `allowsImplicitSearch` assertion on the first implicit search in any
  non-trivial snippet.
- **Phase-run closures must be constructed inside `inContext(newContext)`.** A closure built
  outside captures the enclosing real Context, and the snippet's diagnostics then leak into
  (and fail) the real compilation. This was the one genuinely subtle bug during bring-up.
- Single-phase `MegaPhase` wrapping means `transformFollowing`/`transformAllDeep` inside a
  nested phase sees only that phase, whereas the real pipeline may fuse consecutive minis.
  Fine for diagnostics; a known fidelity gap for tree shapes.
- **Diagnostics go through the run-wide rewriting, then render through `Message.toString`, not
  `Diagnostic.message`.** The fresh typer state bypasses the `CustomReporter` that
  `PreTyperPhase.initContext` installs for the real run, so the collection step applies the
  SAME rewriting through the shared `DiagnosticRewriter`: position normalization, dedup (an
  inline-expansion error re-raised at several positions must render once; asserted with
  `assertSinglePluginError`), the DFHDL-mismatch postscript drop, and the guide rails (which
  name the enclosing call from the snippet's parse tree). The rewriter's `unitSource` must be
  the snippet's virtual source: a nested diagnostic's position chain extends past it into the
  real unit (the marker call site), so the outermost frame does not identify the unit. The
  rendering itself matters too: `message` renders under `Message.inMessageContext`, which pins
  the printer to the compiler's own `Message.Printer` and therefore never sees the DFHDL type
  printer, whereas `toString` renders under the context the message captured, where that
  printer is live. Going through `toString` is what lets a snippet assert the text a user
  actually reads (`Bits[8] <> VAR` rather than `dfhdl.core.DFVal[...]`), and it is the only
  way to test the printer at all: `typeCheckErrors` packs its diagnostics with `message` and
  cannot be made to do otherwise. ANSI colour escapes are stripped the same way
  `Diagnostic.message` strips them.

## Beyond plugin errors: the DFHDL type printer

`TypePrinterSpec` uses the same helper for a different purpose: asserting how the plugin's
`DFHDLTypePrinter` names DFHDL types inside ordinary compiler diagnostics (a type mismatch, a
missing member). Those are typer errors, so `assertCompileError` reaches them just fine, but it
renders them with the compiler's own `Message.Printer` and would report
`dfhdl.core.DFVal[dfhdl.core.DFType[...], ...]` where the user sees `Bits[8] <> VAR`. Use
`assertPluginError` for anything that asserts on how a type is printed.

`-P:dfhdl.plugin:disableCustomPrinter` leaves the printer and the `CustomReporter` uninstalled for
a compilation, so diagnostics come out in the compiler's own vocabulary. It is a debugging aid for
work on the DSL: when a diagnostic is hard to read, it separates a genuinely confusing error from
one the printer made confusing, without commenting out `PreTyperPhase.initContext` to find out.
Expect `TypePrinterSpec` (and `DFMatchSpec`'s literal-pattern case) to fail while it is on, since
those assert the printer's output:

```bash
sbtn.bat 'set core/Test/scalacOptions += "-P:dfhdl.plugin:disableCustomPrinter"; core/Test/compile'
```

## Coverage

Every plugin `report.error` site is covered by an `assertPluginError` test in the core spec
that matches its subject (EDMethodSpec, StaticFunctionSpec, DFMatchSpec, DFTypeSpec,
DFDecimalSpec, DFBoolOrBitSpec, RTProcessSpec), EXCEPT the following, which are deliberately
untested:

- **TopAnnotPhase errors and the missing-`@top` instantiation error**: exercising them needs
  `@top` (a lib class) on the classpath, and lib deliberately does not enable the interceptor
  (`@top` is not really exposed to the user by default). Additionally, the `@top`
  default-value and companion-`main` errors need a package/object-level `@top`, which a block
  snippet cannot host, and the instantiation error is suppressed by the spec's
  `NoTopAnnotIsRequired` through the owner chain.
- **Re-reported exceptions**: the CustomControlPhase sites that resurface a core
  `IllegalArgumentException` message (no fixed text to assert).
- **Internal errors**: the missing-implicit-context error (MetaContextGen), the
  missing-companion error (TopAnnot), the unknown-pattern catch-all (CustomControl), and
  `pluginCheckErrors`' own non-constant-argument error (self-referential).
- **Interface-based errors** (anonymous-instance, protected-ports): `dfhdl.core.Interface`
  is reserved for the official interface work and is not used in tests.
- **No known trigger**: the `.toScalaTuple` hint (shadowed by the tuple-arity path) and the
  non-constant string-interpolation bind width (needs a non-literal width type).
