# DFHDL — Claude Code Guide

> **For contributors and Claude Code users working on the DFHDL project.**
> This file is version-controlled — keep it updated as the project structure evolves.
> Skills for deeper topics live in [.claude/commands/](.claude/commands/).

## Project Overview

**DFHDL (DFiant HDL)** is a dataflow hardware description language embedded as a Scala 3 library. It provides timing-agnostic and device-agnostic hardware design with three levels of abstraction:

- **Dataflow (DF)**: Timing-agnostic, uses dataflow firing rules
- **Register-Transfer (RT)**: Equivalent to Chisel/Amaranth
- **Event-Driven (ED)**: Equivalent to Verilog/VHDL

Outputs: Verilog, SystemVerilog, VHDL.

## Build System

**Tool**: SBT 1.12.12 — **Scala**: 3.8.4 (nightly resolver enabled)

```bash
sbtn compile          # compile all subprojects
sbtn Test/compile     # compile all tests (separate from running them)
sbtn test             # run all unit tests
sbtn testApps         # run simulation/app tests (requires OSS CAD tools)
sbtn corePlayground   # limit test scope to core/Playground.scala only (fast iteration)
sbtn libPlayground    # limit test scope to lib/Playground.scala only (fast iteration)
sbtn clearSandbox     # delete sandbox/ directory (generated output + step cache)
sbtn clearElabCache   # delete every target/scala-*/dfhdl-cache/ (sub-design elaboration cache)
sbtn clearDFHDL       # both of the above
sbtn docExamplesRefUpdate  # copy generated HDL from sandbox/ to lib/src/test/resources/ref/
```

Always use `sbtn` (sbt client) instead of `sbt` for faster startup. On Windows use `sbtn.bat`.

## Subproject Structure

Dependencies flow left to right:

```
internals → plugin → compiler_ir → core → compiler_stages → lib → platforms
                                                                 → ips
```

| Subproject | SBT name | Directory | Purpose |
|---|---|---|---|
| internals | `internals` | `internals/` | Core utilities: BitVector, MetaContext, DiskCache, etc. |
| plugin | `plugin` | `plugin/` | Scala 3 compiler plugin (14 phases) |
| compiler_ir | `compiler_ir` | `compiler/ir/` | IR/AST data structures, type system |
| core | `core` | `core/` | HDL language abstractions (DFVal, DFType, Design) |
| compiler_stages | `compiler_stages` | `compiler/stages/` | 50+ transformation stages for code generation |
| lib | `lib` | `lib/` | Standard library: arithmetic, memory, ALU, crypto |
| platforms | `platforms` | `platforms/` | FPGA board wrappers (Apache 2.0 licensed) |
| ips | `ips` | `ips/` | IP cores library |

## Compiler Plugin Phases

Located in `plugin/src/main/scala/plugin/`, in the order `Plugin.initialize` lists them:

1. `PreTyperPhase` — untyped parse-tree rewrites (`<>` precedence, auto-`@top`)
2. `TopAnnotPhase` — top-level annotation processing
3. `PureCheckPhase` — purity analysis for elaboration caching
4. `CodeDigestPhase` — code digests, the elaboration cache keys
5. `ScalaVarPhase` — the permission list for a Scala `var` holding a DFHDL value (see [devdocs/scala-var-rules.md](devdocs/scala-var-rules.md))
6. `MetaContextPlacerPhase` — places meta-context markers
7. `FlattenInlinedPhase` — flattens `Inlined` wrappers
8. `LoopFSMPhase` — loop-to-FSM transformations
9. `CustomControlPhase` — custom control flow
10. `MethodsPhase` — DFHDL method (`def`) processing
11. `MetaContextDelegatePhase` — meta-context delegation
12. `MetaContextGenPhase` — meta-context code generation
13. `OnCreateEventsPhase` — on-create event handling
14. `DesignClsSkipPhase` — skips design classes in later standard transforms

`PluginTestPhase` (pipeline name `PluginErrCheck`) is a 15th phase, appended only under
`-P:dfhdl.plugin:testing`. Adding a phase means registering it in **both** `Plugin.initialize` and
`PluginTestPhase.freshPluginPhases`, or `assertPluginError` will not see its diagnostics.

Where the plugin is applied (verify with `show <proj>/<scope>/scalacOptions`, not by reading `build.sbt`):

| Subproject | `Compile` | `Test` |
|---|---|---|
| `internals`, `plugin`, `compiler_ir` | — | — |
| `core` | — | ✔ + `-P:dfhdl.plugin:testing` |
| `compiler_stages` | — | ✔ |
| `lib`, `platforms`, `ips`, `benchmarks` | ✔ | ✔ |

So `core` and `compiler_stages` build their own sources *without* the plugin and only apply it to their tests. `core`'s test scope additionally enables the `PluginErrCheck` phase behind `assertPluginError` (see [devdocs/plugin-error-testing.md](devdocs/plugin-error-testing.md)).

`internals` is the only plugin-free subproject with a test directory, which makes it the sandbox for minimizing a suspected *compiler* bug away from DFHDL's own machinery (see [/bugfix](.claude/commands/bugfix.md)).

Plugin options are `-P:dfhdl.plugin:<option>`, parsed in `plugin/src/main/scala/plugin/Setting.scala` (register a new flag there, or it is read as the config file path instead):

| Option | Purpose |
|---|---|
| `testing` | Enables the `PluginErrCheck` phase behind `assertPluginError`. DFHDL's own tests only; see [devdocs/plugin-error-testing.md](devdocs/plugin-error-testing.md) |
| `disableCustomPrinter` | Leaves the DFHDL type printer and diagnostic re-reporter uninstalled, so errors read in the compiler's own vocabulary. Debugging aid; `TypePrinterSpec` fails while it is on |

## Testing

**Framework**: munit 1.2.2

- **Stage tests**: `compiler/stages/src/test/scala/StagesSpec/` — tests each compiler stage
- **Doc example tests**: `lib/src/test/scala/docExamples/` — validates documentation examples
- **Arithmetic tests**: `lib/src/test/scala/ArithSpec/`
- **AES tests**: `lib/src/test/scala/AES/`
- **Base class**: `DesignSpec` — provides `assertCodeString()` and `assertElaborationErrors()`
- **Playground**: `lib/src/test/scala/Playground.scala` — used for quick local iteration via `quickTestSetup`

Generated HDL reference files live in `lib/src/test/resources/ref/`. Update them with `sbt docExamplesRefUpdate` after intentional output changes.

`testApps` auto-detects installed simulation tools (ghdl, nvc, verilator, iverilog, questa, vivado) and runs the AES cipher simulation against all available tool/dialect combinations.

## Code Conventions

- **Formatting**: scalafmt 3.10.6, max 100 columns, Scala 3 dialect
  - Optional braces removed (`removeOptionalBraces = oldSyntaxToo`)
  - End markers inserted for blocks ≥ 15 lines
  - Run `scalafmt` before committing
- **Compiler flags**: `-language:strictEquality`, `-unchecked`, `-feature`, `-preview`, `-deprecation`
- **Implicit conversions**: only enabled in `internals` and `compiler_ir` via `implicitConversionSettings`
- **Naming**: `DF`-prefixed types (e.g., `DFVal`, `DFType`), `DFC` for context; stage names follow `Drop*`, `Add*`, `Connect*`, `Break*` patterns
- **Package root**: `dfhdl.*`
- **No em dashes**: never use em dashes in official documentation (`docs/`) or in code documentation/comments; use commas, colons, semicolons, parentheses, or separate sentences instead

## Key Files

| File | Purpose |
|---|---|
| `build.sbt` | Multi-project build definition |
| `project/DFHDLCommands.scala` | Custom SBT commands |
| `.scalafmt.conf` | Code formatting rules |
| `properdocs.yml` | Documentation site config (ProperDocs, a maintained MkDocs fork; build with `properdocs build`) |
| `sandbox/` | Generated output during tests/apps (gitignored, cleared by `clearSandbox`) |
| `*/target/scala-*/dfhdl-cache/` | Sub-design elaboration cache, keyed by code digest (cleared by `clearElabCache`; see [devdocs/elaboration-caching.md](devdocs/elaboration-caching.md)) |
| `lib/src/test/resources/ref/` | Reference HDL output snapshots for regression tests |

## External Simulation Tools (for `testApps`)

CI installs these via OSS CAD Suite:
- **Verilog**: verilator, iverilog (sv2005 skipped for iverilog), questa, vivado
- **VHDL**: ghdl, nvc, questa, vivado (v2008 skipped for vivado)

## Claude Instructions

- When asked to **fix a reported bug**, especially one where the generated HDL is wrong or illegal, invoke the `/bugfix` skill before doing any work.
- When asked to **create a new compiler stage** or **modify an existing compiler stage**, always invoke the `/new-stage` skill before doing any work. This applies to *any* edit under `compiler/stages/`, including a one-line change; reading the skill file is not the same as invoking it.
- When working on **compile time performance**, invoke the `/compile-perf` skill to review the methodology, known bottlenecks, and what has already been tried.
- When working on the **DFacsimile native simulator** (`compiler/stages/src/main/scala/dfhdl/sim/`), invoke the `/dfacsimile` skill for the architecture, the fidelity contract, the lockstep testing methodology, and the banked gotchas.

## Licenses

- Main library (`internals`, `plugin`, `compiler_ir`, `core`, `compiler_stages`, `lib`, `ips`): **LGPL v3.0**
- `platforms/`: **Apache 2.0**
