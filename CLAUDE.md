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
sbtn clearSandbox     # delete sandbox/ directory
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
| plugin | `plugin` | `plugin/` | Scala 3 compiler plugin (9 phases) |
| compiler_ir | `compiler_ir` | `compiler/ir/` | IR/AST data structures, type system |
| core | `core` | `core/` | HDL language abstractions (DFVal, DFType, Design) |
| compiler_stages | `compiler_stages` | `compiler/stages/` | 50+ transformation stages for code generation |
| lib | `lib` | `lib/` | Standard library: arithmetic, memory, ALU, crypto |
| platforms | `platforms` | `platforms/` | FPGA board wrappers (Apache 2.0 licensed) |
| ips | `ips` | `ips/` | IP cores library |

## Compiler Plugin Phases

Located in `plugin/src/main/scala/plugin/`:

1. `PreTyperPhase` — pre-typing transformations
2. `TopAnnotPhase` — top-level annotation processing
3. `MetaContextPlacerPhase` — places meta-context markers
4. `LoopFSMPhase` — loop-to-FSM transformations
5. `CustomControlPhase` — custom control flow
6. `DesignDefsPhase` — design definition processing
7. `MetaContextDelegatePhase` — meta-context delegation
8. `MetaContextGenPhase` — meta-context code generation
9. `OnCreateEventsPhase` — on-create event handling

The plugin is applied to `core`, `compiler_stages`, `lib`, `platforms`, and `ips` via `pluginUseSettings` / `pluginTestUseSettings`.

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

## Key Files

| File | Purpose |
|---|---|
| `build.sbt` | Multi-project build definition |
| `project/DFHDLCommands.scala` | Custom SBT commands |
| `.scalafmt.conf` | Code formatting rules |
| `mkdocs.yml` | Documentation site config |
| `sandbox/` | Generated output during tests/apps (gitignored, cleared by `clearSandbox`) |
| `lib/src/test/resources/ref/` | Reference HDL output snapshots for regression tests |

## External Simulation Tools (for `testApps`)

CI installs these via OSS CAD Suite:
- **Verilog**: verilator, iverilog (sv2005 skipped for iverilog), questa, vivado
- **VHDL**: ghdl, nvc, questa, vivado (v2008 skipped for vivado)

## Claude Instructions

- When asked to **create a new compiler stage** or **modify an existing compiler stage**, always invoke the `/new-stage` skill before doing any work.
- When working on **compile time performance**, invoke the `/compile-perf` skill to review the methodology, known bottlenecks, and what has already been tried.

## Licenses

- Main library (`internals`, `plugin`, `compiler_ir`, `core`, `compiler_stages`, `lib`, `ips`): **LGPL v3.0**
- `platforms/`: **Apache 2.0**
