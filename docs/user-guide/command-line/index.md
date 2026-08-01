# Command-Line Interface {#cli}

Every top-level DFHDL design is also a runnable program, and that program has a full command-line interface. Running it with no arguments elaborates and compiles the design with the defaults declared in your source. Every one of those defaults can be overridden for a single run from the command line, without touching the code.

## Anatomy of a Command Line

```{.console linenums="0"}
<your program> [design-args] [app-options] <mode> [mode-options]
```

| Group | What it sets | Examples |
|---|---|---|
| Design arguments | The design's own parameters | `--width 12` |
| App options | Behavior that applies to the whole run | `--nocache`, `--log info` |
| Mode | How far down the pipeline to go | `commit`, `simulate` |
| Mode options | Settings specific to that mode | `-b vhdl`, `-t questa` |

/// admonition | Order matters
    type: warning
Design arguments and app options belong **before** the mode; mode options belong **after** it. This is a plain subcommand structure, so an option placed on the wrong side of the mode is reported as unknown:

```{.console linenums="0"}
[scallop] Error: Unknown option 'log'
```
///

### Running It

The program is a normal Scala entry point, so how you launch it depends on how you build your project. The usage line at the top of the [help output](#cli-help) always shows the exact form for the environment you are currently in.

/// tab | Scala CLI
```{.console .copy linenums="0"}
scala run . -M Counter -- --width 12 commit -b vhdl
```
///

/// tab | sbt
From your shell:

```{.console .copy linenums="0"}
sbt "runMain Counter --width 12 commit -b vhdl"
```

Or from inside the sbt shell:

```{.console .copy linenums="0"}
runMain Counter --width 12 commit -b vhdl
```
///

/// tab | Packaged program
```{.console .copy linenums="0"}
./counter --width 12 commit -b vhdl
```
///

## Getting Help {#cli-help}

`help` is a mode of its own, and `-h` / `--help` prints the same text. The help screen lists the design name, the usage line, the design arguments, the app options, and every mode with its options. It is generated from your design and your source-level options, so the `(default = ...)` next to each option is always the value that a run would actually use.

```{.console .copy linenums="0"}
<your program> help
```

```title="help output (truncated)"
Design Name: Counter
Usage: runMain Counter [design-args] <mode> [options]
 Design arguments:
      --width  <Int>   (default = 8)

      --cache          Enable caching
      --nocache        Disable caching
      --log  <level>   Set the log level of the compilation and app progress
                       messages Choices: off, error, warn, info, debug, trace,
                       all (default = warn)
  -h, --help           Show help message

Mode: elaborate - Elaboration only (no compilation)
...
Mode: commit - Committing to disk (after elaboration and compilation) [default]
...
```

The mode marked `[default]` is the one that runs when you give no mode at all.

Three sub-modes list the choices that are too long to inline into the main screen:

| Command | Lists |
|---|---|
| `help backend` | Every backend language and dialect |
| `help lint-tool` | Every integrated linter |
| `help simulate-tool` | Every integrated simulator |

Add `-s` / `--scan` to the two tool listings to also probe your system for each tool and report the version it found:

```{.console .copy linenums="0"}
<your program> help simulate-tool --scan
```

```title="help simulate-tool --scan output (truncated)"
Selectable Verilog/SystemVerilog simulation tools:
verilator            - Verilator (default) Found version 5.049
iverilog             - Icarus Verilog      Found version 14.0
vlog|questa|modelsim - QuestaSim/ModelSim  Found version 2023.3
xvlog|vivado|xsim    - Vivado Simulator    Not found on your system
```

## Modes {#cli-modes}

The modes are stages of one pipeline. Selecting a mode runs it and everything it depends on, so `simulate` also elaborates, compiles, and commits.

```d2 pad="10"
direction: right
elaborate -> compile -> commit
commit -> lint
commit -> simulate
commit -> build -> program
```

| Mode | What it does |
|---|---|
| `elaborate` | Elaborates the Scala design into the DFHDL representation |
| `compile` | Compiles it down to the backend language, in memory only |
| `commit` | Writes the generated backend files to disk |
| `lint` | Runs an external linter over the committed files |
| `simulate` | Builds and runs an external simulation of the committed files |
| `build` | Runs an external FPGA build (synthesis, place and route, bitstream) |
| `program` | Programs the FPGA, or its flash device, with the built bitstream |
| `help` | Prints the usage text and exits |

Committed files are written under `sandbox/<TopName>/`. The location is controlled by the `#!scala given options.CompilerOptions.CommitFolder` and `#!scala given options.CompilerOptions.NewFolderForTop` declarations.

### The Default Mode

When you give no mode, DFHDL picks one that fits the design:

| The design | Default mode |
|---|---|
| Runs in the browser (Scastie) | `compile` |
| Has no ports, so it is a testbench or a self-contained design | `simulate` |
| Declares board resources, so it targets a physical device | `program` |
| Anything else | `commit` |

Declare a mode in your source to override that choice:

```scala
given options.AppOptions.AppMode = options.AppOptions.AppMode.lint
```

## Design Arguments {#cli-design-args}

Every design parameter that DFHDL can represent is exposed as an option named after the parameter. Parameters of other Scala types (a custom class, for instance) are not exposed. See [Design Hierarchy][design-params-syntax] for how parameters are declared.

| Declared type | Pass it as | Example |
|---|---|---|
| `Int` | A plain integer | `--width 12` |
| `Double` | A plain double | `--rate 3.14` |
| `String` | Plain text | `--name hello` |
| `Boolean <> CONST` | `true` or `false` | `--enable false` |
| `Bit <> CONST` | `0` or `1` | `--sel 1` |
| `Bits[W] <> CONST` | A DFHDL bits literal | `--mask h"5a"` |
| `UInt[W] <> CONST` | A DFHDL unsigned literal | `--limit d"16'100"` |
| `SInt[W] <> CONST` | A DFHDL signed literal | `--offset sd"32'-42"` |

There is no implicit toggle for `Boolean` or `Bit`: the value is always explicit, so `--enable` on its own is not a way to turn a `Boolean` parameter on.

/// admonition | Quoting bit-accurate literals
    type: tip
The `Bits`, `UInt`, and `SInt` literals contain double quotes, which your shell will strip unless you protect them. Quote or escape the whole argument the way your shell requires, for example `--mask 'h"5a"'` in a POSIX shell.
///

## App Options {#cli-app-options}

These apply to the whole run, whichever mode you select, and are given before the mode.

### `--cache` / `--nocache`

Caching is on by default, and the flag covers both of the caches a run may consult:

* Each pipeline step stores its result under `sandbox/<TopName>/cache` and reuses it when nothing it depends on has changed, including your design's own code.
* During elaboration, a sub-design whose code and arguments are unchanged is adopted from the sub-design cache instead of having its body elaborated again.

Pass `--nocache` to force everything to run afresh:

```{.console .copy linenums="0"}
<your program> --nocache commit
```

### `--log <level>`

Sets how much the run reports:

```{.console .copy linenums="0"}
<your program> --log info commit
```

| Level | What you see |
|---|---|
| `off` | Nothing |
| `error` | Errors only |
| `warn` | Errors and warnings, the compiler's default |
| `info` | Adds a progress line per compilation stage |
| `debug` | Adds a consistency check of the design after every stage |
| `trace` | Adds a printout of the DFHDL code after every stage that changed the design |
| `all` | The most permissive level |

The level applies to both the compilation messages and the program's own progress messages (`Elaborating design...`, `Compiling design...`). The default shown in the help screen, `warn`, is the compilation one. The progress messages have their own, louder default, so a run without `--log` still reports its progress, while `--log off` silences the run completely.

/// admonition | A cached step logs nothing
    type: tip
`--log trace` on a fully cached run prints very little, because no stage actually runs. Combine it with `--nocache` when you want to watch the compilation itself.
///

## Mode Options {#cli-mode-options}

Each mode adds the options of the modes it depends on, so `commit` accepts everything `compile` and `elaborate` accept.

### `elaborate`

| Option | Default | Description |
|---|---|---|
| `--print-elaborate` | off | Print the DFHDL design after elaboration |
| `--Werror` / `--noWerror` | `--noWerror` | Make elaboration warnings fatal and exit with a non-zero code |

### `compile`

| Option | Default | Description |
|---|---|---|
| `-b`, `--backend <lang[.dialect]>` | `verilog.sv2009` | Target language and dialect, see `help backend` |
| `--print-compile` | off | Print the DFHDL design after compilation |
| `--print-backend` | off, on in the browser | Print the generated backend code |
| `--global-defs-name <name>` | none | Override the name of the global definitions file, without its suffix |

The backend takes a language and an optional dialect. `-b vhdl` selects VHDL with its default dialect, `-b verilog.v2001` pins Verilog 2001:

| Language | Dialects |
|---|---|
| `verilog` | `v95`, `v2001`, `sv2005`, `sv2009` (default), `sv2012`, `sv2017` |
| `vhdl` | `v93`, `v2008` (default), `v2019` |

### `commit`

No options of its own beyond those of `compile`.

### External tool modes

`lint`, `simulate`, `build`, and `program` all invoke an external tool, and share these options:

| Option | Default | Description |
|---|---|---|
| `--tools-location <location>` | `dftools` | `dftools` runs the tools from the pinned DFTools image, `local` uses the tools found on your `PATH` |
| `--Werror-tool` / `--noWerror-tool` | `--noWerror-tool` | Make tool warnings fatal and exit with a non-zero code |

`lint` and `simulate` select their tool per language, and you may give one side or both, separated by `/`:

```{.console .copy linenums="0"}
<your program> simulate -t questa
<your program> simulate -t iverilog/ghdl
```

| Option | Default | Description |
|---|---|---|
| `-t`, `--tool <[verilog][/][vhdl]>` | `verilator/ghdl` | Verilog and VHDL tool selection, see `help lint-tool` or `help simulate-tool` |

`build` and `program` select a tool family instead:

| Option | Default | Description |
|---|---|---|
| `-t`, `--tool <family>` | `vendor` | `foss` for free and open source tools, `vendor` for the device vendor's tools |
| `--flash` / `--noflash` | `--noflash` | Target the on-board flash device rather than the FPGA alone |
| `--compress` / `--nocompress` (`build` only) | `--compress` | Compress the bitstream file |

Because `program` depends on `build`, `program -t foss` both builds and programs with the open source flow, and `program --flash` builds a flash image as well.

## The Command Line and `given` Options {#cli-vs-given}

The `#!scala given options.*` declarations in your source are the defaults, and the command line overrides them for one run. The two describe the same settings:

```scala title="in your source"
given options.CompilerOptions.Backend = _.vhdl
```

```{.console linenums="0" title="for a single run"}
<your program> compile -b verilog
```

Use a `given` for what your project always wants, such as the target language of a design that only makes sense in VHDL, and the command line for what varies between runs, such as which simulator you happen to have installed today. Since the help screen reads its defaults from the givens, `help` is also the quickest way to confirm what your source-level options actually resolved to.

## Warnings and Exit Codes {#cli-exit-codes}

A run that fails exits with a non-zero code, which is what a CI job checks. Warnings are not failures by default, and two options promote them:

* `--Werror` makes DFHDL's own elaboration warnings fatal.
* `--Werror-tool` makes the warnings reported by an external linter, simulator, builder, or programmer fatal.

```{.console .copy linenums="0" title="a strict CI invocation"}
<your program> --nocache lint --Werror --Werror-tool -t verilator
```
