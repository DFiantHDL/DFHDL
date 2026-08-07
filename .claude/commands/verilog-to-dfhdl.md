# Verilog-to-DFHDL Conversion Guide

> **For porting existing Verilog/SystemVerilog RTL (a module or a whole hierarchy) to DFHDL designs.**
> This skill is version-controlled alongside the codebase - keep it updated when the frontend or
> clock/reset infrastructure changes. After a port, add any new pattern/pitfall you hit to the
> "Keeping This Skill Up to Date" section at the bottom.

## Read these first (do not duplicate them here)

The language-level translation is already documented. **Read them before porting** and follow them
for all the mechanics this skill deliberately omits:

- **[Transitioning from Verilog][from-verilog]** (`docs/transitioning/from-verilog/index.md`) -
  module/param/port mapping, `logic`/`reg`/`wire` → `VAR init`, `UInt`/`Bits`/`SInt` choice, numeric
  literals, `$clog2` → `.until`/`.to`, `always` → `process`, blocking/non-blocking → `:=`/`:==`, FSM →
  `enum extends Encoded`, integer `case` → `match`, `default:`/`others` → `case _` (kept for formal
  equivalence unless binary encoding with exactly 2^n cases), functions/tasks → methods, all operators (shift,
  `|&~`, reductions, `.repeat`, `++`/`.toBits`, part-select `-:`/`+:`, signed arithmetic), `generate
  for` → Scala `for`, reserved-keyword escaping (backtick / `@targetName`), `Bits` init `all(0)`,
  ternary → `.sel`.
- **[Design Domains][design-domains]** (`docs/user-guide/design-domains/index.md`) - the RT-domain
  register model (`VAR.REG`/`OUT.REG init`, `.din :=`, `.reg` aliases, conditional/enable
  registration) and the `@hw.constraints.timing.clock/reset` annotation fields (rate, edge, mode,
  active, portName, inclusionPolicy, grpName, `@timing.related`, empty `@timing.clock()`).
- **[Type System][type-system]** (`docs/user-guide/type-system/index.md`) - declarations,
  conversions, slicing, concatenation, parameterization, per-type operations.

This skill only covers what those pages do **not**: the porting **workflow**, the clock/reset
**magnet wiring across a hierarchy**, `initFile` memories, and the emitter gotchas.

## RTDesign vs EDDesign

The from-verilog guide shows `EDDesign` + `process(clk.rising)` + `:==`. For ordinary **synchronous
RTL** prefer `RTDesign` + `VAR.REG`/`OUT.REG init` + `.din :=`: the clock/reset become magnets
(below), reset values fold into register inits, and there is no explicit `clk`/`rst` port or process
to write. Reserve `EDDesign`/`process` for genuinely event-driven or multi-edge logic.

## Workflow (bottom-up)

1. **Read the baseline module fully.** Note: exact port names + directions + widths, the clock/reset
   port names, **which registers the reset actually targets** (a "MINI"/partial reset resets only
   some), the parameters, and any `generate`-gated variants.
2. **One module per design, in a same-named file** (case-sensitive: `serv_alu` in `serv_alu.scala`).
   Match port names exactly, `i_`/`o_` prefixes included.
3. **Compile it standalone and read the emitted HDL:**
   ```bash
   sbtn.bat ";clearSandbox ;<proj>/runMain <pkg>.<ClassName> compile"
   ```
   `@top` is implicit, so every design is runnable. Whatever you compile is the **top**, written to
   `sandbox/<ClassName>/hdl/`. **`clearSandbox` between runs** so stale output does not mislead you.
4. **Diff against intent:** port list, clock/reset ports, reset branches, register inits.
5. Move up. Compiling a parent pulls in every child, so the top-level compile is the integration
   check. Whole-package sanity: `<proj>/Test/compile`.

## Clock and reset - the magnet model

Clocks/resets are **magnets**: not ordinary ports, and they **auto-connect across the hierarchy**.
Never wire `child.clk <> clk` - a parent's `wb_clk` connects to a child's `clk`/`i_clk`/`i_wb_clk`
automatically (the emitted parent shows `child_i_clk = wb_clk`). Beyond the per-module
`@hw.constraints.timing.clock/reset` annotations (see [Design Domains][design-domains]):

- **Set the package-wide default port names** with top-level givens in one file (e.g. `config.scala`).
  They are visible to every file's implicit `@top` main and win over the library default (`clk`/`rst`):
  ```scala
  given options.ElaborationOptions.DefaultClkCfg =
    hw.constraints.timing.clock(portName = "wb_clk")
  given options.ElaborationOptions.DefaultRstCfg =
    hw.constraints.timing.reset(portName = "wb_rst")
  ```
  Use the top's names as the global default; override the internals per-module.
- **A clock-only annotation removes the reset.** On an `RTDesign` *with* registers, annotating only
  `@hw.constraints.timing.clock(portName = "...")` (no reset annotation) suppresses the default
  reset entirely; the register `init`s emit as **power-up only** (`logic r = 1'b0;`, no `if(rst)`).
  This is how you port a no-reset module (pipeline, RAM).
- **Annotation ⇒ auto-reset.** A `@..reset` annotation synchronously resets every register with a
  real `init` to that init (`if (rst) r <= init;`). A Verilog `if(rst) r <= RESET_VAL` mux folds
  straight into `init RESET_VAL` - drop the explicit mux.
- **A declared `Rst <> IN` is readable but does NOT auto-reset.** To read the reset combinationally
  (e.g. `o_cyc := r && !i_rst.actual`), declare `val i_rst = Rst <> IN` and read `i_rst.actual`.
  Unlike the annotation this does **not** reset the registers. If a module both reads the reset and
  needs registers reset, write the reset explicitly at the end (last-write-wins):
  ```scala
  val i_rst = Rst <> IN
  o_ibus_cyc := ibus_cyc_r && !i_rst.actual
  ...
  if (i_rst.actual)         // explicit MINI reset; a declared Rst port won't auto-reset
    cnt.din := 0
    init_done.din := 0
  ```
- A port literally named **`rst`/`clk`** collides with the magnet ("Unsupported read-to-read
  connection"). Use the baseline's real name (`i_rst`, `wb_clk`, ...).

## Memories and `initFile`

`reg [W-1:0] mem [0:D-1]` → `val mem = Bits(W) X D <> VAR.REG` with `initFile "path.hex"` (readmemh
preload) or `init all(all(0))`. Access: `mem[addr][7:0] <= d` → `if (we) mem(addr.uint)(7,0).din := d`;
the index must be an exact-`clog2`-width `UInt`.

**MEMORY + RESET (keep an `initFile` memory out of the reset):** a `VAR.REG`/`OUT.REG` memory with
`init`/`initFile` in a **reset domain** is swept into the synchronous reset - DFHDL emits
`if (rst) mem <= '{0:.., 1:.., ...}`, reloading the *entire array* every reset cycle (huge fanout,
wrong hardware). `init ?` avoids reset but drops the power-up value. To keep the module's reset (for
its other flops), the `initFile` power-up, **and** a single clock port, put the memory in a nested
`RTDomain` that is `@timing.related(self, includeReset = false)` (shares the parent clock, no reset;
see [Design Domains][design-domains]) plus `@hw.annotation.flattenMode.transparent`, with the
read/write logic **inside** the domain:
```scala
class servant_ram(...) extends RTDesign:
  self =>
  val o_wb_rdt = Bits(32) <> OUT.REG init all(0)   // module-reset flop
  val o_wb_ack = Bit       <> OUT.REG init 0        // module-reset flop
  @hw.constraints.timing.related(self, includeReset = false)
  @hw.annotation.flattenMode.transparent
  val write = new RTDomain:
    val mem = Bits(32) X words <> VAR.REG initFile memfile
    if (we && i_wb_sel(0)) mem(i_wb_adr)(7, 0).din := i_wb_dat(7, 0)   // writes INSIDE the domain
  o_wb_rdt.din := write.mem(i_wb_adr)               // read in the parent scope
```
The memory emits power-up-initialized and *outside* the reset, with non-blocking `<=` writes
(read-first, matching a Verilog RAM); the reset block resets only the other flops.

Pitfalls: leaving the write logic in the **parent** scope makes domain-flattening pull `mem` back
into the parent reset; a bare `RTDomain` with its own `@timing.clock` spawns a *duplicate* clock
port; a `VAR.SHARED` mem is for **multi-ported** RAMs (its clocked writes also lower to non-blocking
`<=`, read-first; historically they emitted blocking `=` until issue #437); an `EDDomain` (or a
`Clk`/`Rst`/`process` *inside* one) is rejected by DFacsimile. DFacsimile builds a `@timing.related` RT domain
inline and binds an explicit `Clk`/`Rst` port to its deasserted value (both added to
`DFacsimile.scala` alongside this port).

## Parameters - beyond the guide

Follow [from-verilog][from-verilog] for `Int <> CONST`/`String <> CONST` (they emit as SV
`parameter int`/`parameter string`) and `.toScalaInt`/`.toScalaString` for elaboration-time use
(widths, `Vec` sizes, `initFile` paths, Scala `if`). Additionally:

- **Dependent-type-on-`private` gotcha:** a public member whose type depends on a `private val`
  (`Bits(32) X words` where `words` is `private`) fails with *"refers to private value ... in its type
  signature"*. Make the helper `val` non-private.
- **No `generate` for structural params yet.** Parameters that change structure (bus width `W`,
  optional sub-blocks) cannot be made generic; hardwire them to the target configuration and note it.
  Standalone `runMain <ClassName> compile` needs a **default** for every CONST param.
- **DFacsimile rejects `String <> CONST`** (the minimum tier can't resolve a `DFString` const's
  param-dependent width). For an elaboration-only string (e.g. an `initFile` path), use a plain Scala
  `String` parameter, not `String <> CONST`, so it never enters the simulated IR. `Int <> CONST`
  widths do resolve.

## Emitter gotchas not in the guide

- **`buf`** (and other Verilog keywords) leak unescaped into emitted **port** names → syntax error.
  `i_buf` is fine; a bare `buf` is not. (Scala-reserved names are handled by backtick/@targetName per
  the guide; this is about Verilog-reserved emitted names.)
- Naming a bit-select of an **assignable** port (`val pc = ibus_adr(0)`) registers as a *connection
  into* that port → "multiple connections write" at backend stages. Inline the select at its uses.
- **NTFS is case-insensitive:** writing `servant.scala` while `Servant.scala` exists writes *into* the
  old file. Delete old-cased files before renaming, and `clearSandbox` before regenerating renamed
  output.

## Non-synthesizable baseline constructs

`$finish`/`$display`/`$write`/`$fopen`/`forever @(negedge ...)` have no synthesizable equivalent.
Replace with **observation output ports** (e.g. an `o_halt` pulse instead of `$finish`) and/or a
**synthesizable stand-in** design; note every deviation in the file header.

## Simulating ported designs (DFacsimile)

The typed sim API does **not** expose the implicit reset magnet as `dut.rst`. DFacsimile applies all
register/memory inits at time zero (the reset values), so **do not poke a reset preamble** - just
`run.continue(n)`. Peek nested members through the instance path (`dut.soc.cpu.state.cnt.peek`).

## Keeping This Skill Up to Date

When a port surfaces a new clk/rst subtlety, memory behaviour, or emitter gotcha **not covered by the
guide pages above**, add it here with a one-line Verilog→DFHDL example. If it *is* a general language
feature, put it in the user guide instead and link it.

[from-verilog]: ../../docs/transitioning/from-verilog/index.md
[design-domains]: ../../docs/user-guide/design-domains/index.md
[type-system]: ../../docs/user-guide/type-system/index.md
