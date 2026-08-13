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
   Match port names exactly, `i_`/`o_` prefixes included. A module named `top` needs no
   workaround: nothing named `top` enters scope through `import dfhdl.*`, so `class top` both
   declares and instantiates normally, and the emitted module name stays `top`.
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
- **What the elaboration *reads*, it pins.** `.toScalaInt` on a param, or a Scala `if` on one, makes
  the design non-generic: DFHDL emits a `$fatal` design-parameter constraint and one *specialised*
  module per distinct parameterisation (`rvrangecheck_0/_1/_2`). That is correct behaviour, not a
  bug, but it is rarely what a port wants, because the baseline is one module instantiated N times.
  Keep it generic by never reading the parameter:
  - `clog2` takes an `Int <> CONST` directly, so `10 + clog2(SIZE)` emits the baseline's own
    `localparam int MASK_BITS = 10 + $clog2(CCM_SIZE);`. **No `.toScalaInt`.**
  - Slice bounds accept `Int <> CONST` and keep the name: `addr(31, MASK_BITS)` → `addr[31:MASK_BITS]`.
  - A `generate`-style choice between two bodies becomes **`.sel` on a constant condition**, not a
    Scala `if`: `x <> base & (SIZE == 48).sel(masked, 1)` folds at synthesis and covers both arms
    while leaving `SIZE` free. Prefer this to dropping the dead arm.
  - Reserve `.toScalaInt` for what genuinely needs a Scala `Int` (an `initFile` path, a `Vec` size
    the frontend cannot take as a const).
- **`all(0)` for an explicitly-typed constant default** — `val CCM_SADR: Bits[32] <> CONST = all(0)`
  rather than spelling out `h"32'00000000"`.
- **DFacsimile rejects `String <> CONST`** (the minimum tier can't resolve a `DFString` const's
  param-dependent width). For an elaboration-only string (e.g. an `initFile` path), use a plain Scala
  `String` parameter, not `String <> CONST`, so it never enters the simulated IR. `Int <> CONST`
  widths do resolve.

## Writing the body - idioms that keep the baseline's shape

The from-verilog guide covers the operators; these are the choices *between* equally-legal spellings,
and they decide how closely the emitted HDL tracks the gold.

- **A Verilog `assign` is a connection: `<>`, not `:=`.**
- **Prefer a named value to a variable.** DFHDL does not need a variable to hold an expression:
  `val x = <expr>` beats `val x = T <> VAR` followed by an assignment. Declare a `VAR` only where the
  baseline drives the bits **separately** (a per-bit `assign`, a `generate` of assigns), which is
  exactly when a single named value cannot express it:
  ```scala
  val error_mask = Bits(39) <> VAR                       // 39 independent assigns: a VAR
  for (i <- 1 until 40) error_mask(i - 1) <> (syndrome == i)
  ```
  Intermediates that are pure renames upstream should just disappear.
- **Bit logic uses `&`, `|`, `~`** — not `&&`, `||`, `!`. It rarely changes 2-state behaviour but it
  can for **x-value equivalence**. Write the bitwise form and let the emitter choose: it prints `&`
  when the operands are `Bit` and `&&` when they are `Boolean`, matching whichever the baseline used.
- **Concatenate with a tuple**, not chained `++`:
  ```scala
  val x: Bits[39] <> VAL = (a, b, c)   // ascribed, which also checks the total width
  val y = (a, b, c).toBits             // unascribed
  port <> (a, b, c)                    // connecting straight out
  ```
- **`reduce`/`foldLeft` over DFHDL values need an explicit element type** — the result of an
  operation is a plain value, which will not unify with the collection's element type. DFHDL's error
  names the fix:
  ```scala
  group.map(din(_)).foldLeft[Bit <> VAL](ecc_in(i))(_ ^ _)   // ecc_in[i] ^ din[..] ^ ...
  group.map(din(_)).reduce[Bit <> VAL](_ ^ _)                // also fine
  ```
  Seeding a `foldLeft` with the baseline's own first term emits a **flat** chain; `reduce` adds a
  paren group. Prefer `foldLeft` when the baseline starts the chain from a distinguished operand.
  Neither works for a *widening* fold (`_ ++ _`), where no fixed element type exists.
- **A comparison yields `Boolean <> VAL`, not `Bit`.** `.bit` converts, and is needed before
  concatenating a comparison result.
- **Convert once, at the definition.** `val syndrome = ecc_check(5, 0).uint` so every use reads
  `syndrome == i`, rather than restating `.uint` at each use.
- **A Scala `var` accumulator is an ED-domain construct.** The elaboration-time
  `var acc: Bits[Int] <> VAL = ...; acc = acc ++ x` idiom in the type-system guide is rejected by the
  plugin inside an `RTDesign`; use a `VAR` there. The error says so explicitly.
- **A purely combinational design gets no clock or reset ports** — an `RTDesign` with no registers
  emits a clean port list, so combinational leaf modules need no annotation at all.

## Emitter gotchas not in the guide

- **`buf`** (and other Verilog keywords) leak unescaped into emitted **port** names → syntax error.
  `i_buf` is fine; a bare `buf` is not. (Scala-reserved names are handled by backtick/@targetName per
  the guide; this is about Verilog-reserved emitted names.)
- Naming a bit-select of an **assignable** port (`val pc = ibus_adr(0)`) registers as a *connection
  into* that port → "multiple connections write" at backend stages. Inline the select at its uses.
- **NTFS is case-insensitive:** writing `servant.scala` while `Servant.scala` exists writes *into* the
  old file. Delete old-cased files before renaming, and `clearSandbox` before regenerating renamed
  output.
- **Verilog ranges with a non-zero base do not survive.** `logic [31:1] prett` and `logic [18:2] x`
  become `[30:0]` and `[16:0]`: the same width, packing identically inside a struct, but **indexed
  differently**. Baseline `prett[j]` is `prett(j - 1)`. It compiles clean either way, so every slice
  of such a field has to be translated deliberately; note it at the declaration.
- **A fully-assigned `VAR` read through a *parameter*-bounded slice is misreported as a latch**
  (DFHDL#484). A local `Int <> CONST` bound is fine; only a design parameter trips it, and only for a
  `VAR` (a port or parameter sliced the same way is fine). Where the variable is a pure rename, slice
  the parameter directly instead.

## Packed structs and the type package

- **Field order is the baseline's, unreversed.** A SystemVerilog `struct packed` packs its
  first-declared field at the MSB and DFHDL's `Struct` does the same, emitting a real
  `typedef struct packed` into `<Top>_defs.svh` in declaration order. Worth confirming per port with
  a two-field probe, since packets sliced as flat vectors would diverge silently.
- **Ascribe constants so the names reach the HDL.** A plain Scala `Int` folds into a literal and the
  name is gone; `: Int <> CONST` emits a named `parameter int` and **preserves the definition chain**
  (`parameter int DCCM_BITS = RV_DCCM_BITS;`), so derived widths print as `[DCCM_BITS - 1:0]` rather
  than `[15:0]`. That is what makes the generated HDL diffable against the gold. Only constants the
  elaborated design references reach the defs header, so declaring the full set costs nothing.
- **An include's *scope* decides its Scala form.** A globally-included macro header maps to
  **top-level definitions in the package** (visible everywhere, no import). A header `` `include ``d
  *inside module bodies* makes its localparams members of each module, which only **`export`**
  reproduces: it puts the names on the type, where a plain `import` would not. Note that a package
  cannot be an export target, so body-scoped headers must be an `object`.
- **Macros that are only `` `ifdef ``-tested** (and ones naming an SRAM cell) are plain Scala
  `Boolean`s/`String`s: they select code at elaboration and must not reach the IR.
- **Scaladoc on a constant propagates into the emitted HDL** as a comment, so width derivations can
  be explained in the generated header too.

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
