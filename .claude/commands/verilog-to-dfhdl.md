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

**Do not trust a green result until the check can go red.** Every one of these produced a passing
signal that meant nothing, in a single port:

- a `sed` mutation that silently did not match, so the "control" tested the unmodified design;
- a `sed -i` on the proof script that rewrote the *gold* filename too, so the tool errored out and
  the grep for a failure string found none;
- a mutation that applied but was **semantically null** (widening `pc[8:7]` to `pc[9:7]` truncates
  back to the same bits), so it changed the text and not the function;
- `grep -c "proof finished"`, which matches the failure line as happily as the success line;
- `cmd | head` reporting success because `head` exited 0 while the command behind it failed;
- several probe classes in one file, where a compile error in one is reported against another that
  never ran;
- `grep '^\[error\]'` finding nothing because the tool prefixes lines with an ANSI escape.

The habits that catch them: **assert the mutation applied** (diff the line count) and **assert the
control fails** before believing any pass; grep for the exact success string, never a substring
shared with failure; put each probe in its own file; and check for the artifact the run should have
produced rather than an exit code.

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
  reset entirely; the register `init`s emit as **power-up only** (an `initial` block, no `rst_l` port
  and no `if(rst)` arm). This is how you define a domain with an init and no reset signal, i.e. how
  you port a no-reset module (pipeline, RAM). It is *not* a bug, and it is silent, so when you
  rename a clock port on a module that **does** reset, restate `@..reset` alongside it or the flop
  quietly becomes reset-less:
  ```scala
  @timing.clock(portName = "rawclk")                                    // alone: no reset
  @timing.reset(mode = _.async, active = _.low, portName = "rst_l")     // keep the reset
  ```
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
- **Declaring the baseline's own `clk` port is fine and unifies with the magnet.** `val clk = Clk <>
  IN` in a design that also has registers emits one `clk` port, the registers still clock on it, and
  `clk.actual` reads it as a `Bit` (needed to drive a derived clock from the root clock, as an
  ungated ICG does). What does collide is a *non-magnet* port whose name shadows the magnet's, so
  keep a data port off the names `clk`/`rst` ("Unsupported read-to-read connection").
- **Renaming a child's clock port works, and the parent binds it correctly.**
  `@timing.clock(portName = "rawclk")` on a child emits `input wire logic rawclk`, and the parent
  connects **its own** clock to it (`assign f_rawclk = clk;`) because the magnet matches by domain,
  not by name. This is how you port a cell whose clock input the baseline calls something else.
  The domain propagates *down*, so a grandchild instantiated inside that cell is emitted with
  `rawclk` too, while the same class instantiated from an ordinary `clk` design is emitted with
  `clk`.
  <!-- USER-GUIDE DOC GAP: the two sentences above are general language behaviour (portName
       rename + magnet binds by domain, not name + downward domain propagation) and belong in
       docs/user-guide/design-domains/index.md, linked from here. Kept inline for now because
       they are inseparable from the three emitter traps below. -->
  Two traps come with it, both found on VeeR's `rvdff_fpga`:
  - **Keep a renamed-clock design childless.** If it instantiates anything, the emission is invalid
    SystemVerilog: duplicated `rst_l_0`/`rst_l_1` ports against a `.rst_l` connection, plus
    `assign <ModuleTypeName>.<child>.clk = clk;` - a hierarchical assign through the module *type*
    name. It also surfaces at the parent as a bogus `Found multiple connections write to the same
    variable/port <inst>_clk`, once the parent instantiates a same-domain sibling. Both faces
    vanish when the cell has no children, so write the leaf logic directly instead of wrapping a
    child. It elaborates clean either way, so **run the emitted files through slang** after
    renaming a clock port.
  - **`val clk = Clk <> IN` silently beats `portName`.** In a design annotated
    `@timing.clock(portName = "rawclk")`, adding `val clk = Clk <> IN` makes *that* the domain
    clock: `rawclk` disappears from the port list entirely and the registers clock on whatever the
    parent wires to `clk`. No diagnostic. So a cell that takes two clock inputs and flops on the
    *renamed* one must declare the other as a plain `Bit <> IN` - which is safe, and the magnet
    does **not** claim it despite the name:
  ```scala
  @timing.clock(portName = "rawclk")                                  // rvdff_fpga, FPGA arm
  @timing.reset(mode = _.async, active = _.low, portName = "rst_l")
  class rvdff_fpga(val WIDTH: Int <> CONST = 1) extends RTDesign:
    val clk  = Bit <> IN                     // the baseline's dead clock input
    val dout = Bits(WIDTH) <> OUT.REG init all(0)
    if (clken) dout.din := din               // gold: rvdffs (.clk(rawclk), .en(clken), .*)
  // parent emits: assign f_rawclk = clk;  (root clock)   assign f_clk = <derived>;  (dead input)
  ```

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

## Derived (gated) clock ports - the faithful option

When the baseline threads gated clocks as ordinary ports (VeeR's `active_clk`, `*_c1_*_clk`) and you
want to **keep** those ports rather than reduce them to enables, declare a related domain with its
own input clock (see "Derived Clocks" in [Design Domains][design-domains]):

```scala
@hw.constraints.timing.related(this)
val active = new RTDomain:
  val clk = Clk <> IN          // identifies (and flattens) as `active_clk`
  // flops clocked by the gated clock, still reset by the module's shared reset
```

**Declare the clock once, then open regions of it** — the domain-and-regions pattern in
[Design Domains][design-domains]. Putting registers directly in the `RTDerivedClkDomain` prefixes
every one of them with the domain name (`gpr_bank_id` becomes `active_gpr_bank_id`), and adding
`@flattenMode.transparent` to that same domain is worse: it strips the prefix from the `clk` dcl
too, which then collides with the design's own `clk` and the pair emits as `clk_0`/`clk_1`,
renaming the design's clock port. An `RTRegion` has no naming footprint, so it carries the logic:

```scala
val active = new RTDerivedClkDomain {}     // declares the `active_clk` port, nothing else
val bankid = new active.RTRegion:
  val gpr_bank_id = Bits(1) <> VAR.REG init all(0)   // flattens as `gpr_bank_id`
  if (wen_bank_id) gpr_bank_id.din := wr_bank_id
import bankid.gpr_bank_id                  // scope for the rest of the design body
```

Regions are sparse and scattered by design, so **put each one where the baseline declares those
flops** rather than collecting a module's gated logic into one block: the transcription keeps the
gold's statement order, and the emitted HDL is identical either way.

Use **`import`, not `export`** to reach the members afterwards: `export` is rejected outright
(*"not accessible"*) because the region's type is anonymous, and scope is all that is wanted here —
an `import` adds no member and no net.

Same-named domains+ports of the same clock group unify into one clock across the hierarchy,
threaded through auto-added `active_clk` pass-through ports. The source is either a `Clk <> OUT`
related-domain port (the internal gating site, driven from its design scope:
`active.clk <> icgOut.as(active.Clk)`, the veer.sv structure) or a parent's explicit connection to
a child's input port (`child.active.clk <> g.as(child.active.Clk)`). A derived clock is NEVER
implicitly merged onto the root clock: with no source anywhere it surfaces as a top-level input
port (a forgotten connection is a visible port, not a silently dead clock), and the ungated
`RV_FPGA_OPTIMIZE` form (`.active_clk(clk)`) is an explicit wrapper connection from a declared root
clock port. Only `Clk <> IN`/`Clk <> OUT` are legal in a related domain (no `VAR`, no `Rst`). The
reduce-to-enables strategy remains the right call when the target build ties all derived clocks to
the root anyway and the ports are noise.

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
  - Reserve `.toScalaInt` for what genuinely needs a Scala `Int` (an `initFile` path). It is **not**
    needed for a `Bits(W) X N` size, a slice bound, or a `for (i <- 0 until N)` loop bound — the
    ascribed constant works directly in all three.
  - Note that writing `.toScalaInt` is not what pins a parameter, and dropping it does not unpin
    one: the *read* pins it, and an elaboration-time loop reads its bound either way. Removing a
    redundant `.toScalaInt` is a readability fix, not a genericity fix.
- **A parameter that is a pure function of another belongs in the body, not the signature.** If
  every use is a width or slice bound and the parent computes it from a sibling parameter, declare
  it as a body `Int <> CONST`; it emits as a `localparam int` in the parameter port list with the
  expression intact, and the module can no longer be instantiated with an inconsistent pair. Two
  consequences: **transcribe the derivation exactly** — `$clog2(1)` is 0, so a baseline's
  `(N == 1) ? 1 : $clog2(N)` is a zero-width guard, and "simplifying" it to `clog2(N)` is a real
  bug — and a formal harness must stop passing the value, since a `localparam` cannot be overridden.
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
- **A `VAR` is for a value the baseline drives a bit (or a range) at a time.** A per-bit `assign`,
  a `generate` of assigns, or two `assign`s to different ranges of the same signal — those are the
  cases a single named value cannot express. Everything else is a named value.
- **A `genvar` loop over a parameterised width reads the parameter, so it pins it** (see the
  parameters section). That is usually acceptable — check how many distinct widths the baseline
  actually instantiates before trying to avoid it.
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
- **Use the carry operators for a widening add.** Verilog catches a carry by zero-extending both
  operands (`{cout,sum} = {1'b0,a} + {1'b0,b}`); DFHDL says that directly with `+^` (and `-^`,
  `*^`), which is defined as exactly that widening. `a +^ b` emits `assign sum = a + b;` with `sum`
  one bit wider, and the carry is just `sum(top)`.
- **Transcribe the baseline; do not improve it.** The baseline's structure *is* the specification
  and its tricks are the design, not workarounds to be modernised. Every departure costs the thing
  the port is built on: emitted HDL that diffs against the gold.
- **Write the direct form and let the compiler object.** Nearly every needless complication below
  came from *predicting* that DFHDL would reject the obvious spelling and pre-emptively working
  around an error that was never raised. "I think it will not typecheck" is not a reason until the
  compiler says so; it costs seconds to find out, and the diagnostics are good — they name the fix
  (`.foldLeft[Bit <> VAL](...)`, "declare a DFHDL variable and assign it with `:=`"). Every row here
  is a real correction from one port, and **not one was caught by compiling, by reading the emitted
  HDL, or by formal equivalence**:
  | wrote first | actually needed | why the rewrite was wrong |
  |---|---|---|
  | `(b"0", a).toBits.uint + (b"0", b).toBits.uint` | `a +^ b` | the zero-extension is not part of the operation; it is Verilog's only way to keep a carry |
  | `(x_hi.uint + 1).bits(19, 0)` | `x_hi + 1` | arithmetic on `Bits` already yields `UInt[W] <> VAL`, modular at that width, converting back implicitly; the `.uint` forced a widening the `.bits` undid, and that pair is what hit DFHDL#486 |
  | `(a, b).toBits` in an expression | `(a, b)` | a tuple converts implicitly wherever a `Bits` is wanted |
  | `for (i <- 0 until W.toScalaInt)` | `for (i <- 0 until W)` | an ascribed constant works directly as a loop bound, `Vec` size or slice bound |
  | `enum … (val value: UInt[4] <> CONST) extends Encoded.Manual(4)` with 15 explicit values | `enum … extends Encoded` | the default binary encoding already numbers from 0 in declaration order |
  | `~(mask(i) ^ data(i))` for `mask[i] == data[i]` | `mask(i) == data(i)` | **substituting an operator the baseline chose**, on an unverified guess that `Bit \| Boolean` would not typecheck |
  | `c.sel(v, all(0))` for `{N{c}} & v` | `c.repeat(N) & v` | a mask is not a mux waiting to be discovered |
  | build a concat, connect the whole port | connect the pieces the baseline drives (`dout(12, 1) <> …`, `dout(31, 13) <> …`) | an invented intermediate hides the two assignments the gold has |
  | `Bits(31)` plus `-1` at every slice | `BitsHL(31, 1)` | the base belongs on the declaration |
  Three smells, in rising order of seriousness: **a conversion the compiler did not ask for**; **an
  intermediate value with no counterpart in the baseline**; and **an operator the baseline did not
  use**. The last is the one to fear, because an operator that is merely *equivalent* — `==` against
  XNOR on one bit — passes every check in the ladder and is visible only to a reader holding the two
  files side by side.
- **A comparison yields `Boolean <> VAL`, not `Bit`.** `.bit` converts, and is needed before
  concatenating a comparison result.
- **Convert once, at the definition.** `val syndrome = ecc_check(5, 0).uint` so every use reads
  `syndrome == i`, rather than restating `.uint` at each use.
- **A Scala `var` accumulator is an ED-domain construct.** The elaboration-time
  `var acc: Bits[Int] <> VAL = ...; acc = acc ++ x` idiom in the type-system guide is rejected by the
  plugin inside an `RTDesign`; use a `VAR` there. The error says so explicitly.
- **`.reg(n, init = ...)` for a plain delay chain**, rather than declaring and chaining registers:
  `dout <> din.reg(2, init = all(0))` is the baseline's two chained `rvdff`s. But note what it costs
  formally: `.reg` names its flops after the source signal (`din_reg1`, `din_reg2`), so they no
  longer match the baseline's net names, and `equiv_make` -- which pairs by *identical wire name* --
  loses those internal anchors. On a 2-flop module that is free (the proof closes on the outputs
  alone). On a large sequential module the anchors are what keeps induction tractable, so there
  declare the register under the baseline's own net name. **The choice is a verification one, not a
  style one.**
- **A baseline module whose body is a single `assign` over macros is a method, not a design.**
  `@inline def f(...): Bits[W] <> DFRET = <expr>` inlines at the call site. It still proves against
  the baseline module: wrap it in a design carrying that module's port list. Give the wrapper a
  *different* name — a design class collides with a same-named method in the package.
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
- **A Verilog range with a non-zero base is `BitsHL`, not `Bits`.** `logic [31:1] pc` is
  `BitsHL(31, 1)` (type-only spelling `BitsHL[31, 1] <> VAL` for a struct field or parameter).
  Selection then uses the **baseline's own absolute indices** and the emitted declaration keeps the
  range, so the code and the HDL both read like the gold:
  ```scala
  val pc  = BitsHL(31, 1) <> IN        // input wire logic [31:1] pc
  val hi  = pc(31, 13)                 // absolute; the result is a zero-based Bits[19]
  ```
  Selection results are always zero-based and assignment/connection/comparison stay width-based, so
  a `BitsHL` and an equal-width `Bits` remain interchangeable. Do **not** hand-translate to
  `Bits(31)` and subtract one at each use: that compiles clean, loses the declaration, and every
  slice becomes an off-by-one that only formal equivalence will catch. Prefer plain `Bits(width)`
  whenever the base *is* zero.
  In a **method signature** that follows from the same "declaration, not value" property:
  - a **return type is always plain `Bits`** — selections are zero-based, so `BitsHL` can never
    appear on the right of `<> DFRET`;
  - a **parameter needs `BitsHL` only if the method indexes it with the baseline's indices**;
    anything merely combined (XOR, concat, compare) takes `Bits[W]`, and width-based compatibility
    passes a `BitsHL` argument straight in;
  - it works as a `Struct` field too, with literal or constant bounds:
    `index: BitsHL[RV_BTB_ADDR_HI.type, RV_BTB_ADDR_LO.type] <> VAL` emits
    `logic [RV_BTB_ADDR_HI:RV_BTB_ADDR_LO] index;` inside the packed struct.
- **`BitsHL` covers a non-zero-base *bit range* only; a non-zero-base *array* has no counterpart.**
  `logic [31:1][31:0] gpr_out` is 31 words indexed 1..31, and a DFHDL `Vec` is always 0-based
  (`Bits(32) X 31`), so the baseline's index `j` reaches it as `j - 1`. Keep the baseline's own loop
  bounds and put the `- 1` at the `Vec` subscript alone, so every other appearance of `j` — the
  address compare, the `BitsHL` write-enable bit — still reads like the gold:
  ```scala
  for (j <- 1 until 32)
    w0v(j)        <> wen0 & (waddr0.uint == j)          // BitsHL: absolute
    gpr_in(j - 1) <> (w0v(j).repeat(32) & wd0) | ...    // Vec: shifted
  ```
  This one is not cosmetic; see the flat-order trap in the verification section.
- **A `Bits` never compares against a Scala `Int`** ("An integer value cannot be a candidate for a
  Bits type"). The baseline's `addr[4:0] == 5'(j)` is an unsigned compare, so it transcribes as
  `addr.uint == j`. That is the operator, not an intermediate, so it stays inline at each use — the
  "convert once at the definition" rule applies to a value the baseline itself names.
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

## Proving a port against its baseline (yosys)

The ladder itself (combinational miter, `equiv_make`/`equiv_simple`/`equiv_induct` for sequential,
`async2sync`, the mandatory negative control) belongs in the port's own plan. These are the parts
that are about **DFHDL's output specifically** and recur in every port:

- **Read the DFHDL output with `read_slang`, not `read_verilog`** (`yosys -m slang`, plugin shipped
  with OSS CAD Suite; the training repo's `cav` does this automatically for both sides when the
  plugin is present, via `CAV_FRONTEND=auto`). yosys's own frontend rejects the assignment pattern DFHDL emits to reset a
  vector — `gpr_out <= '{default: '{default: 32'h0}};` — with *"syntax error, unexpected
  TOK_DEFAULT"* (yosys#6120). It is the **`default:` key** that has no grammar rule, not the nesting
  and not unpacked arrays: the positional form `'{a, b, c, d}` parses, while every keyed form fails
  (packed, unpacked, declaration initializer, nested). The construct is legal SystemVerilog and
  Verilator accepts it, so this is a frontend gap, not something to work around in the design.
  Since the emitter uses it for *any* vector-wide constant, expect it in every module with a reset
  array. slang also elaborates only the parameterizations actually instantiated, which
  incidentally fixes a baseline whose `generate` arm `$error`s under its *default* parameters
  (`rvdffe`'s "width must be >= 8"); `read_verilog -defer` is the equivalent for the gold.
- **A `Vec` and a Verilog packed array flatten in opposite order.** DFHDL packs `Vec` index 0 at the
  MSB; Verilog packs the *highest* index of `[31:1]` at the MSB. So a `Vec` holding the same 31
  registers is bit-reversed against the baseline's aggregate — harmless in hardware, fatal to
  `equiv_make`, which pairs public wires **by name** and will pair those two 992-bit wires
  bit-for-bit and wrongly. Every register then reports unproven and the failure looks like a design
  bug. Fix it in the harness, not the design:
  ```tcl
  cd <top>                                    # `rename` needs the module selected, or "Object not found"
  rename \u.gpr_out \u.gpr_out_vecorder       # unpair the reversed aggregates
  rename \u.gpr_in  \u.gpr_in_vecorder
  cd ..
  ```
- **Add canonical-order state taps when the flop names differ.** With the aggregates unpaired, and
  with the gold's flops buried under `rvdff` instance paths (`u.gpr_banks[0].gpr[7]...dffs.dout`)
  while the gate's are slices of one `Vec` wire, induction has no internal anchor. Give **both**
  wrappers the same extra outputs, each side reading its own layout:
  ```systemverilog
  output logic [31:1][31:0] dbg;
  for (genvar j = 1; j < 32; j++) assign dbg[j] = u.gpr_out[0][j];      // gold
  for (genvar j = 1; j < 32; j++) assign dbg[j] = u.gpr_out[0][j-1];    // gate, Vec is 0-based
  ```
  Extra observation points are extra proof obligations, so they can only make the check stronger —
  they cannot manufacture a false pass.
- **A dropped derived-clock port needs a gold wrapper**, not an edit to the gold. Wrap the baseline
  with the DFHDL port list and tie the derived clock to the root (`u (.*, .active_clk(clk))`); give
  the gate an identically-named wrapper so `equiv_make` still pairs the ports.
- **`equiv_simple`/`equiv_induct` ignore the CLK net.** They model every `$dff` as advancing one
  step per cycle no matter which net drives it, so **the proof says nothing about which clock a
  flop sits on**. Verified, not assumed: tying a derived clock to constant `1'b0` in both wrappers —
  so the gold's flop can never clock — still reports *"Equivalence successfully proven"* against a
  gate whose flop was moved to the root clock. Consequences:
  - Moving a flop between a derived clock and the root clock is **not a valid negative control**.
    Use a data-path or enable mutation instead (dropping the `if (en)` on the derived-clock flop is
    a good one: it targets that domain and goes red).
  - **Tie the derived clock to the root clock in both wrappers** rather than leaving it a free
    input. It cannot make the check weaker (the tool ignores it either way), it matches the build
    being verified, and it puts the assumption in the harness instead of leaving it implicit in the
    tool's semantics.
  - Clock *assignment* is therefore checked by reading the emitted `always_ff` sensitivity lists and
    the parent's connection, not by the proof. A build where derived clocks are genuinely gated
    would need `clk2fflogic`, which models clocks explicitly.
- **Identical `stat` cell counts across gold and gate** (same `$aldff`/`$and`/`$eq`/`$or` totals) is
  a fast structural sanity check before spending minutes in `equiv_induct`, and it is what tells you
  an "unproven" result is a *pairing* problem rather than a logic one.

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
