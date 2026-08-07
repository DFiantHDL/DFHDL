# Transitioning from Verilog to DFHDL

This guide helps Verilog/SystemVerilog users translate common patterns into DFHDL. For full type system details, see the [Type System reference][type-system].

## Design Structure

/// admonition | Module Definition
    type: verilog
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module _module_name_ #(
  //param declarations
) (
  //port declarations
);
  //internal declarations
endmodule
```

```scala linenums="0" title="DFHDL"
class _design_name_(
  //param declarations
) extends EDDesign:
  //port & internal declarations


end _design_name_ //optional
```

```sv linenums="0" title="Verilog"
module AndGate (
  input a, b;
  output o
);
  assign o = a & b
endmodule
```

```scala linenums="0" title="DFHDL"
class AndGate extends EDDesign:
  val a, b = Bit <> IN
  val o    = Bit <> OUT

  o <> a && b
end AndGate
```

</div>
///

/// admonition | Parameter Declarations
    type: verilog
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
parameter [7:0] p = 8’b1011;
```

```scala linenums="0" title="DFHDL"
val p: Bits[8] <> CONST = b"8'1011"
```
</div>

Inter-dependent parameters and ports example:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module Concat#(
    parameter int len1 = 8,
    parameter int len2 = 8,
    parameter logic [7:0] midVec = 8'h55
)(
  input  wire logic [len1 - 1:0]   i1,
  input  wire logic [len2 - 1:0]   i2,
  output      logic [outlen - 1:0] o
);
  localparam int midLen = 8;
  localparam int outlen = len1 + midLen + len2;
  assign o = {i1, midVec, i2};
endmodule
```

```scala linenums="0" title="DFHDL"
class Concat(
    val len1: Int <> CONST = 8,
    val len2: Int <> CONST = 8,
    val midVec: Bits[Int] <> CONST = h"55"
) extends EDDesign:
  val midLen = midVec.width
  val outlen = len1 + midLen + len2
  val i1 = Bits(len1) <> IN
  val i2 = Bits(len2) <> IN
  val o = Bits(outlen) <> OUT

  o <> (i1, midVec, i2)
end Concat
```

</div>
///

/// admonition | Unconnected Output Ports
    type: verilog
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
child_mod child_inst(
    .clk    (clk),
    .din    (din),
    .debug  (),     // unconnected
    .dout   (dout)
);
```

```scala linenums="0" title="DFHDL"
val child_inst = child_mod()
child_inst.clk   <> clk
child_inst.din   <> din
child_inst.debug <> OPEN  // unconnected
child_inst.dout  <> dout
```

</div>

Use `OPEN` to explicitly mark an output port as unconnected. This is equivalent to Verilog’s empty port connection (`.port()`), but only for outputs: Verilog also accepts an empty connection on an *input* port and leaves it floating, whereas DFHDL requires every input port of a design instance to be driven. Give such a port an explicit value (`child_inst.din <> all(0)`) when porting. See [Open (Unconnected) Ports][open-ports] for more details.
///

/// admonition | logic/reg/wire
    type: verilog
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
logic [7:0] v = 8’b1011;
wire  [7:0] v = 8’b1011;
reg   [7:0] v = 8’b1011;
```

```scala linenums="0" title="DFHDL"
val v = Bits(8) <> VAR init b"8’1011"
```

</div>
///

/// admonition | Choosing `UInt` vs `Bits` vs `SInt` for Verilog `logic`
    type: verilog
Verilog `logic` and `wire` are untyped bit vectors. When translating to DFHDL, choose the type based on how the signal is used:

| Verilog usage pattern | DFHDL type |
|---|---|
| Signed values, subtraction below zero | `SInt` |
| Exact bit-width required (addresses, masks, bitwise ops) | `Bits` |
| Unsigned arithmetic (counters, addition, comparison with integers) | `UInt` |
| FSM state encoding | `enum extends Encoded` |

When a signal is used in both arithmetic and bitwise contexts, prefer `UInt` and convert with `.bits` for bitwise operations. When exact bit-width must be preserved (e.g., an address bus that is also incremented), prefer `Bits` to avoid accidental width extension.
///

## Types and Literals

/// admonition | Numeric Literals
    type: verilog
DFHDL uses string interpolators for sized literals. Each type has its own interpolator, so do not mix Verilog base prefixes (`’b`, `’d`, `’h`) inside them.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
8’b1011_0000   // binary
8’hB0          // hex
8’d176         // decimal
5’d27          // 5-bit decimal
```

```scala linenums="0" title="DFHDL"
b"8’1011_0000"  // binary (b"...")
h"8’B0"         // hex (h"...")
d"8’176"        // unsigned decimal (d"...")
d"5’27"         // 5-bit unsigned decimal
```

</div>

`b"..."` accepts only binary digits (`0`, `1`, `?`). Writing `b"5’d27"` is an error. Use `d"5’27"` for decimal values.
///

/// admonition | `$clog2` and Width Computation
    type: verilog
Instead of computing widths manually with `$clog2`, use `.until` or `.to` constructors which set the width automatically:

| Verilog | DFHDL | Width |
|---------|-------|-------|
| `$clog2(N)` | `UInt.until(N)` / `Bits.until(N)` | `clog2(N)` bits, valid for N >= 2 |
| `$clog2(N+1)` | `UInt.to(N)` / `Bits.to(N)` | `clog2(N+1)` bits, valid for N >= 1 |

`UInt.until(1)` is **invalid** (would produce 0-bit width). For counters that count 0 to N inclusive (common with `$clog2(N+1)`), use `UInt.to(N)`.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
parameter RATE = 5208;
localparam WIDTH = $clog2(RATE);
reg [WIDTH-1:0] counter;

// Counter 0..SCALE inclusive
reg [$clog2(SCALE+1)-1:0] cnt = 0;
```

```scala linenums="0" title="DFHDL"
val RATE: Int <> CONST = 5208
val counter = UInt.until(RATE) <> VAR


// Counter 0..SCALE inclusive
val cnt = UInt.to(SCALE) <> VAR init 0
```

</div>

/// admonition | Choosing `.until(N)` vs `.to(N)` for counters
    type: warning
If the Verilog counter resets *when it reaches* `N` (i.e., `counter == N`), the variable must be able to hold the value `N`, so use `UInt.to(N)`. If the counter only ever holds values `0..N-1`, use `UInt.until(N)`. For many values of `N`, both produce the same bit width (`clog2(N) == clog2(N+1)`), so the bug is silent until formal verification catches an out-of-range comparison.
///

**Reusing computed types and extracting widths:**
You can name a DFHDL type and reuse it across multiple declarations. You can also extract the `.width` from an existing DFHDL value (not from a type constructor) to derive new widths:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module Sum#(parameter int MAX = 16)(
  input  wire logic [A_WIDTH - 1:0] a,
  input  wire logic [A_WIDTH - 1:0] b,
  output      logic [IPW - 1:0]     sum
);
  localparam int A_WIDTH = $clog2(MAX);
  localparam int IPW = A_WIDTH + 1;
  assign sum = a + b;
endmodule
```

```scala linenums="0" title="DFHDL"
class Sum(val MAX: Int <> CONST = 16)
    extends EDDesign:
  // Name and reuse a type
  val MyUInt = UInt.until(MAX)
  val a = MyUInt <> IN
  val b = MyUInt <> IN

  // Extract width from an existing value
  val IPW = a.width + 1
  val sum = UInt(IPW) <> OUT
  sum <> a + b
```

</div>

`.width` works on any DFHDL value (a port, variable, or constant: anything declared with `<>`; see [logic/reg/wire](#logicregwire) above). Use it to derive widths from existing declarations:

```scala
val a = UInt.until(MAX) <> IN
val a_width = a.width          // equivalent to clog2(MAX)
val extended = UInt(a_width + 1) <> OUT
```

Using `UInt.to`/`UInt.until` with `.width` on values is preferred over calling `clog2` directly (see the [`clog2` anti-pattern warning][int-param-ops]).

See [UInt/SInt constructors][DFDecimal] and [Bits constructors][DFBits] for details.
///

## Processes and Sequential Logic

/// admonition | Process Blocks (always)
    type: verilog
Verilog `always` blocks map to DFHDL ED domain `process(...)` blocks.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
// Combinational
always @(*) begin
  y = a + b;
end

// Sequential (clocked)
always @(posedge clk) begin
  counter <= counter + 1;
end

// Sequential with async reset
always @(posedge clk or posedge rst)
  if (rst)
    q <= 0;
  else
    q <= d;
```

```scala linenums="0" title="DFHDL"
// Combinational
process(all):
  y := a + b


// Sequential (clocked)
process(clk.rising):
  counter :== counter + 1

// Sequential with async reset
process(clk.rising, rst.rising):
  if (rst)
    q :== 0
  else
    q :== d
```

</div>

- `always @(*)` becomes `process(all):`
- `always @(posedge clk)` becomes `process(clk.rising):`
- `always @(posedge clk, negedge rst)` becomes `process(clk.rising, rst.falling):`
- Verilog blocking `=` becomes DFHDL `:=` (use in combinational processes)
- Verilog non-blocking `<=` becomes DFHDL `:==` (use in clocked processes)
///

/// admonition | Variable Initialization (init)
    type: verilog
In ED domain, `init` on a `VAR` generates a Verilog `reg` with an initial value. This maps to both `initial begin` blocks and `reg ... = value` declarations.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
reg [7:0] counter = 8’d0;
// or equivalently:
// initial counter = 8’d0;
```

```scala linenums="0" title="DFHDL"
val counter = UInt(8) <> VAR init 0
```

</div>

An `output reg` with an initial value maps directly to `OUT init`:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module Foo(
  input  clk,
  input  din,
  output reg dout
);
  initial dout = 1'b1;
  always @(posedge clk)
    dout <= din;
endmodule
```

```scala linenums="0" title="DFHDL"
class Foo extends EDDesign:
  val clk  = Bit <> IN
  val din  = Bit <> IN
  val dout = Bit <> OUT init 1
  process(clk.rising):
    dout :== din
```

</div>
///

/// admonition | FSM State Encoding
    type: verilog
Verilog FSMs typically use `parameter` constants and `case`/`if` chains. In DFHDL, the idiomatic translation uses an `enum extends Encoded` and `match`:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
parameter READY = 2'b00,
          AIM   = 2'b01,
          FIRE  = 2'b10;
reg [1:0] state = READY;

always @(posedge clk)
  case (state)
    READY:   if (go) state <= AIM;
    AIM:     state <= FIRE;
    FIRE:    state <= READY;
    default: state <= READY;
  endcase
```

```scala linenums="0" title="DFHDL"
enum State extends Encoded:
  case Ready, Aim, Fire
import State.*
val state = State <> VAR init Ready

process(clk.rising):
  state match
    case Ready => if (go) state :== Aim
    case Aim   => state :== Fire
    case Fire  => state :== Ready
    case _     => state :== Ready
```

</div>

If the encoded Verilog state values follow a standard pattern (incremental, gray, one-hot), use the corresponding `Encoded` variant. For non-standard encodings, use `Encoded.Manual` with a constructor parameter:

```scala
// Verilog: parameter INIT=3'b000, RUN=3'b011, DONE=3'b101, ERR=3'b110;
enum Phase(val value: UInt[3] <> CONST) extends Encoded.Manual(3):
  case Init extends Phase(0)
  case Run  extends Phase(3)
  case Done extends Phase(5)
  case Err  extends Phase(6)
```

The constructor parameter `(val value: UInt[N] <> CONST)` is required for `Encoded.Manual` and the bit width `N` must match the argument to `Encoded.Manual(N)`. Omitting it causes a compile error. See [Enumeration][DFEnum] for all encoding options.

When the Verilog FSM uses an explicit reset instead of `initial`, omit the `init` and handle reset inside the clocked process:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
always @(posedge clk)
  if (rst)
    state <= READY;
  else
    case (state)
      READY:   if (go) state <= AIM;
      AIM:     state <= FIRE;
      FIRE:    state <= READY;
      default: state <= READY;
    endcase
```

```scala linenums="0" title="DFHDL"
val state = State <> VAR  // no init

process(clk.rising):
  if (rst)
    state :== Ready
  else
    state match
      case Ready => if (go) state :== Aim
      case Aim   => state :== Fire
      case Fire  => state :== Ready
      case _     => state :== Ready
```

</div>

Avoid modeling FSM states as `Bits` or `UInt` constants; it is an anti-pattern. When compiling to SystemVerilog (SV), the SV enums are being utilized as well.
///

/// admonition | Integer `case` Statements (non-enum)
    type: verilog
When the Verilog `case` selector is a plain integer counter (not an FSM), use `match` with integer literal cases directly:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
case (sel)
  'd0: out <= 60;
  'd1: out <= 110;
  'd2 | 'd3: out <= 200;
  default: out <= 0;
endcase
```

```scala linenums="0" title="DFHDL"
sel match
  case 0     => out :== 60
  case 1     => out :== 110
  case 2 | 3 => out :== 200
  case _     => out :== 0
end match
```

</div>

Integer literal pattern matching works with `UInt` and `SInt` selectors. Guard conditions (`case _ if sel == N`) also work but are less idiomatic. See [Match Expressions][match-expressions] for full details.
///

## Functions and Tasks

Verilog `function`s and `task`s map to DFHDL [methods][Methods]: Scala `def`s declared inside the design. A value-returning method is a function (`<> EDRET`); a `Unit`-returning method is a procedure, which emits as a Verilog `task`. The method's Scala name becomes the generated subprogram name. See the [Methods][Methods] page for the full reference, including static (`<> CONSTRET`) functions and VHDL output.

/// admonition | Function
    type: verilog
Arguments are marked `<> VAL`; the return type is `<> EDRET`. The method is called in an assignment or inside a process, exactly like a Verilog function.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
function automatic [7:0] add(
  input [7:0] l, input [7:0] r
);
  add = l + r;
endfunction

assign y = add(a, b);
```

```scala linenums="0" title="DFHDL"
def add(
  l: UInt[8] <> VAL, r: UInt[8] <> VAL
): UInt[8] <> EDRET =
  l + r
end add //optional

y <> add(a, b)
```

</div>

A function that reads module-level signals or parameters directly maps naturally: reference the outer port/constant in the body and it is captured automatically, with no extra argument at the call site.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
localparam [7:0] k = 8'd5;
function automatic [7:0] addBK(input [7:0] l);
  addBK = l + b + k;   // b, k read directly
endfunction
```

```scala linenums="0" title="DFHDL"
val k: UInt[8] <> CONST = 5
def addBK(l: UInt[8] <> VAL): UInt[8] <> EDRET =
  l + b + k   // b, k captured automatically
end addBK //optional
```

</div>
///

/// admonition | Task
    type: verilog
A `Unit`-returning method is a procedure and emits as a Verilog `task`. It is called from statement position inside a `process`.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
task automatic show(input [7:0] l);
  $display("value is %0d", l);
  #1;
endtask

always begin
  show(a);
end
```

```scala linenums="0" title="DFHDL"
def show(l: UInt[8] <> IN): Unit <> EDRET =
  report(s"value is $l")
  wait(1.ns)
end show //optional

process:
  show(a)
```

</div>
///

/// admonition | Pure constant helpers
    type: verilog
A side-effect-free helper whose arguments are all constants can be a **static function** (`<> CONSTRET`). It emits as a Verilog `function automatic` and, unlike an ordinary ED function, may also be evaluated to compute a design parameter.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
function automatic [7:0] twice(input [7:0] n);
  twice = n + n;
endfunction
```

```scala linenums="0" title="DFHDL"
def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET =
  n + n
end twice //optional
```

</div>
///

## Operations

/// admonition | Shift Operators
    type: verilog
Verilog has separate `>>` (logical) and `>>>` (arithmetic) right shift operators. DFHDL uses only `>>`, but the behavior depends on the operand type:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module RightShifter(
  input  wire logic [7:0] data,
  output      logic [7:0] logical,
  output      logic [7:0] arith
);
  assign logical = data >> 1;
  assign arith   = data >>> 1;
endmodule
```

```scala linenums="0" title="DFHDL"
class RightShifter extends EDDesign:
  val data    = Bits(8) <> IN
  val logical = Bits(8) <> OUT
  val arith   = Bits(8) <> OUT

  logical <> data >> 1
  arith   <> (data.sint >> 1).bits
end RightShifter
```

</div>

There is no `>>>` operator in DFHDL. The type of the LHS determines the shift semantics: `>>` on `UInt`/`Bits` zero-fills, `>>` on `SInt` sign-extends. See [Shift Operations][shift-ops] for details.
///


/// admonition | Bit/Boolean Operators: `|`/`&`/`~` and `||`/`&&`/`!`
    type: verilog
In DFHDL, `||`/`&&`/`!` and `|`/`&`/`~` are interchangeable on `Bit` and `Boolean` types. The generated Verilog operator depends on the LHS type: `Bit` produces bitwise `|`/`&`/`~`, `Boolean` produces logical `||`/`&&`/`!`.
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
input  a, b, c;
output o1, o2, o3;
assign o1 = a | b | c;
assign o2 = a | b | c;
assign o3 = a || b || c;
```

```scala linenums="0" title="DFHDL"
val a, b, c = Bit <> IN
val o1, o2, o3 = Bit <> OUT
o1 <> a | b | c
o2 <> a || b || c
o3 <> a.bool || b || c
```

</div>

See [Logical Operations][logical-ops] for the full reference and Verilog/VHDL mapping tables.
///

/// admonition | Bitwise NOT and Unary Negation (`~x`, `-x`)
    type: verilog
DFHDL's `~` is Verilog's bitwise NOT: it applies to `Bits` and `UInt` vector values and returns the same type as its argument.

Unary negation `-x` differs from Verilog. In Verilog, `-x` is evaluated at the context-determined width, so assigning it to a same-width signal wraps modulo `2**W` (two's complement). In DFHDL, `-x` on a `UInt[W]` or `Bits[W]` value implicitly converts the value to `SInt[W + 1]` and returns `SInt[W + 1]`, preserving the exact negated value; `SInt` values keep their type and width. To reproduce Verilog's same-width wrap-around negation, reinterpret the value as signed (same width), negate, and reinterpret back: `(-x.sint).bits` for a `Bits` value, or `(-x.bits.sint).bits.uint` for a `UInt` value.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module Negate(
  input  wire logic [7:0]        x,
  output      logic [7:0]        inv,
  output      logic [7:0]        neg,
  output      logic signed [8:0] negw
);
  assign inv  = ~x;  // bitwise NOT
  assign neg  = -x;  // wraps modulo 2**8
  assign negw = -x;  // widened by context
endmodule
```

```scala linenums="0" title="DFHDL"
class Negate extends EDDesign:
  val x    = Bits(8) <> IN
  val inv  = Bits(8) <> OUT
  val neg  = Bits(8) <> OUT
  val negw = SInt(9) <> OUT

  inv  <> ~x              // bitwise NOT, Bits[8]
  neg  <> (-x.sint).bits  // same-width wrap-around
  negw <> -x              // SInt[9]: exact negated value
end Negate
```

</div>

This wrap-around conversion may appear cumbersome, and that is intentional: it hints at a code smell. It usually means one of two things: either the base type was chosen badly (a value that is conceptually a signed number should be declared as `SInt`, where `-x` preserves the type directly), or the importance of the preserved sign bit in the widened `SInt[W + 1]` result is not understood (discarding it silently corrupts the value; e.g., same-width negation of `d"8'128"` yields 128 again, not -128).

See [Logical Operations][logical-ops] for the bitwise NOT and [Arithmetic Operations][arithmetic-ops] for the unary negation type rules.
///

/// admonition | Reduction Operators (`&v`, `|v`, `^v`)
    type: verilog
Verilog's unary reduction operators have direct DFHDL equivalents using postfix `.&`, `.|`, `.^` on `Bits` values:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
logic [7:0] v;
logic all_set  = &v;    // AND reduce
logic any_set  = |v;    // OR reduce
logic parity   = ^v;    // XOR reduce
logic not_all  = ~&v;   // NAND reduce
logic none_set = ~|v;   // NOR reduce
```

```scala linenums="0" title="DFHDL"
val v = Bits(8) <> VAR
val all_set  = v.&     // Bit: AND reduce
val any_set  = v.|     // Bit: OR reduce
val parity   = v.^     // Bit: XOR reduce
val not_all  = ~v.&    // Bit: NAND reduce
val none_set = ~v.|    // Bit: NOR reduce
```

</div>

See [Bit Reduction Operations][reduction-ops] for full details.
///

/// admonition | Bit Replication (`{N{expr}}`)
    type: verilog
Verilog's replication operator `{N{expr}}` maps to `.repeat(N)` in DFHDL:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
parameter W = 8;
logic [7:0]  all_ones  = {8{1'b1}};
logic [7:0]  all_zeros = {8{1'b0}};
logic [15:0] doubled   = {2{all_ones}};

// Parametric replication
logic [W-1:0] fill_one = {W{1'b1}};
```

```scala linenums="0" title="DFHDL"
val W: Int <> CONST = 8
val all_ones  = b"1".repeat(8)
val all_zeros = b"0".repeat(8)
val doubled   = all_ones.repeat(2)

// Parametric replication
val fill_one = b"1".repeat(W)
```

</div>

`.repeat` works on any `Bits` value, including single-bit literals.
///

/// admonition | Bit Concatenation (`{a, b, ...}`)
    type: verilog
Verilog's concatenation operator `{a, b}` maps to the `++` operator or tuple `.toBits` in DFHDL:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
logic [7:0] a, b;
logic [15:0] concat = {a, b};

// Parametric: {1'b1, {(N-1){1'b0}}}
parameter N = 8;
logic [N-1:0] half = {1'b1, {(N-1){1'b0}}};
```

```scala linenums="0" title="DFHDL"
val a, b = Bits(8) <> VAR
val concat = a ++ b  // Bits[16]

// Parametric: MSB=1, rest zeros
val N: Int <> CONST = 8
val half = b"1" ++ b"0".repeat(N - 1)
```

</div>

Alternative: use tuple `.toBits` syntax for multi-value concatenation:

```scala
val concat = (a, b, c).toBits  // equivalent to a ++ b ++ c
```
///

/// admonition | Part-Select Notation (`-:` and `+:`)
    type: verilog
Verilog's descending and ascending part-select notation maps directly to DFHDL's anchored slice methods `.msbitsAt(base, W)` and `.lsbitsAt(base, W)`, to the `(hi, lo)` range slice, or to the convenience methods `.msbits(W)` and `.lsbits(W)` for the top/bottom cases:

| Verilog | DFHDL | Notes |
|---------|-------|-------|
| `sig[base -: W]` | `sig.msbitsAt(base, W)` or `sig(base, base - W + 1)` | Descending part-select: `W` bits, MSB anchored at `base` |
| `sig[base +: W]` | `sig.lsbitsAt(base, W)` or `sig(base + W - 1, base)` | Ascending part-select: `W` bits, LSB anchored at `base` |
| `sig[N-1 -: W]` | `sig.msbits(W)` or `sig(N-1, N-W)` | Top `W` bits |
| `sig[0 +: W]` | `sig.lsbits(W)` or `sig(W-1, 0)` | Bottom `W` bits |
| `sig[idx]` | `sig(idx)` | Single bit access |

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
logic [15:0]        data;
logic [15:0]        addr;
logic signed [15:0] sdata;
logic [3:0] top4  = data[15 -: 4];
logic [3:0] bot4  = data[0  +: 4];
logic [3:0] mid4  = data[4  +: 4];
logic [3:0] amid4 = addr[4  +: 4];
logic [3:0] smid4 = sdata[4 +: 4];
logic       bit5  = data[5];
```

```scala linenums="0" title="DFHDL"
val data  = Bits(16) <> VAR
val addr  = UInt(16) <> VAR
val sdata = SInt(16) <> VAR
val top4  = data.msbits(4)        // Bits[4]: top 4 bits
val bot4  = data.lsbits(4)        // Bits[4]: bottom 4 bits
val mid4  = data.lsbitsAt(4, 4)   // Bits[4]: data[4 +: 4]
val amid4 = addr.lsbitsAt(4, 4)   // UInt[4]: addr[4 +: 4]
val smid4 = sdata.lsbitsAt(4, 4)  // UInt[4]: unsigned slice
val bit5  = data(5)               // single bit
```

</div>

The part-select base index and width must be elaboration-time constants (Scala `Int` values or `Int <> CONST` parameters), as in a Verilog constant part-select.

Bit-slicing and single-bit access work on `Bits`, `UInt`, and `SInt` values with the same syntax, including the `.msbits(W)`/`.lsbits(W)` and `.msbitsAt(base, W)`/`.lsbitsAt(base, W)` convenience methods. As in Verilog, a slice is a bit-level operation and yields an unsigned result (`SInt` slices return `UInt`, not `SInt`):

| Source type | Slice result |
|---|---|
| `Bits[N]` | `Bits` |
| `UInt[N]` | `UInt` |
| `SInt[N]` | `UInt` (chain `.bits.sint` to re-interpret as signed) |

```scala
val prod  = SInt(16) <> VAR
val top8U = prod(15, 8)             // UInt[8]: raw upper byte
val top8M = prod.msbits(8)          // UInt[8]: same as prod(15, 8)
val top8S = prod(15, 8).bits.sint   // SInt[8]: sign-preserving truncation
val sign  = prod(15)                // single-bit access (Bit)
```

To recover signed semantics on a slice, chain `.bits.sint` (re-interpret the bits as signed, same width). Do **not** use `.signed` for this: `.signed` is a numeric conversion that widens by one zero-extension bit, which is not what slice migration wants.
///

/// admonition | Arithmetic with Signed Values and Constants
    type: verilog
**Arithmetic operand compatibility:**
DFHDL enforces sign and width constraints at compile time. **Commutative operations** (`+`, `*`, `max`, `min`) produce the widest, most signed result, and operand order does not matter. **Non-commutative operations** (`-`, `/`, `%`) require the LHS to be at least as wide and signed as the RHS. When mixing signed and unsigned, the unsigned operand is implicitly sign-extended by 1 bit.

Both Scala `Int` values and DFHDL `Int` parameters act as [wildcards][wildcard-ops]: the wildcard `Int` value adapts to the bit-accurate value's sign and width. If the wildcard `Int` value does not fit, an error is generated.

```scala
// Commutative: result is widest, most signed
d"8'5" + d"4'3"    // UInt[8] (max(8,4) = 8)
d"4'5" + d"8'3"    // UInt[8] (commutative, same result)
sd"8'5" + d"4'3"   // SInt[8] (max(8, 4+1) = 8, signed)
d"8'5" + sd"4'3"   // SInt[9] (max(8+1, 4) = 9, signed)
d"4'5" + d"8'200"  // UInt[8] (larger operand widens the result)

// Wildcard `Int` values adapt to bit-accurate values
d"8'5" + 3         // UInt[8] (3 adapts to UInt[8])
sd"8'5" + (-3)     // SInt[8] (-3 adapts to SInt[8])
val param: Int <> CONST = 10
d"8'5" + param     // UInt[8] (param adapts to UInt[8])
sd"8'5" + param    // SInt[8] (param adapts to SInt[8])
d"8'5" + 1000      // ERROR: 1000 exceeds UInt[8] range
d"8'5" + (-1)      // ERROR: -1 is negative for UInt bit-accurate value

// Non-commutative: LHS-dominant, LHS must be >= RHS
d"8'5" - d"4'3"    // UInt[8]
sd"8'5" - d"4'3"   // SInt[8] (RHS widened to 5 bits, 8 >= 5)
// d"4'5" - d"8'3" // ERROR: RHS width > LHS width
// d"8'5" - sd"4'3" // ERROR: unsigned LHS, signed RHS
```

**Comparison operand compatibility:**
Comparisons (`==`, `!=`, `<`, `>`, `<=`, `>=`) require both operands to have the **same signedness and width**. `Int` values act as wildcards, adapting to the DFHDL value's type.

```scala
d"8'5" == d"8'3"    // OK (same sign, same width)
d"8'5" == 3         // OK (3 adapts to UInt[8])
sd"8'5" < sd"8'3"   // OK (same sign, same width)
sd"8'5" == (-3)     // OK (-3 adapts to SInt[8])
// d"8'5" == d"4'3" // ERROR: different widths
// d"8'5" == sd"8'3" // ERROR: different signedness
// d"4'5" == 300    // ERROR: 300 exceeds UInt[4] range
```

To compare values of different widths, use `.resize(W)` to match widths first. To compare values of different signedness, convert explicitly (e.g., `.bits.sint` or `.signed`).

**UInt-to-SInt conversion methods:**

- `.signed` converts `UInt[W]` to `SInt[W+1]` by adding a sign bit. The value is preserved (always non-negative).
- `.bits.sint` converts `UInt[W]` to `SInt[W]` by reinterpreting the bit pattern. The width stays the same, but the value may become negative if the MSB is set.
Use `.resize(W)` to widen a narrower operand before arithmetic.

**Mixed-width signed arithmetic examples:**

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module MixedArith #(
  parameter W = 16
)(
  input  wire        clk,
  input  wire [2:0]          idx,
  input  wire signed [W-1:0] operand,
  output wire signed [W-1:0] result,
  output reg  signed [W+1:0] acc
);
  // Combinational: UInt expr assigned to SInt
  assign result = 100 - 3 * idx;

  // Clocked: wide accumulator, narrow operand
  always @(posedge clk)
    if (acc <= operand)
      acc <= acc + 2 * operand + 1;
endmodule
```

```scala linenums="0" title="DFHDL"
class MixedArith(
  val W: Int <> CONST = 16
) extends EDDesign:
  val clk     = Bit         <> IN
  val idx     = UInt(3)     <> IN
  val operand = SInt(W)     <> IN
  val result  = SInt(W)     <> OUT
  val acc     = SInt(W + 2) <> OUT

  // Forcing SInt arithmetic
  result <> sd"${W}'100" - idx * 3

  process(clk.rising):
    // Resize narrow operand to match wider acc
    if (acc <= operand.resize(W + 2))
      // switch operand and literal multiplication
      // order to force SInt arithmetic
      acc :== acc + operand * 2 + 1
end MixedArith
```

</div>

See [Arithmetic Operations][arithmetic-ops] and [Carry Arithmetic][carry-ops] for full details.
///

/// admonition | Integer Literal Width and Silent Overflow
    type: verilog
In Verilog, unsized integer literals are 32-bit. When combined with narrower signals, the wider literal causes the entire expression to evaluate at 32-bit width via context-dependent propagation. This prevents intermediate overflow in expressions like `(a + b + c + d) / 4`.

In DFHDL, Scala `Int` literals are implicitly converted to minimum-width bit-accurate types (e.g., `4` becomes `UInt[3]`). Each arithmetic operation independently uses the LHS width, so intermediate results can overflow before reaching a division, shift, or comparison. (An assignment to a wider target is unaffected: the chain is automatically promoted to evaluate at the target width, matching Verilog.)

DFHDL detects this pattern at elaboration and issues a warning. See [Implicit Scala `Int` and Verilog-semantics mismatch][arithmetic-ops] for the full list of warning triggers.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
input  logic [7:0] a, b, c, d;
output logic [7:0] result;

// 4 is 32-bit, so (a+b+c+d) evaluates
// at 32-bit width, no overflow
assign result = (a + b + c + d) / 4;
```

```scala linenums="0" title="DFHDL (with carry ops)"
val a, b, c, d = UInt(8) <> IN
val result     = UInt(8) <> OUT

// Use carry ops to match Verilog's
// overflow-free semantics
result <> ((a +^ b +^ c +^ d) / 4).resize
```

</div>
///


## Parametric Constants

/// admonition | Parametric-Width Bits Constants
    type: verilog
Verilog parameters can be bit-vector constants whose width depends on another parameter. In DFHDL, use `Bits[Int] <> CONST` (unbounded width) for the parameter. The width is inferred from the default literal value, and other local values can derive their widths from it:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module MyDesign #(
    parameter WIDTH = 8,
    parameter [WIDTH-1:0] MASK = 8'hB8
)(
    output logic [WIDTH-1:0] data
);
  // ...
endmodule
```

```scala linenums="0" title="DFHDL"
class MyDesign(
    val WIDTH: Int <> CONST = 8,
    val MASK:  Bits[Int] <> CONST = h"B8"
) extends EDDesign:
  // MASK.width gives the actual width
  val data = Bits(MASK.width) <> OUT
  // ...
```

</div>

See the [Parameter Declarations](#parameter-declarations) section above for a complete inter-dependent parameters example.
///

## Generate Loops and Conditionals

/// admonition | `generate for` Loops
    type: verilog
Verilog `generate for` loops map to Scala `for` loops at design scope. Each iteration is unrolled at elaboration time, so the generated HDL has no loop construct, only the unrolled instances.

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module inc(
  input  [7:0] x,
  output [7:0] y
);
  assign y = x + 1;
endmodule

module inc_bank #(
  parameter N = 4
)(
  input  [7:0] x [0:N-1],
  output [7:0] y [0:N-1]
);
  genvar i;
  generate
    for (i = 0; i < N; i = i + 1) begin : lane
      inc u_inc (.x(x[i]), .y(y[i]));
    end
  endgenerate
endmodule
```

```scala linenums="0" title="DFHDL"
class inc extends EDDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1
end inc

class inc_bank(
  val N: Int <> CONST = 4
) extends EDDesign:
  val x = UInt(8) X N <> IN
  val y = UInt(8) X N <> OUT
  for i <- 0 until N do
    val u_inc = inc()
    u_inc.x <> x(i)
    y(i)    <> u_inc.y
  end for //optional
end inc_bank
```

</div>

At design (concurrent) scope the loop runs at elaboration time and unrolls, so `N` may be an `Int <> CONST` parameter directly; its value is read during elaboration and no explicit `.toScalaInt` conversion is needed. See [Loops][loops] for the full elaboration-time loop semantics, and [reading a constant into Scala][toScala] for the contexts that do need the explicit conversion.

///

/// admonition | `generate if` Conditionals
    type: verilog
Verilog `generate if` maps to a Scala `if` at design scope whose condition is a DFHDL constant. The condition is resolved at elaboration time, so only the taken branch generates hardware; the other branch is elaborated away. (Inside a process, the same `if` stays a hardware conditional even when its condition is constant.)

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module gate #(
  parameter INVERT = 0
)(
  input  [15:0] x,
  output [15:0] y
);
  generate
    if (INVERT) begin : impl
      assign y = ~x;
    end else begin : impl
      assign y = x;
    end
  endgenerate
endmodule
```

```scala linenums="0" title="DFHDL"
class gate(
  val INVERT: Boolean <> CONST = false
) extends EDDesign:
  val x = Bits(16) <> IN
  val y = Bits(16) <> OUT
  if (INVERT)
    y <> ~x
  else
    y <> x
  end if //optional
end gate
```

</div>

**Important difference from Verilog:** DFHDL type-checks **both** branches, regardless of the condition's value, so both must be valid for all parameter values. Because the taken branch depends on the constant, instantiating the design with different parameter values produces distinct elaborated designs. See [Loops][loops] for the analogous elaboration-time loop behavior and workarounds.
///

## Common Pitfalls

/// admonition | Scala Reserved Keywords as DFHDL Port or Variable Names
    type: verilog
Some Verilog port names (`val`, `type`, `class`, `match`, `case`, `object`, etc.) are reserved in Scala. Use backtick escaping:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module foo(
  output reg signed [15:0] val
);
  assign val = 16'sd42;
```

```scala linenums="0" title="DFHDL"
class foo extends EDDesign:
  val `val` = SInt(16) <> OUT
  `val` <> 42
```

</div>

Alternatively, use a non-keyword name with the Scala `@targetName` annotation to set the actual HDL name:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module foo(
  output logic signed [15:0] class
);
  assign class = 16'sd42;
endmodule
```

```scala linenums="0" title="DFHDL"
import scala.annotation.targetName
class foo extends EDDesign:
  @targetName("class") 
  val class_ = SInt(16) <> OUT
  class_ <> 42
```

</div>

Beyond Scala keywords, Verilog module names may also conflict with DFHDL built-in functions brought in by `import dfhdl.*` (e.g., `abs`, `max`, `min`) or with other class names in the same design hierarchy. See [Naming][naming] for the full list of reserved names and resolution patterns (`@targetName`, type aliases, backtick escaping).
///

/// admonition | `Bits` Initialization or Assignment
    type: verilog
`Bits` values cannot be initialized or assigned with plain integers. Use `all(0)` or a sized literal:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
reg [7:0] flags = 8'd0;
reg [7:0] mask  = 8'hFF;
```

```scala linenums="0" title="DFHDL"
val flags = Bits(8) <> VAR init all(0)
val mask  = Bits(8) <> VAR init h"8'FF"
// NOT: Bits(8) <> VAR init 0  // error
```

</div>

Note: `UInt` and `SInt` can be initialized or assigned with plain integers.
///

/// admonition | Inline Conditional Expressions
    type: verilog
Verilog's ternary operator `cond ? a : b` has three DFHDL equivalents:

<div class="grid" markdown>

```sv linenums="0" title="Verilog"
assign out = cond ? a : b;

always @(*)
  out = cond ? a : b;
```

```scala linenums="0" title="DFHDL"
// 1. Using .sel (closest to ternary)
out <> cond.sel(a, b)

// 2. Inline if/else (wrap in parentheses)
out <> (if (cond) a else b)

// 3. Statement form
if (cond) out := a
else out := b
```

</div>

The `.sel` method compiles directly to Verilog's ternary operator. For complex nested conditions, prefer `if`/`else` or `match` over chaining `.sel` calls. See [Selection (.sel)][sel-ops] for details.

When using inline `if`/`else` as the RHS of `:=` or `:==`, **parentheses are required**. Without them, Scala 3 parses the `if` as a statement, not an expression:

```scala
// CORRECT: parenthesized inline if with := and <>
out := (if (cond) a else b)
out <> (if (cond) a else b)

// PARSE ERROR: bare inline if on RHS of := or <>
// out := if (cond) a else b  // "end of statement expected"

// CORRECT: statement form (no parentheses needed)
if (cond) out := a
else out := b
```

This applies to all assignment operators (`:=`, `:==`, `<>`). Use `.sel` or the parenthesized form for inline conditionals; use the statement form for multi-assignment branches.
///

