# Conditionals

DFHDL supports two main types of conditional constructs: `if` expressions and `match` expressions. Both can be used for control flow and value selection in hardware designs.

## If Expressions

### Basic Syntax

```scala
if (condition) consequent
else alternative
```

- `condition` must be a DFHDL `Boolean` or `Bit` value
- `consequent` and `alternative` are the code blocks executed based on the condition
- The `else` clause is optional

### Examples

```scala
// Basic if-else
if (enable) 
  output := input
else 
  output := 0

// Chained if-else
if (state == State.Idle) 
  output := 0
else if (state == State.Active) 
  output := input
else 
  output := lastValue

// If without else
if (reset) 
  counter := 0
```

### Rules

1. **Type Consistency**: Both branches must produce compatible types if used as an expression
2. **Scope**: Variables declared inside if blocks are only visible within that block
3. **Hardware Generation**: Each branch generates hardware - both paths exist in parallel
4. **Constant Conditions**: At a concurrent scope (a design or domain body, outside any process), an `if` whose condition is a DFHDL constant is resolved at **elaboration time**; only the taken branch generates hardware. See [Elaboration-Time Conditionals](#elaboration-time-conditionals) below.

### Elaboration-Time Conditionals

In a **concurrent scope** (a design or domain body, outside any process), an `if` whose condition is a **DFHDL constant** is resolved at **elaboration time**, exactly like a Verilog `generate if` (VHDL `if ... generate`). Only the taken branch is elaborated into hardware; the untaken branch produces none:

```scala
class Gate(val INVERT: Boolean <> CONST = false) extends EDDesign:
  val x = Bits(16) <> IN
  val y = Bits(16) <> OUT
  if (INVERT)          // constant condition: resolved during elaboration
    y <> ~x            // elaborated only when INVERT is true
  else
    y <> x             // elaborated only when INVERT is false
```

With the default `INVERT = false`, only the `else` branch survives elaboration: the generated HDL contains no conditional, just the pass-through.

/// tab | Generated Verilog
```verilog
module Gate#(parameter logic INVERT = 0)(
  input  wire logic [15:0] x,
  output      logic [15:0] y
);
  `include "dfhdl_defs.svh"
  assign y = x;
endmodule
```
///

/// tab | Generated VHDL
```vhdl
entity Gate is
generic (
  INVERT : boolean := false
);
port (
  x : in  std_logic_vector(15 downto 0);
  y : out std_logic_vector(15 downto 0)
);
end Gate;

architecture Gate_arch of Gate is
begin
  y <= x;
end Gate_arch;
```
///

Because the selected branch depends on the constant's value, instantiating the design with different parameter values yields distinct elaborated designs (with enumerated names). The `INVERT` parameter still appears in the generated interface, but the body is fixed to the branch chosen at elaboration.

As in Verilog and VHDL, scope decides what an `if` means: at concurrent (design/architecture) scope it is a generate/elaboration conditional, while inside a process it is an ordinary hardware conditional (a multiplexer or enable). A constant condition does not change that: an `if` inside a process stays a hardware conditional even when its condition is constant, exactly as a procedural `if` inside a Verilog `always` or VHDL `process` block is never a generate conditional.

The one genuine difference from Verilog `generate if` / VHDL `if ... generate` is that **DFHDL type-checks both branches**: even though only the taken branch is elaborated, DFHDL (via Scala) compiles both, so both must be valid for every parameter value. See [Loops](../loops/index.md) for the analogous elaboration-time loop behavior and workarounds.

## Match Expressions

Match expressions provide pattern matching capabilities similar to Scala's match expressions but optimized for hardware description.

### Basic Syntax

```scala
value match
  case pattern1 => expression1
  case pattern2 => expression2
  case _ => defaultExpression
```

### Pattern Types

1. **Literal Patterns**:
```scala
x match
  case 0 => y := 0
  case 1 => y := 1
  case _ => y := x
```

2. **Multiple Values**:
```scala
x match
  case 1 | 2 | 4 => y := 0
  case 3 | 5 | 6 => y := 1
  case _ => y := 2
```

3. **Struct Patterns**:
```scala
pixel match
  case Pixel(0, y) => output := y
  case Pixel(x, 0) => output := x
  case Pixel(x, y) => output := x + y
```

4. **Guard Patterns**:
```scala
x match
  case n if n > 10 => y := 1
  case n if n < 0  => y := 0
  case _           => y := x
```

5. **Bit Patterns with Wildcards**:
```scala
bits match
  case b"1??0" => y := 1
  case b"0??1" => y := 0
  case _       => y := x
```

6. **Bit Extractor Patterns**:
```scala
// Match and extract a 32-bit section between DEAD and BEEF
y match
  case h"DEAD${secret: B[32]}BEEF" => 
    // Use extracted secret bits

// Extract multiple sections
y match
  case h"DE${part1: B[16]}AD${part2: B[16]}BEEF" =>
    // Use part1 and part2 bits

// Store extracted bits in variables
val h"DEAD${extracted: B[32]}BEEF" = input.runtimeChecked

// Extract multiple sections into variables
val h"DE${first: B[16]}ADBE${second: B[16]}EF" = input.runtimeChecked

// Using guards with extracted bit fields
y match
  case h"DEAD${secret: B[32]}BEEF" if secret > h"20000000" =>
    // Match when the secret section is greater than 0x20000000
  case h"DE${part1: B[16]}AD${part2: B[16]}BEEF" if part1 == h"FFFF" =>
    // Match when part1 is all ones
```

Bit extractor patterns allow you to:
- Match specific bit patterns while extracting variable sections
- Use hex or binary notation for the fixed parts
- Specify the width of extracted sections using `B[width]` syntax
- Store extracted bits in variables for later use
- Extract multiple sections in a single pattern match
- Use guards to add conditions on extracted bit fields

### Match to If Conversion

During compilation, match expressions are typically converted to if-else chains for hardware implementation. For example:

```scala
// Original match
x match
  case 22           => y := 0
  case 11 | 33 | 44 => y := 1
  case _ if x == 55 => y := 2
  case _            => y := 3

// Converts to
if (x == sd"16'22") 
  y := sd"16'0"
else if ((x == sd"16'11") || (x == sd"16'33") || (x == sd"16'44")) 
  y := sd"16'1"
else if (x == sd"16'55") 
  y := sd"16'2"
else 
  y := sd"16'3"
```

### Complex Selectors

When using expressions as match selectors, a temporary variable is created:

```scala
// Original
(x + 1) match
  case 22 => y := 0
  case _  => y := 1

// Converts to
val match_sel = x + sd"16'1"
if (match_sel == sd"16'22") 
  y := sd"16'0"
else 
  y := sd"16'1"
```

### Rules

1. **Exhaustiveness**: Match expressions must cover all possible cases (for an enum selector, listing all declared entries suffices; see [Enum Matches and the Wildcard `case _`][enum-match-wildcard] for when to add a wildcard branch anyway)
2. **Pattern Order**: Patterns are evaluated in order, first match wins
3. **Type Safety**: All case branches must produce compatible types if used as an expression
4. **Hardware Implementation**: 
   - Converts to if-else chains for most cases
   - Optimizes bit pattern matching into efficient comparisons
   - Extracts struct fields into temporary variables when needed

/// admonition | Use enumerations for state matching
    type: tip
For FSM-style `match` over states, define an enumeration with the appropriate [encoding](../type-system/index.md#dfhdl-enumeration-enum--extends-encoded) and match on its members:

```scala
enum State extends Encoded:  // 00, 01, 10
  case Idle, Start, Data

val state = State <> VAR
state match
  case State.Idle  => // idle logic
  case State.Start => // start logic
  case State.Data  => // data logic
```

For non-sequential state encodings, use `Encoded.Manual`:
```scala
enum State(val value: UInt[3] <> CONST) extends Encoded.Manual(3):
  case Idle  extends State(d"3'0")
  case Start extends State(d"3'3")
  case Data  extends State(d"3'5")
```
///

### Enum Matches and the Wildcard `case _` {#enum-match-wildcard}

Listing every entry of an enum makes a `match` exhaustive, but the register underneath can often hold encodings that no entry names: the 3-entry `State` enum above occupies 2 bits, leaving `b"11"` representable but unlisted (one-hot, gray, `StartAt`, and `Manual` encodings leave even more). A wildcard branch after all the entries covers exactly those unlisted encodings, and whether to write it depends on the rules below:

```scala
state match
  case State.Idle  => // idle logic
  case State.Start => // start logic
  case State.Data  => // data logic
  // wildcard branch, reached only on the unlisted b"11" encoding;
  // optional in a fresh design, mandatory in a translation 
  // from Verilog's `default:` or VHDL's `when others =>` 
  // (see rules below)
  case _           => state := State.Idle
```

- **Fresh designs:** no need to write `case _ =>` if don't-care behavior on unlisted encodings is acceptable; synthesis exploits the don't-cares for better hardware utilization, and most designs require nothing more. The trade-off: if unstable timing/reset or an environmental effect that randomly flips bits (however unlikely) lands the register on an unlisted encoding, the outcome is unknown, and without a wildcard branch that keeps the current state or jumps to a known initialization state (as above) the chances of recovery are reduced. Only high-reliability designs need it.
- **Translations of existing Verilog/VHDL:** translate a `default:` / `when others =>` branch as `case _ =>` too. Omit it only for the default binary encoding with exactly 2^n unique cases, where every encoding is named and the branch is unreachable; for any other encoding or case count, dropping it makes the translation not formally equivalent to the original. See [FSM State Encoding][fsm-state-encoding] for a worked translation.

### Best Practices

1. **Use Match for Multi-Way Branching**: When dealing with multiple cases, match is often clearer than nested if-else
2. **State Machines**: Match expressions are ideal for state machine implementations
3. **Pattern Priority**: Put more specific patterns before general ones
4. **Wildcards**: Use `_` for catch-all cases
5. **Guards**: Use sparingly as they may generate more complex hardware

## Hardware Implications

Both `if` and `match` expressions generate multiplexer circuits in hardware. The choice between them should consider:

1. **Readability**: Match expressions are often clearer for multiple cases
2. **Hardware Efficiency**: Simple if-else may generate simpler hardware
3. **Timing**: Complex conditions may impact critical paths
4. **Resource Usage**: Each branch generates hardware, even if mutually exclusive

## Examples

### State Machine
```scala
class StateMachine extends RTDesign:
  val state = State <> VAR.REG init State.Idle
  
  state match
    case State.Idle =>
      if (start) state := State.Active
    case State.Active =>
      if (done) state := State.Idle
```

### Decoder
```scala
class Decoder extends DFDesign:
  val input = UInt(4) <> IN
  val output = Bits(16) <> OUT
  
  output := input match
    case 0  => b"0001"
    case 1  => b"0010"
    case 2  => b"0100"
    case _  => b"0000"
```

### Complex Pattern Matching
```scala
class PixelProcessor extends DFDesign:
  val pixel = Pixel <> IN
  val result = UInt(8) <> OUT
  
  result := pixel match
    case Pixel(x, y) if x == y => x
    case Pixel(x, 0)           => x * 2
    case Pixel(0, y)           => y * 2
    case Pixel(x, y)           => (x + y) / 2
```

### Usage Modes

DFHDL conditionals can be used in two ways: as statements and as expressions.

#### Statement Usage
When used as statements, `if` and `match` are used for their side effects (like assignments) rather than producing a value:

```scala
// If statement
if (i) x := 1
else if (!i) x := 2

// Match statement
x match
  case 77 | 11 => x := 1
  case _       =>
    x := 3
    x := 4
```

#### Expression Usage
When used as expressions, `if` and `match` produce values that can be assigned or used in computations:

```scala
// If expression
val res: UInt[8] <> VAL =
  if (i) 1
  else if (!i) x.bits.uint
  else 2

// Match expression
val res: UInt[8] <> VAL =
  x match
    case 0 | 1 | 2 | 3 => 77
    case _ => 22
```

Key differences:
1. **Type Requirements**:
   - Statements: No return type consistency required between branches
   - Expressions: All branches must return compatible types

2. **Assignment Context**:
   - Statements: Can contain multiple assignments per branch
   - Expressions: Must evaluate to a single value

3. **Variable Declaration**:
   ```scala
   // Direct assignment from expression
   val res2 = UInt(8) <> VAR
   res2 := (if (i) 1 else if (!i) x.bits.uint else 2)
   
   // Using statement form for multiple assignments
   if (i) 
     x := 1
     y := 2
   else 
     x := 3
     y := 4
   ```

### Expression vs. Statement Form

DFHDL conditionals can be written in two equivalent forms that generate different hardware structures:

#### Expression Form
In expression form, the conditional is part of the right-hand side of an assignment:

```scala
// If as expression
output := (if (cond) a else b)

// Match as expression
output := (sel match
  case A => a
  case B => b)
```

#### Statement Form
In statement form, the assignments are inside the conditional branches:

```scala
// If as statement
if (cond) 
  output := a
else 
  output := b

// Match as statement
sel match
  case A => output := a
  case B => output := b
```

#### Key Differences

1. **Hardware Structure**:
   - Expression form: Generates a single multiplexer with the condition as select
   - Statement form: Generates multiple assignments with enable signals derived from conditions

2. **Code Organization**:
   - Expression form: Better when the only difference between branches is the value being assigned
   - Statement form: Better when branches perform multiple operations or have different side effects

3. **Timing Implications**:
   - Expression form: All values are computed in parallel, then selected
   - Statement form: Each branch's logic is gated by its condition

4. **Common Use Cases**:
   ```scala
   // Expression form - simple value selection
   val result = UInt(8) <> OUT
   result := (if (valid) computed_value else 0)

   // Statement form - multiple operations
   if (valid)
     result := computed_value
     status := VALID
     counter := counter + 1
   else
     result := 0
     status := INVALID
   ```

Choose the form that best matches your design's requirements:
- Use expression form for simple value selection
- Use statement form for complex control flow or multiple operations per branch


---


