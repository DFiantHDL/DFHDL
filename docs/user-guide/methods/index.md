# Methods {#Methods}

A DFHDL **method** is a Scala `def` declared inside a design. Methods factor repeated or parameterized logic into named, reusable pieces, much like [design hierarchy][design-hierarchy] factors it into sub-designs, but with lightweight call-site syntax instead of explicit port wiring.

The return-type marker on a method selects both its domain and how a call to it compiles:

| Return marker | Domain | A call compiles to |
|---|---|---|
| `<> DFRET` | Dataflow (DF) | a design instance (a *method design*) |
| `<> RTRET` | Register-Transfer (RT) | reserved for future use |
| `<> EDRET` | Event-Driven (ED) | a Verilog function or task, a VHDL function or procedure |
| `<> CONSTRET` | Static | a Verilog `function automatic`, a VHDL `pure function` |

Method arguments are marked `<> VAL` (see the [Type System][type-system]). A method that returns a value is a **function**; a method that returns `Unit` is a **procedure**.

## Two ways a method compiles

DFHDL methods fall into two families, distinguished by what a call becomes in the generated code:

- **DF methods** (`<> DFRET`) each elaborate into a **design of their own**, and every call becomes an instance of that design. The method is the abstraction you write; the design it elaborates into is called a **method design**, and in the output it is an ordinary sub-design, indistinguishable from one declared as a class. This mechanism is [functional composition][functional-composition], covered under Design Hierarchy.

- **ED methods and static functions** (`<> EDRET` / `<> CONSTRET`) stay methods in the output: they compile to HDL subprograms (Verilog functions and tasks, VHDL functions and procedures) that are called in place. The rest of this page covers these.

The `<> RTRET` marker is reserved for future RT methods; it does not currently behave like a DF method.

/// admonition | Method vs. method design
    type: info
A **method** is the source-level abstraction (the `def`). A **method design** is the elaborated design block that a DF method turns into. A DF method call therefore appears in the generated HDL as a module/entity instance, whereas an ED or static method call appears as a function, task, or procedure call. Only DF methods produce method designs; ED and static methods do not.
///

## ED methods

An ED method is declared inside an `EDDesign` (or an ED domain). A value-returning ED method (`<> EDRET`) compiles to a function; a `Unit`-returning ED method compiles to a task (Verilog) or a procedure (VHDL). ED methods may be called from `process` blocks and from concurrent assignments.

### ED functions

/// admonition | Two ED functions, one pure and one capturing outer values
    type: example
`add` is a plain function of its two arguments. `addBK` additionally reads the outer signal `b` and the outer constant `k`; those are *captured* automatically (see [Capturing outer values](#capturing-outer-values) below) rather than passed as arguments.

```scala
class EDFunc extends EDDesign:
  val a = UInt(8) <> IN
  val b = UInt(8) <> IN
  val y = UInt(8) <> OUT
  val z = UInt(8) <> OUT
  def add(l: UInt[8] <> VAL, r: UInt[8] <> VAL): UInt[8] <> EDRET =
    val tmp = UInt(8) <> VAR
    tmp := l + r
    tmp
  val k: UInt[8] <> CONST                        = 5
  def addBK(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + b + k
  y <> add(a, b)
  process(all):
    z := addBK(a)
end EDFunc
```

/// tab | Generated Verilog
```verilog
module EDFunc(
  input  wire logic [7:0] a,
  input  wire logic [7:0] b,
  output logic [7:0] y,
  output logic [7:0] z
);
  `include "dfhdl_defs.svh"
  localparam logic [7:0] k = 8'd5;
  function automatic logic [7:0] add(input logic [7:0] l, input logic [7:0] r);
    logic [7:0] tmp;
  begin
    tmp = l + r;
    add = tmp;
  end
  endfunction

  function automatic logic [7:0] addBK(input logic [7:0] l);
  begin
    addBK = l + b + k;
  end
  endfunction
  assign y = add(a, b);
  always_comb
  begin
    z = addBK(a);
  end
endmodule
```
Both methods emit as `function automatic`. A local `VAR` in the body (`tmp`) becomes a function-local variable, and the return value is assigned to the function name. The captured constant `k` is emitted once as a `localparam`, and the captured signal `b` is read directly inside the function body.
///

/// tab | Generated VHDL
```vhdl
architecture EDFunc_arch of EDFunc is
  constant k : unsigned(7 downto 0) := 8d"5";
  function add(l : unsigned(7 downto 0); r : unsigned(7 downto 0)) return unsigned is
    variable tmp : unsigned(7 downto 0);
  begin
    tmp := l + r;
    return tmp;
  end function;

  impure function addBK(l : unsigned(7 downto 0)) return unsigned is
  begin
    return l + b + k;
  end function;
begin
  y <= add(a, b);
  process (a, b)
  begin
    z <= addBK(a);
  end process;
end EDFunc_arch;
```
In VHDL the pure `add` is a plain `function`, while `addBK` becomes an `impure function` because it reads the outer signal `b`. A captured constant such as `k` is emitted as an architecture `constant`.
///
///

### ED procedures

A `Unit`-returning ED method is a procedure. It performs statements (assignments, `report`, `wait`) for their effect and returns nothing, so it must be called from statement position inside a `process`.

/// admonition | An ED procedure that reports and waits
    type: example
```scala
class EDTask extends EDDesign:
  val a = UInt(8) <> IN
  def show(l: UInt[8] <> IN): Unit <> EDRET =
    report(s"value is $l")
    wait(1.ns)
  def pause(): Unit <> EDRET =
    wait(2.ns)
  process:
    show(a)
    pause()
end EDTask
```

/// tab | Generated Verilog
```verilog
module EDTask(
  input  wire logic [7:0] a
);
  `include "dfhdl_defs.svh"
  task automatic show(input logic [7:0] l);
  begin
    $info("value is %d", l);
    #1ns;
  end
  endtask

  task automatic pause;
  begin
    #2ns;
  end
  endtask
  always
  begin
    show(a);
    pause;
  end
endmodule
```
A `Unit`-returning method emits as a `task automatic`. A parameterless task (`pause`) is declared and called without parentheses.
///

/// tab | Generated VHDL
```vhdl
architecture EDTask_arch of EDTask is
  procedure show(l : unsigned(7 downto 0)) is
  begin
    report "value is " & to_string(l) & "" severity NOTE;
    wait for 1 ns;
  end procedure;

  procedure pause is
  begin
    wait for 2 ns;
  end procedure;
begin
  process
  begin
    show(a);
    pause;
  end process;
end EDTask_arch;
```
In VHDL a `Unit`-returning method emits as a `procedure`.
///
///

### Capturing outer values {#capturing-outer-values}

An ED method may reference values declared outside it (ports, variables, or constants of the enclosing design), as `addBK` does with `b` and `k` in the [ED functions](#ed-functions) example above. Such values are **captured** automatically: they do not appear in the method's argument list or at the call site, and the method body refers to them by name. In the generated code:

- A captured **constant** is emitted once at design scope (a Verilog `localparam`, a VHDL `constant`).
- A captured **signal** is read directly inside the subprogram body. In VHDL this makes the function `impure`; a function that captures no signal stays a plain `function`.

## Static functions

A static function is declared with `<> CONSTRET`. All of its arguments must be constants (`<> CONST`), and it always returns a value, so it is always a function (there are no static procedures). Static functions are pure by definition and may be called from any domain, including to compute a design parameter.

/// admonition | A static function
    type: example
```scala
class StaticFn extends EDDesign:
  val o = UInt(8) <> OUT
  def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = n + n
  o <> twice(d"8'3")
end StaticFn
```

/// tab | Generated Verilog
```verilog
module StaticFn(
  output logic [7:0] o
);
  `include "dfhdl_defs.svh"
  function automatic logic [7:0] twice(input logic [7:0] n);
  begin
    twice = n + n;
  end
  endfunction
  assign o = twice(8'd3);
endmodule
```
///

/// tab | Generated VHDL
```vhdl
architecture StaticFn_arch of StaticFn is
  pure function twice(n : unsigned(7 downto 0)) return unsigned is
  begin
    return n + n;
  end function;
begin
  o <= twice(8d"3");
end StaticFn_arch;
```
A static function always emits as a VHDL `pure function` (and a Verilog `function automatic`), because it is guaranteed to have no side effects.
///
///

The compiler also generates static functions on its own: under the VHDL backend, a multi-statement [`initial` block][initial-blocks] (e.g. a loop initializing a vector) is lowered into a generated static function whose call becomes the declaration's default value, since VHDL has no `initial` construct.

Because a static function is constant-valued, its result can feed a design parameter. Calling `twice` to parameterize a sub-design compiles to an ordinary parameter binding at the instantiation site:

/// admonition | A static function computing a sub-design parameter
    type: example
```scala
class Inner(val k: UInt[8] <> CONST = d"8'0") extends EDDesign:
  val o = UInt(8) <> OUT
  o <> k
class StaticParamTop extends EDDesign:
  val o = UInt(8) <> OUT
  def twice(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = n + n
  val inner = new Inner(twice(d"8'3"))
  o <> inner.o
end StaticParamTop
```

/// tab | Generated Verilog
```verilog
module StaticParamTop(
  output logic [7:0] o
);
  `include "dfhdl_defs.svh"
  logic [7:0] inner_o;
  function automatic logic [7:0] twice(input logic [7:0] n);
  begin
    twice = n + n;
  end
  endfunction
  Inner #(
    .k (twice(8'd3))
  ) inner(
    .o /*-->*/ (inner_o)
  );
  assign o = inner_o;
endmodule
```
The call `twice(d"8'3")` becomes the parameter binding `.k (twice(8'd3))` on the `Inner` instance.
///

/// tab | Generated VHDL
```vhdl
entity StaticParamTop is
port (
  o : out unsigned(7 downto 0)
);
end StaticParamTop;

architecture StaticParamTop_arch of StaticParamTop is
  signal inner_o : unsigned(7 downto 0);
  pure function twice(n : unsigned(7 downto 0)) return unsigned is
  begin
    return n + n;
  end function;
begin
  inner : entity work.Inner(Inner_arch) generic map (
    k => twice(8d"3")
  ) port map (
    o => inner_o
  );
  o <= inner_o;
end StaticParamTop_arch;
```
The call `twice(d"8'3")` becomes the generic binding `k => twice(8d"3")` on the `Inner` instance.
///
///

## Global methods

An ED method or static function is normally emitted *inside* the design that uses it (a VHDL function in the architecture, a Verilog function in the module). When the **same** method is used by **more than one design**, or is called from **global scope**, DFHDL emits it **once** in the shared globals area instead of duplicating it in each design, the same way a named type or a global constant is shared. This happens automatically; there is no separate keyword.

- **VHDL**: the method is declared in a package (a prototype in the package declaration and its body in the package body), and each using architecture pulls it in with `use work.<pkg>.all;`.
- **Verilog / SystemVerilog**: the method is written once in the generated defs header (`.svh` / `.vh`) and included by each using module.

A method qualifies for this shared emission only when it captures nothing from a single design: its body must reference only its own arguments, global constants, and other global methods. A method that captures a design-local signal or constant stays inside each using design, since a shared package has no access to a single design's members.

/// admonition | Declaring a method at the top level
    type: info
The most direct way to share a method across designs is to declare it at the top level (outside any design), just like a global constant. It is then available to every design, and it is emitted once in the globals area as soon as two or more designs call it. A top-level method used by only one design is still emitted locally inside that design.
///

### Static functions at global scope

Because a static function is constant-valued, it can also be **called at global scope** to compute a global constant or a design parameter's default, keeping the generated HDL parametric rather than folding to a literal:

```scala
def clog2(n: UInt[8] <> CONST): UInt[8] <> CONSTRET = /* ... */
val ADDR_W: UInt[8] <> CONST = clog2(d"8'200")
```

`clog2` is emitted once in the globals area, and the global constant is emitted there as its call, `clog2(...)`. A static function called this way is never elaboration-cached (it is re-elaborated per call), which does not affect the generated HDL. (ED methods have no global-scope form: an ED method can only be called inside an event-driven domain.)

## What a method body may contain

Because ED methods and static functions become HDL subprograms rather than designs, their bodies are restricted to what a subprogram can express. A method body may **not** contain a design instance, a `process`, a domain block, or a recursive call to itself. Static functions are further restricted to be pure: they may not perform text output or `wait`, take non-constant arguments, capture non-constant values, return `Unit`, or call an ED method. These rules are checked at compile time, and a violation is reported as an error.

DF methods have their own model: they become [method designs](#two-ways-a-method-compiles), so their bodies follow the rules of the DF domain. See [Functional Composition][functional-composition] for that path.
