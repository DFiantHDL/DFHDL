# Compilation


---

## Elaboration

### Wildcard Arithmetic Value Checking {#wildcard-check}

When [arithmetic operations][arithmetic-ops] involve wildcard `Int` values (Scala `Int` or DFHDL `Int` parameters), the wildcard `Int` value adapts to the bit-accurate value's sign and width. The value is then checked to ensure it fits. This check occurs at three levels, depending on when the value becomes known:

1. **Scala compile-time**: literal Scala integers (e.g., `u8 + 1000`) have known values at compile time. The Scala compiler reports an error immediately if the wildcard `Int` value exceeds the bit-accurate value's range or has incompatible sign.

2. **DFHDL elaboration-time**: non-literal Scala integers (e.g., `val x: Int = computeValue(); u8 + x`) and DFHDL `Int` constants whose values are resolved during elaboration. A DFHDL elaboration error (Scala runtime error) is generated if the value does not fit.

3. **Synthesis/simulation-time**: DFHDL `Int` parameters that are set externally or computed in complex generation loops may not be known until synthesis or simulation. Assertions must be added to verify these values at the target platform level. This is a planned future feature (TODO).

---

## Generated Files {#generated-files}

Compiling a design emits one file per design, plus up to two shared files.

/// tab | Verilog
| File | Contents |
|------|----------|
| `<Design>.sv` | One file per design in the hierarchy |
| `dfhdl_defs.svh` | DFHDL's own macros and helper definitions. Always emitted, and `` `include ``d by every design. Public domain, so it can be redistributed with generated output |
| `<Top>_defs.svh` | **Your design's** global declarations, named after the top design. Emitted only when something needs to be shared across designs |
///

/// tab | VHDL
| File | Contents |
|------|----------|
| `<Design>.vhd` | One file per design in the hierarchy |
| `dfhdl_pkg.vhd` | DFHDL's own helper package. Always emitted |
| `<Top>_pkg.vhd` | **Your design's** global package, named after the top design. Emitted only when something needs to be shared across designs |
///

### The global definitions file {#global-defs}

A declaration goes into `<Top>_defs.svh` / `<Top>_pkg.vhd` when more than one design must name it. The common case is an [enum][DFEnum] appearing in a port type, since the two modules on either side of the connection have to agree on the type:

```scala
enum State extends Encoded:
  case IDLE, RUN

class lane extends EDDesign:
  val s = State <> IN
  // ...

class Foo extends EDDesign:
  val s = State <> IN
  val u = new lane
  u.s <> s
```

/// tab | Generated Verilog
```systemverilog title="Foo_defs.svh"
`ifndef FOO_DEFS
`define FOO_DEFS
typedef enum logic [0:0] {
  State_IDLE = 0,
  State_RUN  = 1
} t_enum_State;
`endif
```

```systemverilog title="Foo.sv"
`include "Foo_defs.svh"

module Foo(
  input  wire t_enum_State s,
  output logic             o
);
  `include "dfhdl_defs.svh"
  // ...
```
The file is include-guarded, so every design that needs it can include it unconditionally.
///

/// tab | Generated VHDL
```vhdl title="Foo_pkg.vhd"
package Foo_pkg is
type t_enum_State is (
  State_IDLE, State_RUN
);
function bitWidth(A: t_enum_State) return integer;
function to_slv(A: t_enum_State) return std_logic_vector;
function to_t_enum_State(A: std_logic_vector) return t_enum_State;
-- ...
end package Foo_pkg;
```
The VHDL package carries the conversion and helper functions for the type alongside its declaration.
///

Had `State` been used only inside a single design, no `<Top>_defs` file would be emitted at all and the typedef would sit inside that one module. The placement follows the usage, not the Scala declaration site.
