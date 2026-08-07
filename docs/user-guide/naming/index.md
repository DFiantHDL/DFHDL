# Naming

## Name Collisions & Shadowing

When translating from Verilog/VHDL, signal, port, and module names may collide with names already in scope: Scala keywords, DFHDL built-ins, or your own design classes. This section is the single place that covers how to detect and resolve all such collisions.

### General recommendation: Instantiate designs with `new`

Always instantiate a child design with `new DesignName(...)`:

```scala
val adder = new Adder(WIDTH = 16)
```

The `new` keyword forces resolution to the **class constructor**, so the instantiation can never be captured by a value, a port, or a DFHDL built-in function that happens to share the name. Writing `Adder(WIDTH = 16)` relies on Scala 3 universal apply methods, which resolve by name and can therefore be shadowed. Using `new` everywhere removes the entire class of instantiation collisions before it appears, so it is the recommended form throughout this documentation.

### Additional recommendation: Capitalize design-class names

On top of `new`, a naming convention keeps the two namespaces visually distinct:

- Name **design classes** with a `Capitalized` (PascalCase) name.
- Name **ports and variables** with `camelCase` names.

Because the two casings can never be identical, a design class and a value will never shadow each other. This is the **preferred convention** for new designs.

```scala
class Adder(val WIDTH: Int <> CONST = 8) extends EDDesign:
  val a = UInt(WIDTH) <> IN
  val b = UInt(WIDTH) <> IN
  val y = UInt(WIDTH) <> OUT
  // ...

// In a parent design, the Capitalized class name never collides with camelCase values:
val adder = new Adder(WIDTH = 16)
```

/// admonition | Caveat: direct Verilog/VHDL translation that preserves original names
    type: warning
When you translate an existing Verilog/VHDL design and deliberately **preserve the original names** (so the generated HDL matches the source), you cannot always apply the Capitalized convention, since the original names may already collide. These cases need the targeted resolutions below.
///

### Scala reserved keywords

Scala keywords cannot be used directly as identifiers. Escape them with backticks:

`val`, `var`, `def`, `type`, `class`, `object`, `trait`, `enum`, `match`, `case`, `if`, `else`, `for`, `while`, `do`, `return`, `throw`, `try`, `catch`, `finally`, `yield`, `import`, `export`, `new`, `this`, `super`, `true`, `false`, `null`, `then`, `end`, `given`, `using`, `extension`, `with`, `abstract`, `final`, `override`, `sealed`, `lazy`, `private`, `protected`

```scala
// Verilog signal named "val"
val `val` = SInt(16) <> OUT
`val` <> 42
```

### DFHDL built-in names

`import dfhdl.*` brings DFHDL built-in functions and types into scope. If a user-defined class has the same name as a built-in, the built-in shadows the class. Built-ins that commonly collide with Verilog module names:

`abs`, `clog2`, `max`, `min`, `all`, `Bit`, `Bits`, `UInt`, `SInt`

```scala
// Module named "abs" conflicts with dfhdl.abs
class abs(val DATA_WIDTH: Int <> CONST = 8) extends EDDesign:
  // ...

// In the parent design, `abs(...)` resolves to the built-in function, not the class.
// Fix: instantiate with `new`, which always resolves to the class constructor
val u_abs = new abs(DATA_WIDTH = 16)
```

This is the [general `new` recommendation](#general-recommendation-instantiate-designs-with-new) applied to a built-in collision. Following it from the start makes this collision a non-issue.

### Design-class name shared with a value name

A **design class** may share its name with a port or variable in scope. This is not a collision that needs resolving: Scala keeps types and terms in separate namespaces, so the class stays reachable and the value keeps its name.

```scala
class stage(val WIDTH: Int <> CONST = 8) extends EDDesign:
  val d = Bits(WIDTH) <> IN
  val q = Bits(WIDTH) <> OUT
  q <> d

class wrapper(val WIDTH: Int <> CONST = 8) extends EDDesign:
  val d     = Bits(WIDTH) <> IN
  val stage = Bits(WIDTH) <> OUT     // port sharing the child class's name

  val stage_inst = new stage(WIDTH)  // resolves to the class constructor
  stage_inst.d <> d
  stage <> stage_inst.q
```

```verilog title="Generated Verilog"
module wrapper#(parameter int WIDTH = 8)(
  input  wire logic [WIDTH - 1:0] d,
  output      logic [WIDTH - 1:0] stage
);
  stage #(.WIDTH (WIDTH)) stage_inst(...);
```

The port keeps the name `stage` and the child module is still instantiated from the class of the same name. The one form that goes wrong is the bare apply, and only because the value wins the term position:

```scala
val stage_inst = stage(WIDTH)  // NOT an instantiation: this bit-selects the port
```

It fails in a way worth recognizing, because it usually stays silent until something downstream reads a member off it:

```
value q is not a member of Bit <> VAR, but could be made available as an extension method.
```

That is `stage(WIDTH)` having quietly become a bit-select on the port. Always instantiating with `new`, per the [general recommendation](#general-recommendation-instantiate-designs-with-new), removes the whole class of problems and needs no renaming and no annotation.

## Resolution Patterns

### Backtick escaping

For Scala keywords used as signal names:

```scala
val `type` = UInt(8) <> IN
val `match` = Bit <> OUT
```

### `@setName` annotation

`@hw.annotation.setName` applies to a **port, variable, or DFHDL method**, and sets the name that construct carries in the generated HDL. Use it when the Scala-side identifier must differ from the HDL name you need to emit:

```scala
class filter(val WIDTH: Int <> CONST = 8) extends EDDesign:
  @hw.annotation.setName("data_out")
  val dataOut = Bits(WIDTH) <> OUT
  dataOut <> all(0)
```

```verilog title="Generated Verilog"
module filter#(parameter int WIDTH = 8)(
  output logic [WIDTH - 1:0] data_out
);
```

The same annotation applies to a **design class**, where it sets the emitted module name. This is what lets a translation follow Scala naming style on the Scala side while still emitting the original Verilog module name. It works on top-level classes too, where Scala's own `@targetName` is rejected (the DFHDL compiler plugin reads the annotation, not the Scala backend):

```scala
@hw.annotation.setName("data_path")
class DataPath(val WIDTH: Int <> CONST = 8) extends EDDesign:
  // ...
// Generated HDL module is named "data_path"
val u_dp = new DataPath(16)
```

/// admonition | Not for names that merely look alike
    type: note
A port sharing a name with a design class needs no annotation and no rename, since types and terms live in separate namespaces. See [Design-class name shared with a value name](#design-class-name-shared-with-a-value-name). Reach for `@hw.annotation.setName` when you need a **different** HDL name, not when two Scala names collide.
///
