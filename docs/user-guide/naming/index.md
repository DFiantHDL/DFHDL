# Naming

## Name Collisions & Shadowing

When translating from Verilog/VHDL, signal, port, and module names may collide with names already in scope — Scala keywords, DFHDL built-ins, or your own design classes. This section is the single place that covers how to detect and resolve all such collisions.

### General recommendation: Capitalize design-class names

The simplest way to avoid the entire class of collisions between a **design class** and a **value** (port/variable) is a naming convention:

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
val adder = Adder(WIDTH = 16)
```

/// admonition | Caveat: direct Verilog/VHDL translation that preserves original names
    type: warning
When you translate an existing Verilog/VHDL design and deliberately **preserve the original names** (so the generated HDL matches the source), you cannot always apply the Capitalized convention — the original names may already collide. These cases need the targeted resolutions below.
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
// Fix: create a type alias before instantiation
type AbsModule = abs
val u_abs = AbsModule(DATA_WIDTH = 16)
```

### Design-class name colliding with a value name

A common translation collision is a **design class** whose name is the same as a port or variable in scope (or a built-in). When the bare name `design_class_name(...)` resolves to the value instead of the class, use one of these two fixes:

**1. Explicitly instantiate with `new`.** The `new` keyword forces resolution to the class constructor, sidestepping the shadowing value:

```scala
// `design_class_name` the value is in scope and shadows the class
val u = new design_class_name(DATA_WIDTH = 16)
```

**2. Use a Capitalized class name plus `@targetName` to preserve the emitted name.** Rename the Scala class to a Capitalized identifier (which can no longer collide with a camelCase value) and pin the original lowercase name onto the generated HDL with `@targetName`:

```scala
import scala.annotation.targetName

@targetName("design_class_name")
class DesignClassName(val DATA_WIDTH: Int <> CONST = 8) extends EDDesign:
  // ...

// No collision with values; generated HDL module is still named "design_class_name"
val u = DesignClassName(DATA_WIDTH = 16)
```

## Resolution Patterns

### Backtick escaping

For Scala keywords used as signal names:

```scala
val `type` = UInt(8) <> IN
val `match` = Bit <> OUT
```

### `@targetName` annotation

When a Scala-side name must differ from the generated HDL name, use `@targetName` to set the hardware name explicitly. This is useful when:

- A port name conflicts with a sub-module class name in the same design.
- You want to rename a Scala identifier but preserve the original Verilog port/module name (see [Design-class name colliding with a value name](#design-class-name-colliding-with-a-value-name)).

```scala
import scala.annotation.targetName

// Port "kernel" conflicts with class "kernel" in scope
@targetName("kernel")
val kernel_out = Bits(WIDTH) <> OUT
// Generated HDL port is still named "kernel"

// The class "kernel" remains available for instantiation
val u_kernel = kernel()
```

### Type alias for class-name conflicts

When a class name conflicts with a DFHDL built-in function:

```scala
type AbsModule = abs   // alias resolves the class, not the function
val u_abs = AbsModule(DATA_WIDTH = 8)
```
