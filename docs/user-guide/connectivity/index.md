---
typora-copy-images-to: ./
---
[](){#connectivity}
# Connectivity

DFHDL wires designs together with two kinds of operators:

* The connection operator `<>`.
* The assignment operators `:=` (blocking) and `:==` (non-blocking).

This section focuses on the `<>` connection operator and how it relates to the assignment operators.

## Key Differences Between `<>` and `:=`/`:==`

| Criteria | `<>` Connection | `:=`/`:==` Assignment |
| --- | :--- | :--- |
| Directionality &<br />Commutativity | Commutative: `a <> b` is equivalent to `b <> a`. One side is the *producer* and the other the *consumer*; the dataflow direction is inferred from the operands and the context in which the operator is applied. | Non-commutative: `a := b` makes `b` the *producer*, transferring data to the *consumer* `a`. |
| Mutation | Each consumer bit can be connected at most once. A consumer may still receive several connections, as long as they target disjoint bit ranges (e.g. `y(3, 0) <> a` and `y(7, 4) <> b`). | A consumer bit can be assigned any number of times; the last assignment in program order wins. |
| Statement Order | Connection statements can be placed in any order. | Assignment statements are order-sensitive. |

## Connection `<>` Rules

### Port Direct Connections

The connection operator `<>` is generally used to connect parent designs to their child designs (components) and to connect between sibling designs (children of the same parent). Unlike VHDL/Verilog, there is no need to go through intermediate 'signals' to connect sibling design ports, e.g.:

<div class="grid" markdown>

```scala
class Plus1 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1

class Plus2 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  val p1A = Plus1()
  val p1B = Plus1()
  p1A.x <> x
  p1A.y <> p1B.x
  y <> p1B.y
```

```hdelk width=98%
children = [
  {
    id = Plus2
    inPorts = [x]
    outPorts = [y]
    children = [
      { id = p1A, highlight = 1, type = Plus1, ports = [x, y] },
      { id = p1B, highlight = 1, type = Plus1, ports = [x, y] }
    ]
    edges = [
      [Plus2.x, p1A.x],
      [p1A.y, p1B.x],
      [p1B.y, Plus2.y]
    ]
  },
  {
    id = Plus1
    children = [{ id = op, label = "+1" }]
    inPorts = [x]
    outPorts = [y]
    edges = [
      [Plus1.x, op],
      [op, Plus1.y]
    ]
  }
]
```

</div>

### Port Via Connections

```scala
class Plus1 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1

class Plus2 extends EDDesign:
  val x     = UInt(8) <> IN
  val y     = UInt(8) <> OUT
  val p1A_x = UInt(8) <> VAR
  val p1A_y = UInt(8) <> VAR
  val p1A = new Plus1():
    this.x <> p1A_x
    this.y <> p1A_y
  val p1B_x = UInt(8) <> VAR
  val p1B_y = UInt(8) <> VAR
  val p1B = new Plus1():
    this.x <> p1B_x
    this.y <> p1B_y
  p1A_x <> x
  p1B_x <> p1A_y
  y     <> p1B_y
end Plus2
```

/// details | Runnable example
    type: dfhdl

```scastie main="top_Plus2"
import dfhdl.*

class Plus1 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1

class Plus2 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  val p1A = Plus1()
  val p1B = Plus1()
  p1A.x <> x
  p1A.y <> p1B.x
  y <> p1B.y

given options.CompilerOptions.PrintBackendCode = false
given options.CompilerOptions.PrintDFHDLCode = true
```

///

### Connectable Value Connections

At least one side of a connection must be a *connectable* DFHDL value — a variable (`VAR`) or a port (`IN`/`OUT`/`INOUT`). Connecting two immutable values (e.g. two constants or two read-only aliases) is not allowed, e.g.:

```scala
class Conn1 extends DFDesign:
  val port  = UInt(8) <> OUT
  val temp1 = UInt(8) <> VAR
  val temp2 = UInt(8) <> VAR
  port  <> temp1 // OK: a port connected to a variable
  temp1 <> temp2 // OK: both sides are connectable variables
```

### Partial Selection Connections

A partial selection of a connectable value is itself connectable. This includes bit selection and bit-range selection of `Bits`/`UInt`/`SInt` values, as well as struct field and tuple element selection. This lets you drive a single port or variable from several sources, as long as each bit is driven exactly once (see [Multiple Connections](#multiple-connections)):

```scala
class Split extends EDDesign:
  val hi = UInt(4) <> IN
  val lo = UInt(4) <> IN
  val y  = Bits(8) <> OUT
  y(7, 4) <> hi.bits
  y(3, 0) <> lo.bits
```

### Input Port Assignment `:=` Rule

An input port cannot be assigned to. A connection must be used to drive data into an input port, e.g.:

```scala
class IO extends DFDesign:
  val in  = UInt(8) <> IN
  val out = UInt(8) <> OUT
  out := in // OK: an output port can be assigned internally

class Assign1 extends DFDesign:
  val io = IO()
  // io.in := 1  // Bad assignment! An input port cannot be assigned to
  io.in <> 1     // OK: use a connection instead
  // io.out := 1 // Bad assignment! An output port can only be assigned internally
```

### Immutable Value Connections

When connecting a port to an immutable value (such as a constant), the port must be the consumer. This means the connection is done internally to an output port or externally to an input port, e.g.:

```scala
class IO extends DFDesign:
  val i = UInt(8) <> IN
  val o = UInt(8) <> OUT
  o <> 1     // OK: `o` (output) is the consumer of the constant `1`
  // i <> 1  // Bad connection! `i` is a producer internally; a constant cannot drive into it

class IOUser extends DFDesign:
  val io = IO()
  io.i <> 1    // OK: `io.i` is a consumer externally
  // io.o <> 1 // Bad connection! `io.o` is a producer externally
```

### Different Type Connections

Connecting between different types requires the types to match, possibly through an explicit cast. Different widths are considered different types and require an explicit cast/resize. A casted/converted dataflow value is immutable for the purposes of the connection (see [above](#immutable-value-connections)), so it can only be used as a producer. Here are some examples:

```scala
class DifferentTypesConn extends DFDesign:
  val o   = UInt(8) <> OUT
  val ob9 = Bits(9) <> OUT
  val b8  = Bits(8) <> VAR
  val b9  = Bits(9) <> VAR
  o   <> b8.uint  // OK: an explicit Bits-to-UInt cast is applied
  ob9 <> b9       // OK: matching 9-bit Bits widths
  // o   <> b8    // Bad connection! There is no automatic casting between Bits and UInt
  // ob9 <> b8    // Bad connection! Bit vectors are NOT automatically extended (8 vs 9)
  // o.bits <> b8 // Bad connection! `.bits` is a type-cast alias (immutable) of the output port
```

In contrast to type casts, *bit and bit-range selections* of a port (e.g. `o(3, 0)`) are connectable, as shown in [Partial Selection Connections](#partial-selection-connections).

### Multiple Connections {#multiple-connections}

A given consumer bit can be connected at most once. A single producer, however, can be connected to more than one consumer. Connecting two producers to the *same* consumer bit is an error:

```scala
class Conn2 extends DFDesign:
  val in1   = UInt(8) <> IN
  val in2   = UInt(8) <> IN
  val out   = UInt(8) <> OUT
  val temp1 = UInt(8) <> VAR
  temp1 <> in1   // OK
  out   <> in1   // Also OK! The same producer can connect to more than one consumer
  // temp1 <> in2 // Bad connection! A second producer drives the same bits of `temp1`
```

Because a partial selection is connectable, the same consumer may be driven by several connections that cover *disjoint* bit ranges:

```scala
class Conn3 extends EDDesign:
  val a   = UInt(4) <> IN
  val b   = UInt(4) <> IN
  val out = Bits(8) <> OUT
  out(3, 0) <> a.bits // OK: drives bits 3..0
  out(7, 4) <> b.bits // OK: drives bits 7..4 (disjoint from the above)
  // out(0) <> a(0)    // Bad connection! bit 0 is already driven above
```

### Mixing Assignments and Connections

The same consumer bit cannot be both assigned to (`:=`/`:==`) and connected to (`<>`), e.g.:

```scala
class Conn4 extends RTDesign:
  val out1 = UInt(8) <> OUT
  val out2 = UInt(8) <> OUT
  val out3 = UInt(8) <> OUT
  out1 <> 1   // OK
  // out1 := 1 // Bad assignment! Cannot assign to a connected value

  out2 := 2   // OK
  // out2 <> 2 // Bad connection! Cannot connect to an assigned value

  out3 := 1   // OK
  out3 := 2   // Also OK! Multiple assignments to the same bits are accepted
```

Different bits of the same value may be split between an assignment and a connection, as long as the bit ranges are disjoint:

```scala
class Conn5 extends RTDesign:
  val a = Bits(8) <> IN
  val y = Bits(8) <> OUT
  y(3, 0) <> a(3, 0) // OK: connection drives bits 3..0
  y(7, 4) := a(7, 4) // OK: assignment drives bits 7..4 (disjoint from the above)
```

### Connection Statement Order

The order of `<>` connection statements does not matter.

### Open (Unconnected) Ports {#connectivity-open-ports}

Ports have two connection sides: a consumer side and a producer side. Typically both sides are connected, except for top-level ports. When either side is unconnected, the port is *open*, with the following behavior:

* When the port consumer side is open, the port produces its initial value. An uninitialized open-consumer port produces a bubble (undefined) value.

* When the port producer side is open (unless it is a top-level output port), the port is considered unused and is pruned during compilation, along with any logic used only to drive it.

To explicitly mark a port as unconnected, use the `OPEN` keyword with the `<>` connection operator:

```scala
class Sensor extends EDDesign:
  val din   = UInt(8) <> IN
  val dout  = UInt(8) <> OUT
  val debug = UInt(8) <> OUT
  dout  <> din
  debug <> din

class Top extends EDDesign:
  val din  = UInt(8) <> IN
  val dout = UInt(8) <> OUT
  val sensor_inst = Sensor()
  sensor_inst.din   <> din
  sensor_inst.dout  <> dout
  sensor_inst.debug <> OPEN // explicitly unconnected
```

/// admonition
    type: note
`OPEN` can only be used with the `<>` connection operator. Using it with `:=` assignment results in a compile error.
///

## Valid Connection and Assignment Examples

<div class="grid" markdown>

```scala
class IODesign extends DFDesign:
  val i = UInt(8) <> IN
  val o = UInt(8) <> OUT
  o <> i
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = IODesign
    inPorts = [i]
    outPorts = [o]
    edges = [[IODesign.i, IODesign.o]]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class IODesign1 extends DFDesign:
  val i   = UInt(8) <> IN
  val o   = UInt(8) <> OUT
  val tmp = UInt(8) <> VAR
  tmp <> i
  o   <> tmp
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = IODesign1
    inPorts = [i]
    outPorts = [o]
    children = [{ id = tmp }]
    edges = [
      [IODesign1.i, tmp]
      [tmp, IODesign1.o]
    ]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class IODesign2 extends DFDesign:
  val i1 = UInt(8) <> IN
  val o1 = UInt(8) <> OUT
  val i2 = UInt(8) <> IN
  val o2 = UInt(8) <> OUT
  o1 <> i1
  o2 <> i2
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = IODesign2
    inPorts = [i1, i2]
    outPorts = [o1, o2]
    edges = [
      [IODesign2.i1, IODesign2.o1]
      [IODesign2.i2, IODesign2.o2]
    ]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class Container extends DFDesign:
  val i  = UInt(8) <> IN
  val o  = UInt(8) <> OUT
  val io = IODesign()
  i    <> io.i // connecting owner input to child input
  io.o <> o    // connecting child output to owner output
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = Container
    inPorts = [i]
    outPorts = [o]
    children = [
      { id = io, type = IODesign, highlight = 1, inPorts = [i], outPorts = [o] }
    ]
    edges = [
      [Container.i, io.i]
      [io.o, Container.o]
    ]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class Container2 extends DFDesign:
  val i   = UInt(8) <> IN
  val o   = UInt(8) <> OUT
  val io1 = IODesign()
  val io2 = IODesign()
  i     <> io1.i // connecting owner input to child input
  io1.o <> io2.i // connecting between siblings (output <> input)
  io2.o <> o     // connecting child output to owner output
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = Container2
    inPorts = [i]
    outPorts = [o]
    children = [
      { id = io1, type = IODesign, highlight = 1, inPorts = [i], outPorts = [o] }
      { id = io2, type = IODesign, highlight = 1, inPorts = [i], outPorts = [o] }
    ]
    edges = [
      [Container2.i, io1.i]
      [io1.o, io2.i]
      [io2.o, Container2.o]
    ]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class Container3 extends DFDesign:
  val i  = UInt(8) <> IN
  val o  = UInt(8) <> OUT
  val io = IODesign2()
  i <> io.i1 // connecting owner input to child input
  i <> io.i2 // connecting owner input to child input
  o <> (io.o1 + io.o2)
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = Container3
    inPorts = [i]
    outPorts = [o]
    children = [
      { id = io, type = IODesign2, highlight = 1, inPorts = [i1, i2], outPorts = [o1, o2] }
      { id = op, label = "+" }
    ]
    edges = [
      [Container3.i, io.i1]
      [Container3.i, io.i2]
      [io.o1, op]
      [io.o2, op]
      [op, Container3.o]
    ]
  }
]
```

</div>

---

<div class="grid" markdown>

```scala
class Container4 extends DFDesign:
  val i  = UInt(8) <> IN
  val o  = UInt(8) <> OUT
  val io = IODesign2()
  i     <> io.i1 // connecting owner input to child input
  io.i2 <> 5     // connecting a constant value to a child input
  o     <> io.o2
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = Container4
    inPorts = [i]
    outPorts = [o]
    children = [
      { id = const, label = 5, constant = 1 }
      { id = io, type = IODesign2, highlight = 1, inPorts = [i1, i2], outPorts = [o1, o2] }
    ]
    edges = [
      [Container4.i, io.i1]
      [const, io.i2]
      [io.o2, Container4.o]
    ]
  }
]
```

</div>

---

## Magnet Port Connections

## Future Work

* In the future `<>` will be used to connect multi-port interfaces.
* Connecting between any ancestor which is not a direct parent and child. Currently not fully supported.
