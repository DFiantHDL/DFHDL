---
typora-copy-images-to: ./
---
[](){#interfaces}
# Interfaces

An *interface* groups a set of related ports (and the parameters that shape them) into
a single, reusable bundle, and describes, through one or more *views*, how that bundle
is driven from each side of a connection. Interfaces let you declare a protocol such as a
streaming handshake or an AXI bus *once*, then wire it between designs with a single `<>`
connection instead of dozens of per-port connections.

<div class="grid" markdown>

/// admonition | Terminology
    type: quote
* _interface_ - A Scala class extending `Interface` that bundles related ports and
  parameters. It is purely structural: it carries ports and views but no behavior (no
  processes, connections, or assignments).
* _port_ - A value declared inside an interface (e.g. `Bits(8) <> VAR`). Ports are
  internal to the interface and are exposed only through a view.
* _view_ - A named, directional projection of an interface's ports. A view assigns each
  port a direction (`in`/`out`) *from the point of view of the design that uses it*. An
  interface may declare several views over the same ports.
* _flip_ - The converse of a view: every `in` becomes `out` and every `out` becomes `in`.
  Used to derive the opposite side of a protocol (e.g. `subordinate = manager.flip`).
* _interface instance_ - An interface class instantiated inside a design, exactly like a
  child design instance.
///

```scala linenums="0" title="A streaming handshake interface, used by two designs"
class Stream(width: Int <> CONST) extends Interface:
  protected val data  = Bits(width) <> VAR
  protected val valid = Bit         <> VAR
  protected val ready = Bit         <> VAR
  // the producer drives data+valid and reads ready
  val source = view.out(data, valid).in(ready)
  // the consumer side is simply the converse
  val sink   = source.flip
```

</div>

## What Interfaces Are For

Hardware protocols rarely consist of a single wire. A handshake carries `data`, `valid`,
and `ready`; an AXI channel carries a dozen signals that always travel together and always
have a fixed relative direction. Declaring and connecting those signals individually is
verbose and error-prone; a single flipped direction or forgotten port is a connectivity
bug.

Interfaces solve this by letting you:

* **Bundle related ports** into one named, parameterized unit that can be instantiated
  anywhere a child design can.
* **Declare directionality once** with a *view*, and obtain the opposite side for free with
  `flip` (so a `manager`/`subordinate` or `source`/`sink` pair is always consistent).
* **Connect an entire protocol with one `<>`**, with the connection checked port-by-port
  through the whole (possibly nested) hierarchy.
* **Map to native target constructs** (SystemVerilog `interface`/`modport` and VHDL-2019
  mode views) while remaining portable to every tool through an automatic flattening
  fallback (see [Generated HDL](#generated-hdl)).

Interfaces are the DFHDL analog of a SystemVerilog `interface` with `modport`s, or a
VHDL-2019 record with mode `view`s.

## Declaring an Interface

### Syntax {#interface-dcl-syntax}

An interface declaration follows standard [Scala class](https://docs.scala-lang.org/tour/classes.html){target="_blank"}
syntax and extends the `Interface` base class:

```scala linenums="0" title="Interface declaration syntax"
/** _documentation_ */
[_modifiers_] class _name_(_params_) extends Interface:
  protected val _port_ = _type_ <> VAR   // one or more ports
  val _view_ = view._dir_(_ports_)...    // one or more views
end _name_ //optional `end` marker
```

* __`_name_`__ - The Scala class name for the interface. As with designs, this name is
  preserved by the DFHDL compiler and used in generated artifacts (the SystemVerilog
  `interface` name / VHDL record name) and in error messages. See [naming][naming].

* __`(_params_)`__ - An optional parameter block, identical to a
  [design parameter block][design-params-syntax]. DFHDL parameters (`_type_ <> CONST`) are
  preserved through compilation and may shape port widths; pure Scala parameters are
  inlined during elaboration. Interface parameters must be `protected` (see below).

* __`_port_`__ - An interface port, declared just like a design [variable/port][Dcl] but
  always with the `<> VAR` modifier. Ports are undirected at declaration; a view assigns
  each port its direction. Ports must be `protected`.

* __`_view_`__ - A named view built with the `view` builder (see [Views](#views)).

Unlike a design, an interface is **domain-neutral**: there is a single `Interface` base
class rather than `DF`/`RT`/`ED` variants, and the same interface can be instantiated
inside a design of any [domain][design-domains].

### Ports and Parameters Must Be `protected` {#protected-rule}

Every port and DFHDL parameter of an interface must be declared `protected` (or
`private`). This is enforced by the DFHDL compiler plugin:

```scala linenums="0"
class BadIfc extends Interface:
  val data = Bits(8) <> VAR   // error: interface ports must be `protected`
```

```scala linenums="0"
class GoodIfc extends Interface:
  protected val data = Bits(8) <> VAR  // OK
  val mon = view.out(data)             // views stay public; they are the public surface
```

/// admonition | Why ports are protected
    type: note
A view, not a raw port, is the public surface of an interface. Ports are the internal
wiring, reachable only through a view's `.VIEW` projection. Marking them `protected` means
Scala's own access control (enforced by the typechecker) blocks any `ifc.data` selection
from outside the interface, and the ports are hidden from IDE autocomplete. This keeps every
connection going through a *directed* view, so the compiler can check directionality. The
rule applies transitively: ports declared in a subclass of an interface must be `protected`
too.
///

### Example: a parameterized handshake {#stream-example}
/// admonition | A reusable streaming handshake interface
    type: example
The `Stream` interface below bundles a `data` bus with a `valid`/`ready` handshake. The
`width` of the data bus is a DFHDL parameter, so it is preserved in the generated backend
code. Two views describe the two sides of the protocol: `source` (the producer) drives
`data` and `valid` and reads `ready`; `sink` (the consumer) is simply its converse.

```scala linenums="0"
class Stream(protected val width: Int <> CONST = 8) extends Interface:
  protected val data  = Bits(width) <> VAR
  protected val valid = Bit         <> VAR
  protected val ready = Bit         <> VAR
  val source = view.out(data, valid).in(ready)
  val sink   = source.flip
```
///

## Views

A *view* is a named, directional projection of an interface's ports. Each port that
participates in a view is given a direction with `view.in(...)` or `view.out(...)`, and an
interface may declare any number of views over the same ports (e.g. a producer view, a
consumer view, and a monitor view).

### Defining views {#defining-views}

Views are created with the `view` builder inside the interface body. Calls can be chained
to mix directions:

```scala linenums="0" title="View builder forms"
val v1 = view.out(a, b)          // a, b are outputs of the using design
val v2 = view.in(a, b, c)        // a, b, c are inputs of the using design
val v3 = view.in(a, b).out(c)    // mixed: a, b inputs; c output
```

The ports passed to `view.in`/`view.out` must be ports declared in *this* interface; the
builder reads them by reference. A view is itself a public `val`; it is the member you
reach through from outside the interface.

### Direction perspective {#direction-perspective}

A view's directions are written **from the point of view of the design that uses the
view**:

* `view.out(p)`: port `p` is an **output** of the using design (the design *drives* `p`).
* `view.in(p)`: port `p` is an **input** to the using design (the design *reads* `p`).

This is the same convention as a SystemVerilog `modport` (directions are from the consuming
module's viewpoint). It follows that the two ends of a connection must use *opposite*
directions for each shared port, which is exactly what `flip` produces.

### `flip`: the converse view {#flip}

`flip` returns the converse of a view: every `in` becomes `out` and every `out` becomes
`in`. It is the idiomatic way to declare the opposite side of a protocol without repeating
(and risking inconsistency in) the direction list:

```scala linenums="0"
val source = view.out(data, valid).in(ready)
val sink   = source.flip   // in(data, valid).out(ready)
```

`flip` is involutive (`source.flip.flip` is `source`) and maps directly onto the target
languages: a VHDL-2019 `'converse` alias, or an inverted SystemVerilog `modport`.

### The generic view {#generic-view}

Every interface instance also exposes a generic, undirected view through `.VIEW` directly
on the instance (without naming a declared view). The generic view carries all ports with
no enforced direction and is useful for internal wiring where directionality is not being
checked. Prefer a named, directed view for design boundaries.

## Using an Interface in a Design

### Instantiation {#instantiation}

An interface is instantiated inside a design exactly like a child design, by calling its
constructor. Empty parentheses are required even when there are no parameters:

```scala linenums="0"
class Producer extends RTDesign:
  val io = Stream(width = 8)   // an interface instance
  // ...
```

### Projecting a view: `instance.view.VIEW` {#projecting-a-view}

To obtain a connectable value, select a declared view on the instance and call `.VIEW`.
This *projects* the view onto that specific instance:

```scala linenums="0"
val src = io.source.VIEW   // the producer side of `io`
val snk = io.sink.VIEW     // the consumer side of `io`
```

Because the interface ports are `protected`, `io.data` is rejected by the compiler; all
access goes through a view's `.VIEW`.

### Connecting views with `<>` {#connecting-views}

Two views are connected with the [`<>` connection operator][connectivity], just like
scalar ports. The connection is checked **port-by-port through the entire (possibly nested)
hierarchy**: each shared port must have complementary directions on the two sides. Pairing
a view with its `flip` is therefore always a legal connection:

```scala linenums="0"
class Link extends RTDesign:
  val io   = Stream(8)
  val prod = Producer()
  val cons = Consumer()
  prod.out <> io.source.VIEW   // producer drives the source side
  cons.in  <> io.sink.VIEW     // consumer reads the sink side
```

A single `<>` here replaces one connection per port (`data`, `valid`, `ready`), and the
direction of every wire is guaranteed consistent by construction.

/// admonition | Interfaces are purely structural
    type: note
An interface body may contain only ports and views. Processes, `<>` connections, and `:=`
/`:==` assignments are **not** allowed inside an interface; these belong in designs.
Additionally, an interface must be a *named* class: anonymous instances such as
`new Stream() {}` are rejected by the compiler, because each interface needs a stable
identity to be emitted as a named SystemVerilog `interface` / VHDL record.
///

## Nested Interfaces {#nested-interfaces}

An interface may instantiate other interfaces and compose their views into a higher-level
view. This is how multi-channel protocols such as AXI are modeled: one interface per
channel, then a top interface that nests them and composes their per-channel views with the
`view(...)` apply form (which takes already-directed sub-views).

/// admonition | AXI4-Lite: a nested, multi-channel interface
    type: example
Each AXI4-Lite channel is its own interface with a `manager` view (directions written from
the manager/master point of view) and a `subordinate = manager.flip`. The top `Axi4Lite`
interface nests the five channels and composes their `manager` views into a single
`manager` view; `subordinate` flips recursively through every nested channel.

```scala linenums="0"
// One interface per AXI4-Lite channel (write-address channel shown).
class Axi4LiteAW(protected val addrWidth: Int <> CONST) extends Interface:
  protected val AWADDR  = UInt(addrWidth) <> VAR
  protected val AWPROT  = Bits(3)         <> VAR
  protected val AWVALID = Bit             <> VAR
  protected val AWREADY = Bit             <> VAR
  val manager     = view.out(AWADDR, AWPROT, AWVALID).in(AWREADY)
  val subordinate = manager.flip
// ... Axi4LiteW, Axi4LiteB, Axi4LiteAR, Axi4LiteR similarly ...

// Top interface nesting the five channels and composing their views.
class Axi4Lite(
    protected val addrWidth: Int <> CONST,
    protected val dataWidth: Int <> CONST
) extends Interface:
  protected val aw = Axi4LiteAW(addrWidth)
  protected val w  = Axi4LiteW(dataWidth)
  protected val b  = Axi4LiteB()
  protected val ar = Axi4LiteAR(addrWidth)
  protected val r  = Axi4LiteR(dataWidth)
  val manager     = view(aw.manager, w.manager, b.manager, ar.manager, r.manager)
  val subordinate = manager.flip   // flips recursively through every nested channel
```

A manager and a subordinate side then connect with a single `<>`:

```scala linenums="0"
val bus = Axi4Lite(addrWidth = 32, dataWidth = 32)
val mgr = bus.manager.VIEW
val sub = bus.subordinate.VIEW
mgr <> sub   // legal: matches port-per-port through all five nested channels
```
///

## Vector Views {#vector-views}

A view can be replicated into a vector with the `X` operator, mirroring DFHDL
[vector construction][type-system]. This is useful for interconnects, for example a
1-to-N crossbar that exposes N manager-side buses. Individual elements are selected by
index:

```scala linenums="0"
val managers = Axi4Lite(32, 32).manager.VIEW X 4   // a vector of 4 manager-side views
val m0 = managers(0)                               // select element 0
```

## How Interfaces Map to Generated HDL {#generated-hdl}

DFHDL emits interfaces in one of two ways, depending on the target dialect.

**Portable default (flattening).** By default, the compiler *flattens* interfaces and views
into plain scalar ports and per-port connections before backend emission. This produces
valid, synthesizable HDL on **every** supported tool, with no reliance on
`interface`/`modport` or mode-view support. Nested interfaces are expanded with prefixed
signal names (`aw_AWADDR`, …). The top-level design boundary is *always* flattened to scalar
ports, regardless of dialect.

**Native emission (opt-in).** For dialects that support them, interfaces can be emitted
natively, preserving the structure:

/// tab | SystemVerilog (SV2017/SV2019)
A DFHDL interface becomes a SystemVerilog `interface`, each view becomes a `modport`, and
`flip` becomes a `modport` with inverted directions:

```systemverilog linenums="0"
interface Axi4LiteAW #(parameter ADDR_WIDTH = 32) ();
  logic [ADDR_WIDTH-1:0] AWADDR;
  logic [2:0]            AWPROT;
  logic                  AWVALID;
  logic                  AWREADY;
  modport manager     (output AWADDR, AWPROT, AWVALID, input  AWREADY);
  modport subordinate (input  AWADDR, AWPROT, AWVALID, output AWREADY);
endinterface
```

> Nested interfaces are not portable for synthesis (e.g. Vivado does not support them), so
> they are flattened unless the target is known to accept them.
///

/// tab | VHDL-2019
A DFHDL interface becomes a `record` type, each view becomes a mode `view`, and `flip`
becomes a `'converse` alias:

```vhdl linenums="0"
type axi4lite_aw_t is record
  AWADDR  : unsigned;
  AWPROT  : std_logic_vector(2 downto 0);
  AWVALID : std_logic;
  AWREADY : std_logic;
end record;

view axi4lite_aw_manager of axi4lite_aw_t is
  AWADDR, AWPROT, AWVALID : out;
  AWREADY                 : in;
end view;
alias axi4lite_aw_subordinate is axi4lite_aw_manager'converse;
```

> VHDL-2019 mode views are currently supported mainly by NVC. The portable default
> therefore stays flattened for VHDL; native mode-view emission is opt-in.
///

## Restrictions Summary {#restrictions}

* Interface ports and DFHDL parameters must be `protected` (or `private`).
* An interface body may contain only ports and views: no processes, connections, or
  assignments.
* Interfaces must be named classes; anonymous instances (`new Ifc() {}`) are rejected.
* External access to ports is only through a view's `.VIEW` projection.
