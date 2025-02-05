---
typora-copy-images-to: ./
---
[](){#connectivity}
# Hierarchy and Connectivity

DFHDL supports composable design hierarchies by instantiating design classes and connecting ports.

<div class="grid" markdown>

/// admonition | terminology
    type: quote
* _design_ - a Scala class extending `XXDesign`, where `XX` can be `DF`, `RT`, or `ED`, corresponding to the desired [design domain][design-domains].
* _design member_ - any DFHDL object instantiated within a design (the design *contains* or *owns* all its members).
* _child design/component_ - a design instance that is owned by another design.
* _top design_ - the highest-level design in the hierarchy (no other design contains it), also known as the *top-level design*.
* _top-app design_ - a `@top` annotated *top design* that generates a main entry with the default application.

///

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = Top
    label = "top"
    parameters = [param]
    inPorts = [{id = i1, label = "in port"}]
    outPorts = [
      {id = o1, label = "out port"}
      {id = o2, label = " "}
      {id = o3, label = " "}
    ]
    children = [
      {
        id = const 
        constant = 1
      },
      { 
        id = A 
        label = "child A"
        highlight = 1
        parameters = [param1, param2]
        inPorts = [
          {id = i1, label = " "}
          {id = i2, label = " "}
        ]
        outPorts = [
          {id = o1, label = " "}
          {id = o2, label = " "}
        ]
        children = [
          {id = opA1, label = ""},
          {id = opA2, label = ""},
        ]
        edges = [
          [A.i1, opA1]
          [opA1, A.o1]
          [A.i2, opA2]
          [opA2, A.o2]
        ]
      },
      { 
        id = B
        label = "child B"
        highlight = 1
        inPorts = [{id = i1, label = " "}]
        outPorts = [{id = o1, label = " "}]
        children = [{id = opB1, label = ""}]
        edges = [
          [B.i1, opB1]
          [opB1, B.o1]
        ]
      },
      {id = opTop, label = ""}
    ]
    edges = [
      [Top.i1, A.i1]
      [A.o1, A.i2]
      [A.o1, opTop]
      [opTop, Top.o1]
      [A.o2, B.i1]
      [B.o1, Top.o2]
      [A.o2, Top.o3]
      [Top.param, A.param2]
      [const, A.param1]
    ]
  }
]
```
</div>

## Design Declaration

### Syntax {#design-dcl-syntax}

A DFHDL design declaration follows the standard [Scala class](https://docs.scala-lang.org/tour/classes.html){target="_blank"} syntax, with specialized handling by the DFHDL Scala compiler plugin under the hood.

```scala linenums="0" title="Design declaration syntax"
/** _documentation_ */
@top(genMain) //required only for top-level designs
class _name_(_params_) extends XXDesign:
  _contents_
end _name_ //optional `end` marker
```

* __`_name_`__ is the Scala class name reference for the design you declared. The DFHDL compiler preserves this class name and uses it in error messages and the final generated artifacts (e.g., Verilog modules or VHDL entities). See the [naming][naming] section for more details.
* __`(_params_)`__ is an optional parameter block. The parameter block can include either Scala parameters that are inlined for the design elaboration stage or DFHDL design parameters that are preserved through the design elaboration and compilation stages. If you do not need parameters, Scala syntax accepts both empty parentheses `()` and no parentheses. See [Parameter Block Syntax][design-params-syntax] for more information.
* __`_XXDesign_`__ is the class to extends depending on the desired [design domain][design-domains], where `XX` can be `DF` for dataflow, `RT` for register-transfer, or `ED` for event-driven.
* __`_contents_`__ are the design interface (ports/interfaces/domains) and functionality (variables, functions, child designs, processes, etc.), depending on the semantics of the selected design domain.
* __`@top(genMain)`__ is a special obligatory annotation for top-level designs (designs that are instantiated not within another design). The annotation has an optional `#!scala val genMain: Boolean = true` parameter. When `genMain = false`, all this annotation is doing is providing a default top-level context for the design (e.g., [implicit/given](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2){target="_blank"} compiler options). When `genMain = true`, the design becomes a top-app design where all design parameters must have default values, and a main Scala entry point in the name `top__name_` is generated (e.g., for a top-app design named `Foo`, the entry name is named `top_Foo`).
* __`_documentation_`__ is the design documentation in [Scaladoc format](https://docs.scala-lang.org/style/scaladoc.html){target="_blank"}. This documentation is a meta information that is preserved throughout the compilation process and finally generated as documentation for the generated backend code. 

/// admonition | Basic top-app design example: a two-bits left shifter
The DFHDL code below implements a two-bits left shifter design named `LeftShift2` under register-transfer (RT) domain semantics, as indicated by the class `LeftShift2` extending `RTDesign`. The design has one 8-bit input port and one 8-bit output port and implements the 2-bit leftshift functionality by applying it on the input and assigning it to the output.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo1/LeftShift2.scala:3"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = LeftShift2
    inPorts = [iBits]
    outPorts = [oBits]
    children = [{id = op, label = "<<"}]
    edges = [
      [LeftShift2.iBits, op]
      [op, LeftShift2.oBits]
    ]
  }
]
```

</div>

This design is also a top-app design, since it's annotated with `@top`. This means that we have an executable Scala program that compiles the design and generates a Verilog or VHDL backend code. The backend configuration option can be set via a CLI argument, or alternatively, be set via an implicit backend setting like in the code above. The `@top` annotation captures the [implicit/given](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2){target="_blank"} options within its scope and feeds them to the top-app CLI program as default to run when no CLI arguments are given.

/// tab | Generated Verilog
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo1.LeftShift2Spec/verilog.sv2009/hdl/LeftShift2.sv"
```
///

/// tab | Generated VHDL
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo1.LeftShift2Spec/vhdl.v2008/hdl/LeftShift2.vhd"
```
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo1/LeftShift2.scala:3"
```
///
///




### Parameter Block Syntax {#design-params-syntax}
Just like any Scala class parameter blocks, the DFHDL design accepts a sequence of comma-delimited parameter declarations.

```scala linenums="0" title="Design declaration parameter block syntax"
([_access_] _name_: _type_ [= _default_], ...)
```

* __`_type_`__ is either a pure Scala parameter type or a DFHDL parameter type in the form of `DFType <> CONST`.
    - Pure Scala parameters are completely transparent to the DFHDL compiler and are inlined during elaboration. Any type of pure Scala parameter is acceptable, except for top-app design parameters which are currently limited to `#!scala String`, `#!scala Boolean`, `#!scala Int`, and `#!scala Double` types.
    - DFHDL parameters are preserved throughout the compilation process and manifest as parameters in the generated backend code. Top-app design DFHDL parameters are currently limited to `Int <> CONST`, `Bit <> CONST`, and `Boolean <> CONST` types.
* __`_name_`__ is the Scala parameter name reference. The DFHDL compiler preserves this parameter name for DFHDL parameters types only. For the top-app command-line interface (CLI), these names are also preserved, so that the parameters can be listed and modified through the CLI.
* __`_default_`__ is the optional default value of the parameter. According to the Scala language rules, once a parameter has a default value defined, all parameters that follow it must also have default values defined. For top-app designs, all parameters must have default values.
* __`_access_`__ is the optional Scala parameter access modifier. By default a Scala class parameter access is `#!scala private val`. If the parameter affects the type of a public value (e.g., width of a DFHDL port) then the

/// admonition | Scala-parameterized top-app design example: a basic left shifter
The DFHDL code below implements a basic left shifter design named `LeftShiftBasic`. This design is similar to the earlier example of `LeftShift2` except here the design has the shift value as an input, and its input and output port widths are set according to the Scala parameter `width`.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo2/LeftShiftBasic.scala:6"
```

```hdelk width=90%
stroke-width = 0
children = [
  {
    id = top
    label = LeftShiftBasic
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [{id = op, label = "<<"}]
    edges = [
      [top.shift, op]
      [top.iBits, op]
      [op, top.oBits]
    ]
  }
]
```

</div>

/// tab | Generated Verilog
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo2.LeftShiftBasicSpec/verilog.sv2009/hdl/LeftShiftBasic.sv"
```
///

/// tab | Generated VHDL
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo2.LeftShiftBasicSpec/vhdl.v2008/hdl/LeftShiftBasic.vhd"
```
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo2/LeftShiftBasic.scala:3"
```
///
///

/// admonition | DFHDL-parameterized top-app design example: a generic left shifter
The DFHDL code below implements a generic left shifter design named `LeftShiftGen`. This design is similar to the earlier example of `LeftShiftBasic` except here the `width` parameter is now a DFHDL parameter as indicated by its `Int <> CONST` type. This enables the DFHDL compiler to preserve the parameter name and directly use it in the generated backend code where applicable.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo3/LeftShiftGen.scala:6"
```

```hdelk width=90%
stroke-width = 0
children = [
  {
    id = top
    label = LeftShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [{id = op, label = "<<"}]
    edges = [
      [top.shift, op]
      [top.iBits, op]
      [op, top.oBits]
    ]
  }
]
```

</div>

/// tab | Generated Verilog
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo3.LeftShiftGenSpec/verilog.sv2009/hdl/LeftShiftGen.sv"
```
///

/// tab | Generated VHDL
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo3.LeftShiftGenSpec/vhdl.v2008/hdl/LeftShiftGen.vhd"
```
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo3/LeftShiftGen.scala:3"
```
///
///


### Rules {#design-dcl-rules}

#### Design class inheritance
It is possible to leverage the power of Scala inheritance to share design functionality between design class declarations.

/// admonition | Design class inheritance example: left and right shifters
The DFHDL code below implements a generic left shifter design named `LeftShiftGen`. This design is similar to the earlier example of `LeftShiftBasic` except here the `width` parameter is now a DFHDL parameter as indicated by its `Int <> CONST` type. This enables the DFHDL compiler to preserve the parameter name and directly use it in the generated backend code where applicable.

<div class="grid" markdown>

```scala
/** A generic abstract shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
abstract class ShiftGen(
    val width: Int <> CONST = 8,
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
```

```hdelk width=90%
stroke-width = 0
children = [
  {
    id = top
    label = ShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [{id = op, label = "<<"}]
    edges = [
      [top.shift, op]
      [top.iBits, op]
      [op, top.oBits]
    ]
  }
]
```

</div>

/// tab | Generated Verilog

```verilog
/* A generic left shifter 
     
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LeftShiftGen_defs.svh"

module LeftShiftGen#(parameter int width = 8)(
  /* bits input */
  input  wire logic [width - 1:0]         iBits,
  /* requested shift */
  input  wire logic [$clog2(width) - 1:0] shift,
  /* bits output */
  output      logic [width - 1:0]         oBits
);
  `include "dfhdl_defs.svh"
  assign oBits = iBits << shift;
endmodule
```
///

/// tab | Generated VHDL
```vhdl
-- A generic left shifter 
--   
-- @param width
--   the width of the input and output bits
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LeftShiftGen_pkg.all;

entity LeftShiftGen is
generic (
  width : integer := 8
);
port (
  -- bits input 
  iBits : in  std_logic_vector(width - 1 downto 0);
  -- requested shift 
  shift : in  unsigned(clog2(width) - 1 downto 0);
  -- bits output 
  oBits : out std_logic_vector(width - 1 downto 0)
);
end LeftShiftGen;

architecture LeftShiftGen_arch of LeftShiftGen is
begin
  oBits <= slv_sll(iBits, to_integer(shift));
end LeftShiftGen_arch;
```
///

/// details | Runnable example
    type: dfhdl
```scastie
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog
/** A generic abstract shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
abstract class ShiftGen(
    val width: Int <> CONST = 8,
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
```
///
///

#### Design class modifier limitations
A DFHDL design class cannot be declared as `#!scala final class` or `#!scala case class`. Attempting to do so produces an error:
```scala title="DFHDL design class modifier limitation example"
//error: DFHDL classes cannot be final classes.
final class Foo extends DFDesign
//error: DFHDL classes cannot be case classes.
case class Bar() extends DFDesign
```
All other Scala class modifiers have no special effect or limitation from a DFHDL compiler perspective. Nonetheless, these modifiers can be relevant when defining a more complex design API, as part of the DFHDL meta-programming capabilities through the Scala language (e.g., change class access to `#!scala protected`).

#### Design parameter limitations

#### Top-app design parameter type limitations


## Design Instantiation


## Key Differences Between `<>` and `:=`/`:==`

| Criteria                            | `<>` Connection                                              | `:=`/`:==` Assignment                                              |
| ----------------------------------- | :----------------------------------------------------------- | :----------------------------------------------------------- |
| Directionality &<br />Commutativity | The operator is commutative, meaning `a <> b` is equivalent to b `b <> a`.  One argument is the *producer*, while the other *consumer*. The dataflow direction is sensitive to the context in which the operator is applied. | The operator is non-commutative, meaning `a := b` determines that `b` is the *producer*, transferring data to the *consumer* `a`. |
| Mutation                            | A consumer can only be connected once.                       | Consumer assignments are unlimited.                          |
| Statement Order                     | Connections statements can be placed in any order.           | Assignment statements                                        |

##Connection `<>` Rules

###Port Direct Connections

The onnection operator `<>` is generally used to connect parent designs to their child designs (components) and connect between sibling designs (children of the same parent). Opposed to VHDL/Verilog, there is no need to go through 'signals' to connect sibling design ports, e.g.:

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

###Port Via Connections

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
  p1A_x    <> x
  p1B_x    <> p1A_y
  y        <> p1B_y
end Plus2
```

/// details | Runnable example
    type: dfhdl

```scastie
import dfhdl.* 

class Plus1 extends DFDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1

@top class Plus2 extends DFDesign:
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



###Dataflow Value Connections

At least one of the connected sides must be a dataflow port (cannot connect two dataflow values together), e.g.:

```scala
trait Conn1 {
  val port = DFUInt(8) <> OUT
  val temp1 = DFUInt(8)
  val temp2 = DFUInt(8)
  port <> temp1 //OK!
  temp1 <> temp2 //Bad connection! At least one connection side must be a port
}
```



### Dataflow Input Port Assignment `:=` Rule

An input port cannot be assigned to. A connection must be used to transfer data to an input port, e.g.:

```scala
trait IO extends DFDesign {
  val in  = DFUInt(8) <> IN
  val out = DFUInt(8) <> OUT
  out := in //OK! Can assign internally to an output port
}
trait Assign1 extends DFDesign {
  val io = new IO{}
  io.in := 1 //Bad assignment! Must use a connection annotation
  io.in <> 1 //OK!
  io.out := 1 //Bad assignment! Output ports can only be assigned internally
}
```



###Immutable Value Connections

When connecting a port to an immutable value, the port must be a consumer, meaning the connection is done internally to an output port or externally to an input port, e.g.:

```scala
trait IO extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  //For brevity, we consider every connection/assignment in this example separately.
  //We ignore multiple connection issues that should arise.
  o <> 1 //OK!
  i <> 1 //Bad connection! 1 is immutable (constant)
  i <> o.prev //Bad connection! o.prev is immutable
  i.prev <> o //OK!
}
trait IOUser extends DFDesign {
  val io = new IO {}
  io.i <> 1 //OK!
  io.o <> 1 //Bad connection! 1 is immutable
}
```

 <div style="page-break-after: always;"></div>
### Different Type Connections

Connecting between different types is possible, but depends on the specific type: if it enables automatic conversion for the connection to succeed. Different port widths are considered different types and casting is required. An alias/casted/converted dataflow value is considered immutable for the connection (see above). Here are some examples:

```scala
trait DifferentTypesConn extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val ob9 = DFBits(9) <> OUT
  
  val u7 = DFUInt(7)
  val u9 = DFUInt(9)
  val b8 = DFBits(8)
  
  //For brevity, we consider every connection/assignment in this example separately.
  //We ignore multiple connection issues that should arise.
  u7 <> o //OK! u7 is automatically extended to connect to 
  u7 <> i //Bad connection! u7 is considered immutable when extended to 8 bits
  o <> b8 //Bad connection! There is not automatic casting between bits and uint
  o <> b8.uint //OK!
  o.bits <> b8 //Bad connection! An alias of output port cannot be connected to
               //This may change in the future.
  o.bits := b8 //OK!
  u9 <> i //OK! In this example u9 is the consumer
  ob9 <> b8 //Bad connection! Bit vectors are NOT automatically extended.
  ob9 := b8 //Bad assignment! Bit vectors are NOT automatically extended.
}
```

 <div style="page-break-after: always;"></div>
### Multiple Connections

Two or more dataflow producers cannot be connected to the same consumer (a single producer can be connected to more than one consumer), e.g.:

```scala
trait Gen extends DFDesign {
  val out1 = DFUInt(8) <> OUT init 1
  val out2 = DFUInt(8) <> OUT init 2
}
trait Conn2 extends DFDesign {
  val in1 = DFUInt(8) <> IN
  val in2 = DFUInt(8) <> IN
  val out = DFUInt(8) <> OUT
  val temp1 = DFUInt(8)
  temp1 <> in1 //OK!
  out   <> in1 //Also OK! (Same producer can connect to more than one cosumer)
  temp1 <> in2 //Bad connection! Second producer connection to temp1
  
  val gen = new Gen {}
  val temp2 = DFUInt(8)
  val temp3 = DFUInt(8)
  gen.out1 <> temp2 //OK!
  gen.out1 <> temp3 //Also OK! (Same producer can connect to more than one cosumer)
  gen.out2 <> temp2 //Bad connection! Second producer connection to temp2
} 
```



###Mixing Assignments and Connections 

The same consumer cannot be both assigned to and connected to as the consumer, e.g.:

```scala
trait Conn3 extends DFDesign {
  val out1 = DFUInt(8) <> OUT
  val out2 = DFUInt(8) <> OUT
  val out3 = DFUInt(8) <> OUT
  out1 <> 1 //OK!
  out1 := 1 //Bad assignment! Cannot assign to a connected dataflow variable

  out2 := 2 //OK!
  out2 <> 2 //Bad connection! Cannot connect to an assigned dataflow variable

  out3 := 1 //OK!
  out3 := 2 //Also OK! (Multiple assignments are accepted)
}
```



### Connection Statement Order

The connection `<>` statement ordering does not matter.

 <div style="page-break-after: always;"></div>
### Connection and Initial Conditions

A connection `<>` transfers initial conditions to the consumer, but if the consumer is already initialized then the consumer keeps its existing initial conditions. Here is an example:

```scala
trait IOInit extends DFDesign {
  val i = DFUInt(8)        //init = (11, 12) Overriden from TopInit connection
  val o = DFUInt(8) init 5 //init = (5)      Not overridden due to assignment
  val ip = i.prev          //init = (12)     Prev moves down the init queue
  o := ip 
}
trait TopInit extends DFDesign {
  val i = DFUInt(8) <> IN.init(1, 2)  //init = (1, 2)   The top-level initial conditions
  val o = DFUInt(8) <> OUT init 1     //init = (1)      Keeps its initializaion
  val iPlus10 = in + 10               //init = (11, 12) Arithmetics affect init
  val io = new IOInit {}
  io.i <> inPlus10  										
  o <> io.o											
}
```

![1541618117728](1541618117728.png)

We learn from the above that port initial conditions are often overridden due to connections. So why should we apply initial conditions to a port? Answer: If we want to define what happens when a port is open (unconnected). Read the next two sections for more information.

 <div style="page-break-after: always;"></div>
###Open (Unconnected) Ports

Ports have two connection sides: a consumer side and a producer side. Typically ports have both sides connected, except for top-level ports. When either port side is unconnected, we refer to it as *open*, and expect the following behavior:

* When the port consumer side is open, the port produces tokens according to its initial condition. Uninitialized open-consumer ports generate bubble tokens.

* When the port producer side is open (unless it is a top-level output port), the port is considered as not used, and is pruned during compilation. All dataflow streams that are only used by this port will be pruned as well.

**Note**: the current compiler implementation does not warn of open ports.  

Example:

```scala
trait IOInit2 extends DFDesign {
  val i1 = DFUInt(8) <> IN init 5
  val o1 = DFUInt(8) <> OUT
  val i2 = DFUInt(8) <> IN
  val o2 = DFUInt(8) <> OUT init 2
  o1 <> i1 
}
trait TopIO2 extends DFDesign {
  val i = DFUInt(8) <> IN  
  val o = DFUInt(8) <> OUT //Will generate infinite tokens of 2, due to io.o2 init
  val io = new IO5 {}
  o <> io.o2
  i <> io.i1
  io.i2 <> 5
}
```

![1541633749924](1541633749924.png)

 <div style="page-break-after: always;"></div>
### Initial Condition Cyclic Loop Errors

Connections enable dataflow feedbacks and even dataflow dependency loops. There is no problem in dependency loops, other than pipelining limitations (see chapter TBD for more information). However, if we only apply connections and references that transfer initial conditions, we end up with a cyclic dependency for initial condition which is illegal. Therefore to enable dependency loops, at least one link in the loop must be an assignment, which has an implicit state and does not affect initial conditions. Consider the following examples:

```scala
trait IO1 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  o <> i //Connection transfers initial conditions from i to o
}
trait BadConnLoop1 extends DFDesign {
  val o = DFUInt(8) <> OUT
  val io = new IO1 {}
  io.i <> io.o //Bad connection! An initial conditions cyclic loop
  o  <> io.o
}
trait IO2 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  o <> i.prev //prev transfers initial conditions
}
trait BadConnLoop2 extends DFDesign {
  val o = DFUInt(8) <> OUT
  val io = new IO2 {}
  io.i <> io.o //Bad connection! An initial conditions cyclic loop
  o  <> io.o
}
trait IO3 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  o := i //Assignment does not affect initial conditions and therefore breaks the loop
}
trait OKConnLoop extends DFDesign {
  val o = DFUInt(8) <> OUT
  val io = new IO3 {}
  io.i <> io.o //OK!
  o  <> io.o
}
```

![1541525350659](1541603175175.png)

**Note**: when following the drawing convention within this document, we want to avoid a double-lined loop in order to avoid a cyclic initial conditions dependency.

 <div style="page-break-after: always;"></div>
##Valid Connection and Assignment Examples

```scala
trait IODesign extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  o <> i
}
```

![1531312715988](1541501963343.png)

---

```scala
trait IODesign1 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val tmp = DFUInt(8)
  tmp <> i
  o <> tmp
}
```

![1531313031884](1541502005694.png)

---

```scala
trait IODesign2 extends DFDesign {
  val i1 = DFUInt(8) <> IN
  val o1 = DFUInt(8) <> OUT
  val i2 = DFUInt(8) <> IN
  val o2 = DFUInt(8) <> OUT
  o1 <> i1
  o2 <> i2
}
```

![1531313204197](1541501792059.png)

---

```scala
trait Container extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesign {}
  i    <> io.i //Connecting between owner input and child input
  io.o <> o    //Connecting between child output and owner output
}
```

![1531313619621](1541502049075.png)

---

```scala
trait Container2 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io1 = new IODesign {}
  val io2 = new IODesign {}
  i     <> io1.i //Connecting between owner input and child input
  io1.o <> io2.i //Connecting between siblings (output <> input)
  io2.o <> o     //Connecting between child output and owner output
}
```

![1531314589019](1541502098181.png)

---

```scala
trait Container3 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesign2 {}
  i <> io.i1 //Connecting between owner input and child input
  i <> io.i2 //Connecting between owner input and child input
  o <> (io.o1 + io.o2)
}
```

![1531322811065](1541502127823.png)

---

```scala
trait Container4 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new IODesign2 {}
  i     <> io.i1 //Connecting between owner input and child input
  io.i2 <> 5     //Connecting between constant value and child input
  o     <> io.o2
}
```

![1531344446287](1541502151965.png)

---

```scala
trait Blank2 extends DFDesign {
  val i1 = DFUInt(8) <> IN
  val o1 = DFUInt(8) <> OUT
  val i2 = DFUInt(8) <> IN
  val o2 = DFUInt(8) <> OUT    
}
trait Container5 extends DFDesign {
  val i = DFUInt(8) <> IN
  val o = DFUInt(8) <> OUT
  val io = new Blank2 {
    o1 <> i1 //Assignment
    o2 <> i2 //Internal connection   
  }
  i     <> io.i1 //Connecting between owner input and child input
  io.i2 <> io.o1 //External connection between child input/output creates a feeback
  o     <> io.o2
}
```

![1531345077704](1541609861696.png)

Note: although there is a feedback in this design, there is no circular initial conditions dependency.

---

## Via Connections


## Magnet Port Connections

## Future Work

* In the future `<>` will be used to connect multi-port interfaces.
* We will add support to treat an alias of a port as a port when connection `<>` rules are enforced.
* Connecting between any ancestor which is not a parent and child. Currently not supported fully.