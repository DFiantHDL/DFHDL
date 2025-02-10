---
typora-copy-images-to: ./
---
# Design Hierarchy

DFHDL supports composable design hierarchies by instantiating design classes and connecting their ports.

<div class="grid" markdown>

/// admonition | Terminology
    type: quote
* _design_ - A Scala class extending `XXDesign`, where `XX` can be `DF`, `RT`, or `ED`, corresponding to the desired [design domain][design-domains].
* _design member_ - Any DFHDL object instantiated within a design (the design *contains* or *owns* all its members).
* _child design/component_ - A design instance that is owned by another design.
* _top design_ - The highest-level design in the hierarchy (no other design contains it), also known as the *top-level design*.
* _top-app design_ - A `@top` annotated *top design* that generates a main entry with the default application.

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
[_modifiers_] class _name_(_params_) extends XXDesign:
  _contents_
end _name_ //optional `end` marker
```

* __`_name_`__ is the Scala class name reference for the design you declared. The DFHDL compiler preserves this class name and uses it in error messages and the final generated artifacts (e.g., Verilog modules or VHDL entities). See the [naming][naming] section for more details.
* __`(_params_)`__ is an optional parameter block. The parameter block can include either Scala parameters that are inlined for the design elaboration stage or DFHDL design parameters that are preserved through the design elaboration and compilation stages. If you do not need parameters, Scala syntax accepts both empty parentheses `()` and no parentheses. See [Parameter Block Syntax][design-params-syntax] for more information.
* __`_XXDesign_`__ is the class to extend depending on the desired [design domain][design-domains], where `XX` can be `DF` for dataflow, `RT` for register-transfer, or `ED` for event-driven.
* __`_contents_`__ are the design interface (ports/interfaces/domains) and functionality (variables, functions, child designs, processes, etc.), depending on the semantics of the selected design domain.
* __`@top(genMain)`__ is a special obligatory annotation for top-level designs (designs that are not instantiated within another design). The annotation has an optional `#!scala val genMain: Boolean = true` parameter. When `genMain = false`, all this annotation does is provide a default top-level context for the design (e.g., [implicit/given](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2){target="_blank"} compiler options). When `genMain = true`, the design becomes a top-app design where all design parameters must have default values, and a main Scala entry point named `top__name_` is generated (e.g., for a top-app design named `Foo`, the entry point is named `top_Foo`).
* __`_documentation_`__ is the design documentation in [Scaladoc format](https://docs.scala-lang.org/style/scaladoc.html){target="_blank"}. This documentation is meta information that is preserved throughout the compilation process and finally generated as documentation for the generated backend code.
* __`_modifiers_`__ are optional Scala modifiers. See the [Design Class Modifier Rules][design-class-modifier-rules] section for more information.

/// admonition | Basic top-app design example: a two-bits left shifter
    type: example
The DFHDL code below implements a two-bit left shifter design named `LeftShift2` under register-transfer (RT) domain semantics, as indicated by the class `LeftShift2` extending `RTDesign`. The design has one 8-bit input port and one 8-bit output port and implements the 2-bit left shift functionality by applying it to the input and assigning it to the output.

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
    children = [
      {
        id = const
        label = 2 
        constant = 1
      }
      {
        id = op 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [const, op.shift]
      [LeftShift2.iBits, op]
      [op, LeftShift2.oBits]
    ]
  }
]
```

</div>

This design is also a top-app design, since it's annotated with `@top`. This means that we have an executable Scala program that compiles the design and generates Verilog or VHDL backend code. The backend configuration option can be set via a CLI argument, or alternatively, be set via an implicit backend setting like in the code above. The `@top` annotation captures the [implicit/given](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2){target="_blank"} options within its scope and feeds them to the top-app CLI program as defaults to run when no CLI arguments are given.

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
    - Pure Scala parameters are completely transparent to the DFHDL compiler and are inlined during elaboration. 
    - DFHDL parameters are preserved throughout the compilation process and manifest as parameters in the generated backend code.
    - See the [Design Parameter Type Rules][design-parameter-type-rules] section for more information.
* __`_name_`__ is the Scala parameter name reference. The DFHDL compiler preserves this parameter name for DFHDL parameter types only. For the top-app command-line interface (CLI), these names are also preserved, so that the parameters can be listed and modified through the CLI.
* __`_default_`__ is the optional default value of the parameter. See the [Design Parameter Default Value Rules][design-parameter-default-value-rules] section for more information.
* __`_access_`__ is the optional Scala parameter access modifier. Usually, you should add the `#!scala val` keyword modifier to make the parameter public. See the [Design Parameter Access Rules][design-parameter-access-rules] section for more information.

/// admonition | Scala-parameterized top-app design example: a basic left shifter
    type: example
The DFHDL code below implements a basic left shifter design named `LeftShiftBasic`. This design is similar to the earlier example of `LeftShift2` except here the design has the shift value as an input, and its input and output port widths are set according to the Scala parameter `width`.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo2/LeftShiftBasic.scala:6"
```

```hdelk width=75%
stroke-width = 0
children = [
  {
    id = top
    label = LeftShiftBasic
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = op 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]    
    edges = [
      [top.shift, op.shift]
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

The basic code shifter above did not generate the `width` parameter in the Verilog and VHDL backend code. The following example shows how to preserve the `width` parameter:

/// admonition | DFHDL-parameterized top-app design example: a generic left shifter
    type: example
The DFHDL code below implements a generic left shifter design named `LeftShiftGen`. This design is similar to the earlier example of `LeftShiftBasic`, except here the `width` parameter is now a DFHDL parameter, as indicated by its `Int <> CONST` type. This enables the DFHDL compiler to preserve the parameter name and directly use it in the generated backend code where applicable.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo3/LeftShiftGen.scala:6"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = top
    label = LeftShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = op 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [top.shift, op.shift]
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

#### Design Parameter Type Rules
- Any pure Scala parameter or DFHDL parameter types are acceptable.
- Top-app design parameters, to be modifiable from the CLI, must be one of the following types:
    - Pure Scala Types: `#!scala String`, `#!scala Boolean`, `#!scala Int`, and `#!scala Double`.
    - DFHDL Types: `#!scala Int <> CONST`, `#!scala Bit <> CONST`, and `#!scala Boolean <> CONST`.

/// admonition | Top-app design with accepted and ignored arguments example
    type: example
In this example, the top-app supported parameters `pureIntArg` and `dfhdlIntArg` are preserved to be modifiable from the CLI, whereas `ignored` and `dfhdlIgnored` are ignored in the CLI and will keep their default values.
```scala title="DFHDL code"
import dfhdl.*
class CustomArg
@top class Foo(
    val pureIntArg:   Int              = 5,
    val dfhdlIntArg:  Int <> CONST     = 7,
    val ignored:      CustomArg        = CustomArg(),
    val dfhdlIgnored: Bits[8] <> CONST = all(0)
) extends DFDesign
```
``` title="CLI output, when running via sbt (truncated)"
Design Name: Foo
Usage: sbt runMain "top_Foo [design-args] <mode> [options]"
 Design arguments:
      --pureIntArg  <Int>    (default = 5)
      --dfhdlIntArg  <Int>   (default = 7)
```

/// details | Runnable example
    type: dfhdl
```scastie
import dfhdl.*
//this option forces the top-app
//to run help mode by default
given options.AppOptions.DefaultMode = 
  options.AppOptions.DefaultMode.help
class CustomArg
@top class Foo(
    val pureIntArg:   Int              = 5,
    val dfhdlIntArg:  Int <> CONST     = 7,
    val ignored:      CustomArg        = CustomArg(),
    val dfhdlIgnored: Bits[8] <> CONST = all(0)
) extends DFDesign
```
///
///

#### Design Parameter Default Value Rules
For top-app designs, all parameters must have default values.
/// admonition | `@top` annotation and default parameter value requirement example
    type: example
This example shows `FooErr` is missing a default value and throws an error. There are three ways to overcome this error, as shown in `FooOK1`, `FooOK2`, and `FooOK3`.
```scala
//Error: Missing argument's default value for top-level design with a default app entry point.
//Either add a default value or disable the app entry point generation with `@top(false)`.
@top class FooErr(
    val arg1: Int = 5,
    val arg2: Boolean <> CONST
) extends DFDesign

//OK: all parameters have default values
//Top-app capability: YES
//Top-level design capability: YES
@top class FooOK1(
    val arg1: Int = 5,
    val arg2: Boolean <> CONST = true
) extends DFDesign

//OK: the `genMain` argument in the `@top` annotation is set to `false`
//Top-app capability: NO
//Top-level design capability: YES
@top(false) class FooOK2(
    val arg1: Int = 5,
    val arg2: Boolean <> CONST
) extends DFDesign

//OK: no top annotation
//Top-app capability: NO
//Top-level design capability: NO
class FooOK3(
    val arg1: Int = 5,
    val arg2: Boolean <> CONST
) extends DFDesign
```
///

/// admonition | Good design practice - avoid default parameter value overuse
    type: tip
Overusing default parameter values is considered bad design practice. In general, default values should be used sparingly and only to define "sensible defaults" for parameters that are rarely changed. A good rule of thumb is to *avoid* default values that affect a design's interface (e.g., the width of a port).
///

#### Design Parameter Access Rules
By default, a Scala class parameter access is `#!scala private val`.
, and in many cases it is required to explicitly define the parameter to be public by using the `#!scala val` keyword modifier.

### Design Class Modifier Rules
A DFHDL design class cannot be declared as `#!scala final class` or `#!scala case class`. Attempting to do so produces an error:
```scala title="DFHDL design class modifier limitation example"
//error: DFHDL classes cannot be final classes.
final class Foo extends DFDesign
//error: DFHDL classes cannot be case classes.
case class Bar() extends DFDesign
```
All other Scala class modifiers have no special effect or limitation from a DFHDL compiler perspective. Nonetheless, these modifiers can be relevant when defining a more complex design API, as part of the DFHDL meta-programming capabilities through the Scala language (e.g., changing class access to `#!scala protected`).

### Design Class Inheritance
It is possible to leverage the power of Scala inheritance to share design functionality between design class declarations.

/// admonition | Generic left and right shifters, design class inheritance example
    type: example
The DFHDL code below demonstrates how to implement both left and right generic shifters efficiently by using a common `#!scala abstract class` named `ShiftGen`. The `width` parameter is declared as an abstract class field (without an assigned value) inside the `ShiftGen` class body. By extending `ShiftGen`, both `LeftShiftGen` and `RightShiftGen` can utilize the IOs already declared in `ShiftGen`. They only need to explicitly declare the `width` parameter and implement the shift functionality in their respective class bodies.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo5/LRShiftDirect.scala:6:35"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = left
    label = LeftShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = opLeft 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [left.shift, opLeft.shift]
      [left.iBits, opLeft]
      [opLeft, left.oBits]
    ]
  },
  {
    id = right
    label = RightShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = opRight 
        label = ">>"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [right.shift, opRight.shift]
      [right.iBits, opRight]
      [opRight, right.oBits]
    ]
  }
]
```

</div>
///



## Design Composition & Instantiation
DFHDL supports three mechanisms to form a design hierarchy through design instantiation and composition:

  * [Direct Connection Composition][direct-connection-composition] - The most common and recommended mechanism to construct complex design hierarchies with multiple inputs and outputs. Within this mechanism, the design instantiation and port connection can be executed separately. This enables child design ports to be referenced without declaring and connecting intermediate variables.
  * [Via Connection Composition][via-connection-composition] - A legacy mechanism to connect ports only within a design instantiation. This mechanism mainly exists for coexistence with the Verilog module instancing and VHDL component instancing mechanisms. The DFHDL compiler automatically transforms a direct connection composition into a via connection composition.
  * [Functional Composition][functional-composition] - A method call mechanism to describe design composition. This mechanism is reserved for dataflow designs only and is mostly relevant for arithmetic/logic design functionality that has a single output port. The DFHDL compiler automatically transforms a functional composition into direct design composition.

The following subsections dive into further details of the three design composition mechanisms. For this purpose, we continue with our running example of a bit shifter. To demonstrate composition, let's first describe a more complex shifter that has both left and right shift capabilities, as a flat (composition-less) design:

/// admonition | Generic left-right shifter, flat design example
    type: example
The DFHDL code below implements a generic left-right shifter flat design named `LRShiftFlat`. This design expands on `LeftShiftGen` by adding a `dir` enum port value that specifies the shift direction and a shift operation multiplexer through a `#!scala match` statement.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo4/LRShiftFlat.scala:6"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = top
    label = LRShiftFlat
    inPorts = [iBits, shift, dir]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = opL 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
      {
        id = opR
        label = ">>"
        southPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
      {
        id = mux 
        label = "mux"
        northPorts = [{id = sel, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [top.shift, opR.shift]
      [top.shift, opL.shift]
      [top.iBits, opR]
      [top.iBits, opL]
      [top.dir, mux.sel]
      [opL, mux]
      [opR, mux]
      [mux, top.oBits]
    ]
  }
]
```

</div>

/// tab | Generated Verilog
/// tab | LRShiftFlat.sv
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo4.LRShiftFlatSpec/verilog.sv2009/hdl/LRShiftFlat.sv"
```
///
/// tab | LRShiftFlat_defs.svh
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo4.LRShiftFlatSpec/verilog.sv2009/hdl/LRShiftFlat_defs.svh"
```
///
///

/// tab | Generated VHDL
/// tab | LRShiftFlat.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo4.LRShiftFlatSpec/vhdl.v2008/hdl/LRShiftFlat.vhd"
```
///
/// tab | LRShiftFlat_pkg.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo4.LRShiftFlatSpec/vhdl.v2008/hdl/LRShiftFlat_pkg.vhd"
```
///
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo4/LRShiftFlat.scala:3"
```
///
///

### Direct Connection Composition
/// admonition | Generic left-right shifter, direct connection composed design example
    type: example
The DFHDL code below implements a generic left-right shifter composed (hierarchical) design named `LRShiftDirect`. This design implements the exact same functionality as seen earlier in `LeftShiftFlat`, but this time leveraging design composition and direct connectivity capabilities of DFHDL by splitting the left and right shift operations into their own separate designs named `LeftShiftGen` and `RightShiftGen`, respectively. Additionally, as shown in the `ShiftGen` example, we use design class inheritance to avoid redefining the same IOs across the three design classes.

/// admonition
    type: note
This example is only meant to illustrate direct composition. 
For simpler designs, a flat approach is often preferred. 
However, for complex designs, splitting them into sub-components promotes reuse, simplifies verification, and upholds best design practices.
///

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo5/LRShiftDirect.scala:6"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = left
    label = LeftShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = opLeft 
        label = "<<"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [left.shift, opLeft.shift]
      [left.iBits, opLeft]
      [opLeft, left.oBits]
    ]
  },
  {
    id = right
    label = RightShiftGen
    inPorts = [iBits, shift]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = opRight 
        label = ">>"
        northPorts = [{id = shift, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [right.shift, opRight.shift]
      [right.iBits, opRight]
      [opRight, right.oBits]
    ]
  },
  {
    id = lr
    label = LRShiftDirect
    inPorts = [iBits, shift, dir]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = lshifter
        type = LeftShiftGen
        inPorts = [iBits, shift]
        outPorts = [oBits]
        parameters = [width]
        highlight = 1
      },
      {
        id = rshifter
        type = RightShiftGen
        inPorts = [iBits, shift]
        outPorts = [oBits]
        parameters = [width]
        highlight = 1
      },
      {
        id = mux 
        northPorts = [{id = sel, label = " ", height = 5, width = 1}]
      }
    ]
    edges = [
      [lshifter.oBits, mux]
      [rshifter.oBits, mux]
      [lr.dir, mux.sel]
      [mux, lr.oBits]
      [lr.shift, lshifter.shift]
      [lr.shift, rshifter.shift]
      [lr.iBits, lshifter.iBits]
      [lr.iBits, rshifter.iBits]
      [lr.width, lshifter.width]
      [lr.width, rshifter.width]
    ]
  }]
```

</div>

/// tab | Generated Verilog
/// tab | LRShiftDirect.sv
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/verilog.sv2009/hdl/LRShiftDirect.sv"
```
///
/// tab | LeftShiftGen.sv
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/verilog.sv2009/hdl/LeftShiftGen.sv"
```
///
/// tab | RightShiftGen.sv
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/verilog.sv2009/hdl/RightShiftGen.sv"
```
///
/// tab | LRShiftDirect_defs.svh
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/verilog.sv2009/hdl/LRShiftDirect_defs.svh"
```
///
///

/// tab | Generated VHDL
/// tab | LRShiftDirect.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/vhdl.v2008/hdl/LRShiftDirect.vhd"
```
///
/// tab | LeftShiftGen.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/vhdl.v2008/hdl/LeftShiftGen.vhd"
```
///
/// tab | RightShiftGen.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/vhdl.v2008/hdl/RightShiftGen.vhd"
```
///
/// tab | LRShiftDirect_pkg.vhd
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo5.LRShiftDirectSpec/vhdl.v2008/hdl/LRShiftDirect_pkg.vhd"
```
///
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo5/LRShiftDirect.scala:3"
```
///
///

### Via Connection Composition

### Functional Composition
