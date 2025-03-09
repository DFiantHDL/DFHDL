---
typora-copy-images-to: ./
---
# Design Hierarchy

DFHDL supports composable design hierarchies through design class instantiation and port connections.

<div class="grid" markdown>

/// admonition | Terminology
    type: quote
* _design_ - A Scala class extending `XXDesign`, where `XX` can be `DF`, `RT`, or `ED`, corresponding to the desired [design domain][design-domains].
* _design member_ - Any DFHDL object instantiated within a design (the design *contains* or *owns* all its members).
* _child design/component_ - A design instance that is owned by another design.
* _top design_ - The highest-level design in the hierarchy (not contained by any other design), also known as the *top-level design*.
* _top-app design_ - A `@top` annotated *top design* that generates a main entry point with the default application.

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

A DFHDL design declaration follows standard [Scala class](https://docs.scala-lang.org/tour/classes.html){target="_blank"} syntax, with specialized handling by the DFHDL Scala compiler plugin.

```scala linenums="0" title="Design declaration syntax"
/** _documentation_ */
@top(genMain) //required only for top-level designs
[_modifiers_] class _name_(_params_) extends XXDesign:
  _contents_
end _name_ //optional `end` marker
```

* __`_name_`__ - The Scala class name for the design. The DFHDL compiler preserves this name and uses it in error messages and generated artifacts (e.g., Verilog modules or VHDL entities). See the [naming][naming] section for details.

* __`(_params_)`__ - An optional parameter block. This can include either Scala parameters that are inlined during design elaboration, or DFHDL design parameters that are preserved through elaboration and compilation. If no parameters are needed, Scala syntax accepts either empty parentheses `()` or no parentheses. See [Parameter Block Syntax][design-params-syntax] for details.

* __`_XXDesign_`__ - The base class to extend, where `XX` specifies the [design domain][design-domains]: `DF` for dataflow, `RT` for register-transfer, or `ED` for event-driven.

* __`_contents_`__ - The design interface (ports/interfaces/domains) and functionality (variables, functions, child designs, processes, etc.), based on the selected design domain's semantics.

* __`@top(genMain)`__ - A required annotation for top-level designs (designs not instantiated within another design). The annotation has an optional `#!scala val genMain: Boolean = true` parameter:
    - When `genMain = true`, the design becomes a top-app design where all parameters must have default values, and a main Scala entry point named `top__name_` is generated
    - When `genMain = false`, the annotation only provides a default top-level context for the design

* __`_documentation_`__ - Design documentation in [Scaladoc format](https://docs.scala-lang.org/style/scaladoc.html){target="_blank"}. This documentation is preserved throughout compilation and included in the generated backend code.

* __`_modifiers_`__ - Optional Scala modifiers. See [Design Class Modifier Rules][design-class-modifier-rules] for details.

### `LeftShift2` example {#LeftShift2}
/// admonition | Basic top-app design example: a two-bits left shifter
    type: example
The DFHDL code below implements a two-bit left shifter design named `LeftShift2`. The design:

- Uses register-transfer (RT) domain semantics by extending `RTDesign`
- Has an 8-bit input port and an 8-bit output port 
- Performs a fixed 2-bit left shift operation on the input

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

Since this design is annotated with `@top`, it is a top-app design that generates an executable Scala program. This program compiles the design and generates backend code (Verilog or VHDL). The backend can be configured through:

- Command-line arguments when running the program
- Implicit backend settings in the code (as shown in this example)

The `@top` annotation captures any [implicit/given](https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2){target="_blank"} options within its scope and provides them as defaults when no CLI arguments are specified.

/// tab | Generated Verilog
```verilog
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo1.LeftShift2Spec/verilog.sv2009/hdl/LeftShift2.sv"
```
Looking at the generated Verilog code, we can observe several key differences from the DFHDL source:

1. **Module Interface**: DFHDL's Scala-style port declarations (`<> IN/OUT`) are translated to traditional Verilog port declarations (`input wire`/`output logic`)

2. **Documentation**: Scaladoc comments are preserved and converted to Verilog-style comments (`/* */`)

3. **Default Settings**: The compiler adds standard Verilog settings like `` `default_nettype none`` and `` `timescale``

4. **Include Files**: The compiler adds necessary include files for backend-specific definitions

5. **Assignment Syntax**: DFHDL's `:=` assignments are translated to Verilog's `assign` statements

///

/// tab | Generated VHDL
```vhdl
--8<-- "lib/src/test/resources/ref/docExamples.ugdemos.demo1.LeftShift2Spec/vhdl.v2008/hdl/LeftShift2.vhd"
```
The generated VHDL code shows similar transformations from the DFHDL source:

1. **Entity Interface**: DFHDL's port declarations are translated to VHDL's `in`/`out` mode declarations with explicit signal types

2. **Documentation**: Scaladoc comments are preserved as VHDL comments (`--`)

3. **Library/Package Usage**: The compiler adds necessary library and package references (`ieee`, `std_logic_1164`, etc.)

4. **Signal Types**: DFHDL's `Bits` type is translated to VHDL's `std_logic_vector` with appropriate widths

5. **Assignment Syntax**: While both DFHDL and VHDL use `:=`, the semantics differ - DFHDL represents high-level connections while VHDL represents signal assignments
///

/// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo1/LeftShift2.scala:3"
```
///
///


### Parameter Block Syntax {#design-params-syntax}
The DFHDL design parameter block follows standard Scala syntax, accepting a comma-separated list of parameter declarations:

```scala linenums="0" title="Design declaration parameter block syntax"
([_access_] _name_: _type_ [= _default_], ...)
```

* __`_type_`__ - Either a pure Scala type or a DFHDL parameter type (`DFType <> CONST`):
    - Pure Scala parameters are inlined during elaboration
    - DFHDL parameters are preserved in the generated backend code
    - See [Design Parameter Type Rules][design-parameter-type-rules] for details

* __`_name_`__ - The parameter name. For DFHDL parameters, this name is:
    - Preserved throughout compilation
    - Used in the generated backend code
    - Available through the CLI for top-app designs

* __`_default_`__ - Optional default value. Required for all parameters in top-app designs. See [Design Parameter Default Value Rules][design-parameter-default-value-rules] for details.

* __`_access_`__ - Optional [Scala access modifier](https://docs.scala-lang.org/scala3/book/domain-modeling-oop.html#access-modifiers){target="_blank"}. Usually `#!scala val` to make the parameter public. See [Design Parameter Access Rules][design-parameter-access-rules] for details.

#### `LeftShiftBasic` example {#LeftShiftBasic}
/// admonition | Scala-parameterized top-app design example: a basic left shifter
    type: example
The DFHDL code below implements a basic left shifter design named `LeftShiftBasic`. This design is similar to the earlier example of [`LeftShift2`][LeftShift2], except here the design has the shift value as an input, and its input and output port widths are set according to the Scala parameter `width`.

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

#### `LeftShiftGen` example {#LeftShiftGen}
/// admonition | DFHDL-parameterized top-app design example: a generic left shifter
    type: example
The DFHDL code below implements a generic left shifter design named `LeftShiftGen`. This design is similar to the earlier example of [`LeftShiftBasic`][LeftShiftBasic], except here the `width` parameter is now a DFHDL parameter, as indicated by its `Int <> CONST` type. This enables the DFHDL compiler to preserve the parameter name and directly use it in the generated backend code where applicable.

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
- Top-app design parameters that can be modified from the CLI must be one of:
    - Pure Scala Types: `#!scala String`, `#!scala Boolean`, `#!scala Int`, and `#!scala Double`
    - DFHDL Types: `#!scala Int <> CONST`, `#!scala Bit <> CONST`, and `#!scala Boolean <> CONST`

/// admonition | Top-app design with accepted and ignored CLI arguments example
    type: example
In this example, the top-app supported parameters `pureIntArg` and `dfhdlIntArg` are preserved to be modifiable from the CLI, while `ignored` and `dfhdlIgnored` keep their default values.

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

```title="CLI help mode output, when running via sbt (truncated)"
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
This example shows `FooErr` is missing a default value and throws an error. There are three ways to resolve this, shown in `FooOK1`, `FooOK2`, and `FooOK3`.

```scala
//Error: Missing argument's default value 
//for top-level design with a default app 
//entry point. Either add a default value 
//or disable the app entry point generation 
//with `@top(false)`.
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

//OK: the `genMain` argument in the `@top` 
//annotation is set to `false`
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
Without any [Scala access modifier](https://docs.scala-lang.org/scala3/book/domain-modeling-oop.html#access-modifiers){target="_blank"}, a Scala class parameter access is declared as `#!scala private val`. This default access leads to an error if that parameter affects the type of non-private class member (e.g., a `width` parameter affecting the bits width of a port). To resolve this error, the parameter can be declared as public `#!scala val`, as `#!scala protected val`, or even `#!scala private[scope] val` with a scope [access qualifier](https://www.scala-lang.org/files/archive/spec/3.4/05-classes-and-objects.html#private){target="_blank"}.

/// admonition | Parameter access example
    type: example
This example shows an access error in `FooErr`, where a private parameter `width` affects the type of a non-private member `v`. There are four ways to resolve this error, shown in `FooOK1`, `FooOK2`, `FooOK3`, and `FooOK4`. `FooOK5` demonstrates that private parameters can be accessed by public members as long as they don't affect the type.

```scala
//Error: non-private value v in class FooErr 
//refers to private value width ... (lengthy 
//type description)
class FooErr(
    width: Int <> CONST
) extends DFDesign:
  val v = UInt(width) <> VAR

//OK: both parameter and its dependent design 
//members are public
class FooOK1(
    val width: Int <> CONST
) extends DFDesign:
  val v = UInt(width) <> VAR

//OK: both parameter and its dependent design 
//members are private (note that ports should
//never be private, and therefore parameters
//that affect their types should never be 
//private)
class FooOK2(
    width: Int <> CONST
) extends DFDesign:
  private val v = UInt(width) <> VAR

//OK: the parameter is protected
class FooOK3(
    protected val width: Int <> CONST
) extends DFDesign:
  val v = UInt(width) <> VAR

package Bar:
  //OK: the parameter is private but only 
  //outside the scope of `Bar`.
  class FooOK4(
      private[Bar] val width: Int <> CONST
  ) extends DFDesign:
    val v = UInt(width) <> VAR

//OK: the parameter `v0` is private, but it
//does not affect the type of the public
//dfhdl variable `v`. Only `width` affects
//the type of `v` and it's public as well.
class FooOK5(
    v0: Int <> CONST,
    val width: Int <> CONST
) extends DFDesign:
  val v = UInt(width) <> VAR init v0
```
///

/// admonition | Good design practice - how to choose the right parameter/member access?
    type: tip
* For Simple Development - During initial development, you can declare all parameters and named design members as public `#!scala val` for simplicity.
* Protection for Shared Code - When sharing your design with others in DFHDL (Scala) form, follow good design practices to maintain source and binary compatibility of your Scala library artifacts. Remember: You can always remove protection without breaking code, but adding protection later will cause breakage.
    * Public Interface, Protected Implementation - Keep the design interface (ports, domains, interfaces, and their type-affecting parameters) public. All other design class members should have private or protected access modifiers.
    * Private vs. Protected Access - Use private access for members that should only be accessible within the design class itself. Use protected access for members that should also be accessible by subclasses.
    * Scoped Protection for Testing - For verification code that needs access to internal design members, use package-private access with a scope qualifier (e.g. `#!scala private[mylib]`) instead of making members fully public. This restricts access to just your library's test code.

In this example, the `Foo` class demonstrates good design practices for parameter and member access.
```scala title="Good design practice example"
package mylib
import dfhdl.*

class Foo(
    val width: Int <> CONST,
    val boolParam: Boolean <> CONST,
    protected[mylib] val someValue: Int <> CONST
) extends DFDesign:
  //Public interface
  val i = UInt(width) <> IN
  val o = UInt(width) <> OUT
  //Protected implementation
  protected[mylib] val v = UInt(width) <> VAR
```
///

### Design Class Modifier Rules
A DFHDL design class cannot be declared as `#!scala final class` or `#!scala case class`. Attempting to do so produces an error:

```scala title="DFHDL design class modifier limitation example"
//error: DFHDL classes cannot be final classes.
final class Foo extends DFDesign
//error: DFHDL classes cannot be case classes.
case class Bar() extends DFDesign
```

All other Scala class modifiers have no special effect or limitation from a DFHDL compiler perspective. Nonetheless, these modifiers can be relevant when defining a more complex design API, as part of the DFHDL meta-programming capabilities through the Scala language (e.g., changing class access to `#!scala protected`).

### `LRShiftFlat` example {#LRShiftFlat}
/// admonition | Generic left-right shifter, flat design example
    type: example
The DFHDL code below implements a generic left-right shifter flat design named `LRShiftFlat`. This design expands on [`LeftShiftGen`][LeftShiftGen] by adding a `dir` enum port value that specifies the shift direction and a shift operation multiplexer through a `#!scala match` statement.

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

### Design Class Inheritance
DFHDL leverages Scala inheritance to enable sharing functionality between design classes.

#### `ShiftGen` example {#ShiftGen}
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

* [Direct Connection Composition][direct-connection-composition] - The recommended mechanism for complex design hierarchies with multiple inputs and outputs. Design instantiation and port connection can be done separately, allowing child design ports to be referenced without intermediate variables.

* [Via Connection Composition][via-connection-composition] - A legacy mechanism that connects ports only within a design instantiation. This exists for compatibility with Verilog module instancing and VHDL component instancing. The DFHDL compiler automatically transforms direct connections into via connections.

* [Functional Composition][functional-composition] - A method-call mechanism for dataflow designs, primarily used for arithmetic/logic functionality with a single output port. The DFHDL compiler automatically transforms functional composition into direct design composition.

The following sections explore these composition mechanisms using our running example of a bit shifter:

### Direct Connection Composition
Direct connection composition is the recommended approach for building hierarchical designs in DFHDL. It offers several key advantages:

1. Separate instantiation and connection - Create child design instances first, then connect their ports later
2. Direct port references - Access child design ports without intermediate variables 
3. Flexible connectivity - Connect ports in any order and across multiple statements

#### Syntax
The syntax for direct composition follows standard Scala class instantiation, with port connections made via the `<>` operator:

```scala linenums="0" title="Direct composition syntax"
//instantiate a child design
val _childDesignName_ = _designClass_(_params_)
//port connection (repeat for each child port)
_childDesignName_._childPort_ <> _connectedValue_ 
```

Where:

* `_childDesignName_` - The instance name for the child design. This name is preserved by the DFHDL compiler and used in error messages and generated artifacts. See the [naming][naming] section for details.

* `_designClass_` - The design class to instantiate.

* `_params_` - Parameters for the child design. Empty parentheses `()` are required even if no parameters are needed. Parameters can be specified:
    - As ordered values (e.g., `Counter(8, Up)`)
    - As named parameters (e.g., `Counter(width = 8, dir = Up)`) 
    - Parameters with default values can be omitted

* `_childPort_` - The port of the child design to connect.

* `_connectedValue_` - The value to connect to the child port. Can be:
    - A constant
    - A variable
    - A port of the parent design
    - A port of another child design instance

The `<>` connection operator has no explicit directionality - it automatically infers producer/consumer relationships based on the connected value types and scope. See the [connectivity][connectivity] section for details.

#### `LRShiftDirect` example {#LRShiftDirect}
/// admonition | Generic left-right shifter using direct connection composition
    type: example
The code below implements a generic left-right shifter named `LRShiftDirect`. This design provides the same functionality as [`LRShiftFlat`][LRShiftFlat], but uses composition to split left and right shift operations into separate designs. The implementation leverages the design class inheritance shown in the [`ShiftGen`][ShiftGen] example.

/// admonition  
    type: note
While this example demonstrates direct composition, a flat approach is often preferable for simpler designs.
For complex designs, however, splitting functionality into sub-components promotes code reuse, simplifies verification, and follows good design practices.
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
Via connection composition is a legacy mechanism that connects child design ports within the child design instantiation. It exists for compatibility with Verilog module instantiation and VHDL component instantiation. The DFHDL compiler automatically transforms direct connections into via connections.

#### Syntax
The syntax for via composition uses Scala anonymous class instantiation, with port connections made inside the instantiation block:

```scala linenums="0" title="Via composition syntax"
//instantiate a child design
val _childDesignName_ = new _designClass_(_params_):
    //port connection (repeat for each child port)
    _childPort_ <> _connectedValue_
```

The `#!scala new` keyword and colon `:` syntax creates an anonymous class instance. Port connections must be made within this instantiation block, similar to Verilog module and VHDL component instantiation. This means connected values must be declared before they are used in the connection operation.

/// admonition | Handling port name collisions between parent and child designs
    type: tip
When connecting ports with the same name in parent and child designs, Scala's name shadowing rules will favor the child port name. For example, when connecting the `iBits` port of `LeftShiftGen` to the `iBits` port of `LRShiftVia`, we need a way to reference the parent's `iBits` port from within the child design.

To solve this, we use Scala's class self reference feature and name it `parent`, as shown in the [`LRShiftVia`][LRShiftVia] example below.
///

#### `LRShiftVia` example {#LRShiftVia}
/// admonition | Generic left-right shifter, via composition example
    type: example
The DFHDL code below implements the same generic left-right shifter composition seen in the [`LRShiftDirect`][LRShiftDirect] example, but uses via composition instead of direct composition. We define a `parent` self reference for the `LRShiftVia` design to refer to the `LRShiftVia` design within the `lshifter: LeftShiftGen` and `rshifter: RightShiftGen` child designs. We also use intermediate variables for the `oBits` ports of the `lshifter` and `rshifter` child designs and apply the multiplexer logic to select between them.

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo5/LRShiftVia.scala:5"
``` 
///

Another interesting example is the automatic transformation of a direct composition design into a via composition design:
/// admonition | Direct-to-via compilation transformation example
    type: example
The code below shows how the DFHDL compiler transforms the [`LRShiftDirect`][LRShiftDirect] design into via composition form. For each child design port, the compiler:

1. Creates an intermediate variable with the same type
2. Connects the child port to this variable inside the child's instantiation block
3. Connects the variable to the appropriate value in the parent design

```scala
class LRShiftDirect(val width: Int <> CONST = 8) extends EDDesign:
  /** bits input */
  val iBits          = Bits(width)        <> IN
  /** requested shift */
  val shift          = UInt(clog2(width)) <> IN
  /** bits output */
  val oBits          = Bits(width)        <> OUT
  /** direction of shift */
  val dir            = ShiftDir           <> IN
  val lshifter_iBits = Bits(width)        <> VAR
  val lshifter_shift = UInt(clog2(width)) <> VAR
  val lshifter_oBits = Bits(width)        <> VAR
  val lshifter = new LeftShiftGen(width = width):
    this.iBits   <>/*<--*/ lshifter_iBits
    this.shift   <>/*<--*/ lshifter_shift
    this.oBits   <>/*-->*/ lshifter_oBits
  val rshifter_iBits = Bits(width)        <> VAR
  val rshifter_shift = UInt(clog2(width)) <> VAR
  val rshifter_oBits = Bits(width)        <> VAR
  val rshifter = new RightShiftGen(width = width):
    this.iBits   <>/*<--*/ rshifter_iBits
    this.shift   <>/*<--*/ rshifter_shift
    this.oBits   <>/*-->*/ rshifter_oBits
  lshifter_iBits <> iBits
  lshifter_shift <> shift
  rshifter_iBits <> iBits
  rshifter_shift <> shift
  process(all):
    dir match
      case ShiftDir.Left  => oBits := lshifter_oBits
      case ShiftDir.Right => oBits := rshifter_oBits
    end match
end LRShiftDirect
``` 
Note how the compiler adds comments (`/*<--*/` and `/*-->*/`) to indicate the direction of data flow in the child design port connections.

The following runnable example is the same as the [`LRShiftDirect`][LRShiftDirect] example, except for the default compiler options, which we altered to print the compiled design in DFHDL code format rather than the backend code format.
/// details | Runnable example
    type: dfhdl
```scastie
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog
//disable the default backend code print (in scastie)
given options.CompilerOptions.PrintBackendCode = false
//enable the DFHDL code print after compilation
given options.CompilerOptions.PrintDFHDLCode = true

--8<-- "lib/src/test/scala/docExamples/ugdemos/demo5/LRShiftDirect.scala:6"
```
///
///

### Functional Composition
Functional composition is a method-call based mechanism primarily used for dataflow designs with a single output. It's particularly useful for arithmetic and logic operations. The DFHDL compiler automatically transforms functional composition into direct design composition.

#### Syntax
The syntax for functional composition follows standard Scala method declaration and invocation to declare and invoke *design definitions* (aka design methods or design functions):

```scala linenums="0" title="Functional composition syntax"
//declare a design definition
def _designName_(
    _param1_: _type1_ <> VAL,
    _param2_: _type2_ <> VAL,
    ...,
    _paramN_: _typeN_ <> VAL
): _returnType_ <> DFRET = _expression_

//invoke the design definition
_designName_(_param1_, _param2_, ..., _paramN_)
```

Where:

* `_designName_` - The name of the design definition
* `_paramN_` - Input parameters that act as design ports
* `_typeN_` - DFHDL types for the parameters
* `_returnType_` - DFHDL type for the output port
* `<> VAL` - Marks parameters as design input ports
* `<> DFRET` - Marks the return type as a design output port
* `_expression_` - The design functionality

#### `LRShiftFunc` example {#LRShiftFunc}
/// admonition | Generic left-right shifter using functional composition
    type: example
The code below implements the same generic left-right shifter functionality seen in previous examples, but uses functional composition. The implementation is split into three function designs:

1. `LeftShiftGen` - Performs left shift operation
2. `RightShiftGen` - Performs right shift operation  
3. `LRShiftFunc` - Top-level function that selects between left/right shift

Since functions can't be top-level designs in DFHDL, we wrap `LRShiftFunc` in a top-level design class `LRShiftFuncWrapper`.

<div class="grid" markdown>

```scala
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo6/LRShiftFunc.scala:6"
```

```hdelk width=100%
stroke-width = 0
children = [
  {
    id = wrapper
    label = LRShiftFuncWrapper
    inPorts = [iBits, shift, dir]
    outPorts = [oBits]
    parameters = [width]
    children = [
      {
        id = func
        label = LRShiftFunc
        inPorts = [iBits, shift, dir]
        outPorts = [oBits]
        children = [
          {
            id = left
            label = LeftShiftGen
            inPorts = [iBits, shift]
            outPorts = [oBits]
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
            id = mux
            label = "mux"
            northPorts = [{id = sel, label = " ", height = 5, width = 1}]
          }
        ]
        edges = [
          [func.iBits, left.iBits]
          [func.shift, left.shift]
          [func.iBits, right.iBits]
          [func.shift, right.shift]
          [func.dir, mux.sel]
          [left.oBits, mux]
          [right.oBits, mux]
          [mux, func.oBits]
        ]
      }
    ]
    edges = [
      [wrapper.iBits, func.iBits]
      [wrapper.shift, func.shift]
      [wrapper.dir, func.dir]
      [func.oBits, wrapper.oBits]
    ]
  }
]
```

</div>

/// admonition | Advantages of functional composition
    type: tip
Functional composition offers several benefits:

1. Concise syntax - No need for explicit port declarations and connections
2. Natural expression - Operations can be written in a more natural mathematical style
3. Easy reuse - Functions can be composed and reused easily
4. Type safety - Scala's type system ensures correct port connections
///

<!-- TODO: add runnable example -->
<!-- /// details | Runnable example
    type: dfhdl
```scastie
--8<-- "lib/src/test/scala/docExamples/ugdemos/demo6/LRShiftFunc.scala:3"
```
/// -->
///

