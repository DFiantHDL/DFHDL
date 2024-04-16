---
typora-copy-images-to: ./
---
[](){#user-guide}
# Type System

DFHDL is a Scala library and thus inherently supports type safe and rich language constructs. This chapter covers the rules and API of this type system. 

??? info "Checkout the benefits of the DFHDL type system"

    <div class="grid cards" markdown>
    
    - :mechanical_arm:{ .lg .middle } __Strongly-typed__
    
        ---
    
        Most type checking is done statically and enforces strict rules that prevent ambiguity.
    
        ```scala linenums="0"
        val u8 = UInt(8) <> IN
        val u2 = UInt(2) <> IN
        val y1 = u8 - u2 //ok
        //error prevents ambiguous 
        //behavior when a wider num is 
        //subtracted from a narrow num
        val y2 = u2 - u8 //error
        ```
        ![strongly-typed-example](strongly-typed-example.png)


    -   :material-bullseye-arrow:{ .lg .middle } __Bit-accurate__
    
        ---
    
        Each DFHDL value has known bit-width which is used to enforce various rules to prevent data loss.
    
        ```scala linenums="0"
        val u8 = UInt(8) <> IN
        val s8 = SInt(8) <> OUT
        //error prevents data loss when
        //u8 is converted to a 9-bit signed
        //to be assigned to s8 which is
        //only 8-bits wide
        s8 := u8 //error
        ```
        ![bit-accurate-example](bit-accurate-example.png)
    
    -   :simple-googlecloudcomposer:{ .lg .middle } __Composable__
    
        ---
    
        Types can be composed through [structs](#struct-dfhdl-values) or [tuples](#tuple-dfhdl-values) to form new types.
    
        ```scala linenums="0"
        //new Pixel type as a structure
        //of two unsigned 8-bit numbers
        case class Pixel(
          x: UInt[8] <> VAL,
          y: UInt[8] <> VAL
        ) extends Struct
    
        val pixel = Pixel <> VAR
        //select and assign fields
        pixel.x := pixel.y
        ```
    
    -   :material-expand-all:{ .lg .middle } __Expandable__
    
        ---
    
        New types can be defined, and methods can be added for new or existing types.
    
        ```scala linenums="0"
        //new AESByte type of unsigned 8-bit num
        case class AESByte() 
          extends Opaque(UInt(8))
        //define addition between two AESByte
        //values as a xor operation
        extension (lhs: AESByte <> VAL)
          def +(rhs: AESByte <> VAL): AESByte <> DFRET =
            (lhs.actual ^ rhs.actual).as(AESByte)
        val x, y = AESByte <> VAR
        val z = x + y //actually XOR
        ```
    
      </div>

Each DFHDL value is simply a Scala object that has two critical fields:

* **(Shape) Type, aka DFType** - Determines the bit-width and bit-structure of the value. Currently the supported types are: 
	* `Bit` ,`Boolean` 
	* `Bits`
	* `UInt`,`SInt`, `Int`
	* Enumerations
	*  `Vector` (of DFTypes)
	*  `Struct` (of DFTypes)
	* `Tuple` (of DFTypes)
	* `Opaque` (of a DFType)
* **(Access) Modifier** - Determines what kind of access the user has on the value. While underneath the hood this mechanism can be quite complex, for the user the only explicit modifiers are very simple and are limited to: `VAR`, `IN`, `OUT`, `INOUT`, `VAL` and `CONST`.



??? dfiant "Internal Type-System Hierarchy (For Advanced Users)"
	DFHDL brings type driven development concepts to hardware design, by creating an extensible type class hierarchy. Any DFHDL value is a Scala object instance of the class `DFVal[T <: DFTypeAny, M <: ModifierAny]`, where `T` is the type (shape) of value and `M` is a modifier that sets additional characteristics of the DFHDL value, like if its assignable, connectable, initializable, etc. 

	![type-system](type-system-light.png#only-light)
	![type-system](type-system-dark.png#only-dark)
	
	For example for a port `x` set like `#!scala val x = Boolean <> IN`


## Mutable Dataflow Variables and Immutable Dataflow Values

DFiant supports dataflow variables mutability via the `:=` operator. Do not confuse with Scala-level mutability which is enabled by using `#!scala var` instead of `#!scala val`. Each dataflow class has two variations: an immutable class, which inherits from `DFAny.Val` and a mutable class, which inherits from `DFAny.Var` and accepts `:=`. The difference between the types enforces an immutable right-hand-side (RHS), where required, and a mutable variable creation. 

Consider, for instance, the DFiant implementation of `g` in Table \ref`tbl:StateExDefImpl`: `a` is immutable because it is a RHS addition between the dataflow variable `i` and a literal value `5`. Contrarily, `c` is mutable, since it is a dataflow variable constructor (`.init` constructs a new initialized variable, while preserving the mutability trait). 

Fig. 1 demonstrates a dual class definition for every type  (immutable and mutable). The naming convention helps to reason about the mutability. For example, `DFBits` and `DFBits.Var` are immutable and mutable classes, respectively. Constructing a new variable via `DFBits` (e.g, `#!scala val a = DFBits[5]`) returns the mutable `DFBits.Var[5]`. Usually, we either receive or return an immutable type, hence we do not require annotating a type with its mutable variation. In cases where we want to return a mutable type, we annotate it as an output port (see Section~\ref`sec:io_ports`).

!!! error "Don't use `var` with dataflow values/variables"
	Because the semantics may get confusing, we enforced a compiler error if a dataflow variable is constructed and fed into a Scala `#!scala var` reference. For example `#!scala var a = DFUInt(8)` will generate a Scala compiler error. 


## Bit-Accurate Operations, Type Inference, and Data Structures

All DFiant's dataflow types are bit-accurate and structurally static, with their bit-width set upon construction (e.g., `DFBits[5]` is a 5-bit vector). Operations between dataflow variables produce a bit-accurate result with the proper type inference. For example, an addition between an unsigned 5-bit variable (`DFUInt[5]`) and a signed 10-bit variable (`DFSInt[10]`) produces an adder that can be implicitly converted to a 10-bit signed variable, if carry is not required, or an 11-bit signed variable by explicitly invoking `.wc` from the addition.

DFiant also allows operations between dataflow types and their corresponding Scala numeric types, by treating the Scala numeric types as constants (e.g., addition between `DFSInt` and `Integer` variables). A constant in the dataflow graph is a node that can produce infinite tokens of the same value.   

## Bit Aliasing and Casting

Aliasing in DFiant enables referencing a part of a dataflow variable, by invoking `.bits(hiIdx, loIdx)`, which creates a bits vector alias that references the original variable at the given index parameters. Every change of a dataflow variable affects its alias and vice versa (similar to VHDL's signal aliasing). Since this function also casts the variable as `DFBits`, this feature is used as a raw-data cast between different dataflow types. Aliasing of an alias is also possible, while maintaining relative bits indexing. Aliasing preserves the mutability trait: an alias of an immutable value is immutable, while an alias of a mutable variable is mutable. 

Fig.~\ref`fig:Aliasing` demonstrates aliasing code and its effect on the contents of a dataflow variable (`bits128`). Each line code does as follows:



  1. Constructs a new 128-bit vector, `bits128`, and clears it.
  2. Creates a new alias, `alias64`, which references the most significant 64 bits of `bits128`. Since `bits128` is a `DFBits` variable, there is no need to invoke `.bits()`, and we can apply the required indexes directly.
  3. Creates a new alias, `alias32`, which references the least significant 32 bits of `alias64`, which reference bits 64 to 95 of `bits128`.
  4. Constructs a new double precision floating point dataflow variable, `dbl`, and initialize its value as `1.0` (hexadecimal value of `0x3FF00...0`).
  5. Modifies the least significant byte of `dbl`.
  6. Sets the most significant bit of `bits128`.
  7. Assigns `dbl` to the least significant 64 bits of `bits128` through casting. All the bits of `dbl` are selected because `.bits()` is invoked without index parameters.
  8. Modifies a byte of `bits128`.


## `Bit`, `Boolean` DFHDL Values

## `Bits` DFHDL Values

## `UInt`, `SInt`, `Int` DFHDL Values

## Enumeration DFHDL Values

## Vector DFHDL Values

## Struct DFHDL Values

## Tuple DFHDL Values

## Opauqe DFHDL Values
