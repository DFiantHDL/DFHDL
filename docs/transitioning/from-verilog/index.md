# Transitioning from Verilog to DFHDL

## Using ChatGPT

Help me ChatGPT, you're my only hope

## Summary

/// admonition | Module Definition
    type: rtl
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
module _module_name_ #(
  //param declarations
) (
  //port declarations
);
  //internal declarations
endmodule
```

```scala linenums="0" title="DFHDL"
class _design_name_(
  //param declarations
) extends EDDesign:
  //port & internal declarations


end _design_name_ //optional
```

```sv linenums="0" title="Verilog"
module AndGate (
  input a, b;
  output o
);
  assign o = a & b
endmodule
```

```scala linenums="0" title="DFHDL"
class AndGate extends EDDesign:
  val a, b = Bit <> IN
  val o    = Bit <> OUT

  o <> a && b
end AndGate
```

</div>
///

/// admonition | Parameter Declarations
    type: rtl
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
parameter [7:0] p = 8’b1011;
```

```scala linenums="0" title="DFHDL"
val p: Bits[8] <> CONST = b"8'1011"
```

```sv linenums="0" title="Verilog"
module Concat #(
  parameter  int len1;
  parameter  int len2;
  localparam int outlen = len1 + len2
) (
  input  [len1-1:0]   i1;
  input  [len2-1:0]   i2;
  output [outlen-1:0] o
);
  assign o = {i1, i2};
endmodule
```

```scala linenums="0" title="DFHDL"
class Concat(
    val len1: Int <> CONST
    val len2: Int <> CONST
) extends EDDesign:
  val outlen = len1 + len2
  val i1 = Bits(len1)   <> IN
  val i2 = Bits(len2)   <> IN
  val o  = Bits(outlen) <> OUT
  
  o <> (i1, i2)
end Concat
```

</div>
///

/// admonition | logic/reg/wire
    type: rtl
<div class="grid" markdown>

```sv linenums="0" title="Verilog"
logic [7:0] v = 8’b1011;
wire  [7:0] v = 8’b1011;
reg   [7:0] v = 8’b1011;
```

```scala linenums="0" title="DFHDL"
val v = Bits(8) <> VAR init b"8'1011"
```

</div>
///

