//  This file (dfhdl_defs.vh) is free and unencumbered software 
//  released into the public domain.
//
//  Anyone is free to copy, modify, publish, use, compile, sell, or
//  distribute this software, either in source code form or as a compiled
//  binary, for any purpose, commercial or non-commercial, and by any
//  means.
//  
//  In jurisdictions that recognize copyright laws, the author or authors
//  of this software dedicate any and all copyright interest in the
//  software to the public domain. We make this dedication for the benefit
//  of the public at large and to the detriment of our heirs and
//  successors. We intend this dedication to be an overt act of
//  relinquishment in perpetuity of all present and future rights to this
//  software under copyright law.
//  
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
//  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
//  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
//  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
//  OTHER DEALINGS IN THE SOFTWARE.
//  
//  For more information, please refer to <http://unlicense.org/>

`define MAX(a, b) ((a) > (b) ? (a) : (b))
`define MIN(a, b) ((a) < (b) ? (a) : (b))
`define ABS(a) ((a) < 0 ? -(a) : (a))
// Fixed-point types: `M` integer (magnitude) bits and `F` fraction bits, laid out with the
// binary point at index 0 so bit weights are 2^index (integer bits M-1..0, fraction bits
// -1..-F). `sfix` carries the `signed` keyword so it is not repeated at the declaration.
// Verilog-95 has no `signed` keyword, so signed fixed-point uses `sfix_v95` (an unsigned
// range) and applies signedness at the operation sites, like `SInt`.
`define ufix(M, F) [(M)-1:-(F)]
`define sfix(M, F) signed [(M)-1:-(F)]
`define sfix_v95(M, F) [(M)-1:-(F)]
`define hPW(hex, hw, vw) \
    /* verilator lint_off WIDTH */ \
    ((vw) == (hw) ? hw'h``hex : {{(vw - hw){1'b0}}, hw'h``hex}) \
    /* verilator lint_on WIDTH */
`define dPW(dec, dw, vw) \
    /* verilator lint_off WIDTH */ \
    ((vw) == (dw) ? dw'd``dec : {{(vw - dw){1'b0}}, dw'd``dec}) \
    /* verilator lint_on WIDTH */
`define sdPPW(dec, dw, vw) \
    /* verilator lint_off WIDTH */ \
    $signed((vw) == (dw) ? dw'sd``dec : {{(vw - dw){1'b0}}, dw'sd``dec}) \
    /* verilator lint_on WIDTH */
`define sdNPW(dec, dw, vw) \
    /* verilator lint_off WIDTH */ \
    $signed((vw) == (dw) ? -dw'sd``dec : {{(vw - dw){1'b1}}, -dw'sd``dec}) \
    /* verilator lint_on WIDTH */
`define sdNPW_V95(dec, dw, vw) \
    /* verilator lint_off WIDTH */ \
    ((vw) == (dw) ? -dw'd``dec : {{(vw - dw){1'b1}}, -dw'd``dec}) \
    /* verilator lint_on WIDTH */
`define TRUNCATE(vec, toW) \
    vec[toW - 1:0] 
`define EXTEND_U(vec, fromW, toW) \
    /* verilator lint_off WIDTH */ \
    ((toW) == (fromW) ? vec : {{((toW) - (fromW)){1'b0}}, vec}) \
    /* verilator lint_on WIDTH */
`define EXTEND_S_V95(vec, fromW, toW) \
    /* verilator lint_off WIDTH */ \
    ((toW) == (fromW) ? vec : {{((toW) - (fromW)){vec[fromW - 1]}}, vec}) \
    /* verilator lint_on WIDTH */
`define EXTEND_S(vec, fromW, toW) $signed(`EXTEND_S_V95(vec, fromW, toW))
// Relative widening: extend `vec` by `by` bits. The zero-extension needs no width at
// all; the sign-extension replicates the sign bit, indexed via the source width.
// `vec` must be an indexable primary (an identifier) for the sign-extending forms.
`define EBY_U(vec, by) {{(by){1'b0}}, vec}
`define EBY_S_V95(vec, fromW, by) {{(by){vec[(fromW) - 1]}}, vec}
`define EBY_S(vec, fromW, by) $signed(`EBY_S_V95(vec, fromW, by))
`define SIGNED_GREATER_THAN(a, b, width)  \
    ((a[width-1] && !b[width-1]) ? 1'b0 : /* a is negative, b is positive */ \
     (!a[width-1] && b[width-1]) ? 1'b1 : /* a is positive, b is negative */ \
     (a > b))                          /* both are same sign */
`define SIGNED_LESS_THAN(a, b, width)  \
    ((a[width-1] && !b[width-1]) ? 1'b1 : /* a is negative, b is positive */ \
     (!a[width-1] && b[width-1]) ? 1'b0 : /* a is positive, b is negative */ \
     (a < b))                          /* both are same sign */
`define SIGNED_GREATER_EQUAL(a, b, width) \
     (`SIGNED_GREATER_THAN(a, b, width) || a != b)
 `define SIGNED_LESS_EQUAL(a, b, width)   \
    (`SIGNED_LESS_THAN(a, b, width) || a == b)
`define SIGNED_SHIFT_RIGHT(data, shift, width) \
    ((data[width-1] == 1'b1) ? ((data >> shift) | ({width{1'b1}} << (width - shift))) : (data >> shift))
function integer clog2;
input integer n;
integer result, temp;
begin
  result = 0;
  temp = n - 1;
  while (temp > 0) begin
    temp = temp >> 1;
    result = result + 1;
  end
  clog2 = result;
end
endfunction
// Function to perform base raised to the power of exp (base ** exp)
function integer power;
input integer base;
input integer exp;
integer i;  // Loop variable
begin
  if (exp == 0) begin
    power = 1;
  end else begin
    power = 1;  
    for (i = 0; i < exp; i = i + 1) begin
      power = power * base;
    end
  end
end
endfunction

