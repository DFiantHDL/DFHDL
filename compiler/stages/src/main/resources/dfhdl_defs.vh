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

`ifndef DFHDL_DEFS
`define DFHDL_DEFS
`define MAX(a,b) ((a) > (b) ? (a) : (b))
`define MIN(a,b) ((a) < (b) ? (a) : (b))
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
integer result;
begin
  result = 0;
  n = n - 1;
  while (n > 0) begin
    n = n >> 1;
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
`endif

