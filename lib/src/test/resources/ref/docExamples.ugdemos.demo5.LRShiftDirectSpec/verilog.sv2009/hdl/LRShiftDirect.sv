/* A left-right bits shifter, direct composition
  
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LRShiftDirect_defs.svh"

module LRShiftDirect#(parameter int width = 8)(
  /* bits input */
  input  wire logic [width - 1:0]         iBits,
  /* requested shift */
  input  wire logic [$clog2(width) - 1:0] shift,
  /* bits output */
  output      logic [width - 1:0]         oBits,
  /* direction of shift */
  input  wire t_enum_ShiftDir             dir
);
  `include "dfhdl_defs.svh"
  logic [width - 1:0] lshifter_iBits;
  logic [$clog2(width) - 1:0] lshifter_shift;
  logic [width - 1:0] lshifter_oBits;
  logic [width - 1:0] rshifter_iBits;
  logic [$clog2(width) - 1:0] rshifter_shift;
  logic [width - 1:0] rshifter_oBits;
  LeftShiftGen #(
    .width (width)
  ) lshifter(
    .iBits /*<--*/ (lshifter_iBits),
    .shift /*<--*/ (lshifter_shift),
    .oBits /*-->*/ (lshifter_oBits)
  );
  RightShiftGen #(
    .width (width)
  ) rshifter(
    .iBits /*<--*/ (rshifter_iBits),
    .shift /*<--*/ (rshifter_shift),
    .oBits /*-->*/ (rshifter_oBits)
  );
  assign lshifter_iBits = iBits;
  assign lshifter_shift = shift;
  assign rshifter_iBits = iBits;
  assign rshifter_shift = shift;
  always_comb
  begin
    case (dir)
      ShiftDir_Left:  oBits = lshifter_oBits;
      ShiftDir_Right: oBits = rshifter_oBits;
    endcase
  end
endmodule
