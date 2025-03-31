/* A left-right bits shifter, direct composition
  
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LRShiftDirect_defs.vh"

module LRShiftDirect(
  iBits,
  shift,
  oBits,
  dir
);
  `include "dfhdl_defs.vh"
  `include "LRShiftDirect_defs.vh"
  /* the width of the input and output bits */
  parameter integer width = 8;
  /* bits input */
  input  wire  [width - 1:0]        iBits;
  /* requested shift */
  input  wire  [clog2(width) - 1:0] shift;
  /* bits output */
  output reg [width - 1:0]          oBits;
  /* direction of shift */
  input  wire  [0:0]                dir;
  wire [width - 1:0] lshifter_iBits;
  wire [clog2(width) - 1:0] lshifter_shift;
  wire [width - 1:0] lshifter_oBits;
  wire [width - 1:0] rshifter_iBits;
  wire [clog2(width) - 1:0] rshifter_shift;
  wire [width - 1:0] rshifter_oBits;
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
  always @(dir or lshifter_oBits or rshifter_oBits)
  begin
    case (dir)
      `ShiftDir_Left: oBits = lshifter_oBits;
      `ShiftDir_Right: oBits = rshifter_oBits;
    endcase
  end
endmodule
