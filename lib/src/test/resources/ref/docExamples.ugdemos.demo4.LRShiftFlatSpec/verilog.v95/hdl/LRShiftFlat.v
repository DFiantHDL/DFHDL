/* A left-right bits shifter (flat version)
  
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LRShiftFlat_defs.vh"

module LRShiftFlat(
  iBits,
  shift,
  dir,
  oBits
);
  `include "dfhdl_defs.vh"
  parameter integer width = 8;
  /* bits input */
  input  wire  [width - 1:0]        iBits;
  /* requested shift */
  input  wire  [clog2(width) - 1:0] shift;
  /* direction of shift */
  input  wire  [0:0]                dir;
  /* bits output */
  output reg [width - 1:0]          oBits;
  always @(dir or iBits or shift)
  begin
    case (dir)
      `ShiftDir_Left: oBits = iBits << shift;
      `ShiftDir_Right: oBits = iBits >> shift;
    endcase
  end
endmodule
