/* A left-right bits shifter (flat version)
  
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LRShiftFlat_defs.svh"

module LRShiftFlat#(parameter int width = 8)(
  /* bits input */
  input  wire logic [width - 1:0]         iBits,
  /* requested shift */
  input  wire logic [$clog2(width) - 1:0] shift,
  /* direction of shift */
  input  wire t_enum_ShiftDir             dir,
  /* bits output */
  output      logic [width - 1:0]         oBits
);
  `include "dfhdl_defs.svh"
  always_comb
  begin
    case (dir)
      ShiftDir_Left:  oBits = iBits << shift;
      ShiftDir_Right: oBits = iBits >> shift;
    endcase
  end
endmodule
