/* A generic left shifter 
     
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LeftShiftGen_defs.svh"

module LeftShiftGen#(parameter int width = 8)(
  /* bits input */
  input  wire logic [width - 1:0]         iBits,
  /* requested shift */
  input  wire logic [$clog2(width) - 1:0] shift,
  /* bits output */
  output      logic [width - 1:0]         oBits
);
  `include "dfhdl_defs.svh"
  assign oBits = iBits << shift;
endmodule
