/* A generic left shifter 
     
   @param width
     the width of the input and output bits
  */
`default_nettype none
`timescale 1ns/1ps
`include "LRShiftDirect_defs.vh"

module LeftShiftGen(
  iBits,
  shift,
  oBits
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
  output wire [width - 1:0]         oBits;
  assign oBits = iBits << shift;
endmodule
