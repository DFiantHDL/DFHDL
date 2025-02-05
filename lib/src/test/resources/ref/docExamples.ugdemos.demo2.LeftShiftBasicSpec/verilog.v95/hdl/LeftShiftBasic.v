/* A basic left shifter */
`default_nettype none
`timescale 1ns/1ps
`include "LeftShiftBasic_defs.vh"

module LeftShiftBasic(
  iBits,
  shift,
  oBits
);
  `include "dfhdl_defs.vh"
  /* bits input */
  input  wire  [7:0] iBits;
  /* requested shift */
  input  wire  [2:0] shift;
  /* bits output */
  output wire [7:0]  oBits;
  assign oBits = iBits << shift;
endmodule
