/* A two-bits left shifter */
`default_nettype none
`timescale 1ns/1ps
`include "LeftShift2_defs.vh"

module LeftShift2(
  /* bits input */
  input  wire  [7:0] iBits,
  /* bits output */
  output wire [7:0]  oBits
);
  `include "dfhdl_defs.vh"
  assign oBits = iBits << 2;
endmodule
