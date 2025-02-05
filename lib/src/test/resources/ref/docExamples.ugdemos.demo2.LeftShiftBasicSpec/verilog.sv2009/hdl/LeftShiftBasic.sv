/* A basic left shifter */
`default_nettype none
`timescale 1ns/1ps
`include "LeftShiftBasic_defs.svh"

module LeftShiftBasic(
  /* bits input */
  input  wire logic [7:0] iBits,
  /* requested shift */
  input  wire logic [2:0] shift,
  /* bits output */
  output      logic [7:0] oBits
);
  `include "dfhdl_defs.svh"
  assign oBits = iBits << shift;
endmodule
