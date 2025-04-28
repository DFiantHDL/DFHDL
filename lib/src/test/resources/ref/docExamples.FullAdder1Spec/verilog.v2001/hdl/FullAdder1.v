`default_nettype none
`timescale 1ns/1ps
`include "FullAdder1_defs.vh"

module FullAdder1(
  input  wire a,
  input  wire b,
  input  wire c_in,
  output wire sum,
  output wire c_out
);
  `include "dfhdl_defs.vh"
  `include "FullAdder1_defs.vh"
  assign sum   = (a ^ b) ^ c_in;
  assign c_out = ((a & b) | (b & c_in)) | (c_in & a);
endmodule
