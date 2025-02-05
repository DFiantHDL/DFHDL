`default_nettype none
`timescale 1ns/1ps
`include "FullAdderN_defs.svh"

module FullAdder1(
  input  wire logic a,
  input  wire logic b,
  input  wire logic c_in,
  output      logic sum,
  output      logic c_out
);
  `include "dfhdl_defs.svh"
  assign sum   = (a ^ b) ^ c_in;
  assign c_out = ((a & b) | (b & c_in)) | (c_in & a);
endmodule
