`default_nettype	    none
`timescale 1ns/1ps
`include "Conc_defs.v"


module Conc(
  input  wire [31:0] i,
  input  wire [31:0] j,
  output reg  [31:0] a,
  output reg  [31:0] b,
  output reg  [31:0] c,
  output reg  [31:0] d,
  output reg  [31:0] e
);
  always @(*)
  begin
    a                = i + 5;
    b                = a * 3;
    c                = a + b;
    d                = i - 1;
    e                = j / 4;
  end
endmodule