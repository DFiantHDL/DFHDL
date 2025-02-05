`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module rotWord(
  lhs,
  o
);
  `include "dfhdl_defs.vh"
  input  wire  [7:0] lhs [0:3];
  output wire [7:0] o [0:3];
  assign o = '{lhs[1], lhs[2], lhs[3], lhs[0]};
endmodule
