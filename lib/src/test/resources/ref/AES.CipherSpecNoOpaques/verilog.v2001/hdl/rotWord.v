`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module rotWord(
  input  wire  [31:0] lhs,
  output wire [31:0]  o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  assign o = {lhs[23:16], lhs[15:8], lhs[7:0], lhs[31:24]};
endmodule
