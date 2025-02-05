`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module mulByte_2#(parameter logic [7:0] lhs)(
  input  wire  [7:0] rhs,
  output wire [7:0]  o
);
  `include "dfhdl_defs.vh"
  assign o = 8'h00 ^ rhs;
endmodule
