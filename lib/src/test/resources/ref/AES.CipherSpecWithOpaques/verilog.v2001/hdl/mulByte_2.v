`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module mulByte_2#(parameter [7:0] lhs = 8'h01)(
  input  wire [7:0] rhs,
  output wire [7:0] o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  assign o = 8'h00 ^ rhs;
endmodule
