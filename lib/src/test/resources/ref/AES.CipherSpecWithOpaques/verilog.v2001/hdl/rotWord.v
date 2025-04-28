`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module rotWord(
  input  wire  [7:0] lhs [0:3],
  output wire [7:0] o [0:3]
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  assign o = '{lhs[1], lhs[2], lhs[3], lhs[0]};
endmodule
