`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module sbox(
  input  wire  [7:0] lhs,
  output wire [7:0]  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  assign o = `sboxLookupTable[lhs];
endmodule
