`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module sbox(
  lhs,
  o
);
  `include "dfhdl_defs.vh"
  input  wire  [7:0] lhs;
  output wire [7:0]  o;
  assign o = `sboxLookupTable[lhs];
endmodule
