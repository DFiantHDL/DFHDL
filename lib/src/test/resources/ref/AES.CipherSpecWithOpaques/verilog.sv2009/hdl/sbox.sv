`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module sbox(
  input  wire t_opaque_AESByte lhs,
  output t_opaque_AESByte      o
);
  `include "dfhdl_defs.svh"
  assign o = sboxLookupTable[lhs];
endmodule
