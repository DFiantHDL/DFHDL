`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module sbox(
  input  wire AESByte lhs,
  output AESByte      o
);
  `include "dfhdl_defs.svh"
  assign o = sboxLookupTable[lhs];
endmodule
