`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module rotWord(
  input  wire AESWord lhs,
  output AESWord      o
);
  `include "dfhdl_defs.svh"
  assign o = '{3: lhs[0], 2: lhs[3], 1: lhs[2], 0: lhs[1]};
endmodule
