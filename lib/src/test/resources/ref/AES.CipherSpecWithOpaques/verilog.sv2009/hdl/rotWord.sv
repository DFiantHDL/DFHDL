`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module rotWord(
  input  wire t_opaque_AESWord lhs,
  output t_opaque_AESWord      o
);
  `include "dfhdl_defs.svh"
  assign o = '{lhs[1], lhs[2], lhs[3], lhs[0]};
endmodule
