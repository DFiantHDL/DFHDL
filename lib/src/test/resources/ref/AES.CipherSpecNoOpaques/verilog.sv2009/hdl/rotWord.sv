`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module rotWord(
  input  wire t_opaque_AESWord lhs,
  output t_opaque_AESWord      o
);
  `include "dfhdl_defs.svh"
  assign o = '{0: lhs[1], 1: lhs[2], 2: lhs[3], 3: lhs[0]};
endmodule
