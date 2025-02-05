`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module mulByte_2#(parameter logic [7:0] lhs)(
  input  wire t_opaque_AESByte rhs,
  output t_opaque_AESByte      o
);
  `include "dfhdl_defs.svh"
  assign o = 8'h00 ^ rhs;
endmodule
