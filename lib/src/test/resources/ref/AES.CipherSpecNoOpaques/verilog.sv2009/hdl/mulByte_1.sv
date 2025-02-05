`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module mulByte_1#(parameter logic [7:0] lhs)(
  input  wire t_opaque_AESByte rhs,
  output t_opaque_AESByte      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESByte a_lhs;
  t_opaque_AESByte a_o;
  xtime a(
    .lhs /*<--*/ (a_lhs),
    .o   /*-->*/ (a_o)
  );
  assign a_lhs = rhs;
  assign o     = (8'h00 ^ rhs) ^ a_o;
endmodule
