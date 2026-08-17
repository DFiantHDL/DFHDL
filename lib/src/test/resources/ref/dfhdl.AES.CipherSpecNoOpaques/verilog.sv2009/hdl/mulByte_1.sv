`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module mulByte_1#(parameter logic [7:0] lhs = 8'hxx)(
  input  wire AESByte rhs,
  output AESByte      o
);
  `include "dfhdl_defs.svh"
  AESByte a_lhs;
  AESByte a_o;
  xtime a(
    .lhs /*<--*/ (a_lhs),
    .o   /*-->*/ (a_o)
  );
  assign a_lhs = rhs;
  assign o     = 8'h00 ^ rhs ^ a_o;
endmodule
