`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module mulByte_0(
  rhs,
  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  parameter [7:0] lhs = 8'h02;
  input  wire  [7:0] rhs;
  output wire [7:0]  o;
  wire [7:0] a_lhs;
  wire [7:0] a_o;
  xtime a(
    .lhs /*<--*/ (a_lhs),
    .o   /*-->*/ (a_o)
  );
  assign a_lhs = rhs;
  assign o     = 8'h00 ^ a_o;
endmodule
