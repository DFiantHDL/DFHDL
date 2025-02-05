`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module mulByte_1#(parameter logic [7:0] lhs)(
  input  wire  [7:0] rhs,
  output wire [7:0]  o
);
  `include "dfhdl_defs.vh"
  wire [7:0] a_lhs;
  wire [7:0] a_o;
  xtime a(
    .lhs /*<--*/ (a_lhs),
    .o   /*-->*/ (a_o)
  );
  assign a_lhs = rhs;
  assign o     = (8'h00 ^ rhs) ^ a_o;
endmodule
