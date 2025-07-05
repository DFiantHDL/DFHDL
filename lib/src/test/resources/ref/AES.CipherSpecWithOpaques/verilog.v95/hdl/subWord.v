`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module subWord(
  lhs,
  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  input  wire  [31:0] lhs;
  output wire [31:0]  o;
  wire [7:0] o_part_sbox_inst_lhs;
  wire [7:0] o_part_sbox_inst_o;
  wire [7:0] sbox_inst_0_lhs;
  wire [7:0] sbox_inst_0_o;
  wire [7:0] sbox_inst_1_lhs;
  wire [7:0] sbox_inst_1_o;
  wire [7:0] sbox_inst_2_lhs;
  wire [7:0] sbox_inst_2_o;
  sbox o_part_sbox_inst(
    .lhs /*<--*/ (o_part_sbox_inst_lhs),
    .o   /*-->*/ (o_part_sbox_inst_o)
  );
  sbox sbox_inst_0(
    .lhs /*<--*/ (sbox_inst_0_lhs),
    .o   /*-->*/ (sbox_inst_0_o)
  );
  sbox sbox_inst_1(
    .lhs /*<--*/ (sbox_inst_1_lhs),
    .o   /*-->*/ (sbox_inst_1_o)
  );
  sbox sbox_inst_2(
    .lhs /*<--*/ (sbox_inst_2_lhs),
    .o   /*-->*/ (sbox_inst_2_o)
  );
  assign o_part_sbox_inst_lhs = lhs[31:24];
  assign sbox_inst_0_lhs = lhs[23:16];
  assign sbox_inst_1_lhs = lhs[15:8];
  assign sbox_inst_2_lhs = lhs[7:0];
  assign o               = {o_part_sbox_inst_o, sbox_inst_0_o, sbox_inst_1_o, sbox_inst_2_o};
endmodule
