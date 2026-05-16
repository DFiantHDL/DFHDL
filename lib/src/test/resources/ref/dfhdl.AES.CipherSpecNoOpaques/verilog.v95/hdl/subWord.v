`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module subWord(
  lhs,
  o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  input  wire [31:0] lhs;
  output wire [31:0] o;
  wire [7:0] o_part_sbox_inst_0_lhs;
  wire [7:0] o_part_sbox_inst_0_o;
  wire [7:0] o_part_sbox_inst_1_lhs;
  wire [7:0] o_part_sbox_inst_1_o;
  wire [7:0] o_part_sbox_inst_2_lhs;
  wire [7:0] o_part_sbox_inst_2_o;
  wire [7:0] o_part_sbox_inst_3_lhs;
  wire [7:0] o_part_sbox_inst_3_o;
  sbox o_part_sbox_inst_0(
    .lhs /*<--*/ (o_part_sbox_inst_0_lhs),
    .o   /*-->*/ (o_part_sbox_inst_0_o)
  );
  sbox o_part_sbox_inst_1(
    .lhs /*<--*/ (o_part_sbox_inst_1_lhs),
    .o   /*-->*/ (o_part_sbox_inst_1_o)
  );
  sbox o_part_sbox_inst_2(
    .lhs /*<--*/ (o_part_sbox_inst_2_lhs),
    .o   /*-->*/ (o_part_sbox_inst_2_o)
  );
  sbox o_part_sbox_inst_3(
    .lhs /*<--*/ (o_part_sbox_inst_3_lhs),
    .o   /*-->*/ (o_part_sbox_inst_3_o)
  );
  assign o_part_sbox_inst_0_lhs = lhs[31:24];
  assign o_part_sbox_inst_1_lhs = lhs[23:16];
  assign o_part_sbox_inst_2_lhs = lhs[15:8];
  assign o_part_sbox_inst_3_lhs = lhs[7:0];
  assign o                      = {o_part_sbox_inst_0_o, o_part_sbox_inst_1_o, o_part_sbox_inst_2_o, o_part_sbox_inst_3_o};
endmodule
