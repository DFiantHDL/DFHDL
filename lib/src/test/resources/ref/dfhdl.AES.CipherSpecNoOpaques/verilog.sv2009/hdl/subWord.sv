`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module subWord(
  input  wire t_opaque_AESWord lhs,
  output t_opaque_AESWord      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESByte o_part_sbox_inst_0_lhs;
  t_opaque_AESByte o_part_sbox_inst_0_o;
  t_opaque_AESByte o_part_sbox_inst_1_lhs;
  t_opaque_AESByte o_part_sbox_inst_1_o;
  t_opaque_AESByte o_part_sbox_inst_2_lhs;
  t_opaque_AESByte o_part_sbox_inst_2_o;
  t_opaque_AESByte o_part_sbox_inst_3_lhs;
  t_opaque_AESByte o_part_sbox_inst_3_o;
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
  assign o_part_sbox_inst_0_lhs = lhs[0];
  assign o_part_sbox_inst_1_lhs = lhs[1];
  assign o_part_sbox_inst_2_lhs = lhs[2];
  assign o_part_sbox_inst_3_lhs = lhs[3];
  assign o                      = '{0: o_part_sbox_inst_0_o, 1: o_part_sbox_inst_1_o, 2: o_part_sbox_inst_2_o, 3: o_part_sbox_inst_3_o};
endmodule
