`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module subWord(
  input  wire t_opaque_AESWord lhs,
  output t_opaque_AESWord      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESByte o_part_sbox_inst_lhs;
  t_opaque_AESByte o_part_sbox_inst_o;
  t_opaque_AESByte sbox_inst_0_lhs;
  t_opaque_AESByte sbox_inst_0_o;
  t_opaque_AESByte sbox_inst_1_lhs;
  t_opaque_AESByte sbox_inst_1_o;
  t_opaque_AESByte sbox_inst_2_lhs;
  t_opaque_AESByte sbox_inst_2_o;
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
  assign o_part_sbox_inst_lhs = lhs[0];
  assign sbox_inst_0_lhs = lhs[1];
  assign sbox_inst_1_lhs = lhs[2];
  assign sbox_inst_2_lhs = lhs[3];
  assign o               = '{o_part_sbox_inst_o, sbox_inst_0_o, sbox_inst_1_o, sbox_inst_2_o};
endmodule
