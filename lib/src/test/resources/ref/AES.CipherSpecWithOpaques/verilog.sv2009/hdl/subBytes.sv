`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module subBytes(
  input  wire t_opaque_AESState state,
  output t_opaque_AESState      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESByte sbox_inst_00_lhs;
  t_opaque_AESByte sbox_inst_00_o;
  t_opaque_AESByte sbox_inst_01_lhs;
  t_opaque_AESByte sbox_inst_01_o;
  t_opaque_AESByte sbox_inst_02_lhs;
  t_opaque_AESByte sbox_inst_02_o;
  t_opaque_AESByte sbox_inst_03_lhs;
  t_opaque_AESByte sbox_inst_03_o;
  t_opaque_AESByte sbox_inst_04_lhs;
  t_opaque_AESByte sbox_inst_04_o;
  t_opaque_AESByte sbox_inst_05_lhs;
  t_opaque_AESByte sbox_inst_05_o;
  t_opaque_AESByte sbox_inst_06_lhs;
  t_opaque_AESByte sbox_inst_06_o;
  t_opaque_AESByte sbox_inst_07_lhs;
  t_opaque_AESByte sbox_inst_07_o;
  t_opaque_AESByte sbox_inst_08_lhs;
  t_opaque_AESByte sbox_inst_08_o;
  t_opaque_AESByte sbox_inst_09_lhs;
  t_opaque_AESByte sbox_inst_09_o;
  t_opaque_AESByte sbox_inst_10_lhs;
  t_opaque_AESByte sbox_inst_10_o;
  t_opaque_AESByte sbox_inst_11_lhs;
  t_opaque_AESByte sbox_inst_11_o;
  t_opaque_AESByte sbox_inst_12_lhs;
  t_opaque_AESByte sbox_inst_12_o;
  t_opaque_AESByte sbox_inst_13_lhs;
  t_opaque_AESByte sbox_inst_13_o;
  t_opaque_AESByte sbox_inst_14_lhs;
  t_opaque_AESByte sbox_inst_14_o;
  t_opaque_AESByte sbox_inst_15_lhs;
  t_opaque_AESByte sbox_inst_15_o;
  sbox sbox_inst_00(
    .lhs /*<--*/ (sbox_inst_00_lhs),
    .o   /*-->*/ (sbox_inst_00_o)
  );
  sbox sbox_inst_01(
    .lhs /*<--*/ (sbox_inst_01_lhs),
    .o   /*-->*/ (sbox_inst_01_o)
  );
  sbox sbox_inst_02(
    .lhs /*<--*/ (sbox_inst_02_lhs),
    .o   /*-->*/ (sbox_inst_02_o)
  );
  sbox sbox_inst_03(
    .lhs /*<--*/ (sbox_inst_03_lhs),
    .o   /*-->*/ (sbox_inst_03_o)
  );
  sbox sbox_inst_04(
    .lhs /*<--*/ (sbox_inst_04_lhs),
    .o   /*-->*/ (sbox_inst_04_o)
  );
  sbox sbox_inst_05(
    .lhs /*<--*/ (sbox_inst_05_lhs),
    .o   /*-->*/ (sbox_inst_05_o)
  );
  sbox sbox_inst_06(
    .lhs /*<--*/ (sbox_inst_06_lhs),
    .o   /*-->*/ (sbox_inst_06_o)
  );
  sbox sbox_inst_07(
    .lhs /*<--*/ (sbox_inst_07_lhs),
    .o   /*-->*/ (sbox_inst_07_o)
  );
  sbox sbox_inst_08(
    .lhs /*<--*/ (sbox_inst_08_lhs),
    .o   /*-->*/ (sbox_inst_08_o)
  );
  sbox sbox_inst_09(
    .lhs /*<--*/ (sbox_inst_09_lhs),
    .o   /*-->*/ (sbox_inst_09_o)
  );
  sbox sbox_inst_10(
    .lhs /*<--*/ (sbox_inst_10_lhs),
    .o   /*-->*/ (sbox_inst_10_o)
  );
  sbox sbox_inst_11(
    .lhs /*<--*/ (sbox_inst_11_lhs),
    .o   /*-->*/ (sbox_inst_11_o)
  );
  sbox sbox_inst_12(
    .lhs /*<--*/ (sbox_inst_12_lhs),
    .o   /*-->*/ (sbox_inst_12_o)
  );
  sbox sbox_inst_13(
    .lhs /*<--*/ (sbox_inst_13_lhs),
    .o   /*-->*/ (sbox_inst_13_o)
  );
  sbox sbox_inst_14(
    .lhs /*<--*/ (sbox_inst_14_lhs),
    .o   /*-->*/ (sbox_inst_14_o)
  );
  sbox sbox_inst_15(
    .lhs /*<--*/ (sbox_inst_15_lhs),
    .o   /*-->*/ (sbox_inst_15_o)
  );
  assign sbox_inst_00_lhs = state[0][0];
  assign sbox_inst_01_lhs = state[0][1];
  assign sbox_inst_02_lhs = state[0][2];
  assign sbox_inst_03_lhs = state[0][3];
  assign sbox_inst_04_lhs = state[1][0];
  assign sbox_inst_05_lhs = state[1][1];
  assign sbox_inst_06_lhs = state[1][2];
  assign sbox_inst_07_lhs = state[1][3];
  assign sbox_inst_08_lhs = state[2][0];
  assign sbox_inst_09_lhs = state[2][1];
  assign sbox_inst_10_lhs = state[2][2];
  assign sbox_inst_11_lhs = state[2][3];
  assign sbox_inst_12_lhs = state[3][0];
  assign sbox_inst_13_lhs = state[3][1];
  assign sbox_inst_14_lhs = state[3][2];
  assign sbox_inst_15_lhs = state[3][3];
  assign o = '{
    '{sbox_inst_00_o, sbox_inst_01_o, sbox_inst_02_o, sbox_inst_03_o},
    '{sbox_inst_04_o, sbox_inst_05_o, sbox_inst_06_o, sbox_inst_07_o},
    '{sbox_inst_08_o, sbox_inst_09_o, sbox_inst_10_o, sbox_inst_11_o},
    '{sbox_inst_12_o, sbox_inst_13_o, sbox_inst_14_o, sbox_inst_15_o}
  };
endmodule
