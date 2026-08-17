`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module subBytes(
  input  wire AESState state,
  output AESState      o
);
  `include "dfhdl_defs.svh"
  AESByte o_part_sbox_inst_00_lhs;
  AESByte o_part_sbox_inst_00_o;
  AESByte o_part_sbox_inst_01_lhs;
  AESByte o_part_sbox_inst_01_o;
  AESByte o_part_sbox_inst_02_lhs;
  AESByte o_part_sbox_inst_02_o;
  AESByte o_part_sbox_inst_03_lhs;
  AESByte o_part_sbox_inst_03_o;
  AESByte o_part_sbox_inst_04_lhs;
  AESByte o_part_sbox_inst_04_o;
  AESByte o_part_sbox_inst_05_lhs;
  AESByte o_part_sbox_inst_05_o;
  AESByte o_part_sbox_inst_06_lhs;
  AESByte o_part_sbox_inst_06_o;
  AESByte o_part_sbox_inst_07_lhs;
  AESByte o_part_sbox_inst_07_o;
  AESByte o_part_sbox_inst_08_lhs;
  AESByte o_part_sbox_inst_08_o;
  AESByte o_part_sbox_inst_09_lhs;
  AESByte o_part_sbox_inst_09_o;
  AESByte o_part_sbox_inst_10_lhs;
  AESByte o_part_sbox_inst_10_o;
  AESByte o_part_sbox_inst_11_lhs;
  AESByte o_part_sbox_inst_11_o;
  AESByte o_part_sbox_inst_12_lhs;
  AESByte o_part_sbox_inst_12_o;
  AESByte o_part_sbox_inst_13_lhs;
  AESByte o_part_sbox_inst_13_o;
  AESByte o_part_sbox_inst_14_lhs;
  AESByte o_part_sbox_inst_14_o;
  AESByte o_part_sbox_inst_15_lhs;
  AESByte o_part_sbox_inst_15_o;
  sbox o_part_sbox_inst_00(
    .lhs /*<--*/ (o_part_sbox_inst_00_lhs),
    .o   /*-->*/ (o_part_sbox_inst_00_o)
  );
  sbox o_part_sbox_inst_01(
    .lhs /*<--*/ (o_part_sbox_inst_01_lhs),
    .o   /*-->*/ (o_part_sbox_inst_01_o)
  );
  sbox o_part_sbox_inst_02(
    .lhs /*<--*/ (o_part_sbox_inst_02_lhs),
    .o   /*-->*/ (o_part_sbox_inst_02_o)
  );
  sbox o_part_sbox_inst_03(
    .lhs /*<--*/ (o_part_sbox_inst_03_lhs),
    .o   /*-->*/ (o_part_sbox_inst_03_o)
  );
  sbox o_part_sbox_inst_04(
    .lhs /*<--*/ (o_part_sbox_inst_04_lhs),
    .o   /*-->*/ (o_part_sbox_inst_04_o)
  );
  sbox o_part_sbox_inst_05(
    .lhs /*<--*/ (o_part_sbox_inst_05_lhs),
    .o   /*-->*/ (o_part_sbox_inst_05_o)
  );
  sbox o_part_sbox_inst_06(
    .lhs /*<--*/ (o_part_sbox_inst_06_lhs),
    .o   /*-->*/ (o_part_sbox_inst_06_o)
  );
  sbox o_part_sbox_inst_07(
    .lhs /*<--*/ (o_part_sbox_inst_07_lhs),
    .o   /*-->*/ (o_part_sbox_inst_07_o)
  );
  sbox o_part_sbox_inst_08(
    .lhs /*<--*/ (o_part_sbox_inst_08_lhs),
    .o   /*-->*/ (o_part_sbox_inst_08_o)
  );
  sbox o_part_sbox_inst_09(
    .lhs /*<--*/ (o_part_sbox_inst_09_lhs),
    .o   /*-->*/ (o_part_sbox_inst_09_o)
  );
  sbox o_part_sbox_inst_10(
    .lhs /*<--*/ (o_part_sbox_inst_10_lhs),
    .o   /*-->*/ (o_part_sbox_inst_10_o)
  );
  sbox o_part_sbox_inst_11(
    .lhs /*<--*/ (o_part_sbox_inst_11_lhs),
    .o   /*-->*/ (o_part_sbox_inst_11_o)
  );
  sbox o_part_sbox_inst_12(
    .lhs /*<--*/ (o_part_sbox_inst_12_lhs),
    .o   /*-->*/ (o_part_sbox_inst_12_o)
  );
  sbox o_part_sbox_inst_13(
    .lhs /*<--*/ (o_part_sbox_inst_13_lhs),
    .o   /*-->*/ (o_part_sbox_inst_13_o)
  );
  sbox o_part_sbox_inst_14(
    .lhs /*<--*/ (o_part_sbox_inst_14_lhs),
    .o   /*-->*/ (o_part_sbox_inst_14_o)
  );
  sbox o_part_sbox_inst_15(
    .lhs /*<--*/ (o_part_sbox_inst_15_lhs),
    .o   /*-->*/ (o_part_sbox_inst_15_o)
  );
  assign o_part_sbox_inst_00_lhs = state[0][0];
  assign o_part_sbox_inst_01_lhs = state[0][1];
  assign o_part_sbox_inst_02_lhs = state[0][2];
  assign o_part_sbox_inst_03_lhs = state[0][3];
  assign o_part_sbox_inst_04_lhs = state[1][0];
  assign o_part_sbox_inst_05_lhs = state[1][1];
  assign o_part_sbox_inst_06_lhs = state[1][2];
  assign o_part_sbox_inst_07_lhs = state[1][3];
  assign o_part_sbox_inst_08_lhs = state[2][0];
  assign o_part_sbox_inst_09_lhs = state[2][1];
  assign o_part_sbox_inst_10_lhs = state[2][2];
  assign o_part_sbox_inst_11_lhs = state[2][3];
  assign o_part_sbox_inst_12_lhs = state[3][0];
  assign o_part_sbox_inst_13_lhs = state[3][1];
  assign o_part_sbox_inst_14_lhs = state[3][2];
  assign o_part_sbox_inst_15_lhs = state[3][3];
  assign o = '{
    3: '{3: o_part_sbox_inst_15_o, 2: o_part_sbox_inst_14_o, 1: o_part_sbox_inst_13_o, 0: o_part_sbox_inst_12_o},
    2: '{3: o_part_sbox_inst_11_o, 2: o_part_sbox_inst_10_o, 1: o_part_sbox_inst_09_o, 0: o_part_sbox_inst_08_o},
    1: '{3: o_part_sbox_inst_07_o, 2: o_part_sbox_inst_06_o, 1: o_part_sbox_inst_05_o, 0: o_part_sbox_inst_04_o},
    0: '{3: o_part_sbox_inst_03_o, 2: o_part_sbox_inst_02_o, 1: o_part_sbox_inst_01_o, 0: o_part_sbox_inst_00_o}
  };
endmodule
