`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module cipher(
  input  wire t_opaque_AESData data,
  input  wire t_opaque_AESKey  key,
  output t_opaque_AESData      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESKey      keySchedule_key;
  t_opaque_AESKeySchedule keySchedule_o;
  t_opaque_AESState    state_00_state;
  t_opaque_AESRoundKey state_00_key;
  t_opaque_AESState    state_00_o;
  t_opaque_AESState    o_part_subBytes_inst_state;
  t_opaque_AESState    o_part_subBytes_inst_o;
  t_opaque_AESState    o_part_shiftRows_inst_state;
  t_opaque_AESState    o_part_shiftRows_inst_o;
  t_opaque_AESState    o_part_mixColumns_inst_state;
  t_opaque_AESState    o_part_mixColumns_inst_o;
  t_opaque_AESState    state_01_state;
  t_opaque_AESRoundKey state_01_key;
  t_opaque_AESState    state_01_o;
  t_opaque_AESState    subBytes_inst_0_state;
  t_opaque_AESState    subBytes_inst_0_o;
  t_opaque_AESState    shiftRows_inst_0_state;
  t_opaque_AESState    shiftRows_inst_0_o;
  t_opaque_AESState    mixColumns_inst_0_state;
  t_opaque_AESState    mixColumns_inst_0_o;
  t_opaque_AESState    state_02_state;
  t_opaque_AESRoundKey state_02_key;
  t_opaque_AESState    state_02_o;
  t_opaque_AESState    subBytes_inst_1_state;
  t_opaque_AESState    subBytes_inst_1_o;
  t_opaque_AESState    shiftRows_inst_1_state;
  t_opaque_AESState    shiftRows_inst_1_o;
  t_opaque_AESState    mixColumns_inst_1_state;
  t_opaque_AESState    mixColumns_inst_1_o;
  t_opaque_AESState    state_03_state;
  t_opaque_AESRoundKey state_03_key;
  t_opaque_AESState    state_03_o;
  t_opaque_AESState    subBytes_inst_2_state;
  t_opaque_AESState    subBytes_inst_2_o;
  t_opaque_AESState    shiftRows_inst_2_state;
  t_opaque_AESState    shiftRows_inst_2_o;
  t_opaque_AESState    mixColumns_inst_2_state;
  t_opaque_AESState    mixColumns_inst_2_o;
  t_opaque_AESState    state_04_state;
  t_opaque_AESRoundKey state_04_key;
  t_opaque_AESState    state_04_o;
  t_opaque_AESState    subBytes_inst_3_state;
  t_opaque_AESState    subBytes_inst_3_o;
  t_opaque_AESState    shiftRows_inst_3_state;
  t_opaque_AESState    shiftRows_inst_3_o;
  t_opaque_AESState    mixColumns_inst_3_state;
  t_opaque_AESState    mixColumns_inst_3_o;
  t_opaque_AESState    state_05_state;
  t_opaque_AESRoundKey state_05_key;
  t_opaque_AESState    state_05_o;
  t_opaque_AESState    subBytes_inst_4_state;
  t_opaque_AESState    subBytes_inst_4_o;
  t_opaque_AESState    shiftRows_inst_4_state;
  t_opaque_AESState    shiftRows_inst_4_o;
  t_opaque_AESState    mixColumns_inst_4_state;
  t_opaque_AESState    mixColumns_inst_4_o;
  t_opaque_AESState    state_06_state;
  t_opaque_AESRoundKey state_06_key;
  t_opaque_AESState    state_06_o;
  t_opaque_AESState    subBytes_inst_5_state;
  t_opaque_AESState    subBytes_inst_5_o;
  t_opaque_AESState    shiftRows_inst_5_state;
  t_opaque_AESState    shiftRows_inst_5_o;
  t_opaque_AESState    mixColumns_inst_5_state;
  t_opaque_AESState    mixColumns_inst_5_o;
  t_opaque_AESState    state_07_state;
  t_opaque_AESRoundKey state_07_key;
  t_opaque_AESState    state_07_o;
  t_opaque_AESState    subBytes_inst_6_state;
  t_opaque_AESState    subBytes_inst_6_o;
  t_opaque_AESState    shiftRows_inst_6_state;
  t_opaque_AESState    shiftRows_inst_6_o;
  t_opaque_AESState    mixColumns_inst_6_state;
  t_opaque_AESState    mixColumns_inst_6_o;
  t_opaque_AESState    state_08_state;
  t_opaque_AESRoundKey state_08_key;
  t_opaque_AESState    state_08_o;
  t_opaque_AESState    subBytes_inst_7_state;
  t_opaque_AESState    subBytes_inst_7_o;
  t_opaque_AESState    shiftRows_inst_7_state;
  t_opaque_AESState    shiftRows_inst_7_o;
  t_opaque_AESState    mixColumns_inst_7_state;
  t_opaque_AESState    mixColumns_inst_7_o;
  t_opaque_AESState    state_09_state;
  t_opaque_AESRoundKey state_09_key;
  t_opaque_AESState    state_09_o;
  t_opaque_AESState    subBytes_inst_8_state;
  t_opaque_AESState    subBytes_inst_8_o;
  t_opaque_AESState    shiftRows_inst_8_state;
  t_opaque_AESState    shiftRows_inst_8_o;
  t_opaque_AESState    state_10_state;
  t_opaque_AESRoundKey state_10_key;
  t_opaque_AESState    state_10_o;
  keyExpansion keySchedule(
    .key   /*<--*/ (keySchedule_key),
    .o     /*-->*/ (keySchedule_o)
  );
  addRoundKey state_00(
    .state /*<--*/ (state_00_state),
    .key   /*<--*/ (state_00_key),
    .o     /*-->*/ (state_00_o)
  );
  subBytes o_part_subBytes_inst(
    .state /*<--*/ (o_part_subBytes_inst_state),
    .o     /*-->*/ (o_part_subBytes_inst_o)
  );
  shiftRows o_part_shiftRows_inst(
    .state /*<--*/ (o_part_shiftRows_inst_state),
    .o     /*-->*/ (o_part_shiftRows_inst_o)
  );
  mixColumns o_part_mixColumns_inst(
    .state /*<--*/ (o_part_mixColumns_inst_state),
    .o     /*-->*/ (o_part_mixColumns_inst_o)
  );
  addRoundKey state_01(
    .state /*<--*/ (state_01_state),
    .key   /*<--*/ (state_01_key),
    .o     /*-->*/ (state_01_o)
  );
  subBytes subBytes_inst_0(
    .state /*<--*/ (subBytes_inst_0_state),
    .o     /*-->*/ (subBytes_inst_0_o)
  );
  shiftRows shiftRows_inst_0(
    .state /*<--*/ (shiftRows_inst_0_state),
    .o     /*-->*/ (shiftRows_inst_0_o)
  );
  mixColumns mixColumns_inst_0(
    .state /*<--*/ (mixColumns_inst_0_state),
    .o     /*-->*/ (mixColumns_inst_0_o)
  );
  addRoundKey state_02(
    .state /*<--*/ (state_02_state),
    .key   /*<--*/ (state_02_key),
    .o     /*-->*/ (state_02_o)
  );
  subBytes subBytes_inst_1(
    .state /*<--*/ (subBytes_inst_1_state),
    .o     /*-->*/ (subBytes_inst_1_o)
  );
  shiftRows shiftRows_inst_1(
    .state /*<--*/ (shiftRows_inst_1_state),
    .o     /*-->*/ (shiftRows_inst_1_o)
  );
  mixColumns mixColumns_inst_1(
    .state /*<--*/ (mixColumns_inst_1_state),
    .o     /*-->*/ (mixColumns_inst_1_o)
  );
  addRoundKey state_03(
    .state /*<--*/ (state_03_state),
    .key   /*<--*/ (state_03_key),
    .o     /*-->*/ (state_03_o)
  );
  subBytes subBytes_inst_2(
    .state /*<--*/ (subBytes_inst_2_state),
    .o     /*-->*/ (subBytes_inst_2_o)
  );
  shiftRows shiftRows_inst_2(
    .state /*<--*/ (shiftRows_inst_2_state),
    .o     /*-->*/ (shiftRows_inst_2_o)
  );
  mixColumns mixColumns_inst_2(
    .state /*<--*/ (mixColumns_inst_2_state),
    .o     /*-->*/ (mixColumns_inst_2_o)
  );
  addRoundKey state_04(
    .state /*<--*/ (state_04_state),
    .key   /*<--*/ (state_04_key),
    .o     /*-->*/ (state_04_o)
  );
  subBytes subBytes_inst_3(
    .state /*<--*/ (subBytes_inst_3_state),
    .o     /*-->*/ (subBytes_inst_3_o)
  );
  shiftRows shiftRows_inst_3(
    .state /*<--*/ (shiftRows_inst_3_state),
    .o     /*-->*/ (shiftRows_inst_3_o)
  );
  mixColumns mixColumns_inst_3(
    .state /*<--*/ (mixColumns_inst_3_state),
    .o     /*-->*/ (mixColumns_inst_3_o)
  );
  addRoundKey state_05(
    .state /*<--*/ (state_05_state),
    .key   /*<--*/ (state_05_key),
    .o     /*-->*/ (state_05_o)
  );
  subBytes subBytes_inst_4(
    .state /*<--*/ (subBytes_inst_4_state),
    .o     /*-->*/ (subBytes_inst_4_o)
  );
  shiftRows shiftRows_inst_4(
    .state /*<--*/ (shiftRows_inst_4_state),
    .o     /*-->*/ (shiftRows_inst_4_o)
  );
  mixColumns mixColumns_inst_4(
    .state /*<--*/ (mixColumns_inst_4_state),
    .o     /*-->*/ (mixColumns_inst_4_o)
  );
  addRoundKey state_06(
    .state /*<--*/ (state_06_state),
    .key   /*<--*/ (state_06_key),
    .o     /*-->*/ (state_06_o)
  );
  subBytes subBytes_inst_5(
    .state /*<--*/ (subBytes_inst_5_state),
    .o     /*-->*/ (subBytes_inst_5_o)
  );
  shiftRows shiftRows_inst_5(
    .state /*<--*/ (shiftRows_inst_5_state),
    .o     /*-->*/ (shiftRows_inst_5_o)
  );
  mixColumns mixColumns_inst_5(
    .state /*<--*/ (mixColumns_inst_5_state),
    .o     /*-->*/ (mixColumns_inst_5_o)
  );
  addRoundKey state_07(
    .state /*<--*/ (state_07_state),
    .key   /*<--*/ (state_07_key),
    .o     /*-->*/ (state_07_o)
  );
  subBytes subBytes_inst_6(
    .state /*<--*/ (subBytes_inst_6_state),
    .o     /*-->*/ (subBytes_inst_6_o)
  );
  shiftRows shiftRows_inst_6(
    .state /*<--*/ (shiftRows_inst_6_state),
    .o     /*-->*/ (shiftRows_inst_6_o)
  );
  mixColumns mixColumns_inst_6(
    .state /*<--*/ (mixColumns_inst_6_state),
    .o     /*-->*/ (mixColumns_inst_6_o)
  );
  addRoundKey state_08(
    .state /*<--*/ (state_08_state),
    .key   /*<--*/ (state_08_key),
    .o     /*-->*/ (state_08_o)
  );
  subBytes subBytes_inst_7(
    .state /*<--*/ (subBytes_inst_7_state),
    .o     /*-->*/ (subBytes_inst_7_o)
  );
  shiftRows shiftRows_inst_7(
    .state /*<--*/ (shiftRows_inst_7_state),
    .o     /*-->*/ (shiftRows_inst_7_o)
  );
  mixColumns mixColumns_inst_7(
    .state /*<--*/ (mixColumns_inst_7_state),
    .o     /*-->*/ (mixColumns_inst_7_o)
  );
  addRoundKey state_09(
    .state /*<--*/ (state_09_state),
    .key   /*<--*/ (state_09_key),
    .o     /*-->*/ (state_09_o)
  );
  subBytes subBytes_inst_8(
    .state /*<--*/ (subBytes_inst_8_state),
    .o     /*-->*/ (subBytes_inst_8_o)
  );
  shiftRows shiftRows_inst_8(
    .state /*<--*/ (shiftRows_inst_8_state),
    .o     /*-->*/ (shiftRows_inst_8_o)
  );
  addRoundKey state_10(
    .state /*<--*/ (state_10_state),
    .key   /*<--*/ (state_10_key),
    .o     /*-->*/ (state_10_o)
  );
  assign keySchedule_key              = key;
  assign state_00_state               = data;
  assign state_00_key                 = '{keySchedule_o[0], keySchedule_o[1], keySchedule_o[2], keySchedule_o[3]};
  assign o_part_subBytes_inst_state   = state_00_o;
  assign o_part_shiftRows_inst_state  = o_part_subBytes_inst_o;
  assign o_part_mixColumns_inst_state = o_part_shiftRows_inst_o;
  assign state_01_state               = o_part_mixColumns_inst_o;
  assign state_01_key                 = '{keySchedule_o[4], keySchedule_o[5], keySchedule_o[6], keySchedule_o[7]};
  assign subBytes_inst_0_state        = state_01_o;
  assign shiftRows_inst_0_state       = subBytes_inst_0_o;
  assign mixColumns_inst_0_state      = shiftRows_inst_0_o;
  assign state_02_state               = mixColumns_inst_0_o;
  assign state_02_key                 = '{keySchedule_o[8], keySchedule_o[9], keySchedule_o[10], keySchedule_o[11]};
  assign subBytes_inst_1_state        = state_02_o;
  assign shiftRows_inst_1_state       = subBytes_inst_1_o;
  assign mixColumns_inst_1_state      = shiftRows_inst_1_o;
  assign state_03_state               = mixColumns_inst_1_o;
  assign state_03_key                 = '{keySchedule_o[12], keySchedule_o[13], keySchedule_o[14], keySchedule_o[15]};
  assign subBytes_inst_2_state        = state_03_o;
  assign shiftRows_inst_2_state       = subBytes_inst_2_o;
  assign mixColumns_inst_2_state      = shiftRows_inst_2_o;
  assign state_04_state               = mixColumns_inst_2_o;
  assign state_04_key                 = '{keySchedule_o[16], keySchedule_o[17], keySchedule_o[18], keySchedule_o[19]};
  assign subBytes_inst_3_state        = state_04_o;
  assign shiftRows_inst_3_state       = subBytes_inst_3_o;
  assign mixColumns_inst_3_state      = shiftRows_inst_3_o;
  assign state_05_state               = mixColumns_inst_3_o;
  assign state_05_key                 = '{keySchedule_o[20], keySchedule_o[21], keySchedule_o[22], keySchedule_o[23]};
  assign subBytes_inst_4_state        = state_05_o;
  assign shiftRows_inst_4_state       = subBytes_inst_4_o;
  assign mixColumns_inst_4_state      = shiftRows_inst_4_o;
  assign state_06_state               = mixColumns_inst_4_o;
  assign state_06_key                 = '{keySchedule_o[24], keySchedule_o[25], keySchedule_o[26], keySchedule_o[27]};
  assign subBytes_inst_5_state        = state_06_o;
  assign shiftRows_inst_5_state       = subBytes_inst_5_o;
  assign mixColumns_inst_5_state      = shiftRows_inst_5_o;
  assign state_07_state               = mixColumns_inst_5_o;
  assign state_07_key                 = '{keySchedule_o[28], keySchedule_o[29], keySchedule_o[30], keySchedule_o[31]};
  assign subBytes_inst_6_state        = state_07_o;
  assign shiftRows_inst_6_state       = subBytes_inst_6_o;
  assign mixColumns_inst_6_state      = shiftRows_inst_6_o;
  assign state_08_state               = mixColumns_inst_6_o;
  assign state_08_key                 = '{keySchedule_o[32], keySchedule_o[33], keySchedule_o[34], keySchedule_o[35]};
  assign subBytes_inst_7_state        = state_08_o;
  assign shiftRows_inst_7_state       = subBytes_inst_7_o;
  assign mixColumns_inst_7_state      = shiftRows_inst_7_o;
  assign state_09_state               = mixColumns_inst_7_o;
  assign state_09_key                 = '{keySchedule_o[36], keySchedule_o[37], keySchedule_o[38], keySchedule_o[39]};
  assign subBytes_inst_8_state        = state_09_o;
  assign shiftRows_inst_8_state       = subBytes_inst_8_o;
  assign state_10_state               = shiftRows_inst_8_o;
  assign state_10_key                 = '{keySchedule_o[40], keySchedule_o[41], keySchedule_o[42], keySchedule_o[43]};
  assign o                            = state_10_o;
endmodule
