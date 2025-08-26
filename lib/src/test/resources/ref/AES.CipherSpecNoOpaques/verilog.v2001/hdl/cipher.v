`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module cipher(
  input  wire [127:0] data,
  input  wire [127:0] key,
  output wire [127:0] o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  wire [127:0] keySchedule_key;
  wire [1407:0] keySchedule_o;
  wire [127:0] state_00_state;
  wire [127:0] state_00_key;
  wire [127:0] state_00_o;
  wire [127:0] o_part_subBytes_inst_state;
  wire [127:0] o_part_subBytes_inst_o;
  wire [127:0] o_part_shiftRows_inst_state;
  wire [127:0] o_part_shiftRows_inst_o;
  wire [127:0] o_part_mixColumns_inst_state;
  wire [127:0] o_part_mixColumns_inst_o;
  wire [127:0] state_01_state;
  wire [127:0] state_01_key;
  wire [127:0] state_01_o;
  wire [127:0] subBytes_inst_0_state;
  wire [127:0] subBytes_inst_0_o;
  wire [127:0] shiftRows_inst_0_state;
  wire [127:0] shiftRows_inst_0_o;
  wire [127:0] mixColumns_inst_0_state;
  wire [127:0] mixColumns_inst_0_o;
  wire [127:0] state_02_state;
  wire [127:0] state_02_key;
  wire [127:0] state_02_o;
  wire [127:0] subBytes_inst_1_state;
  wire [127:0] subBytes_inst_1_o;
  wire [127:0] shiftRows_inst_1_state;
  wire [127:0] shiftRows_inst_1_o;
  wire [127:0] mixColumns_inst_1_state;
  wire [127:0] mixColumns_inst_1_o;
  wire [127:0] state_03_state;
  wire [127:0] state_03_key;
  wire [127:0] state_03_o;
  wire [127:0] subBytes_inst_2_state;
  wire [127:0] subBytes_inst_2_o;
  wire [127:0] shiftRows_inst_2_state;
  wire [127:0] shiftRows_inst_2_o;
  wire [127:0] mixColumns_inst_2_state;
  wire [127:0] mixColumns_inst_2_o;
  wire [127:0] state_04_state;
  wire [127:0] state_04_key;
  wire [127:0] state_04_o;
  wire [127:0] subBytes_inst_3_state;
  wire [127:0] subBytes_inst_3_o;
  wire [127:0] shiftRows_inst_3_state;
  wire [127:0] shiftRows_inst_3_o;
  wire [127:0] mixColumns_inst_3_state;
  wire [127:0] mixColumns_inst_3_o;
  wire [127:0] state_05_state;
  wire [127:0] state_05_key;
  wire [127:0] state_05_o;
  wire [127:0] subBytes_inst_4_state;
  wire [127:0] subBytes_inst_4_o;
  wire [127:0] shiftRows_inst_4_state;
  wire [127:0] shiftRows_inst_4_o;
  wire [127:0] mixColumns_inst_4_state;
  wire [127:0] mixColumns_inst_4_o;
  wire [127:0] state_06_state;
  wire [127:0] state_06_key;
  wire [127:0] state_06_o;
  wire [127:0] subBytes_inst_5_state;
  wire [127:0] subBytes_inst_5_o;
  wire [127:0] shiftRows_inst_5_state;
  wire [127:0] shiftRows_inst_5_o;
  wire [127:0] mixColumns_inst_5_state;
  wire [127:0] mixColumns_inst_5_o;
  wire [127:0] state_07_state;
  wire [127:0] state_07_key;
  wire [127:0] state_07_o;
  wire [127:0] subBytes_inst_6_state;
  wire [127:0] subBytes_inst_6_o;
  wire [127:0] shiftRows_inst_6_state;
  wire [127:0] shiftRows_inst_6_o;
  wire [127:0] mixColumns_inst_6_state;
  wire [127:0] mixColumns_inst_6_o;
  wire [127:0] state_08_state;
  wire [127:0] state_08_key;
  wire [127:0] state_08_o;
  wire [127:0] subBytes_inst_7_state;
  wire [127:0] subBytes_inst_7_o;
  wire [127:0] shiftRows_inst_7_state;
  wire [127:0] shiftRows_inst_7_o;
  wire [127:0] mixColumns_inst_7_state;
  wire [127:0] mixColumns_inst_7_o;
  wire [127:0] state_09_state;
  wire [127:0] state_09_key;
  wire [127:0] state_09_o;
  wire [127:0] subBytes_inst_8_state;
  wire [127:0] subBytes_inst_8_o;
  wire [127:0] shiftRows_inst_8_state;
  wire [127:0] shiftRows_inst_8_o;
  wire [127:0] state_10_state;
  wire [127:0] state_10_key;
  wire [127:0] state_10_o;
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
  assign state_00_key                 = {keySchedule_o[1407:1376], keySchedule_o[1375:1344], keySchedule_o[1343:1312], keySchedule_o[1311:1280]};
  assign o_part_subBytes_inst_state   = state_00_o;
  assign o_part_shiftRows_inst_state  = o_part_subBytes_inst_o;
  assign o_part_mixColumns_inst_state = o_part_shiftRows_inst_o;
  assign state_01_state               = o_part_mixColumns_inst_o;
  assign state_01_key                 = {keySchedule_o[1279:1248], keySchedule_o[1247:1216], keySchedule_o[1215:1184], keySchedule_o[1183:1152]};
  assign subBytes_inst_0_state        = state_01_o;
  assign shiftRows_inst_0_state       = subBytes_inst_0_o;
  assign mixColumns_inst_0_state      = shiftRows_inst_0_o;
  assign state_02_state               = mixColumns_inst_0_o;
  assign state_02_key                 = {keySchedule_o[1151:1120], keySchedule_o[1119:1088], keySchedule_o[1087:1056], keySchedule_o[1055:1024]};
  assign subBytes_inst_1_state        = state_02_o;
  assign shiftRows_inst_1_state       = subBytes_inst_1_o;
  assign mixColumns_inst_1_state      = shiftRows_inst_1_o;
  assign state_03_state               = mixColumns_inst_1_o;
  assign state_03_key                 = {keySchedule_o[1023:992], keySchedule_o[991:960], keySchedule_o[959:928], keySchedule_o[927:896]};
  assign subBytes_inst_2_state        = state_03_o;
  assign shiftRows_inst_2_state       = subBytes_inst_2_o;
  assign mixColumns_inst_2_state      = shiftRows_inst_2_o;
  assign state_04_state               = mixColumns_inst_2_o;
  assign state_04_key                 = {keySchedule_o[895:864], keySchedule_o[863:832], keySchedule_o[831:800], keySchedule_o[799:768]};
  assign subBytes_inst_3_state        = state_04_o;
  assign shiftRows_inst_3_state       = subBytes_inst_3_o;
  assign mixColumns_inst_3_state      = shiftRows_inst_3_o;
  assign state_05_state               = mixColumns_inst_3_o;
  assign state_05_key                 = {keySchedule_o[767:736], keySchedule_o[735:704], keySchedule_o[703:672], keySchedule_o[671:640]};
  assign subBytes_inst_4_state        = state_05_o;
  assign shiftRows_inst_4_state       = subBytes_inst_4_o;
  assign mixColumns_inst_4_state      = shiftRows_inst_4_o;
  assign state_06_state               = mixColumns_inst_4_o;
  assign state_06_key                 = {keySchedule_o[639:608], keySchedule_o[607:576], keySchedule_o[575:544], keySchedule_o[543:512]};
  assign subBytes_inst_5_state        = state_06_o;
  assign shiftRows_inst_5_state       = subBytes_inst_5_o;
  assign mixColumns_inst_5_state      = shiftRows_inst_5_o;
  assign state_07_state               = mixColumns_inst_5_o;
  assign state_07_key                 = {keySchedule_o[511:480], keySchedule_o[479:448], keySchedule_o[447:416], keySchedule_o[415:384]};
  assign subBytes_inst_6_state        = state_07_o;
  assign shiftRows_inst_6_state       = subBytes_inst_6_o;
  assign mixColumns_inst_6_state      = shiftRows_inst_6_o;
  assign state_08_state               = mixColumns_inst_6_o;
  assign state_08_key                 = {keySchedule_o[383:352], keySchedule_o[351:320], keySchedule_o[319:288], keySchedule_o[287:256]};
  assign subBytes_inst_7_state        = state_08_o;
  assign shiftRows_inst_7_state       = subBytes_inst_7_o;
  assign mixColumns_inst_7_state      = shiftRows_inst_7_o;
  assign state_09_state               = mixColumns_inst_7_o;
  assign state_09_key                 = {keySchedule_o[255:224], keySchedule_o[223:192], keySchedule_o[191:160], keySchedule_o[159:128]};
  assign subBytes_inst_8_state        = state_09_o;
  assign shiftRows_inst_8_state       = subBytes_inst_8_o;
  assign state_10_state               = shiftRows_inst_8_o;
  assign state_10_key                 = {keySchedule_o[127:96], keySchedule_o[95:64], keySchedule_o[63:32], keySchedule_o[31:0]};
  assign o                            = state_10_o;
endmodule
