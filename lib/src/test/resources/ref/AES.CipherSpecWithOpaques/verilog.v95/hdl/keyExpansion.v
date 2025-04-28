`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module keyExpansion(
  key,
  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  input  wire  [7:0] key [0:3] [0:3];
  output wire [7:0] o [0:43] [0:3];
  wire [7:0] w_0 [0:3];
  wire [7:0] w_1 [0:3];
  wire [7:0] w_2 [0:3];
  wire [7:0] w_3 [0:3];
  wire [7:0] keySchedule_part_000;
  wire [7:0] keySchedule_part_001;
  wire [7:0] keySchedule_part_002;
  wire [7:0] keySchedule_part_003;
  wire [7:0] keySchedule_part_004;
  wire [7:0] keySchedule_part_005;
  wire [7:0] keySchedule_part_006;
  wire [7:0] keySchedule_part_007;
  wire [7:0] keySchedule_part_008;
  wire [7:0] keySchedule_part_009;
  wire [7:0] keySchedule_part_010;
  wire [7:0] keySchedule_part_011;
  wire [7:0] lhs_part_00;
  wire [7:0] lhs_part_01;
  wire [7:0] lhs_part_02;
  wire [7:0] lhs_part_03;
  wire [7:0] lhs_part_04 [0:3];
  wire [7:0] keySchedule_part_012;
  wire [7:0] keySchedule_part_013;
  wire [7:0] keySchedule_part_014;
  wire [7:0] keySchedule_part_015;
  wire [7:0] keySchedule_part_016;
  wire [7:0] keySchedule_part_017;
  wire [7:0] keySchedule_part_018;
  wire [7:0] keySchedule_part_019;
  wire [7:0] keySchedule_part_020;
  wire [7:0] keySchedule_part_021;
  wire [7:0] keySchedule_part_022;
  wire [7:0] keySchedule_part_023;
  wire [7:0] lhs_part_05;
  wire [7:0] lhs_part_06;
  wire [7:0] lhs_part_07;
  wire [7:0] lhs_part_08;
  wire [7:0] lhs_part_09 [0:3];
  wire [7:0] keySchedule_part_024;
  wire [7:0] keySchedule_part_025;
  wire [7:0] keySchedule_part_026;
  wire [7:0] keySchedule_part_027;
  wire [7:0] keySchedule_part_028;
  wire [7:0] keySchedule_part_029;
  wire [7:0] keySchedule_part_030;
  wire [7:0] keySchedule_part_031;
  wire [7:0] keySchedule_part_032;
  wire [7:0] keySchedule_part_033;
  wire [7:0] keySchedule_part_034;
  wire [7:0] keySchedule_part_035;
  wire [7:0] lhs_part_10;
  wire [7:0] lhs_part_11;
  wire [7:0] lhs_part_12;
  wire [7:0] lhs_part_13;
  wire [7:0] lhs_part_14 [0:3];
  wire [7:0] keySchedule_part_036;
  wire [7:0] keySchedule_part_037;
  wire [7:0] keySchedule_part_038;
  wire [7:0] keySchedule_part_039;
  wire [7:0] keySchedule_part_040;
  wire [7:0] keySchedule_part_041;
  wire [7:0] keySchedule_part_042;
  wire [7:0] keySchedule_part_043;
  wire [7:0] keySchedule_part_044;
  wire [7:0] keySchedule_part_045;
  wire [7:0] keySchedule_part_046;
  wire [7:0] keySchedule_part_047;
  wire [7:0] lhs_part_15;
  wire [7:0] lhs_part_16;
  wire [7:0] lhs_part_17;
  wire [7:0] lhs_part_18;
  wire [7:0] lhs_part_19 [0:3];
  wire [7:0] keySchedule_part_048;
  wire [7:0] keySchedule_part_049;
  wire [7:0] keySchedule_part_050;
  wire [7:0] keySchedule_part_051;
  wire [7:0] keySchedule_part_052;
  wire [7:0] keySchedule_part_053;
  wire [7:0] keySchedule_part_054;
  wire [7:0] keySchedule_part_055;
  wire [7:0] keySchedule_part_056;
  wire [7:0] keySchedule_part_057;
  wire [7:0] keySchedule_part_058;
  wire [7:0] keySchedule_part_059;
  wire [7:0] lhs_part_20;
  wire [7:0] lhs_part_21;
  wire [7:0] lhs_part_22;
  wire [7:0] lhs_part_23;
  wire [7:0] lhs_part_24 [0:3];
  wire [7:0] keySchedule_part_060;
  wire [7:0] keySchedule_part_061;
  wire [7:0] keySchedule_part_062;
  wire [7:0] keySchedule_part_063;
  wire [7:0] keySchedule_part_064;
  wire [7:0] keySchedule_part_065;
  wire [7:0] keySchedule_part_066;
  wire [7:0] keySchedule_part_067;
  wire [7:0] keySchedule_part_068;
  wire [7:0] keySchedule_part_069;
  wire [7:0] keySchedule_part_070;
  wire [7:0] keySchedule_part_071;
  wire [7:0] lhs_part_25;
  wire [7:0] lhs_part_26;
  wire [7:0] lhs_part_27;
  wire [7:0] lhs_part_28;
  wire [7:0] lhs_part_29 [0:3];
  wire [7:0] keySchedule_part_072;
  wire [7:0] keySchedule_part_073;
  wire [7:0] keySchedule_part_074;
  wire [7:0] keySchedule_part_075;
  wire [7:0] keySchedule_part_076;
  wire [7:0] keySchedule_part_077;
  wire [7:0] keySchedule_part_078;
  wire [7:0] keySchedule_part_079;
  wire [7:0] keySchedule_part_080;
  wire [7:0] keySchedule_part_081;
  wire [7:0] keySchedule_part_082;
  wire [7:0] keySchedule_part_083;
  wire [7:0] lhs_part_30;
  wire [7:0] lhs_part_31;
  wire [7:0] lhs_part_32;
  wire [7:0] lhs_part_33;
  wire [7:0] lhs_part_34 [0:3];
  wire [7:0] keySchedule_part_084;
  wire [7:0] keySchedule_part_085;
  wire [7:0] keySchedule_part_086;
  wire [7:0] keySchedule_part_087;
  wire [7:0] keySchedule_part_088;
  wire [7:0] keySchedule_part_089;
  wire [7:0] keySchedule_part_090;
  wire [7:0] keySchedule_part_091;
  wire [7:0] keySchedule_part_092;
  wire [7:0] keySchedule_part_093;
  wire [7:0] keySchedule_part_094;
  wire [7:0] keySchedule_part_095;
  wire [7:0] lhs_part_35;
  wire [7:0] lhs_part_36;
  wire [7:0] lhs_part_37;
  wire [7:0] lhs_part_38;
  wire [7:0] lhs_part_39 [0:3];
  wire [7:0] keySchedule_part_096;
  wire [7:0] keySchedule_part_097;
  wire [7:0] keySchedule_part_098;
  wire [7:0] keySchedule_part_099;
  wire [7:0] keySchedule_part_100;
  wire [7:0] keySchedule_part_101;
  wire [7:0] keySchedule_part_102;
  wire [7:0] keySchedule_part_103;
  wire [7:0] keySchedule_part_104;
  wire [7:0] keySchedule_part_105;
  wire [7:0] keySchedule_part_106;
  wire [7:0] keySchedule_part_107;
  wire [7:0] lhs_part_40;
  wire [7:0] lhs_part_41;
  wire [7:0] lhs_part_42;
  wire [7:0] lhs_part_43;
  wire [7:0] lhs_part_44 [0:3];
  wire [7:0] keySchedule_part_108;
  wire [7:0] keySchedule_part_109;
  wire [7:0] keySchedule_part_110;
  wire [7:0] keySchedule_part_111;
  wire [7:0] keySchedule_part_112;
  wire [7:0] keySchedule_part_113;
  wire [7:0] keySchedule_part_114;
  wire [7:0] keySchedule_part_115;
  wire [7:0] keySchedule_part_116;
  wire [7:0] keySchedule_part_117;
  wire [7:0] keySchedule_part_118;
  wire [7:0] keySchedule_part_119;
  wire [7:0] keySchedule [0:43] [0:3];
  wire [7:0] o_part_rotWord_inst_o [0:3];
  wire [7:0] o_part_subWord_inst_lhs [0:3];
  wire [7:0] o_part_subWord_inst_o [0:3];
  wire [7:0] rotWord_inst_0_o [0:3];
  wire [7:0] subWord_inst_0_lhs [0:3];
  wire [7:0] subWord_inst_0_o [0:3];
  wire [7:0] rotWord_inst_1_o [0:3];
  wire [7:0] subWord_inst_1_lhs [0:3];
  wire [7:0] subWord_inst_1_o [0:3];
  wire [7:0] rotWord_inst_2_o [0:3];
  wire [7:0] subWord_inst_2_lhs [0:3];
  wire [7:0] subWord_inst_2_o [0:3];
  wire [7:0] rotWord_inst_3_o [0:3];
  wire [7:0] subWord_inst_3_lhs [0:3];
  wire [7:0] subWord_inst_3_o [0:3];
  wire [7:0] rotWord_inst_4_o [0:3];
  wire [7:0] subWord_inst_4_lhs [0:3];
  wire [7:0] subWord_inst_4_o [0:3];
  wire [7:0] rotWord_inst_5_o [0:3];
  wire [7:0] subWord_inst_5_lhs [0:3];
  wire [7:0] subWord_inst_5_o [0:3];
  wire [7:0] rotWord_inst_6_o [0:3];
  wire [7:0] subWord_inst_6_lhs [0:3];
  wire [7:0] subWord_inst_6_o [0:3];
  wire [7:0] rotWord_inst_7_o [0:3];
  wire [7:0] subWord_inst_7_lhs [0:3];
  wire [7:0] subWord_inst_7_o [0:3];
  wire [7:0] rotWord_inst_8_o [0:3];
  wire [7:0] subWord_inst_8_lhs [0:3];
  wire [7:0] subWord_inst_8_o [0:3];
  rotWord o_part_rotWord_inst(
    .o   /*-->*/ (o_part_rotWord_inst_o),
    .lhs /*<--*/ (w_3)
  );
  subWord o_part_subWord_inst(
    .lhs /*<--*/ (o_part_subWord_inst_lhs),
    .o   /*-->*/ (o_part_subWord_inst_o)
  );
  rotWord rotWord_inst_0(
    .o   /*-->*/ (rotWord_inst_0_o),
    .lhs /*<--*/ (lhs_part_04)
  );
  subWord subWord_inst_0(
    .lhs /*<--*/ (subWord_inst_0_lhs),
    .o   /*-->*/ (subWord_inst_0_o)
  );
  rotWord rotWord_inst_1(
    .o   /*-->*/ (rotWord_inst_1_o),
    .lhs /*<--*/ (lhs_part_09)
  );
  subWord subWord_inst_1(
    .lhs /*<--*/ (subWord_inst_1_lhs),
    .o   /*-->*/ (subWord_inst_1_o)
  );
  rotWord rotWord_inst_2(
    .o   /*-->*/ (rotWord_inst_2_o),
    .lhs /*<--*/ (lhs_part_14)
  );
  subWord subWord_inst_2(
    .lhs /*<--*/ (subWord_inst_2_lhs),
    .o   /*-->*/ (subWord_inst_2_o)
  );
  rotWord rotWord_inst_3(
    .o   /*-->*/ (rotWord_inst_3_o),
    .lhs /*<--*/ (lhs_part_19)
  );
  subWord subWord_inst_3(
    .lhs /*<--*/ (subWord_inst_3_lhs),
    .o   /*-->*/ (subWord_inst_3_o)
  );
  rotWord rotWord_inst_4(
    .o   /*-->*/ (rotWord_inst_4_o),
    .lhs /*<--*/ (lhs_part_24)
  );
  subWord subWord_inst_4(
    .lhs /*<--*/ (subWord_inst_4_lhs),
    .o   /*-->*/ (subWord_inst_4_o)
  );
  rotWord rotWord_inst_5(
    .o   /*-->*/ (rotWord_inst_5_o),
    .lhs /*<--*/ (lhs_part_29)
  );
  subWord subWord_inst_5(
    .lhs /*<--*/ (subWord_inst_5_lhs),
    .o   /*-->*/ (subWord_inst_5_o)
  );
  rotWord rotWord_inst_6(
    .o   /*-->*/ (rotWord_inst_6_o),
    .lhs /*<--*/ (lhs_part_34)
  );
  subWord subWord_inst_6(
    .lhs /*<--*/ (subWord_inst_6_lhs),
    .o   /*-->*/ (subWord_inst_6_o)
  );
  rotWord rotWord_inst_7(
    .o   /*-->*/ (rotWord_inst_7_o),
    .lhs /*<--*/ (lhs_part_39)
  );
  subWord subWord_inst_7(
    .lhs /*<--*/ (subWord_inst_7_lhs),
    .o   /*-->*/ (subWord_inst_7_o)
  );
  rotWord rotWord_inst_8(
    .o   /*-->*/ (rotWord_inst_8_o),
    .lhs /*<--*/ (lhs_part_44)
  );
  subWord subWord_inst_8(
    .lhs /*<--*/ (subWord_inst_8_lhs),
    .o   /*-->*/ (subWord_inst_8_o)
  );
  assign o_part_subWord_inst_lhs = o_part_rotWord_inst_o;
  assign subWord_inst_0_lhs      = rotWord_inst_0_o;
  assign subWord_inst_1_lhs      = rotWord_inst_1_o;
  assign subWord_inst_2_lhs      = rotWord_inst_2_o;
  assign subWord_inst_3_lhs      = rotWord_inst_3_o;
  assign subWord_inst_4_lhs      = rotWord_inst_4_o;
  assign subWord_inst_5_lhs      = rotWord_inst_5_o;
  assign subWord_inst_6_lhs      = rotWord_inst_6_o;
  assign subWord_inst_7_lhs      = rotWord_inst_7_o;
  assign subWord_inst_8_lhs      = rotWord_inst_8_o;
  assign o                       = keySchedule;
  assign w_0                     = key[0];
  assign w_1                     = key[1];
  assign w_2                     = key[2];
  assign w_3                     = key[3];
  assign keySchedule_part_000    = w_0[0] ^ (o_part_subWord_inst_o[0] ^ `Rcon[1][0]);
  assign keySchedule_part_001    = w_0[1] ^ (o_part_subWord_inst_o[1] ^ `Rcon[1][1]);
  assign keySchedule_part_002    = w_0[2] ^ (o_part_subWord_inst_o[2] ^ `Rcon[1][2]);
  assign keySchedule_part_003    = w_0[3] ^ (o_part_subWord_inst_o[3] ^ `Rcon[1][3]);
  assign keySchedule_part_004    = w_1[0] ^ keySchedule_part_000;
  assign keySchedule_part_005    = w_1[1] ^ keySchedule_part_001;
  assign keySchedule_part_006    = w_1[2] ^ keySchedule_part_002;
  assign keySchedule_part_007    = w_1[3] ^ keySchedule_part_003;
  assign keySchedule_part_008    = w_2[0] ^ keySchedule_part_004;
  assign keySchedule_part_009    = w_2[1] ^ keySchedule_part_005;
  assign keySchedule_part_010    = w_2[2] ^ keySchedule_part_006;
  assign keySchedule_part_011    = w_2[3] ^ keySchedule_part_007;
  assign lhs_part_00             = w_3[0] ^ keySchedule_part_008;
  assign lhs_part_01             = w_3[1] ^ keySchedule_part_009;
  assign lhs_part_02             = w_3[2] ^ keySchedule_part_010;
  assign lhs_part_03             = w_3[3] ^ keySchedule_part_011;
  assign lhs_part_04             = '{lhs_part_00, lhs_part_01, lhs_part_02, lhs_part_03};
  assign keySchedule_part_012    = keySchedule_part_000 ^ (subWord_inst_0_o[0] ^ `Rcon[2][0]);
  assign keySchedule_part_013    = keySchedule_part_001 ^ (subWord_inst_0_o[1] ^ `Rcon[2][1]);
  assign keySchedule_part_014    = keySchedule_part_002 ^ (subWord_inst_0_o[2] ^ `Rcon[2][2]);
  assign keySchedule_part_015    = keySchedule_part_003 ^ (subWord_inst_0_o[3] ^ `Rcon[2][3]);
  assign keySchedule_part_016    = keySchedule_part_004 ^ keySchedule_part_012;
  assign keySchedule_part_017    = keySchedule_part_005 ^ keySchedule_part_013;
  assign keySchedule_part_018    = keySchedule_part_006 ^ keySchedule_part_014;
  assign keySchedule_part_019    = keySchedule_part_007 ^ keySchedule_part_015;
  assign keySchedule_part_020    = keySchedule_part_008 ^ keySchedule_part_016;
  assign keySchedule_part_021    = keySchedule_part_009 ^ keySchedule_part_017;
  assign keySchedule_part_022    = keySchedule_part_010 ^ keySchedule_part_018;
  assign keySchedule_part_023    = keySchedule_part_011 ^ keySchedule_part_019;
  assign lhs_part_05             = lhs_part_00 ^ keySchedule_part_020;
  assign lhs_part_06             = lhs_part_01 ^ keySchedule_part_021;
  assign lhs_part_07             = lhs_part_02 ^ keySchedule_part_022;
  assign lhs_part_08             = lhs_part_03 ^ keySchedule_part_023;
  assign lhs_part_09             = '{lhs_part_05, lhs_part_06, lhs_part_07, lhs_part_08};
  assign keySchedule_part_024    = keySchedule_part_012 ^ (subWord_inst_1_o[0] ^ `Rcon[3][0]);
  assign keySchedule_part_025    = keySchedule_part_013 ^ (subWord_inst_1_o[1] ^ `Rcon[3][1]);
  assign keySchedule_part_026    = keySchedule_part_014 ^ (subWord_inst_1_o[2] ^ `Rcon[3][2]);
  assign keySchedule_part_027    = keySchedule_part_015 ^ (subWord_inst_1_o[3] ^ `Rcon[3][3]);
  assign keySchedule_part_028    = keySchedule_part_016 ^ keySchedule_part_024;
  assign keySchedule_part_029    = keySchedule_part_017 ^ keySchedule_part_025;
  assign keySchedule_part_030    = keySchedule_part_018 ^ keySchedule_part_026;
  assign keySchedule_part_031    = keySchedule_part_019 ^ keySchedule_part_027;
  assign keySchedule_part_032    = keySchedule_part_020 ^ keySchedule_part_028;
  assign keySchedule_part_033    = keySchedule_part_021 ^ keySchedule_part_029;
  assign keySchedule_part_034    = keySchedule_part_022 ^ keySchedule_part_030;
  assign keySchedule_part_035    = keySchedule_part_023 ^ keySchedule_part_031;
  assign lhs_part_10             = lhs_part_05 ^ keySchedule_part_032;
  assign lhs_part_11             = lhs_part_06 ^ keySchedule_part_033;
  assign lhs_part_12             = lhs_part_07 ^ keySchedule_part_034;
  assign lhs_part_13             = lhs_part_08 ^ keySchedule_part_035;
  assign lhs_part_14             = '{lhs_part_10, lhs_part_11, lhs_part_12, lhs_part_13};
  assign keySchedule_part_036    = keySchedule_part_024 ^ (subWord_inst_2_o[0] ^ `Rcon[4][0]);
  assign keySchedule_part_037    = keySchedule_part_025 ^ (subWord_inst_2_o[1] ^ `Rcon[4][1]);
  assign keySchedule_part_038    = keySchedule_part_026 ^ (subWord_inst_2_o[2] ^ `Rcon[4][2]);
  assign keySchedule_part_039    = keySchedule_part_027 ^ (subWord_inst_2_o[3] ^ `Rcon[4][3]);
  assign keySchedule_part_040    = keySchedule_part_028 ^ keySchedule_part_036;
  assign keySchedule_part_041    = keySchedule_part_029 ^ keySchedule_part_037;
  assign keySchedule_part_042    = keySchedule_part_030 ^ keySchedule_part_038;
  assign keySchedule_part_043    = keySchedule_part_031 ^ keySchedule_part_039;
  assign keySchedule_part_044    = keySchedule_part_032 ^ keySchedule_part_040;
  assign keySchedule_part_045    = keySchedule_part_033 ^ keySchedule_part_041;
  assign keySchedule_part_046    = keySchedule_part_034 ^ keySchedule_part_042;
  assign keySchedule_part_047    = keySchedule_part_035 ^ keySchedule_part_043;
  assign lhs_part_15             = lhs_part_10 ^ keySchedule_part_044;
  assign lhs_part_16             = lhs_part_11 ^ keySchedule_part_045;
  assign lhs_part_17             = lhs_part_12 ^ keySchedule_part_046;
  assign lhs_part_18             = lhs_part_13 ^ keySchedule_part_047;
  assign lhs_part_19             = '{lhs_part_15, lhs_part_16, lhs_part_17, lhs_part_18};
  assign keySchedule_part_048    = keySchedule_part_036 ^ (subWord_inst_3_o[0] ^ `Rcon[5][0]);
  assign keySchedule_part_049    = keySchedule_part_037 ^ (subWord_inst_3_o[1] ^ `Rcon[5][1]);
  assign keySchedule_part_050    = keySchedule_part_038 ^ (subWord_inst_3_o[2] ^ `Rcon[5][2]);
  assign keySchedule_part_051    = keySchedule_part_039 ^ (subWord_inst_3_o[3] ^ `Rcon[5][3]);
  assign keySchedule_part_052    = keySchedule_part_040 ^ keySchedule_part_048;
  assign keySchedule_part_053    = keySchedule_part_041 ^ keySchedule_part_049;
  assign keySchedule_part_054    = keySchedule_part_042 ^ keySchedule_part_050;
  assign keySchedule_part_055    = keySchedule_part_043 ^ keySchedule_part_051;
  assign keySchedule_part_056    = keySchedule_part_044 ^ keySchedule_part_052;
  assign keySchedule_part_057    = keySchedule_part_045 ^ keySchedule_part_053;
  assign keySchedule_part_058    = keySchedule_part_046 ^ keySchedule_part_054;
  assign keySchedule_part_059    = keySchedule_part_047 ^ keySchedule_part_055;
  assign lhs_part_20             = lhs_part_15 ^ keySchedule_part_056;
  assign lhs_part_21             = lhs_part_16 ^ keySchedule_part_057;
  assign lhs_part_22             = lhs_part_17 ^ keySchedule_part_058;
  assign lhs_part_23             = lhs_part_18 ^ keySchedule_part_059;
  assign lhs_part_24             = '{lhs_part_20, lhs_part_21, lhs_part_22, lhs_part_23};
  assign keySchedule_part_060    = keySchedule_part_048 ^ (subWord_inst_4_o[0] ^ `Rcon[6][0]);
  assign keySchedule_part_061    = keySchedule_part_049 ^ (subWord_inst_4_o[1] ^ `Rcon[6][1]);
  assign keySchedule_part_062    = keySchedule_part_050 ^ (subWord_inst_4_o[2] ^ `Rcon[6][2]);
  assign keySchedule_part_063    = keySchedule_part_051 ^ (subWord_inst_4_o[3] ^ `Rcon[6][3]);
  assign keySchedule_part_064    = keySchedule_part_052 ^ keySchedule_part_060;
  assign keySchedule_part_065    = keySchedule_part_053 ^ keySchedule_part_061;
  assign keySchedule_part_066    = keySchedule_part_054 ^ keySchedule_part_062;
  assign keySchedule_part_067    = keySchedule_part_055 ^ keySchedule_part_063;
  assign keySchedule_part_068    = keySchedule_part_056 ^ keySchedule_part_064;
  assign keySchedule_part_069    = keySchedule_part_057 ^ keySchedule_part_065;
  assign keySchedule_part_070    = keySchedule_part_058 ^ keySchedule_part_066;
  assign keySchedule_part_071    = keySchedule_part_059 ^ keySchedule_part_067;
  assign lhs_part_25             = lhs_part_20 ^ keySchedule_part_068;
  assign lhs_part_26             = lhs_part_21 ^ keySchedule_part_069;
  assign lhs_part_27             = lhs_part_22 ^ keySchedule_part_070;
  assign lhs_part_28             = lhs_part_23 ^ keySchedule_part_071;
  assign lhs_part_29             = '{lhs_part_25, lhs_part_26, lhs_part_27, lhs_part_28};
  assign keySchedule_part_072    = keySchedule_part_060 ^ (subWord_inst_5_o[0] ^ `Rcon[7][0]);
  assign keySchedule_part_073    = keySchedule_part_061 ^ (subWord_inst_5_o[1] ^ `Rcon[7][1]);
  assign keySchedule_part_074    = keySchedule_part_062 ^ (subWord_inst_5_o[2] ^ `Rcon[7][2]);
  assign keySchedule_part_075    = keySchedule_part_063 ^ (subWord_inst_5_o[3] ^ `Rcon[7][3]);
  assign keySchedule_part_076    = keySchedule_part_064 ^ keySchedule_part_072;
  assign keySchedule_part_077    = keySchedule_part_065 ^ keySchedule_part_073;
  assign keySchedule_part_078    = keySchedule_part_066 ^ keySchedule_part_074;
  assign keySchedule_part_079    = keySchedule_part_067 ^ keySchedule_part_075;
  assign keySchedule_part_080    = keySchedule_part_068 ^ keySchedule_part_076;
  assign keySchedule_part_081    = keySchedule_part_069 ^ keySchedule_part_077;
  assign keySchedule_part_082    = keySchedule_part_070 ^ keySchedule_part_078;
  assign keySchedule_part_083    = keySchedule_part_071 ^ keySchedule_part_079;
  assign lhs_part_30             = lhs_part_25 ^ keySchedule_part_080;
  assign lhs_part_31             = lhs_part_26 ^ keySchedule_part_081;
  assign lhs_part_32             = lhs_part_27 ^ keySchedule_part_082;
  assign lhs_part_33             = lhs_part_28 ^ keySchedule_part_083;
  assign lhs_part_34             = '{lhs_part_30, lhs_part_31, lhs_part_32, lhs_part_33};
  assign keySchedule_part_084    = keySchedule_part_072 ^ (subWord_inst_6_o[0] ^ `Rcon[8][0]);
  assign keySchedule_part_085    = keySchedule_part_073 ^ (subWord_inst_6_o[1] ^ `Rcon[8][1]);
  assign keySchedule_part_086    = keySchedule_part_074 ^ (subWord_inst_6_o[2] ^ `Rcon[8][2]);
  assign keySchedule_part_087    = keySchedule_part_075 ^ (subWord_inst_6_o[3] ^ `Rcon[8][3]);
  assign keySchedule_part_088    = keySchedule_part_076 ^ keySchedule_part_084;
  assign keySchedule_part_089    = keySchedule_part_077 ^ keySchedule_part_085;
  assign keySchedule_part_090    = keySchedule_part_078 ^ keySchedule_part_086;
  assign keySchedule_part_091    = keySchedule_part_079 ^ keySchedule_part_087;
  assign keySchedule_part_092    = keySchedule_part_080 ^ keySchedule_part_088;
  assign keySchedule_part_093    = keySchedule_part_081 ^ keySchedule_part_089;
  assign keySchedule_part_094    = keySchedule_part_082 ^ keySchedule_part_090;
  assign keySchedule_part_095    = keySchedule_part_083 ^ keySchedule_part_091;
  assign lhs_part_35             = lhs_part_30 ^ keySchedule_part_092;
  assign lhs_part_36             = lhs_part_31 ^ keySchedule_part_093;
  assign lhs_part_37             = lhs_part_32 ^ keySchedule_part_094;
  assign lhs_part_38             = lhs_part_33 ^ keySchedule_part_095;
  assign lhs_part_39             = '{lhs_part_35, lhs_part_36, lhs_part_37, lhs_part_38};
  assign keySchedule_part_096    = keySchedule_part_084 ^ (subWord_inst_7_o[0] ^ `Rcon[9][0]);
  assign keySchedule_part_097    = keySchedule_part_085 ^ (subWord_inst_7_o[1] ^ `Rcon[9][1]);
  assign keySchedule_part_098    = keySchedule_part_086 ^ (subWord_inst_7_o[2] ^ `Rcon[9][2]);
  assign keySchedule_part_099    = keySchedule_part_087 ^ (subWord_inst_7_o[3] ^ `Rcon[9][3]);
  assign keySchedule_part_100    = keySchedule_part_088 ^ keySchedule_part_096;
  assign keySchedule_part_101    = keySchedule_part_089 ^ keySchedule_part_097;
  assign keySchedule_part_102    = keySchedule_part_090 ^ keySchedule_part_098;
  assign keySchedule_part_103    = keySchedule_part_091 ^ keySchedule_part_099;
  assign keySchedule_part_104    = keySchedule_part_092 ^ keySchedule_part_100;
  assign keySchedule_part_105    = keySchedule_part_093 ^ keySchedule_part_101;
  assign keySchedule_part_106    = keySchedule_part_094 ^ keySchedule_part_102;
  assign keySchedule_part_107    = keySchedule_part_095 ^ keySchedule_part_103;
  assign lhs_part_40             = lhs_part_35 ^ keySchedule_part_104;
  assign lhs_part_41             = lhs_part_36 ^ keySchedule_part_105;
  assign lhs_part_42             = lhs_part_37 ^ keySchedule_part_106;
  assign lhs_part_43             = lhs_part_38 ^ keySchedule_part_107;
  assign lhs_part_44             = '{lhs_part_40, lhs_part_41, lhs_part_42, lhs_part_43};
  assign keySchedule_part_108    = keySchedule_part_096 ^ (subWord_inst_8_o[0] ^ `Rcon[10][0]);
  assign keySchedule_part_109    = keySchedule_part_097 ^ (subWord_inst_8_o[1] ^ `Rcon[10][1]);
  assign keySchedule_part_110    = keySchedule_part_098 ^ (subWord_inst_8_o[2] ^ `Rcon[10][2]);
  assign keySchedule_part_111    = keySchedule_part_099 ^ (subWord_inst_8_o[3] ^ `Rcon[10][3]);
  assign keySchedule_part_112    = keySchedule_part_100 ^ keySchedule_part_108;
  assign keySchedule_part_113    = keySchedule_part_101 ^ keySchedule_part_109;
  assign keySchedule_part_114    = keySchedule_part_102 ^ keySchedule_part_110;
  assign keySchedule_part_115    = keySchedule_part_103 ^ keySchedule_part_111;
  assign keySchedule_part_116    = keySchedule_part_104 ^ keySchedule_part_112;
  assign keySchedule_part_117    = keySchedule_part_105 ^ keySchedule_part_113;
  assign keySchedule_part_118    = keySchedule_part_106 ^ keySchedule_part_114;
  assign keySchedule_part_119    = keySchedule_part_107 ^ keySchedule_part_115;
  assign keySchedule = '{
    w_0,
    w_1,
    w_2,
    w_3,
    '{keySchedule_part_000, keySchedule_part_001, keySchedule_part_002, keySchedule_part_003},
    '{keySchedule_part_004, keySchedule_part_005, keySchedule_part_006, keySchedule_part_007},
    '{keySchedule_part_008, keySchedule_part_009, keySchedule_part_010, keySchedule_part_011},
    lhs_part_04,
    '{keySchedule_part_012, keySchedule_part_013, keySchedule_part_014, keySchedule_part_015},
    '{keySchedule_part_016, keySchedule_part_017, keySchedule_part_018, keySchedule_part_019},
    '{keySchedule_part_020, keySchedule_part_021, keySchedule_part_022, keySchedule_part_023},
    lhs_part_09,
    '{keySchedule_part_024, keySchedule_part_025, keySchedule_part_026, keySchedule_part_027},
    '{keySchedule_part_028, keySchedule_part_029, keySchedule_part_030, keySchedule_part_031},
    '{keySchedule_part_032, keySchedule_part_033, keySchedule_part_034, keySchedule_part_035},
    lhs_part_14,
    '{keySchedule_part_036, keySchedule_part_037, keySchedule_part_038, keySchedule_part_039},
    '{keySchedule_part_040, keySchedule_part_041, keySchedule_part_042, keySchedule_part_043},
    '{keySchedule_part_044, keySchedule_part_045, keySchedule_part_046, keySchedule_part_047},
    lhs_part_19,
    '{keySchedule_part_048, keySchedule_part_049, keySchedule_part_050, keySchedule_part_051},
    '{keySchedule_part_052, keySchedule_part_053, keySchedule_part_054, keySchedule_part_055},
    '{keySchedule_part_056, keySchedule_part_057, keySchedule_part_058, keySchedule_part_059},
    lhs_part_24,
    '{keySchedule_part_060, keySchedule_part_061, keySchedule_part_062, keySchedule_part_063},
    '{keySchedule_part_064, keySchedule_part_065, keySchedule_part_066, keySchedule_part_067},
    '{keySchedule_part_068, keySchedule_part_069, keySchedule_part_070, keySchedule_part_071},
    lhs_part_29,
    '{keySchedule_part_072, keySchedule_part_073, keySchedule_part_074, keySchedule_part_075},
    '{keySchedule_part_076, keySchedule_part_077, keySchedule_part_078, keySchedule_part_079},
    '{keySchedule_part_080, keySchedule_part_081, keySchedule_part_082, keySchedule_part_083},
    lhs_part_34,
    '{keySchedule_part_084, keySchedule_part_085, keySchedule_part_086, keySchedule_part_087},
    '{keySchedule_part_088, keySchedule_part_089, keySchedule_part_090, keySchedule_part_091},
    '{keySchedule_part_092, keySchedule_part_093, keySchedule_part_094, keySchedule_part_095},
    lhs_part_39,
    '{keySchedule_part_096, keySchedule_part_097, keySchedule_part_098, keySchedule_part_099},
    '{keySchedule_part_100, keySchedule_part_101, keySchedule_part_102, keySchedule_part_103},
    '{keySchedule_part_104, keySchedule_part_105, keySchedule_part_106, keySchedule_part_107},
    lhs_part_44,
    '{keySchedule_part_108, keySchedule_part_109, keySchedule_part_110, keySchedule_part_111},
    '{keySchedule_part_112, keySchedule_part_113, keySchedule_part_114, keySchedule_part_115},
    '{keySchedule_part_116, keySchedule_part_117, keySchedule_part_118, keySchedule_part_119},
    '{
      lhs_part_40 ^ keySchedule_part_116, lhs_part_41 ^ keySchedule_part_117,
      lhs_part_42 ^ keySchedule_part_118, lhs_part_43 ^ keySchedule_part_119
    }
  };
endmodule
