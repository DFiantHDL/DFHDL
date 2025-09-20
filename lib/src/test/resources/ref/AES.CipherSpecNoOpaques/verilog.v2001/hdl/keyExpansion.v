`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module keyExpansion(
  input  wire [127:0]  key,
  output wire [1407:0] o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  `Rcon_def
  wire [31:0] w_0;
  wire [31:0] w_1;
  wire [31:0] w_2;
  wire [31:0] w_3;
  wire [7:0] o_part_000;
  wire [7:0] o_part_001;
  wire [7:0] o_part_002;
  wire [7:0] o_part_003;
  wire [7:0] o_part_004;
  wire [7:0] o_part_005;
  wire [7:0] o_part_006;
  wire [7:0] o_part_007;
  wire [7:0] o_part_008;
  wire [7:0] o_part_009;
  wire [7:0] o_part_010;
  wire [7:0] o_part_011;
  wire [7:0] lhs_part_00;
  wire [7:0] lhs_part_01;
  wire [7:0] lhs_part_02;
  wire [7:0] lhs_part_03;
  wire [31:0] lhs_part_04;
  wire [7:0] o_part_012;
  wire [7:0] o_part_013;
  wire [7:0] o_part_014;
  wire [7:0] o_part_015;
  wire [7:0] o_part_016;
  wire [7:0] o_part_017;
  wire [7:0] o_part_018;
  wire [7:0] o_part_019;
  wire [7:0] o_part_020;
  wire [7:0] o_part_021;
  wire [7:0] o_part_022;
  wire [7:0] o_part_023;
  wire [7:0] lhs_part_05;
  wire [7:0] lhs_part_06;
  wire [7:0] lhs_part_07;
  wire [7:0] lhs_part_08;
  wire [31:0] lhs_part_09;
  wire [7:0] o_part_024;
  wire [7:0] o_part_025;
  wire [7:0] o_part_026;
  wire [7:0] o_part_027;
  wire [7:0] o_part_028;
  wire [7:0] o_part_029;
  wire [7:0] o_part_030;
  wire [7:0] o_part_031;
  wire [7:0] o_part_032;
  wire [7:0] o_part_033;
  wire [7:0] o_part_034;
  wire [7:0] o_part_035;
  wire [7:0] lhs_part_10;
  wire [7:0] lhs_part_11;
  wire [7:0] lhs_part_12;
  wire [7:0] lhs_part_13;
  wire [31:0] lhs_part_14;
  wire [7:0] o_part_036;
  wire [7:0] o_part_037;
  wire [7:0] o_part_038;
  wire [7:0] o_part_039;
  wire [7:0] o_part_040;
  wire [7:0] o_part_041;
  wire [7:0] o_part_042;
  wire [7:0] o_part_043;
  wire [7:0] o_part_044;
  wire [7:0] o_part_045;
  wire [7:0] o_part_046;
  wire [7:0] o_part_047;
  wire [7:0] lhs_part_15;
  wire [7:0] lhs_part_16;
  wire [7:0] lhs_part_17;
  wire [7:0] lhs_part_18;
  wire [31:0] lhs_part_19;
  wire [7:0] o_part_048;
  wire [7:0] o_part_049;
  wire [7:0] o_part_050;
  wire [7:0] o_part_051;
  wire [7:0] o_part_052;
  wire [7:0] o_part_053;
  wire [7:0] o_part_054;
  wire [7:0] o_part_055;
  wire [7:0] o_part_056;
  wire [7:0] o_part_057;
  wire [7:0] o_part_058;
  wire [7:0] o_part_059;
  wire [7:0] lhs_part_20;
  wire [7:0] lhs_part_21;
  wire [7:0] lhs_part_22;
  wire [7:0] lhs_part_23;
  wire [31:0] lhs_part_24;
  wire [7:0] o_part_060;
  wire [7:0] o_part_061;
  wire [7:0] o_part_062;
  wire [7:0] o_part_063;
  wire [7:0] o_part_064;
  wire [7:0] o_part_065;
  wire [7:0] o_part_066;
  wire [7:0] o_part_067;
  wire [7:0] o_part_068;
  wire [7:0] o_part_069;
  wire [7:0] o_part_070;
  wire [7:0] o_part_071;
  wire [7:0] lhs_part_25;
  wire [7:0] lhs_part_26;
  wire [7:0] lhs_part_27;
  wire [7:0] lhs_part_28;
  wire [31:0] lhs_part_29;
  wire [7:0] o_part_072;
  wire [7:0] o_part_073;
  wire [7:0] o_part_074;
  wire [7:0] o_part_075;
  wire [7:0] o_part_076;
  wire [7:0] o_part_077;
  wire [7:0] o_part_078;
  wire [7:0] o_part_079;
  wire [7:0] o_part_080;
  wire [7:0] o_part_081;
  wire [7:0] o_part_082;
  wire [7:0] o_part_083;
  wire [7:0] lhs_part_30;
  wire [7:0] lhs_part_31;
  wire [7:0] lhs_part_32;
  wire [7:0] lhs_part_33;
  wire [31:0] lhs_part_34;
  wire [7:0] o_part_084;
  wire [7:0] o_part_085;
  wire [7:0] o_part_086;
  wire [7:0] o_part_087;
  wire [7:0] o_part_088;
  wire [7:0] o_part_089;
  wire [7:0] o_part_090;
  wire [7:0] o_part_091;
  wire [7:0] o_part_092;
  wire [7:0] o_part_093;
  wire [7:0] o_part_094;
  wire [7:0] o_part_095;
  wire [7:0] lhs_part_35;
  wire [7:0] lhs_part_36;
  wire [7:0] lhs_part_37;
  wire [7:0] lhs_part_38;
  wire [31:0] lhs_part_39;
  wire [7:0] o_part_096;
  wire [7:0] o_part_097;
  wire [7:0] o_part_098;
  wire [7:0] o_part_099;
  wire [7:0] o_part_100;
  wire [7:0] o_part_101;
  wire [7:0] o_part_102;
  wire [7:0] o_part_103;
  wire [7:0] o_part_104;
  wire [7:0] o_part_105;
  wire [7:0] o_part_106;
  wire [7:0] o_part_107;
  wire [7:0] lhs_part_40;
  wire [7:0] lhs_part_41;
  wire [7:0] lhs_part_42;
  wire [7:0] lhs_part_43;
  wire [31:0] lhs_part_44;
  wire [7:0] o_part_108;
  wire [7:0] o_part_109;
  wire [7:0] o_part_110;
  wire [7:0] o_part_111;
  wire [7:0] o_part_112;
  wire [7:0] o_part_113;
  wire [7:0] o_part_114;
  wire [7:0] o_part_115;
  wire [7:0] o_part_116;
  wire [7:0] o_part_117;
  wire [7:0] o_part_118;
  wire [7:0] o_part_119;
  wire [31:0] o_part_rotWord_inst_o;
  wire [31:0] o_part_subWord_inst_lhs;
  wire [31:0] o_part_subWord_inst_o;
  wire [31:0] rotWord_inst_0_o;
  wire [31:0] subWord_inst_0_lhs;
  wire [31:0] subWord_inst_0_o;
  wire [31:0] rotWord_inst_1_o;
  wire [31:0] subWord_inst_1_lhs;
  wire [31:0] subWord_inst_1_o;
  wire [31:0] rotWord_inst_2_o;
  wire [31:0] subWord_inst_2_lhs;
  wire [31:0] subWord_inst_2_o;
  wire [31:0] rotWord_inst_3_o;
  wire [31:0] subWord_inst_3_lhs;
  wire [31:0] subWord_inst_3_o;
  wire [31:0] rotWord_inst_4_o;
  wire [31:0] subWord_inst_4_lhs;
  wire [31:0] subWord_inst_4_o;
  wire [31:0] rotWord_inst_5_o;
  wire [31:0] subWord_inst_5_lhs;
  wire [31:0] subWord_inst_5_o;
  wire [31:0] rotWord_inst_6_o;
  wire [31:0] subWord_inst_6_lhs;
  wire [31:0] subWord_inst_6_o;
  wire [31:0] rotWord_inst_7_o;
  wire [31:0] subWord_inst_7_lhs;
  wire [31:0] subWord_inst_7_o;
  wire [31:0] rotWord_inst_8_o;
  wire [31:0] subWord_inst_8_lhs;
  wire [31:0] subWord_inst_8_o;
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
  assign subWord_inst_0_lhs = rotWord_inst_0_o;
  assign subWord_inst_1_lhs = rotWord_inst_1_o;
  assign subWord_inst_2_lhs = rotWord_inst_2_o;
  assign subWord_inst_3_lhs = rotWord_inst_3_o;
  assign subWord_inst_4_lhs = rotWord_inst_4_o;
  assign subWord_inst_5_lhs = rotWord_inst_5_o;
  assign subWord_inst_6_lhs = rotWord_inst_6_o;
  assign subWord_inst_7_lhs = rotWord_inst_7_o;
  assign subWord_inst_8_lhs = rotWord_inst_8_o;
  assign o = {
    w_0, w_1,
    w_2, w_3,
    {o_part_000, o_part_001, o_part_002, o_part_003}, {o_part_004, o_part_005, o_part_006, o_part_007},
    {o_part_008, o_part_009, o_part_010, o_part_011}, lhs_part_04,
    {o_part_012, o_part_013, o_part_014, o_part_015}, {o_part_016, o_part_017, o_part_018, o_part_019},
    {o_part_020, o_part_021, o_part_022, o_part_023}, lhs_part_09,
    {o_part_024, o_part_025, o_part_026, o_part_027}, {o_part_028, o_part_029, o_part_030, o_part_031},
    {o_part_032, o_part_033, o_part_034, o_part_035}, lhs_part_14,
    {o_part_036, o_part_037, o_part_038, o_part_039}, {o_part_040, o_part_041, o_part_042, o_part_043},
    {o_part_044, o_part_045, o_part_046, o_part_047}, lhs_part_19,
    {o_part_048, o_part_049, o_part_050, o_part_051}, {o_part_052, o_part_053, o_part_054, o_part_055},
    {o_part_056, o_part_057, o_part_058, o_part_059}, lhs_part_24,
    {o_part_060, o_part_061, o_part_062, o_part_063}, {o_part_064, o_part_065, o_part_066, o_part_067},
    {o_part_068, o_part_069, o_part_070, o_part_071}, lhs_part_29,
    {o_part_072, o_part_073, o_part_074, o_part_075}, {o_part_076, o_part_077, o_part_078, o_part_079},
    {o_part_080, o_part_081, o_part_082, o_part_083}, lhs_part_34,
    {o_part_084, o_part_085, o_part_086, o_part_087}, {o_part_088, o_part_089, o_part_090, o_part_091},
    {o_part_092, o_part_093, o_part_094, o_part_095}, lhs_part_39,
    {o_part_096, o_part_097, o_part_098, o_part_099}, {o_part_100, o_part_101, o_part_102, o_part_103},
    {o_part_104, o_part_105, o_part_106, o_part_107}, lhs_part_44,
    {o_part_108, o_part_109, o_part_110, o_part_111}, {o_part_112, o_part_113, o_part_114, o_part_115},
    {o_part_116, o_part_117, o_part_118, o_part_119}, {lhs_part_40 ^ o_part_116, lhs_part_41 ^ o_part_117, lhs_part_42 ^ o_part_118, lhs_part_43 ^ o_part_119}
  };
  assign w_0         = key[127:96];
  assign w_1         = key[95:64];
  assign w_2         = key[63:32];
  assign w_3         = key[31:0];
  assign o_part_000  = w_0[31:24] ^ (o_part_subWord_inst_o[31:24] ^ Rcon[319:312]);
  assign o_part_001  = w_0[23:16] ^ (o_part_subWord_inst_o[23:16] ^ Rcon[311:304]);
  assign o_part_002  = w_0[15:8] ^ (o_part_subWord_inst_o[15:8] ^ Rcon[303:296]);
  assign o_part_003  = w_0[7:0] ^ (o_part_subWord_inst_o[7:0] ^ Rcon[295:288]);
  assign o_part_004  = w_1[31:24] ^ o_part_000;
  assign o_part_005  = w_1[23:16] ^ o_part_001;
  assign o_part_006  = w_1[15:8] ^ o_part_002;
  assign o_part_007  = w_1[7:0] ^ o_part_003;
  assign o_part_008  = w_2[31:24] ^ o_part_004;
  assign o_part_009  = w_2[23:16] ^ o_part_005;
  assign o_part_010  = w_2[15:8] ^ o_part_006;
  assign o_part_011  = w_2[7:0] ^ o_part_007;
  assign lhs_part_00 = w_3[31:24] ^ o_part_008;
  assign lhs_part_01 = w_3[23:16] ^ o_part_009;
  assign lhs_part_02 = w_3[15:8] ^ o_part_010;
  assign lhs_part_03 = w_3[7:0] ^ o_part_011;
  assign lhs_part_04 = {lhs_part_00, lhs_part_01, lhs_part_02, lhs_part_03};
  assign o_part_012  = o_part_000 ^ (subWord_inst_0_o[31:24] ^ Rcon[287:280]);
  assign o_part_013  = o_part_001 ^ (subWord_inst_0_o[23:16] ^ Rcon[279:272]);
  assign o_part_014  = o_part_002 ^ (subWord_inst_0_o[15:8] ^ Rcon[271:264]);
  assign o_part_015  = o_part_003 ^ (subWord_inst_0_o[7:0] ^ Rcon[263:256]);
  assign o_part_016  = o_part_004 ^ o_part_012;
  assign o_part_017  = o_part_005 ^ o_part_013;
  assign o_part_018  = o_part_006 ^ o_part_014;
  assign o_part_019  = o_part_007 ^ o_part_015;
  assign o_part_020  = o_part_008 ^ o_part_016;
  assign o_part_021  = o_part_009 ^ o_part_017;
  assign o_part_022  = o_part_010 ^ o_part_018;
  assign o_part_023  = o_part_011 ^ o_part_019;
  assign lhs_part_05 = lhs_part_00 ^ o_part_020;
  assign lhs_part_06 = lhs_part_01 ^ o_part_021;
  assign lhs_part_07 = lhs_part_02 ^ o_part_022;
  assign lhs_part_08 = lhs_part_03 ^ o_part_023;
  assign lhs_part_09 = {lhs_part_05, lhs_part_06, lhs_part_07, lhs_part_08};
  assign o_part_024  = o_part_012 ^ (subWord_inst_1_o[31:24] ^ Rcon[255:248]);
  assign o_part_025  = o_part_013 ^ (subWord_inst_1_o[23:16] ^ Rcon[247:240]);
  assign o_part_026  = o_part_014 ^ (subWord_inst_1_o[15:8] ^ Rcon[239:232]);
  assign o_part_027  = o_part_015 ^ (subWord_inst_1_o[7:0] ^ Rcon[231:224]);
  assign o_part_028  = o_part_016 ^ o_part_024;
  assign o_part_029  = o_part_017 ^ o_part_025;
  assign o_part_030  = o_part_018 ^ o_part_026;
  assign o_part_031  = o_part_019 ^ o_part_027;
  assign o_part_032  = o_part_020 ^ o_part_028;
  assign o_part_033  = o_part_021 ^ o_part_029;
  assign o_part_034  = o_part_022 ^ o_part_030;
  assign o_part_035  = o_part_023 ^ o_part_031;
  assign lhs_part_10 = lhs_part_05 ^ o_part_032;
  assign lhs_part_11 = lhs_part_06 ^ o_part_033;
  assign lhs_part_12 = lhs_part_07 ^ o_part_034;
  assign lhs_part_13 = lhs_part_08 ^ o_part_035;
  assign lhs_part_14 = {lhs_part_10, lhs_part_11, lhs_part_12, lhs_part_13};
  assign o_part_036  = o_part_024 ^ (subWord_inst_2_o[31:24] ^ Rcon[223:216]);
  assign o_part_037  = o_part_025 ^ (subWord_inst_2_o[23:16] ^ Rcon[215:208]);
  assign o_part_038  = o_part_026 ^ (subWord_inst_2_o[15:8] ^ Rcon[207:200]);
  assign o_part_039  = o_part_027 ^ (subWord_inst_2_o[7:0] ^ Rcon[199:192]);
  assign o_part_040  = o_part_028 ^ o_part_036;
  assign o_part_041  = o_part_029 ^ o_part_037;
  assign o_part_042  = o_part_030 ^ o_part_038;
  assign o_part_043  = o_part_031 ^ o_part_039;
  assign o_part_044  = o_part_032 ^ o_part_040;
  assign o_part_045  = o_part_033 ^ o_part_041;
  assign o_part_046  = o_part_034 ^ o_part_042;
  assign o_part_047  = o_part_035 ^ o_part_043;
  assign lhs_part_15 = lhs_part_10 ^ o_part_044;
  assign lhs_part_16 = lhs_part_11 ^ o_part_045;
  assign lhs_part_17 = lhs_part_12 ^ o_part_046;
  assign lhs_part_18 = lhs_part_13 ^ o_part_047;
  assign lhs_part_19 = {lhs_part_15, lhs_part_16, lhs_part_17, lhs_part_18};
  assign o_part_048  = o_part_036 ^ (subWord_inst_3_o[31:24] ^ Rcon[191:184]);
  assign o_part_049  = o_part_037 ^ (subWord_inst_3_o[23:16] ^ Rcon[183:176]);
  assign o_part_050  = o_part_038 ^ (subWord_inst_3_o[15:8] ^ Rcon[175:168]);
  assign o_part_051  = o_part_039 ^ (subWord_inst_3_o[7:0] ^ Rcon[167:160]);
  assign o_part_052  = o_part_040 ^ o_part_048;
  assign o_part_053  = o_part_041 ^ o_part_049;
  assign o_part_054  = o_part_042 ^ o_part_050;
  assign o_part_055  = o_part_043 ^ o_part_051;
  assign o_part_056  = o_part_044 ^ o_part_052;
  assign o_part_057  = o_part_045 ^ o_part_053;
  assign o_part_058  = o_part_046 ^ o_part_054;
  assign o_part_059  = o_part_047 ^ o_part_055;
  assign lhs_part_20 = lhs_part_15 ^ o_part_056;
  assign lhs_part_21 = lhs_part_16 ^ o_part_057;
  assign lhs_part_22 = lhs_part_17 ^ o_part_058;
  assign lhs_part_23 = lhs_part_18 ^ o_part_059;
  assign lhs_part_24 = {lhs_part_20, lhs_part_21, lhs_part_22, lhs_part_23};
  assign o_part_060  = o_part_048 ^ (subWord_inst_4_o[31:24] ^ Rcon[159:152]);
  assign o_part_061  = o_part_049 ^ (subWord_inst_4_o[23:16] ^ Rcon[151:144]);
  assign o_part_062  = o_part_050 ^ (subWord_inst_4_o[15:8] ^ Rcon[143:136]);
  assign o_part_063  = o_part_051 ^ (subWord_inst_4_o[7:0] ^ Rcon[135:128]);
  assign o_part_064  = o_part_052 ^ o_part_060;
  assign o_part_065  = o_part_053 ^ o_part_061;
  assign o_part_066  = o_part_054 ^ o_part_062;
  assign o_part_067  = o_part_055 ^ o_part_063;
  assign o_part_068  = o_part_056 ^ o_part_064;
  assign o_part_069  = o_part_057 ^ o_part_065;
  assign o_part_070  = o_part_058 ^ o_part_066;
  assign o_part_071  = o_part_059 ^ o_part_067;
  assign lhs_part_25 = lhs_part_20 ^ o_part_068;
  assign lhs_part_26 = lhs_part_21 ^ o_part_069;
  assign lhs_part_27 = lhs_part_22 ^ o_part_070;
  assign lhs_part_28 = lhs_part_23 ^ o_part_071;
  assign lhs_part_29 = {lhs_part_25, lhs_part_26, lhs_part_27, lhs_part_28};
  assign o_part_072  = o_part_060 ^ (subWord_inst_5_o[31:24] ^ Rcon[127:120]);
  assign o_part_073  = o_part_061 ^ (subWord_inst_5_o[23:16] ^ Rcon[119:112]);
  assign o_part_074  = o_part_062 ^ (subWord_inst_5_o[15:8] ^ Rcon[111:104]);
  assign o_part_075  = o_part_063 ^ (subWord_inst_5_o[7:0] ^ Rcon[103:96]);
  assign o_part_076  = o_part_064 ^ o_part_072;
  assign o_part_077  = o_part_065 ^ o_part_073;
  assign o_part_078  = o_part_066 ^ o_part_074;
  assign o_part_079  = o_part_067 ^ o_part_075;
  assign o_part_080  = o_part_068 ^ o_part_076;
  assign o_part_081  = o_part_069 ^ o_part_077;
  assign o_part_082  = o_part_070 ^ o_part_078;
  assign o_part_083  = o_part_071 ^ o_part_079;
  assign lhs_part_30 = lhs_part_25 ^ o_part_080;
  assign lhs_part_31 = lhs_part_26 ^ o_part_081;
  assign lhs_part_32 = lhs_part_27 ^ o_part_082;
  assign lhs_part_33 = lhs_part_28 ^ o_part_083;
  assign lhs_part_34 = {lhs_part_30, lhs_part_31, lhs_part_32, lhs_part_33};
  assign o_part_084  = o_part_072 ^ (subWord_inst_6_o[31:24] ^ Rcon[95:88]);
  assign o_part_085  = o_part_073 ^ (subWord_inst_6_o[23:16] ^ Rcon[87:80]);
  assign o_part_086  = o_part_074 ^ (subWord_inst_6_o[15:8] ^ Rcon[79:72]);
  assign o_part_087  = o_part_075 ^ (subWord_inst_6_o[7:0] ^ Rcon[71:64]);
  assign o_part_088  = o_part_076 ^ o_part_084;
  assign o_part_089  = o_part_077 ^ o_part_085;
  assign o_part_090  = o_part_078 ^ o_part_086;
  assign o_part_091  = o_part_079 ^ o_part_087;
  assign o_part_092  = o_part_080 ^ o_part_088;
  assign o_part_093  = o_part_081 ^ o_part_089;
  assign o_part_094  = o_part_082 ^ o_part_090;
  assign o_part_095  = o_part_083 ^ o_part_091;
  assign lhs_part_35 = lhs_part_30 ^ o_part_092;
  assign lhs_part_36 = lhs_part_31 ^ o_part_093;
  assign lhs_part_37 = lhs_part_32 ^ o_part_094;
  assign lhs_part_38 = lhs_part_33 ^ o_part_095;
  assign lhs_part_39 = {lhs_part_35, lhs_part_36, lhs_part_37, lhs_part_38};
  assign o_part_096  = o_part_084 ^ (subWord_inst_7_o[31:24] ^ Rcon[63:56]);
  assign o_part_097  = o_part_085 ^ (subWord_inst_7_o[23:16] ^ Rcon[55:48]);
  assign o_part_098  = o_part_086 ^ (subWord_inst_7_o[15:8] ^ Rcon[47:40]);
  assign o_part_099  = o_part_087 ^ (subWord_inst_7_o[7:0] ^ Rcon[39:32]);
  assign o_part_100  = o_part_088 ^ o_part_096;
  assign o_part_101  = o_part_089 ^ o_part_097;
  assign o_part_102  = o_part_090 ^ o_part_098;
  assign o_part_103  = o_part_091 ^ o_part_099;
  assign o_part_104  = o_part_092 ^ o_part_100;
  assign o_part_105  = o_part_093 ^ o_part_101;
  assign o_part_106  = o_part_094 ^ o_part_102;
  assign o_part_107  = o_part_095 ^ o_part_103;
  assign lhs_part_40 = lhs_part_35 ^ o_part_104;
  assign lhs_part_41 = lhs_part_36 ^ o_part_105;
  assign lhs_part_42 = lhs_part_37 ^ o_part_106;
  assign lhs_part_43 = lhs_part_38 ^ o_part_107;
  assign lhs_part_44 = {lhs_part_40, lhs_part_41, lhs_part_42, lhs_part_43};
  assign o_part_108  = o_part_096 ^ (subWord_inst_8_o[31:24] ^ Rcon[31:24]);
  assign o_part_109  = o_part_097 ^ (subWord_inst_8_o[23:16] ^ Rcon[23:16]);
  assign o_part_110  = o_part_098 ^ (subWord_inst_8_o[15:8] ^ Rcon[15:8]);
  assign o_part_111  = o_part_099 ^ (subWord_inst_8_o[7:0] ^ Rcon[7:0]);
  assign o_part_112  = o_part_100 ^ o_part_108;
  assign o_part_113  = o_part_101 ^ o_part_109;
  assign o_part_114  = o_part_102 ^ o_part_110;
  assign o_part_115  = o_part_103 ^ o_part_111;
  assign o_part_116  = o_part_104 ^ o_part_112;
  assign o_part_117  = o_part_105 ^ o_part_113;
  assign o_part_118  = o_part_106 ^ o_part_114;
  assign o_part_119  = o_part_107 ^ o_part_115;
endmodule
