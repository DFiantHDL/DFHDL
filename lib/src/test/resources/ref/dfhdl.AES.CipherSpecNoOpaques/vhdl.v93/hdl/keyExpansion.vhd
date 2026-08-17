library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.CipherNoOpaques_pkg.all;

entity keyExpansion is
port (
  key : in  AESKey;
  o   : out AESKeySchedule
);
end keyExpansion;

architecture keyExpansion_arch of keyExpansion is
  signal w_0         : AESWord;
  signal w_1         : AESWord;
  signal w_2         : AESWord;
  signal w_3         : AESWord;
  signal o_part_000  : AESByte;
  signal o_part_001  : AESByte;
  signal o_part_002  : AESByte;
  signal o_part_003  : AESByte;
  signal o_part_004  : AESByte;
  signal o_part_005  : AESByte;
  signal o_part_006  : AESByte;
  signal o_part_007  : AESByte;
  signal o_part_008  : AESByte;
  signal o_part_009  : AESByte;
  signal o_part_010  : AESByte;
  signal o_part_011  : AESByte;
  signal lhs_part_00 : AESByte;
  signal lhs_part_01 : AESByte;
  signal lhs_part_02 : AESByte;
  signal lhs_part_03 : AESByte;
  signal lhs_part_04 : AESWord;
  signal o_part_012  : AESByte;
  signal o_part_013  : AESByte;
  signal o_part_014  : AESByte;
  signal o_part_015  : AESByte;
  signal o_part_016  : AESByte;
  signal o_part_017  : AESByte;
  signal o_part_018  : AESByte;
  signal o_part_019  : AESByte;
  signal o_part_020  : AESByte;
  signal o_part_021  : AESByte;
  signal o_part_022  : AESByte;
  signal o_part_023  : AESByte;
  signal lhs_part_05 : AESByte;
  signal lhs_part_06 : AESByte;
  signal lhs_part_07 : AESByte;
  signal lhs_part_08 : AESByte;
  signal lhs_part_09 : AESWord;
  signal o_part_024  : AESByte;
  signal o_part_025  : AESByte;
  signal o_part_026  : AESByte;
  signal o_part_027  : AESByte;
  signal o_part_028  : AESByte;
  signal o_part_029  : AESByte;
  signal o_part_030  : AESByte;
  signal o_part_031  : AESByte;
  signal o_part_032  : AESByte;
  signal o_part_033  : AESByte;
  signal o_part_034  : AESByte;
  signal o_part_035  : AESByte;
  signal lhs_part_10 : AESByte;
  signal lhs_part_11 : AESByte;
  signal lhs_part_12 : AESByte;
  signal lhs_part_13 : AESByte;
  signal lhs_part_14 : AESWord;
  signal o_part_036  : AESByte;
  signal o_part_037  : AESByte;
  signal o_part_038  : AESByte;
  signal o_part_039  : AESByte;
  signal o_part_040  : AESByte;
  signal o_part_041  : AESByte;
  signal o_part_042  : AESByte;
  signal o_part_043  : AESByte;
  signal o_part_044  : AESByte;
  signal o_part_045  : AESByte;
  signal o_part_046  : AESByte;
  signal o_part_047  : AESByte;
  signal lhs_part_15 : AESByte;
  signal lhs_part_16 : AESByte;
  signal lhs_part_17 : AESByte;
  signal lhs_part_18 : AESByte;
  signal lhs_part_19 : AESWord;
  signal o_part_048  : AESByte;
  signal o_part_049  : AESByte;
  signal o_part_050  : AESByte;
  signal o_part_051  : AESByte;
  signal o_part_052  : AESByte;
  signal o_part_053  : AESByte;
  signal o_part_054  : AESByte;
  signal o_part_055  : AESByte;
  signal o_part_056  : AESByte;
  signal o_part_057  : AESByte;
  signal o_part_058  : AESByte;
  signal o_part_059  : AESByte;
  signal lhs_part_20 : AESByte;
  signal lhs_part_21 : AESByte;
  signal lhs_part_22 : AESByte;
  signal lhs_part_23 : AESByte;
  signal lhs_part_24 : AESWord;
  signal o_part_060  : AESByte;
  signal o_part_061  : AESByte;
  signal o_part_062  : AESByte;
  signal o_part_063  : AESByte;
  signal o_part_064  : AESByte;
  signal o_part_065  : AESByte;
  signal o_part_066  : AESByte;
  signal o_part_067  : AESByte;
  signal o_part_068  : AESByte;
  signal o_part_069  : AESByte;
  signal o_part_070  : AESByte;
  signal o_part_071  : AESByte;
  signal lhs_part_25 : AESByte;
  signal lhs_part_26 : AESByte;
  signal lhs_part_27 : AESByte;
  signal lhs_part_28 : AESByte;
  signal lhs_part_29 : AESWord;
  signal o_part_072  : AESByte;
  signal o_part_073  : AESByte;
  signal o_part_074  : AESByte;
  signal o_part_075  : AESByte;
  signal o_part_076  : AESByte;
  signal o_part_077  : AESByte;
  signal o_part_078  : AESByte;
  signal o_part_079  : AESByte;
  signal o_part_080  : AESByte;
  signal o_part_081  : AESByte;
  signal o_part_082  : AESByte;
  signal o_part_083  : AESByte;
  signal lhs_part_30 : AESByte;
  signal lhs_part_31 : AESByte;
  signal lhs_part_32 : AESByte;
  signal lhs_part_33 : AESByte;
  signal lhs_part_34 : AESWord;
  signal o_part_084  : AESByte;
  signal o_part_085  : AESByte;
  signal o_part_086  : AESByte;
  signal o_part_087  : AESByte;
  signal o_part_088  : AESByte;
  signal o_part_089  : AESByte;
  signal o_part_090  : AESByte;
  signal o_part_091  : AESByte;
  signal o_part_092  : AESByte;
  signal o_part_093  : AESByte;
  signal o_part_094  : AESByte;
  signal o_part_095  : AESByte;
  signal lhs_part_35 : AESByte;
  signal lhs_part_36 : AESByte;
  signal lhs_part_37 : AESByte;
  signal lhs_part_38 : AESByte;
  signal lhs_part_39 : AESWord;
  signal o_part_096  : AESByte;
  signal o_part_097  : AESByte;
  signal o_part_098  : AESByte;
  signal o_part_099  : AESByte;
  signal o_part_100  : AESByte;
  signal o_part_101  : AESByte;
  signal o_part_102  : AESByte;
  signal o_part_103  : AESByte;
  signal o_part_104  : AESByte;
  signal o_part_105  : AESByte;
  signal o_part_106  : AESByte;
  signal o_part_107  : AESByte;
  signal lhs_part_40 : AESByte;
  signal lhs_part_41 : AESByte;
  signal lhs_part_42 : AESByte;
  signal lhs_part_43 : AESByte;
  signal lhs_part_44 : AESWord;
  signal o_part_108  : AESByte;
  signal o_part_109  : AESByte;
  signal o_part_110  : AESByte;
  signal o_part_111  : AESByte;
  signal o_part_112  : AESByte;
  signal o_part_113  : AESByte;
  signal o_part_114  : AESByte;
  signal o_part_115  : AESByte;
  signal o_part_116  : AESByte;
  signal o_part_117  : AESByte;
  signal o_part_118  : AESByte;
  signal o_part_119  : AESByte;
  signal o_part_rotWord_inst_00_o : AESWord;
  signal o_part_subWord_inst_00_lhs : AESWord;
  signal o_part_subWord_inst_00_o : AESWord;
  signal o_part_rotWord_inst_01_o : AESWord;
  signal o_part_subWord_inst_01_lhs : AESWord;
  signal o_part_subWord_inst_01_o : AESWord;
  signal o_part_rotWord_inst_02_o : AESWord;
  signal o_part_subWord_inst_02_lhs : AESWord;
  signal o_part_subWord_inst_02_o : AESWord;
  signal o_part_rotWord_inst_03_o : AESWord;
  signal o_part_subWord_inst_03_lhs : AESWord;
  signal o_part_subWord_inst_03_o : AESWord;
  signal o_part_rotWord_inst_04_o : AESWord;
  signal o_part_subWord_inst_04_lhs : AESWord;
  signal o_part_subWord_inst_04_o : AESWord;
  signal o_part_rotWord_inst_05_o : AESWord;
  signal o_part_subWord_inst_05_lhs : AESWord;
  signal o_part_subWord_inst_05_o : AESWord;
  signal o_part_rotWord_inst_06_o : AESWord;
  signal o_part_subWord_inst_06_lhs : AESWord;
  signal o_part_subWord_inst_06_o : AESWord;
  signal o_part_rotWord_inst_07_o : AESWord;
  signal o_part_subWord_inst_07_lhs : AESWord;
  signal o_part_subWord_inst_07_o : AESWord;
  signal o_part_rotWord_inst_08_o : AESWord;
  signal o_part_subWord_inst_08_lhs : AESWord;
  signal o_part_subWord_inst_08_o : AESWord;
  signal o_part_rotWord_inst_09_o : AESWord;
  signal o_part_subWord_inst_09_lhs : AESWord;
  signal o_part_subWord_inst_09_o : AESWord;
begin
  o_part_rotWord_inst_00 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_00_o,
    lhs       => w_3
  );
  o_part_subWord_inst_00 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_00_lhs,
    o         => o_part_subWord_inst_00_o
  );
  o_part_rotWord_inst_01 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_01_o,
    lhs       => lhs_part_04
  );
  o_part_subWord_inst_01 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_01_lhs,
    o         => o_part_subWord_inst_01_o
  );
  o_part_rotWord_inst_02 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_02_o,
    lhs       => lhs_part_09
  );
  o_part_subWord_inst_02 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_02_lhs,
    o         => o_part_subWord_inst_02_o
  );
  o_part_rotWord_inst_03 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_03_o,
    lhs       => lhs_part_14
  );
  o_part_subWord_inst_03 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_03_lhs,
    o         => o_part_subWord_inst_03_o
  );
  o_part_rotWord_inst_04 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_04_o,
    lhs       => lhs_part_19
  );
  o_part_subWord_inst_04 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_04_lhs,
    o         => o_part_subWord_inst_04_o
  );
  o_part_rotWord_inst_05 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_05_o,
    lhs       => lhs_part_24
  );
  o_part_subWord_inst_05 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_05_lhs,
    o         => o_part_subWord_inst_05_o
  );
  o_part_rotWord_inst_06 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_06_o,
    lhs       => lhs_part_29
  );
  o_part_subWord_inst_06 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_06_lhs,
    o         => o_part_subWord_inst_06_o
  );
  o_part_rotWord_inst_07 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_07_o,
    lhs       => lhs_part_34
  );
  o_part_subWord_inst_07 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_07_lhs,
    o         => o_part_subWord_inst_07_o
  );
  o_part_rotWord_inst_08 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_08_o,
    lhs       => lhs_part_39
  );
  o_part_subWord_inst_08 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_08_lhs,
    o         => o_part_subWord_inst_08_o
  );
  o_part_rotWord_inst_09 : entity work.rotWord(rotWord_arch) port map (
    o         => o_part_rotWord_inst_09_o,
    lhs       => lhs_part_44
  );
  o_part_subWord_inst_09 : entity work.subWord(subWord_arch) port map (
    lhs       => o_part_subWord_inst_09_lhs,
    o         => o_part_subWord_inst_09_o
  );
  o_part_subWord_inst_00_lhs <= o_part_rotWord_inst_00_o;
  o_part_subWord_inst_01_lhs <= o_part_rotWord_inst_01_o;
  o_part_subWord_inst_02_lhs <= o_part_rotWord_inst_02_o;
  o_part_subWord_inst_03_lhs <= o_part_rotWord_inst_03_o;
  o_part_subWord_inst_04_lhs <= o_part_rotWord_inst_04_o;
  o_part_subWord_inst_05_lhs <= o_part_rotWord_inst_05_o;
  o_part_subWord_inst_06_lhs <= o_part_rotWord_inst_06_o;
  o_part_subWord_inst_07_lhs <= o_part_rotWord_inst_07_o;
  o_part_subWord_inst_08_lhs <= o_part_rotWord_inst_08_o;
  o_part_subWord_inst_09_lhs <= o_part_rotWord_inst_09_o;
  o           <= (
     0 => w_0,                                                                   1 => w_1,
     2 => w_2,                                                                   3 => w_3,
     4 => (0 => o_part_000, 1 => o_part_001, 2 => o_part_002, 3 => o_part_003),  5 => (0 => o_part_004, 1 => o_part_005, 2 => o_part_006, 3 => o_part_007),
     6 => (0 => o_part_008, 1 => o_part_009, 2 => o_part_010, 3 => o_part_011),  7 => lhs_part_04,
     8 => (0 => o_part_012, 1 => o_part_013, 2 => o_part_014, 3 => o_part_015),  9 => (0 => o_part_016, 1 => o_part_017, 2 => o_part_018, 3 => o_part_019),
    10 => (0 => o_part_020, 1 => o_part_021, 2 => o_part_022, 3 => o_part_023), 11 => lhs_part_09,
    12 => (0 => o_part_024, 1 => o_part_025, 2 => o_part_026, 3 => o_part_027), 13 => (0 => o_part_028, 1 => o_part_029, 2 => o_part_030, 3 => o_part_031),
    14 => (0 => o_part_032, 1 => o_part_033, 2 => o_part_034, 3 => o_part_035), 15 => lhs_part_14,
    16 => (0 => o_part_036, 1 => o_part_037, 2 => o_part_038, 3 => o_part_039), 17 => (0 => o_part_040, 1 => o_part_041, 2 => o_part_042, 3 => o_part_043),
    18 => (0 => o_part_044, 1 => o_part_045, 2 => o_part_046, 3 => o_part_047), 19 => lhs_part_19,
    20 => (0 => o_part_048, 1 => o_part_049, 2 => o_part_050, 3 => o_part_051), 21 => (0 => o_part_052, 1 => o_part_053, 2 => o_part_054, 3 => o_part_055),
    22 => (0 => o_part_056, 1 => o_part_057, 2 => o_part_058, 3 => o_part_059), 23 => lhs_part_24,
    24 => (0 => o_part_060, 1 => o_part_061, 2 => o_part_062, 3 => o_part_063), 25 => (0 => o_part_064, 1 => o_part_065, 2 => o_part_066, 3 => o_part_067),
    26 => (0 => o_part_068, 1 => o_part_069, 2 => o_part_070, 3 => o_part_071), 27 => lhs_part_29,
    28 => (0 => o_part_072, 1 => o_part_073, 2 => o_part_074, 3 => o_part_075), 29 => (0 => o_part_076, 1 => o_part_077, 2 => o_part_078, 3 => o_part_079),
    30 => (0 => o_part_080, 1 => o_part_081, 2 => o_part_082, 3 => o_part_083), 31 => lhs_part_34,
    32 => (0 => o_part_084, 1 => o_part_085, 2 => o_part_086, 3 => o_part_087), 33 => (0 => o_part_088, 1 => o_part_089, 2 => o_part_090, 3 => o_part_091),
    34 => (0 => o_part_092, 1 => o_part_093, 2 => o_part_094, 3 => o_part_095), 35 => lhs_part_39,
    36 => (0 => o_part_096, 1 => o_part_097, 2 => o_part_098, 3 => o_part_099), 37 => (0 => o_part_100, 1 => o_part_101, 2 => o_part_102, 3 => o_part_103),
    38 => (0 => o_part_104, 1 => o_part_105, 2 => o_part_106, 3 => o_part_107), 39 => lhs_part_44,
    40 => (0 => o_part_108, 1 => o_part_109, 2 => o_part_110, 3 => o_part_111), 41 => (0 => o_part_112, 1 => o_part_113, 2 => o_part_114, 3 => o_part_115),
    42 => (0 => o_part_116, 1 => o_part_117, 2 => o_part_118, 3 => o_part_119), 43 => (
      0 => lhs_part_40 xor o_part_116, 1 => lhs_part_41 xor o_part_117,
      2 => lhs_part_42 xor o_part_118, 3 => lhs_part_43 xor o_part_119
    )
  );
  w_0         <= key(0);
  w_1         <= key(1);
  w_2         <= key(2);
  w_3         <= key(3);
  o_part_000  <= w_0(0) xor (o_part_subWord_inst_00_o(0) xor Rcon(1)(0));
  o_part_001  <= w_0(1) xor (o_part_subWord_inst_00_o(1) xor Rcon(1)(1));
  o_part_002  <= w_0(2) xor (o_part_subWord_inst_00_o(2) xor Rcon(1)(2));
  o_part_003  <= w_0(3) xor (o_part_subWord_inst_00_o(3) xor Rcon(1)(3));
  o_part_004  <= w_1(0) xor o_part_000;
  o_part_005  <= w_1(1) xor o_part_001;
  o_part_006  <= w_1(2) xor o_part_002;
  o_part_007  <= w_1(3) xor o_part_003;
  o_part_008  <= w_2(0) xor o_part_004;
  o_part_009  <= w_2(1) xor o_part_005;
  o_part_010  <= w_2(2) xor o_part_006;
  o_part_011  <= w_2(3) xor o_part_007;
  lhs_part_00 <= w_3(0) xor o_part_008;
  lhs_part_01 <= w_3(1) xor o_part_009;
  lhs_part_02 <= w_3(2) xor o_part_010;
  lhs_part_03 <= w_3(3) xor o_part_011;
  lhs_part_04 <= (0 => lhs_part_00, 1 => lhs_part_01, 2 => lhs_part_02, 3 => lhs_part_03);
  o_part_012  <= o_part_000 xor (o_part_subWord_inst_01_o(0) xor Rcon(2)(0));
  o_part_013  <= o_part_001 xor (o_part_subWord_inst_01_o(1) xor Rcon(2)(1));
  o_part_014  <= o_part_002 xor (o_part_subWord_inst_01_o(2) xor Rcon(2)(2));
  o_part_015  <= o_part_003 xor (o_part_subWord_inst_01_o(3) xor Rcon(2)(3));
  o_part_016  <= o_part_004 xor o_part_012;
  o_part_017  <= o_part_005 xor o_part_013;
  o_part_018  <= o_part_006 xor o_part_014;
  o_part_019  <= o_part_007 xor o_part_015;
  o_part_020  <= o_part_008 xor o_part_016;
  o_part_021  <= o_part_009 xor o_part_017;
  o_part_022  <= o_part_010 xor o_part_018;
  o_part_023  <= o_part_011 xor o_part_019;
  lhs_part_05 <= lhs_part_00 xor o_part_020;
  lhs_part_06 <= lhs_part_01 xor o_part_021;
  lhs_part_07 <= lhs_part_02 xor o_part_022;
  lhs_part_08 <= lhs_part_03 xor o_part_023;
  lhs_part_09 <= (0 => lhs_part_05, 1 => lhs_part_06, 2 => lhs_part_07, 3 => lhs_part_08);
  o_part_024  <= o_part_012 xor (o_part_subWord_inst_02_o(0) xor Rcon(3)(0));
  o_part_025  <= o_part_013 xor (o_part_subWord_inst_02_o(1) xor Rcon(3)(1));
  o_part_026  <= o_part_014 xor (o_part_subWord_inst_02_o(2) xor Rcon(3)(2));
  o_part_027  <= o_part_015 xor (o_part_subWord_inst_02_o(3) xor Rcon(3)(3));
  o_part_028  <= o_part_016 xor o_part_024;
  o_part_029  <= o_part_017 xor o_part_025;
  o_part_030  <= o_part_018 xor o_part_026;
  o_part_031  <= o_part_019 xor o_part_027;
  o_part_032  <= o_part_020 xor o_part_028;
  o_part_033  <= o_part_021 xor o_part_029;
  o_part_034  <= o_part_022 xor o_part_030;
  o_part_035  <= o_part_023 xor o_part_031;
  lhs_part_10 <= lhs_part_05 xor o_part_032;
  lhs_part_11 <= lhs_part_06 xor o_part_033;
  lhs_part_12 <= lhs_part_07 xor o_part_034;
  lhs_part_13 <= lhs_part_08 xor o_part_035;
  lhs_part_14 <= (0 => lhs_part_10, 1 => lhs_part_11, 2 => lhs_part_12, 3 => lhs_part_13);
  o_part_036  <= o_part_024 xor (o_part_subWord_inst_03_o(0) xor Rcon(4)(0));
  o_part_037  <= o_part_025 xor (o_part_subWord_inst_03_o(1) xor Rcon(4)(1));
  o_part_038  <= o_part_026 xor (o_part_subWord_inst_03_o(2) xor Rcon(4)(2));
  o_part_039  <= o_part_027 xor (o_part_subWord_inst_03_o(3) xor Rcon(4)(3));
  o_part_040  <= o_part_028 xor o_part_036;
  o_part_041  <= o_part_029 xor o_part_037;
  o_part_042  <= o_part_030 xor o_part_038;
  o_part_043  <= o_part_031 xor o_part_039;
  o_part_044  <= o_part_032 xor o_part_040;
  o_part_045  <= o_part_033 xor o_part_041;
  o_part_046  <= o_part_034 xor o_part_042;
  o_part_047  <= o_part_035 xor o_part_043;
  lhs_part_15 <= lhs_part_10 xor o_part_044;
  lhs_part_16 <= lhs_part_11 xor o_part_045;
  lhs_part_17 <= lhs_part_12 xor o_part_046;
  lhs_part_18 <= lhs_part_13 xor o_part_047;
  lhs_part_19 <= (0 => lhs_part_15, 1 => lhs_part_16, 2 => lhs_part_17, 3 => lhs_part_18);
  o_part_048  <= o_part_036 xor (o_part_subWord_inst_04_o(0) xor Rcon(5)(0));
  o_part_049  <= o_part_037 xor (o_part_subWord_inst_04_o(1) xor Rcon(5)(1));
  o_part_050  <= o_part_038 xor (o_part_subWord_inst_04_o(2) xor Rcon(5)(2));
  o_part_051  <= o_part_039 xor (o_part_subWord_inst_04_o(3) xor Rcon(5)(3));
  o_part_052  <= o_part_040 xor o_part_048;
  o_part_053  <= o_part_041 xor o_part_049;
  o_part_054  <= o_part_042 xor o_part_050;
  o_part_055  <= o_part_043 xor o_part_051;
  o_part_056  <= o_part_044 xor o_part_052;
  o_part_057  <= o_part_045 xor o_part_053;
  o_part_058  <= o_part_046 xor o_part_054;
  o_part_059  <= o_part_047 xor o_part_055;
  lhs_part_20 <= lhs_part_15 xor o_part_056;
  lhs_part_21 <= lhs_part_16 xor o_part_057;
  lhs_part_22 <= lhs_part_17 xor o_part_058;
  lhs_part_23 <= lhs_part_18 xor o_part_059;
  lhs_part_24 <= (0 => lhs_part_20, 1 => lhs_part_21, 2 => lhs_part_22, 3 => lhs_part_23);
  o_part_060  <= o_part_048 xor (o_part_subWord_inst_05_o(0) xor Rcon(6)(0));
  o_part_061  <= o_part_049 xor (o_part_subWord_inst_05_o(1) xor Rcon(6)(1));
  o_part_062  <= o_part_050 xor (o_part_subWord_inst_05_o(2) xor Rcon(6)(2));
  o_part_063  <= o_part_051 xor (o_part_subWord_inst_05_o(3) xor Rcon(6)(3));
  o_part_064  <= o_part_052 xor o_part_060;
  o_part_065  <= o_part_053 xor o_part_061;
  o_part_066  <= o_part_054 xor o_part_062;
  o_part_067  <= o_part_055 xor o_part_063;
  o_part_068  <= o_part_056 xor o_part_064;
  o_part_069  <= o_part_057 xor o_part_065;
  o_part_070  <= o_part_058 xor o_part_066;
  o_part_071  <= o_part_059 xor o_part_067;
  lhs_part_25 <= lhs_part_20 xor o_part_068;
  lhs_part_26 <= lhs_part_21 xor o_part_069;
  lhs_part_27 <= lhs_part_22 xor o_part_070;
  lhs_part_28 <= lhs_part_23 xor o_part_071;
  lhs_part_29 <= (0 => lhs_part_25, 1 => lhs_part_26, 2 => lhs_part_27, 3 => lhs_part_28);
  o_part_072  <= o_part_060 xor (o_part_subWord_inst_06_o(0) xor Rcon(7)(0));
  o_part_073  <= o_part_061 xor (o_part_subWord_inst_06_o(1) xor Rcon(7)(1));
  o_part_074  <= o_part_062 xor (o_part_subWord_inst_06_o(2) xor Rcon(7)(2));
  o_part_075  <= o_part_063 xor (o_part_subWord_inst_06_o(3) xor Rcon(7)(3));
  o_part_076  <= o_part_064 xor o_part_072;
  o_part_077  <= o_part_065 xor o_part_073;
  o_part_078  <= o_part_066 xor o_part_074;
  o_part_079  <= o_part_067 xor o_part_075;
  o_part_080  <= o_part_068 xor o_part_076;
  o_part_081  <= o_part_069 xor o_part_077;
  o_part_082  <= o_part_070 xor o_part_078;
  o_part_083  <= o_part_071 xor o_part_079;
  lhs_part_30 <= lhs_part_25 xor o_part_080;
  lhs_part_31 <= lhs_part_26 xor o_part_081;
  lhs_part_32 <= lhs_part_27 xor o_part_082;
  lhs_part_33 <= lhs_part_28 xor o_part_083;
  lhs_part_34 <= (0 => lhs_part_30, 1 => lhs_part_31, 2 => lhs_part_32, 3 => lhs_part_33);
  o_part_084  <= o_part_072 xor (o_part_subWord_inst_07_o(0) xor Rcon(8)(0));
  o_part_085  <= o_part_073 xor (o_part_subWord_inst_07_o(1) xor Rcon(8)(1));
  o_part_086  <= o_part_074 xor (o_part_subWord_inst_07_o(2) xor Rcon(8)(2));
  o_part_087  <= o_part_075 xor (o_part_subWord_inst_07_o(3) xor Rcon(8)(3));
  o_part_088  <= o_part_076 xor o_part_084;
  o_part_089  <= o_part_077 xor o_part_085;
  o_part_090  <= o_part_078 xor o_part_086;
  o_part_091  <= o_part_079 xor o_part_087;
  o_part_092  <= o_part_080 xor o_part_088;
  o_part_093  <= o_part_081 xor o_part_089;
  o_part_094  <= o_part_082 xor o_part_090;
  o_part_095  <= o_part_083 xor o_part_091;
  lhs_part_35 <= lhs_part_30 xor o_part_092;
  lhs_part_36 <= lhs_part_31 xor o_part_093;
  lhs_part_37 <= lhs_part_32 xor o_part_094;
  lhs_part_38 <= lhs_part_33 xor o_part_095;
  lhs_part_39 <= (0 => lhs_part_35, 1 => lhs_part_36, 2 => lhs_part_37, 3 => lhs_part_38);
  o_part_096  <= o_part_084 xor (o_part_subWord_inst_08_o(0) xor Rcon(9)(0));
  o_part_097  <= o_part_085 xor (o_part_subWord_inst_08_o(1) xor Rcon(9)(1));
  o_part_098  <= o_part_086 xor (o_part_subWord_inst_08_o(2) xor Rcon(9)(2));
  o_part_099  <= o_part_087 xor (o_part_subWord_inst_08_o(3) xor Rcon(9)(3));
  o_part_100  <= o_part_088 xor o_part_096;
  o_part_101  <= o_part_089 xor o_part_097;
  o_part_102  <= o_part_090 xor o_part_098;
  o_part_103  <= o_part_091 xor o_part_099;
  o_part_104  <= o_part_092 xor o_part_100;
  o_part_105  <= o_part_093 xor o_part_101;
  o_part_106  <= o_part_094 xor o_part_102;
  o_part_107  <= o_part_095 xor o_part_103;
  lhs_part_40 <= lhs_part_35 xor o_part_104;
  lhs_part_41 <= lhs_part_36 xor o_part_105;
  lhs_part_42 <= lhs_part_37 xor o_part_106;
  lhs_part_43 <= lhs_part_38 xor o_part_107;
  lhs_part_44 <= (0 => lhs_part_40, 1 => lhs_part_41, 2 => lhs_part_42, 3 => lhs_part_43);
  o_part_108  <= o_part_096 xor (o_part_subWord_inst_09_o(0) xor Rcon(10)(0));
  o_part_109  <= o_part_097 xor (o_part_subWord_inst_09_o(1) xor Rcon(10)(1));
  o_part_110  <= o_part_098 xor (o_part_subWord_inst_09_o(2) xor Rcon(10)(2));
  o_part_111  <= o_part_099 xor (o_part_subWord_inst_09_o(3) xor Rcon(10)(3));
  o_part_112  <= o_part_100 xor o_part_108;
  o_part_113  <= o_part_101 xor o_part_109;
  o_part_114  <= o_part_102 xor o_part_110;
  o_part_115  <= o_part_103 xor o_part_111;
  o_part_116  <= o_part_104 xor o_part_112;
  o_part_117  <= o_part_105 xor o_part_113;
  o_part_118  <= o_part_106 xor o_part_114;
  o_part_119  <= o_part_107 xor o_part_115;
end keyExpansion_arch;
