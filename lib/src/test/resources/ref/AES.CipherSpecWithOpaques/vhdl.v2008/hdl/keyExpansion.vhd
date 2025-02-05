library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity keyExpansion is
port (
  key : in  t_opaque_AESKey;
  o   : out t_opaque_AESKeySchedule
);
end keyExpansion;

architecture keyExpansion_arch of keyExpansion is


  signal w_0                     : t_opaque_AESWord;
  signal w_1                     : t_opaque_AESWord;
  signal w_2                     : t_opaque_AESWord;
  signal w_3                     : t_opaque_AESWord;
  signal keySchedule_part_000    : t_opaque_AESByte;
  signal keySchedule_part_001    : t_opaque_AESByte;
  signal keySchedule_part_002    : t_opaque_AESByte;
  signal keySchedule_part_003    : t_opaque_AESByte;
  signal keySchedule_part_004    : t_opaque_AESByte;
  signal keySchedule_part_005    : t_opaque_AESByte;
  signal keySchedule_part_006    : t_opaque_AESByte;
  signal keySchedule_part_007    : t_opaque_AESByte;
  signal keySchedule_part_008    : t_opaque_AESByte;
  signal keySchedule_part_009    : t_opaque_AESByte;
  signal keySchedule_part_010    : t_opaque_AESByte;
  signal keySchedule_part_011    : t_opaque_AESByte;
  signal lhs_part_00             : t_opaque_AESByte;
  signal lhs_part_01             : t_opaque_AESByte;
  signal lhs_part_02             : t_opaque_AESByte;
  signal lhs_part_03             : t_opaque_AESByte;
  signal lhs_part_04             : t_opaque_AESWord;
  signal keySchedule_part_012    : t_opaque_AESByte;
  signal keySchedule_part_013    : t_opaque_AESByte;
  signal keySchedule_part_014    : t_opaque_AESByte;
  signal keySchedule_part_015    : t_opaque_AESByte;
  signal keySchedule_part_016    : t_opaque_AESByte;
  signal keySchedule_part_017    : t_opaque_AESByte;
  signal keySchedule_part_018    : t_opaque_AESByte;
  signal keySchedule_part_019    : t_opaque_AESByte;
  signal keySchedule_part_020    : t_opaque_AESByte;
  signal keySchedule_part_021    : t_opaque_AESByte;
  signal keySchedule_part_022    : t_opaque_AESByte;
  signal keySchedule_part_023    : t_opaque_AESByte;
  signal lhs_part_05             : t_opaque_AESByte;
  signal lhs_part_06             : t_opaque_AESByte;
  signal lhs_part_07             : t_opaque_AESByte;
  signal lhs_part_08             : t_opaque_AESByte;
  signal lhs_part_09             : t_opaque_AESWord;
  signal keySchedule_part_024    : t_opaque_AESByte;
  signal keySchedule_part_025    : t_opaque_AESByte;
  signal keySchedule_part_026    : t_opaque_AESByte;
  signal keySchedule_part_027    : t_opaque_AESByte;
  signal keySchedule_part_028    : t_opaque_AESByte;
  signal keySchedule_part_029    : t_opaque_AESByte;
  signal keySchedule_part_030    : t_opaque_AESByte;
  signal keySchedule_part_031    : t_opaque_AESByte;
  signal keySchedule_part_032    : t_opaque_AESByte;
  signal keySchedule_part_033    : t_opaque_AESByte;
  signal keySchedule_part_034    : t_opaque_AESByte;
  signal keySchedule_part_035    : t_opaque_AESByte;
  signal lhs_part_10             : t_opaque_AESByte;
  signal lhs_part_11             : t_opaque_AESByte;
  signal lhs_part_12             : t_opaque_AESByte;
  signal lhs_part_13             : t_opaque_AESByte;
  signal lhs_part_14             : t_opaque_AESWord;
  signal keySchedule_part_036    : t_opaque_AESByte;
  signal keySchedule_part_037    : t_opaque_AESByte;
  signal keySchedule_part_038    : t_opaque_AESByte;
  signal keySchedule_part_039    : t_opaque_AESByte;
  signal keySchedule_part_040    : t_opaque_AESByte;
  signal keySchedule_part_041    : t_opaque_AESByte;
  signal keySchedule_part_042    : t_opaque_AESByte;
  signal keySchedule_part_043    : t_opaque_AESByte;
  signal keySchedule_part_044    : t_opaque_AESByte;
  signal keySchedule_part_045    : t_opaque_AESByte;
  signal keySchedule_part_046    : t_opaque_AESByte;
  signal keySchedule_part_047    : t_opaque_AESByte;
  signal lhs_part_15             : t_opaque_AESByte;
  signal lhs_part_16             : t_opaque_AESByte;
  signal lhs_part_17             : t_opaque_AESByte;
  signal lhs_part_18             : t_opaque_AESByte;
  signal lhs_part_19             : t_opaque_AESWord;
  signal keySchedule_part_048    : t_opaque_AESByte;
  signal keySchedule_part_049    : t_opaque_AESByte;
  signal keySchedule_part_050    : t_opaque_AESByte;
  signal keySchedule_part_051    : t_opaque_AESByte;
  signal keySchedule_part_052    : t_opaque_AESByte;
  signal keySchedule_part_053    : t_opaque_AESByte;
  signal keySchedule_part_054    : t_opaque_AESByte;
  signal keySchedule_part_055    : t_opaque_AESByte;
  signal keySchedule_part_056    : t_opaque_AESByte;
  signal keySchedule_part_057    : t_opaque_AESByte;
  signal keySchedule_part_058    : t_opaque_AESByte;
  signal keySchedule_part_059    : t_opaque_AESByte;
  signal lhs_part_20             : t_opaque_AESByte;
  signal lhs_part_21             : t_opaque_AESByte;
  signal lhs_part_22             : t_opaque_AESByte;
  signal lhs_part_23             : t_opaque_AESByte;
  signal lhs_part_24             : t_opaque_AESWord;
  signal keySchedule_part_060    : t_opaque_AESByte;
  signal keySchedule_part_061    : t_opaque_AESByte;
  signal keySchedule_part_062    : t_opaque_AESByte;
  signal keySchedule_part_063    : t_opaque_AESByte;
  signal keySchedule_part_064    : t_opaque_AESByte;
  signal keySchedule_part_065    : t_opaque_AESByte;
  signal keySchedule_part_066    : t_opaque_AESByte;
  signal keySchedule_part_067    : t_opaque_AESByte;
  signal keySchedule_part_068    : t_opaque_AESByte;
  signal keySchedule_part_069    : t_opaque_AESByte;
  signal keySchedule_part_070    : t_opaque_AESByte;
  signal keySchedule_part_071    : t_opaque_AESByte;
  signal lhs_part_25             : t_opaque_AESByte;
  signal lhs_part_26             : t_opaque_AESByte;
  signal lhs_part_27             : t_opaque_AESByte;
  signal lhs_part_28             : t_opaque_AESByte;
  signal lhs_part_29             : t_opaque_AESWord;
  signal keySchedule_part_072    : t_opaque_AESByte;
  signal keySchedule_part_073    : t_opaque_AESByte;
  signal keySchedule_part_074    : t_opaque_AESByte;
  signal keySchedule_part_075    : t_opaque_AESByte;
  signal keySchedule_part_076    : t_opaque_AESByte;
  signal keySchedule_part_077    : t_opaque_AESByte;
  signal keySchedule_part_078    : t_opaque_AESByte;
  signal keySchedule_part_079    : t_opaque_AESByte;
  signal keySchedule_part_080    : t_opaque_AESByte;
  signal keySchedule_part_081    : t_opaque_AESByte;
  signal keySchedule_part_082    : t_opaque_AESByte;
  signal keySchedule_part_083    : t_opaque_AESByte;
  signal lhs_part_30             : t_opaque_AESByte;
  signal lhs_part_31             : t_opaque_AESByte;
  signal lhs_part_32             : t_opaque_AESByte;
  signal lhs_part_33             : t_opaque_AESByte;
  signal lhs_part_34             : t_opaque_AESWord;
  signal keySchedule_part_084    : t_opaque_AESByte;
  signal keySchedule_part_085    : t_opaque_AESByte;
  signal keySchedule_part_086    : t_opaque_AESByte;
  signal keySchedule_part_087    : t_opaque_AESByte;
  signal keySchedule_part_088    : t_opaque_AESByte;
  signal keySchedule_part_089    : t_opaque_AESByte;
  signal keySchedule_part_090    : t_opaque_AESByte;
  signal keySchedule_part_091    : t_opaque_AESByte;
  signal keySchedule_part_092    : t_opaque_AESByte;
  signal keySchedule_part_093    : t_opaque_AESByte;
  signal keySchedule_part_094    : t_opaque_AESByte;
  signal keySchedule_part_095    : t_opaque_AESByte;
  signal lhs_part_35             : t_opaque_AESByte;
  signal lhs_part_36             : t_opaque_AESByte;
  signal lhs_part_37             : t_opaque_AESByte;
  signal lhs_part_38             : t_opaque_AESByte;
  signal lhs_part_39             : t_opaque_AESWord;
  signal keySchedule_part_096    : t_opaque_AESByte;
  signal keySchedule_part_097    : t_opaque_AESByte;
  signal keySchedule_part_098    : t_opaque_AESByte;
  signal keySchedule_part_099    : t_opaque_AESByte;
  signal keySchedule_part_100    : t_opaque_AESByte;
  signal keySchedule_part_101    : t_opaque_AESByte;
  signal keySchedule_part_102    : t_opaque_AESByte;
  signal keySchedule_part_103    : t_opaque_AESByte;
  signal keySchedule_part_104    : t_opaque_AESByte;
  signal keySchedule_part_105    : t_opaque_AESByte;
  signal keySchedule_part_106    : t_opaque_AESByte;
  signal keySchedule_part_107    : t_opaque_AESByte;
  signal lhs_part_40             : t_opaque_AESByte;
  signal lhs_part_41             : t_opaque_AESByte;
  signal lhs_part_42             : t_opaque_AESByte;
  signal lhs_part_43             : t_opaque_AESByte;
  signal lhs_part_44             : t_opaque_AESWord;
  signal keySchedule_part_108    : t_opaque_AESByte;
  signal keySchedule_part_109    : t_opaque_AESByte;
  signal keySchedule_part_110    : t_opaque_AESByte;
  signal keySchedule_part_111    : t_opaque_AESByte;
  signal keySchedule_part_112    : t_opaque_AESByte;
  signal keySchedule_part_113    : t_opaque_AESByte;
  signal keySchedule_part_114    : t_opaque_AESByte;
  signal keySchedule_part_115    : t_opaque_AESByte;
  signal keySchedule_part_116    : t_opaque_AESByte;
  signal keySchedule_part_117    : t_opaque_AESByte;
  signal keySchedule_part_118    : t_opaque_AESByte;
  signal keySchedule_part_119    : t_opaque_AESByte;
  signal keySchedule             : t_arrX1_t_opaque_AESWord(0 to 43);
  signal o_part_rotWord_inst_o   : t_opaque_AESWord;
  signal o_part_subWord_inst_lhs : t_opaque_AESWord;
  signal o_part_subWord_inst_o   : t_opaque_AESWord;
  signal rotWord_inst_0_o        : t_opaque_AESWord;
  signal subWord_inst_0_lhs      : t_opaque_AESWord;
  signal subWord_inst_0_o        : t_opaque_AESWord;
  signal rotWord_inst_1_o        : t_opaque_AESWord;
  signal subWord_inst_1_lhs      : t_opaque_AESWord;
  signal subWord_inst_1_o        : t_opaque_AESWord;
  signal rotWord_inst_2_o        : t_opaque_AESWord;
  signal subWord_inst_2_lhs      : t_opaque_AESWord;
  signal subWord_inst_2_o        : t_opaque_AESWord;
  signal rotWord_inst_3_o        : t_opaque_AESWord;
  signal subWord_inst_3_lhs      : t_opaque_AESWord;
  signal subWord_inst_3_o        : t_opaque_AESWord;
  signal rotWord_inst_4_o        : t_opaque_AESWord;
  signal subWord_inst_4_lhs      : t_opaque_AESWord;
  signal subWord_inst_4_o        : t_opaque_AESWord;
  signal rotWord_inst_5_o        : t_opaque_AESWord;
  signal subWord_inst_5_lhs      : t_opaque_AESWord;
  signal subWord_inst_5_o        : t_opaque_AESWord;
  signal rotWord_inst_6_o        : t_opaque_AESWord;
  signal subWord_inst_6_lhs      : t_opaque_AESWord;
  signal subWord_inst_6_o        : t_opaque_AESWord;
  signal rotWord_inst_7_o        : t_opaque_AESWord;
  signal subWord_inst_7_lhs      : t_opaque_AESWord;
  signal subWord_inst_7_o        : t_opaque_AESWord;
  signal rotWord_inst_8_o        : t_opaque_AESWord;
  signal subWord_inst_8_lhs      : t_opaque_AESWord;
  signal subWord_inst_8_o        : t_opaque_AESWord;
begin
  o_part_rotWord_inst : entity work.rotWord(rotWord_arch) port map (
    o                     => o_part_rotWord_inst_o,
    lhs                   => w_3
  );
  o_part_subWord_inst : entity work.subWord(subWord_arch) port map (
    lhs                   => o_part_subWord_inst_lhs,
    o                     => o_part_subWord_inst_o
  );
  rotWord_inst_0 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_0_o,
    lhs                   => lhs_part_04
  );
  subWord_inst_0 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_0_lhs,
    o                     => subWord_inst_0_o
  );
  rotWord_inst_1 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_1_o,
    lhs                   => lhs_part_09
  );
  subWord_inst_1 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_1_lhs,
    o                     => subWord_inst_1_o
  );
  rotWord_inst_2 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_2_o,
    lhs                   => lhs_part_14
  );
  subWord_inst_2 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_2_lhs,
    o                     => subWord_inst_2_o
  );
  rotWord_inst_3 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_3_o,
    lhs                   => lhs_part_19
  );
  subWord_inst_3 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_3_lhs,
    o                     => subWord_inst_3_o
  );
  rotWord_inst_4 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_4_o,
    lhs                   => lhs_part_24
  );
  subWord_inst_4 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_4_lhs,
    o                     => subWord_inst_4_o
  );
  rotWord_inst_5 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_5_o,
    lhs                   => lhs_part_29
  );
  subWord_inst_5 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_5_lhs,
    o                     => subWord_inst_5_o
  );
  rotWord_inst_6 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_6_o,
    lhs                   => lhs_part_34
  );
  subWord_inst_6 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_6_lhs,
    o                     => subWord_inst_6_o
  );
  rotWord_inst_7 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_7_o,
    lhs                   => lhs_part_39
  );
  subWord_inst_7 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_7_lhs,
    o                     => subWord_inst_7_o
  );
  rotWord_inst_8 : entity work.rotWord(rotWord_arch) port map (
    o                     => rotWord_inst_8_o,
    lhs                   => lhs_part_44
  );
  subWord_inst_8 : entity work.subWord(subWord_arch) port map (
    lhs                   => subWord_inst_8_lhs,
    o                     => subWord_inst_8_o
  );
  o_part_subWord_inst_lhs <= o_part_rotWord_inst_o;
  subWord_inst_0_lhs      <= rotWord_inst_0_o;
  subWord_inst_1_lhs      <= rotWord_inst_1_o;
  subWord_inst_2_lhs      <= rotWord_inst_2_o;
  subWord_inst_3_lhs      <= rotWord_inst_3_o;
  subWord_inst_4_lhs      <= rotWord_inst_4_o;
  subWord_inst_5_lhs      <= rotWord_inst_5_o;
  subWord_inst_6_lhs      <= rotWord_inst_6_o;
  subWord_inst_7_lhs      <= rotWord_inst_7_o;
  subWord_inst_8_lhs      <= rotWord_inst_8_o;
  o                       <= keySchedule;
  w_0                     <= key(0);
  w_1                     <= key(1);
  w_2                     <= key(2);
  w_3                     <= key(3);
  keySchedule_part_000    <= w_0(0) xor (o_part_subWord_inst_o(0) xor Rcon(1)(0));
  keySchedule_part_001    <= w_0(1) xor (o_part_subWord_inst_o(1) xor Rcon(1)(1));
  keySchedule_part_002    <= w_0(2) xor (o_part_subWord_inst_o(2) xor Rcon(1)(2));
  keySchedule_part_003    <= w_0(3) xor (o_part_subWord_inst_o(3) xor Rcon(1)(3));
  keySchedule_part_004    <= w_1(0) xor keySchedule_part_000;
  keySchedule_part_005    <= w_1(1) xor keySchedule_part_001;
  keySchedule_part_006    <= w_1(2) xor keySchedule_part_002;
  keySchedule_part_007    <= w_1(3) xor keySchedule_part_003;
  keySchedule_part_008    <= w_2(0) xor keySchedule_part_004;
  keySchedule_part_009    <= w_2(1) xor keySchedule_part_005;
  keySchedule_part_010    <= w_2(2) xor keySchedule_part_006;
  keySchedule_part_011    <= w_2(3) xor keySchedule_part_007;
  lhs_part_00             <= w_3(0) xor keySchedule_part_008;
  lhs_part_01             <= w_3(1) xor keySchedule_part_009;
  lhs_part_02             <= w_3(2) xor keySchedule_part_010;
  lhs_part_03             <= w_3(3) xor keySchedule_part_011;
  lhs_part_04             <= (lhs_part_00, lhs_part_01, lhs_part_02, lhs_part_03);
  keySchedule_part_012    <= keySchedule_part_000 xor (subWord_inst_0_o(0) xor Rcon(2)(0));
  keySchedule_part_013    <= keySchedule_part_001 xor (subWord_inst_0_o(1) xor Rcon(2)(1));
  keySchedule_part_014    <= keySchedule_part_002 xor (subWord_inst_0_o(2) xor Rcon(2)(2));
  keySchedule_part_015    <= keySchedule_part_003 xor (subWord_inst_0_o(3) xor Rcon(2)(3));
  keySchedule_part_016    <= keySchedule_part_004 xor keySchedule_part_012;
  keySchedule_part_017    <= keySchedule_part_005 xor keySchedule_part_013;
  keySchedule_part_018    <= keySchedule_part_006 xor keySchedule_part_014;
  keySchedule_part_019    <= keySchedule_part_007 xor keySchedule_part_015;
  keySchedule_part_020    <= keySchedule_part_008 xor keySchedule_part_016;
  keySchedule_part_021    <= keySchedule_part_009 xor keySchedule_part_017;
  keySchedule_part_022    <= keySchedule_part_010 xor keySchedule_part_018;
  keySchedule_part_023    <= keySchedule_part_011 xor keySchedule_part_019;
  lhs_part_05             <= lhs_part_00 xor keySchedule_part_020;
  lhs_part_06             <= lhs_part_01 xor keySchedule_part_021;
  lhs_part_07             <= lhs_part_02 xor keySchedule_part_022;
  lhs_part_08             <= lhs_part_03 xor keySchedule_part_023;
  lhs_part_09             <= (lhs_part_05, lhs_part_06, lhs_part_07, lhs_part_08);
  keySchedule_part_024    <= keySchedule_part_012 xor (subWord_inst_1_o(0) xor Rcon(3)(0));
  keySchedule_part_025    <= keySchedule_part_013 xor (subWord_inst_1_o(1) xor Rcon(3)(1));
  keySchedule_part_026    <= keySchedule_part_014 xor (subWord_inst_1_o(2) xor Rcon(3)(2));
  keySchedule_part_027    <= keySchedule_part_015 xor (subWord_inst_1_o(3) xor Rcon(3)(3));
  keySchedule_part_028    <= keySchedule_part_016 xor keySchedule_part_024;
  keySchedule_part_029    <= keySchedule_part_017 xor keySchedule_part_025;
  keySchedule_part_030    <= keySchedule_part_018 xor keySchedule_part_026;
  keySchedule_part_031    <= keySchedule_part_019 xor keySchedule_part_027;
  keySchedule_part_032    <= keySchedule_part_020 xor keySchedule_part_028;
  keySchedule_part_033    <= keySchedule_part_021 xor keySchedule_part_029;
  keySchedule_part_034    <= keySchedule_part_022 xor keySchedule_part_030;
  keySchedule_part_035    <= keySchedule_part_023 xor keySchedule_part_031;
  lhs_part_10             <= lhs_part_05 xor keySchedule_part_032;
  lhs_part_11             <= lhs_part_06 xor keySchedule_part_033;
  lhs_part_12             <= lhs_part_07 xor keySchedule_part_034;
  lhs_part_13             <= lhs_part_08 xor keySchedule_part_035;
  lhs_part_14             <= (lhs_part_10, lhs_part_11, lhs_part_12, lhs_part_13);
  keySchedule_part_036    <= keySchedule_part_024 xor (subWord_inst_2_o(0) xor Rcon(4)(0));
  keySchedule_part_037    <= keySchedule_part_025 xor (subWord_inst_2_o(1) xor Rcon(4)(1));
  keySchedule_part_038    <= keySchedule_part_026 xor (subWord_inst_2_o(2) xor Rcon(4)(2));
  keySchedule_part_039    <= keySchedule_part_027 xor (subWord_inst_2_o(3) xor Rcon(4)(3));
  keySchedule_part_040    <= keySchedule_part_028 xor keySchedule_part_036;
  keySchedule_part_041    <= keySchedule_part_029 xor keySchedule_part_037;
  keySchedule_part_042    <= keySchedule_part_030 xor keySchedule_part_038;
  keySchedule_part_043    <= keySchedule_part_031 xor keySchedule_part_039;
  keySchedule_part_044    <= keySchedule_part_032 xor keySchedule_part_040;
  keySchedule_part_045    <= keySchedule_part_033 xor keySchedule_part_041;
  keySchedule_part_046    <= keySchedule_part_034 xor keySchedule_part_042;
  keySchedule_part_047    <= keySchedule_part_035 xor keySchedule_part_043;
  lhs_part_15             <= lhs_part_10 xor keySchedule_part_044;
  lhs_part_16             <= lhs_part_11 xor keySchedule_part_045;
  lhs_part_17             <= lhs_part_12 xor keySchedule_part_046;
  lhs_part_18             <= lhs_part_13 xor keySchedule_part_047;
  lhs_part_19             <= (lhs_part_15, lhs_part_16, lhs_part_17, lhs_part_18);
  keySchedule_part_048    <= keySchedule_part_036 xor (subWord_inst_3_o(0) xor Rcon(5)(0));
  keySchedule_part_049    <= keySchedule_part_037 xor (subWord_inst_3_o(1) xor Rcon(5)(1));
  keySchedule_part_050    <= keySchedule_part_038 xor (subWord_inst_3_o(2) xor Rcon(5)(2));
  keySchedule_part_051    <= keySchedule_part_039 xor (subWord_inst_3_o(3) xor Rcon(5)(3));
  keySchedule_part_052    <= keySchedule_part_040 xor keySchedule_part_048;
  keySchedule_part_053    <= keySchedule_part_041 xor keySchedule_part_049;
  keySchedule_part_054    <= keySchedule_part_042 xor keySchedule_part_050;
  keySchedule_part_055    <= keySchedule_part_043 xor keySchedule_part_051;
  keySchedule_part_056    <= keySchedule_part_044 xor keySchedule_part_052;
  keySchedule_part_057    <= keySchedule_part_045 xor keySchedule_part_053;
  keySchedule_part_058    <= keySchedule_part_046 xor keySchedule_part_054;
  keySchedule_part_059    <= keySchedule_part_047 xor keySchedule_part_055;
  lhs_part_20             <= lhs_part_15 xor keySchedule_part_056;
  lhs_part_21             <= lhs_part_16 xor keySchedule_part_057;
  lhs_part_22             <= lhs_part_17 xor keySchedule_part_058;
  lhs_part_23             <= lhs_part_18 xor keySchedule_part_059;
  lhs_part_24             <= (lhs_part_20, lhs_part_21, lhs_part_22, lhs_part_23);
  keySchedule_part_060    <= keySchedule_part_048 xor (subWord_inst_4_o(0) xor Rcon(6)(0));
  keySchedule_part_061    <= keySchedule_part_049 xor (subWord_inst_4_o(1) xor Rcon(6)(1));
  keySchedule_part_062    <= keySchedule_part_050 xor (subWord_inst_4_o(2) xor Rcon(6)(2));
  keySchedule_part_063    <= keySchedule_part_051 xor (subWord_inst_4_o(3) xor Rcon(6)(3));
  keySchedule_part_064    <= keySchedule_part_052 xor keySchedule_part_060;
  keySchedule_part_065    <= keySchedule_part_053 xor keySchedule_part_061;
  keySchedule_part_066    <= keySchedule_part_054 xor keySchedule_part_062;
  keySchedule_part_067    <= keySchedule_part_055 xor keySchedule_part_063;
  keySchedule_part_068    <= keySchedule_part_056 xor keySchedule_part_064;
  keySchedule_part_069    <= keySchedule_part_057 xor keySchedule_part_065;
  keySchedule_part_070    <= keySchedule_part_058 xor keySchedule_part_066;
  keySchedule_part_071    <= keySchedule_part_059 xor keySchedule_part_067;
  lhs_part_25             <= lhs_part_20 xor keySchedule_part_068;
  lhs_part_26             <= lhs_part_21 xor keySchedule_part_069;
  lhs_part_27             <= lhs_part_22 xor keySchedule_part_070;
  lhs_part_28             <= lhs_part_23 xor keySchedule_part_071;
  lhs_part_29             <= (lhs_part_25, lhs_part_26, lhs_part_27, lhs_part_28);
  keySchedule_part_072    <= keySchedule_part_060 xor (subWord_inst_5_o(0) xor Rcon(7)(0));
  keySchedule_part_073    <= keySchedule_part_061 xor (subWord_inst_5_o(1) xor Rcon(7)(1));
  keySchedule_part_074    <= keySchedule_part_062 xor (subWord_inst_5_o(2) xor Rcon(7)(2));
  keySchedule_part_075    <= keySchedule_part_063 xor (subWord_inst_5_o(3) xor Rcon(7)(3));
  keySchedule_part_076    <= keySchedule_part_064 xor keySchedule_part_072;
  keySchedule_part_077    <= keySchedule_part_065 xor keySchedule_part_073;
  keySchedule_part_078    <= keySchedule_part_066 xor keySchedule_part_074;
  keySchedule_part_079    <= keySchedule_part_067 xor keySchedule_part_075;
  keySchedule_part_080    <= keySchedule_part_068 xor keySchedule_part_076;
  keySchedule_part_081    <= keySchedule_part_069 xor keySchedule_part_077;
  keySchedule_part_082    <= keySchedule_part_070 xor keySchedule_part_078;
  keySchedule_part_083    <= keySchedule_part_071 xor keySchedule_part_079;
  lhs_part_30             <= lhs_part_25 xor keySchedule_part_080;
  lhs_part_31             <= lhs_part_26 xor keySchedule_part_081;
  lhs_part_32             <= lhs_part_27 xor keySchedule_part_082;
  lhs_part_33             <= lhs_part_28 xor keySchedule_part_083;
  lhs_part_34             <= (lhs_part_30, lhs_part_31, lhs_part_32, lhs_part_33);
  keySchedule_part_084    <= keySchedule_part_072 xor (subWord_inst_6_o(0) xor Rcon(8)(0));
  keySchedule_part_085    <= keySchedule_part_073 xor (subWord_inst_6_o(1) xor Rcon(8)(1));
  keySchedule_part_086    <= keySchedule_part_074 xor (subWord_inst_6_o(2) xor Rcon(8)(2));
  keySchedule_part_087    <= keySchedule_part_075 xor (subWord_inst_6_o(3) xor Rcon(8)(3));
  keySchedule_part_088    <= keySchedule_part_076 xor keySchedule_part_084;
  keySchedule_part_089    <= keySchedule_part_077 xor keySchedule_part_085;
  keySchedule_part_090    <= keySchedule_part_078 xor keySchedule_part_086;
  keySchedule_part_091    <= keySchedule_part_079 xor keySchedule_part_087;
  keySchedule_part_092    <= keySchedule_part_080 xor keySchedule_part_088;
  keySchedule_part_093    <= keySchedule_part_081 xor keySchedule_part_089;
  keySchedule_part_094    <= keySchedule_part_082 xor keySchedule_part_090;
  keySchedule_part_095    <= keySchedule_part_083 xor keySchedule_part_091;
  lhs_part_35             <= lhs_part_30 xor keySchedule_part_092;
  lhs_part_36             <= lhs_part_31 xor keySchedule_part_093;
  lhs_part_37             <= lhs_part_32 xor keySchedule_part_094;
  lhs_part_38             <= lhs_part_33 xor keySchedule_part_095;
  lhs_part_39             <= (lhs_part_35, lhs_part_36, lhs_part_37, lhs_part_38);
  keySchedule_part_096    <= keySchedule_part_084 xor (subWord_inst_7_o(0) xor Rcon(9)(0));
  keySchedule_part_097    <= keySchedule_part_085 xor (subWord_inst_7_o(1) xor Rcon(9)(1));
  keySchedule_part_098    <= keySchedule_part_086 xor (subWord_inst_7_o(2) xor Rcon(9)(2));
  keySchedule_part_099    <= keySchedule_part_087 xor (subWord_inst_7_o(3) xor Rcon(9)(3));
  keySchedule_part_100    <= keySchedule_part_088 xor keySchedule_part_096;
  keySchedule_part_101    <= keySchedule_part_089 xor keySchedule_part_097;
  keySchedule_part_102    <= keySchedule_part_090 xor keySchedule_part_098;
  keySchedule_part_103    <= keySchedule_part_091 xor keySchedule_part_099;
  keySchedule_part_104    <= keySchedule_part_092 xor keySchedule_part_100;
  keySchedule_part_105    <= keySchedule_part_093 xor keySchedule_part_101;
  keySchedule_part_106    <= keySchedule_part_094 xor keySchedule_part_102;
  keySchedule_part_107    <= keySchedule_part_095 xor keySchedule_part_103;
  lhs_part_40             <= lhs_part_35 xor keySchedule_part_104;
  lhs_part_41             <= lhs_part_36 xor keySchedule_part_105;
  lhs_part_42             <= lhs_part_37 xor keySchedule_part_106;
  lhs_part_43             <= lhs_part_38 xor keySchedule_part_107;
  lhs_part_44             <= (lhs_part_40, lhs_part_41, lhs_part_42, lhs_part_43);
  keySchedule_part_108    <= keySchedule_part_096 xor (subWord_inst_8_o(0) xor Rcon(10)(0));
  keySchedule_part_109    <= keySchedule_part_097 xor (subWord_inst_8_o(1) xor Rcon(10)(1));
  keySchedule_part_110    <= keySchedule_part_098 xor (subWord_inst_8_o(2) xor Rcon(10)(2));
  keySchedule_part_111    <= keySchedule_part_099 xor (subWord_inst_8_o(3) xor Rcon(10)(3));
  keySchedule_part_112    <= keySchedule_part_100 xor keySchedule_part_108;
  keySchedule_part_113    <= keySchedule_part_101 xor keySchedule_part_109;
  keySchedule_part_114    <= keySchedule_part_102 xor keySchedule_part_110;
  keySchedule_part_115    <= keySchedule_part_103 xor keySchedule_part_111;
  keySchedule_part_116    <= keySchedule_part_104 xor keySchedule_part_112;
  keySchedule_part_117    <= keySchedule_part_105 xor keySchedule_part_113;
  keySchedule_part_118    <= keySchedule_part_106 xor keySchedule_part_114;
  keySchedule_part_119    <= keySchedule_part_107 xor keySchedule_part_115;
  keySchedule             <= (
    w_0,
    w_1,
    w_2,
    w_3,
    (keySchedule_part_000, keySchedule_part_001, keySchedule_part_002, keySchedule_part_003),
    (keySchedule_part_004, keySchedule_part_005, keySchedule_part_006, keySchedule_part_007),
    (keySchedule_part_008, keySchedule_part_009, keySchedule_part_010, keySchedule_part_011),
    lhs_part_04,
    (keySchedule_part_012, keySchedule_part_013, keySchedule_part_014, keySchedule_part_015),
    (keySchedule_part_016, keySchedule_part_017, keySchedule_part_018, keySchedule_part_019),
    (keySchedule_part_020, keySchedule_part_021, keySchedule_part_022, keySchedule_part_023),
    lhs_part_09,
    (keySchedule_part_024, keySchedule_part_025, keySchedule_part_026, keySchedule_part_027),
    (keySchedule_part_028, keySchedule_part_029, keySchedule_part_030, keySchedule_part_031),
    (keySchedule_part_032, keySchedule_part_033, keySchedule_part_034, keySchedule_part_035),
    lhs_part_14,
    (keySchedule_part_036, keySchedule_part_037, keySchedule_part_038, keySchedule_part_039),
    (keySchedule_part_040, keySchedule_part_041, keySchedule_part_042, keySchedule_part_043),
    (keySchedule_part_044, keySchedule_part_045, keySchedule_part_046, keySchedule_part_047),
    lhs_part_19,
    (keySchedule_part_048, keySchedule_part_049, keySchedule_part_050, keySchedule_part_051),
    (keySchedule_part_052, keySchedule_part_053, keySchedule_part_054, keySchedule_part_055),
    (keySchedule_part_056, keySchedule_part_057, keySchedule_part_058, keySchedule_part_059),
    lhs_part_24,
    (keySchedule_part_060, keySchedule_part_061, keySchedule_part_062, keySchedule_part_063),
    (keySchedule_part_064, keySchedule_part_065, keySchedule_part_066, keySchedule_part_067),
    (keySchedule_part_068, keySchedule_part_069, keySchedule_part_070, keySchedule_part_071),
    lhs_part_29,
    (keySchedule_part_072, keySchedule_part_073, keySchedule_part_074, keySchedule_part_075),
    (keySchedule_part_076, keySchedule_part_077, keySchedule_part_078, keySchedule_part_079),
    (keySchedule_part_080, keySchedule_part_081, keySchedule_part_082, keySchedule_part_083),
    lhs_part_34,
    (keySchedule_part_084, keySchedule_part_085, keySchedule_part_086, keySchedule_part_087),
    (keySchedule_part_088, keySchedule_part_089, keySchedule_part_090, keySchedule_part_091),
    (keySchedule_part_092, keySchedule_part_093, keySchedule_part_094, keySchedule_part_095),
    lhs_part_39,
    (keySchedule_part_096, keySchedule_part_097, keySchedule_part_098, keySchedule_part_099),
    (keySchedule_part_100, keySchedule_part_101, keySchedule_part_102, keySchedule_part_103),
    (keySchedule_part_104, keySchedule_part_105, keySchedule_part_106, keySchedule_part_107),
    lhs_part_44,
    (keySchedule_part_108, keySchedule_part_109, keySchedule_part_110, keySchedule_part_111),
    (keySchedule_part_112, keySchedule_part_113, keySchedule_part_114, keySchedule_part_115),
    (keySchedule_part_116, keySchedule_part_117, keySchedule_part_118, keySchedule_part_119),
    (
      lhs_part_40 xor keySchedule_part_116, lhs_part_41 xor keySchedule_part_117,
      lhs_part_42 xor keySchedule_part_118, lhs_part_43 xor keySchedule_part_119
    )
  );
end keyExpansion_arch;
