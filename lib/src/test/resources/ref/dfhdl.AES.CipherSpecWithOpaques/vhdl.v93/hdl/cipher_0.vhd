library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity cipher_0 is
port (
  data : in  t_opaque_AESData;
  key  : in  t_opaque_AESKey;
  o    : out t_opaque_AESData
);
end cipher_0;

architecture cipher_0_arch of cipher_0 is
  signal keySchedule_key                : t_opaque_AESKey;
  signal keySchedule_o                  : t_opaque_AESKeySchedule;
  signal state_00_state                 : t_opaque_AESState;
  signal state_00_key                   : t_opaque_AESRoundKey;
  signal state_00_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_00_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_00_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_00_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_00_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_0_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_0_o     : t_opaque_AESState;
  signal state_01_state                 : t_opaque_AESState;
  signal state_01_key                   : t_opaque_AESRoundKey;
  signal state_01_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_01_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_01_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_01_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_01_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_1_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_1_o     : t_opaque_AESState;
  signal state_02_state                 : t_opaque_AESState;
  signal state_02_key                   : t_opaque_AESRoundKey;
  signal state_02_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_02_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_02_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_02_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_02_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_2_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_2_o     : t_opaque_AESState;
  signal state_03_state                 : t_opaque_AESState;
  signal state_03_key                   : t_opaque_AESRoundKey;
  signal state_03_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_03_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_03_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_03_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_03_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_3_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_3_o     : t_opaque_AESState;
  signal state_04_state                 : t_opaque_AESState;
  signal state_04_key                   : t_opaque_AESRoundKey;
  signal state_04_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_04_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_04_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_04_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_04_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_4_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_4_o     : t_opaque_AESState;
  signal state_05_state                 : t_opaque_AESState;
  signal state_05_key                   : t_opaque_AESRoundKey;
  signal state_05_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_05_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_05_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_05_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_05_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_5_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_5_o     : t_opaque_AESState;
  signal state_06_state                 : t_opaque_AESState;
  signal state_06_key                   : t_opaque_AESRoundKey;
  signal state_06_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_06_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_06_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_06_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_06_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_6_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_6_o     : t_opaque_AESState;
  signal state_07_state                 : t_opaque_AESState;
  signal state_07_key                   : t_opaque_AESRoundKey;
  signal state_07_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_07_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_07_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_07_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_07_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_7_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_7_o     : t_opaque_AESState;
  signal state_08_state                 : t_opaque_AESState;
  signal state_08_key                   : t_opaque_AESRoundKey;
  signal state_08_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_08_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_08_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_08_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_08_o     : t_opaque_AESState;
  signal o_part_mixColumns_inst_8_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_8_o     : t_opaque_AESState;
  signal state_09_state                 : t_opaque_AESState;
  signal state_09_key                   : t_opaque_AESRoundKey;
  signal state_09_o                     : t_opaque_AESState;
  signal o_part_subBytes_inst_09_state  : t_opaque_AESState;
  signal o_part_subBytes_inst_09_o      : t_opaque_AESState;
  signal o_part_shiftRows_inst_09_state : t_opaque_AESState;
  signal o_part_shiftRows_inst_09_o     : t_opaque_AESState;
  signal state_10_state                 : t_opaque_AESState;
  signal state_10_key                   : t_opaque_AESRoundKey;
  signal state_10_o                     : t_opaque_AESState;
begin
  keySchedule : entity work.keyExpansion(keyExpansion_arch) port map (
    key                          => keySchedule_key,
    o                            => keySchedule_o
  );
  state_00 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_00_state,
    key                          => state_00_key,
    o                            => state_00_o
  );
  o_part_subBytes_inst_00 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_00_state,
    o                            => o_part_subBytes_inst_00_o
  );
  o_part_shiftRows_inst_00 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_00_state,
    o                            => o_part_shiftRows_inst_00_o
  );
  o_part_mixColumns_inst_0 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_0_state,
    o                            => o_part_mixColumns_inst_0_o
  );
  state_01 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_01_state,
    key                          => state_01_key,
    o                            => state_01_o
  );
  o_part_subBytes_inst_01 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_01_state,
    o                            => o_part_subBytes_inst_01_o
  );
  o_part_shiftRows_inst_01 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_01_state,
    o                            => o_part_shiftRows_inst_01_o
  );
  o_part_mixColumns_inst_1 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_1_state,
    o                            => o_part_mixColumns_inst_1_o
  );
  state_02 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_02_state,
    key                          => state_02_key,
    o                            => state_02_o
  );
  o_part_subBytes_inst_02 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_02_state,
    o                            => o_part_subBytes_inst_02_o
  );
  o_part_shiftRows_inst_02 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_02_state,
    o                            => o_part_shiftRows_inst_02_o
  );
  o_part_mixColumns_inst_2 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_2_state,
    o                            => o_part_mixColumns_inst_2_o
  );
  state_03 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_03_state,
    key                          => state_03_key,
    o                            => state_03_o
  );
  o_part_subBytes_inst_03 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_03_state,
    o                            => o_part_subBytes_inst_03_o
  );
  o_part_shiftRows_inst_03 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_03_state,
    o                            => o_part_shiftRows_inst_03_o
  );
  o_part_mixColumns_inst_3 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_3_state,
    o                            => o_part_mixColumns_inst_3_o
  );
  state_04 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_04_state,
    key                          => state_04_key,
    o                            => state_04_o
  );
  o_part_subBytes_inst_04 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_04_state,
    o                            => o_part_subBytes_inst_04_o
  );
  o_part_shiftRows_inst_04 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_04_state,
    o                            => o_part_shiftRows_inst_04_o
  );
  o_part_mixColumns_inst_4 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_4_state,
    o                            => o_part_mixColumns_inst_4_o
  );
  state_05 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_05_state,
    key                          => state_05_key,
    o                            => state_05_o
  );
  o_part_subBytes_inst_05 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_05_state,
    o                            => o_part_subBytes_inst_05_o
  );
  o_part_shiftRows_inst_05 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_05_state,
    o                            => o_part_shiftRows_inst_05_o
  );
  o_part_mixColumns_inst_5 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_5_state,
    o                            => o_part_mixColumns_inst_5_o
  );
  state_06 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_06_state,
    key                          => state_06_key,
    o                            => state_06_o
  );
  o_part_subBytes_inst_06 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_06_state,
    o                            => o_part_subBytes_inst_06_o
  );
  o_part_shiftRows_inst_06 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_06_state,
    o                            => o_part_shiftRows_inst_06_o
  );
  o_part_mixColumns_inst_6 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_6_state,
    o                            => o_part_mixColumns_inst_6_o
  );
  state_07 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_07_state,
    key                          => state_07_key,
    o                            => state_07_o
  );
  o_part_subBytes_inst_07 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_07_state,
    o                            => o_part_subBytes_inst_07_o
  );
  o_part_shiftRows_inst_07 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_07_state,
    o                            => o_part_shiftRows_inst_07_o
  );
  o_part_mixColumns_inst_7 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_7_state,
    o                            => o_part_mixColumns_inst_7_o
  );
  state_08 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_08_state,
    key                          => state_08_key,
    o                            => state_08_o
  );
  o_part_subBytes_inst_08 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_08_state,
    o                            => o_part_subBytes_inst_08_o
  );
  o_part_shiftRows_inst_08 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_08_state,
    o                            => o_part_shiftRows_inst_08_o
  );
  o_part_mixColumns_inst_8 : entity work.mixColumns(mixColumns_arch) port map (
    state                        => o_part_mixColumns_inst_8_state,
    o                            => o_part_mixColumns_inst_8_o
  );
  state_09 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_09_state,
    key                          => state_09_key,
    o                            => state_09_o
  );
  o_part_subBytes_inst_09 : entity work.subBytes(subBytes_arch) port map (
    state                        => o_part_subBytes_inst_09_state,
    o                            => o_part_subBytes_inst_09_o
  );
  o_part_shiftRows_inst_09 : entity work.shiftRows(shiftRows_arch) port map (
    state                        => o_part_shiftRows_inst_09_state,
    o                            => o_part_shiftRows_inst_09_o
  );
  state_10 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                        => state_10_state,
    key                          => state_10_key,
    o                            => state_10_o
  );
  keySchedule_key                <= key;
  state_00_state                 <= data;
  state_00_key <= (0 => keySchedule_o(0), 1 => keySchedule_o(1), 2 => keySchedule_o(2), 3 => keySchedule_o(3));
  o_part_subBytes_inst_00_state  <= state_00_o;
  o_part_shiftRows_inst_00_state <= o_part_subBytes_inst_00_o;
  o_part_mixColumns_inst_0_state <= o_part_shiftRows_inst_00_o;
  state_01_state                 <= o_part_mixColumns_inst_0_o;
  state_01_key <= (0 => keySchedule_o(4), 1 => keySchedule_o(5), 2 => keySchedule_o(6), 3 => keySchedule_o(7));
  o_part_subBytes_inst_01_state  <= state_01_o;
  o_part_shiftRows_inst_01_state <= o_part_subBytes_inst_01_o;
  o_part_mixColumns_inst_1_state <= o_part_shiftRows_inst_01_o;
  state_02_state                 <= o_part_mixColumns_inst_1_o;
  state_02_key <= (0 => keySchedule_o(8), 1 => keySchedule_o(9), 2 => keySchedule_o(10), 3 => keySchedule_o(11));
  o_part_subBytes_inst_02_state  <= state_02_o;
  o_part_shiftRows_inst_02_state <= o_part_subBytes_inst_02_o;
  o_part_mixColumns_inst_2_state <= o_part_shiftRows_inst_02_o;
  state_03_state                 <= o_part_mixColumns_inst_2_o;
  state_03_key <= (0 => keySchedule_o(12), 1 => keySchedule_o(13), 2 => keySchedule_o(14), 3 => keySchedule_o(15));
  o_part_subBytes_inst_03_state  <= state_03_o;
  o_part_shiftRows_inst_03_state <= o_part_subBytes_inst_03_o;
  o_part_mixColumns_inst_3_state <= o_part_shiftRows_inst_03_o;
  state_04_state                 <= o_part_mixColumns_inst_3_o;
  state_04_key <= (0 => keySchedule_o(16), 1 => keySchedule_o(17), 2 => keySchedule_o(18), 3 => keySchedule_o(19));
  o_part_subBytes_inst_04_state  <= state_04_o;
  o_part_shiftRows_inst_04_state <= o_part_subBytes_inst_04_o;
  o_part_mixColumns_inst_4_state <= o_part_shiftRows_inst_04_o;
  state_05_state                 <= o_part_mixColumns_inst_4_o;
  state_05_key <= (0 => keySchedule_o(20), 1 => keySchedule_o(21), 2 => keySchedule_o(22), 3 => keySchedule_o(23));
  o_part_subBytes_inst_05_state  <= state_05_o;
  o_part_shiftRows_inst_05_state <= o_part_subBytes_inst_05_o;
  o_part_mixColumns_inst_5_state <= o_part_shiftRows_inst_05_o;
  state_06_state                 <= o_part_mixColumns_inst_5_o;
  state_06_key <= (0 => keySchedule_o(24), 1 => keySchedule_o(25), 2 => keySchedule_o(26), 3 => keySchedule_o(27));
  o_part_subBytes_inst_06_state  <= state_06_o;
  o_part_shiftRows_inst_06_state <= o_part_subBytes_inst_06_o;
  o_part_mixColumns_inst_6_state <= o_part_shiftRows_inst_06_o;
  state_07_state                 <= o_part_mixColumns_inst_6_o;
  state_07_key <= (0 => keySchedule_o(28), 1 => keySchedule_o(29), 2 => keySchedule_o(30), 3 => keySchedule_o(31));
  o_part_subBytes_inst_07_state  <= state_07_o;
  o_part_shiftRows_inst_07_state <= o_part_subBytes_inst_07_o;
  o_part_mixColumns_inst_7_state <= o_part_shiftRows_inst_07_o;
  state_08_state                 <= o_part_mixColumns_inst_7_o;
  state_08_key <= (0 => keySchedule_o(32), 1 => keySchedule_o(33), 2 => keySchedule_o(34), 3 => keySchedule_o(35));
  o_part_subBytes_inst_08_state  <= state_08_o;
  o_part_shiftRows_inst_08_state <= o_part_subBytes_inst_08_o;
  o_part_mixColumns_inst_8_state <= o_part_shiftRows_inst_08_o;
  state_09_state                 <= o_part_mixColumns_inst_8_o;
  state_09_key <= (0 => keySchedule_o(36), 1 => keySchedule_o(37), 2 => keySchedule_o(38), 3 => keySchedule_o(39));
  o_part_subBytes_inst_09_state  <= state_09_o;
  o_part_shiftRows_inst_09_state <= o_part_subBytes_inst_09_o;
  state_10_state                 <= o_part_shiftRows_inst_09_o;
  state_10_key <= (0 => keySchedule_o(40), 1 => keySchedule_o(41), 2 => keySchedule_o(42), 3 => keySchedule_o(43));
  o                              <= state_10_o;
end cipher_0_arch;
