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
  signal keySchedule_key              : t_opaque_AESKey;
  signal keySchedule_o                : t_opaque_AESKeySchedule;
  signal state_00_state               : t_opaque_AESState;
  signal state_00_key                 : t_opaque_AESRoundKey;
  signal state_00_o                   : t_opaque_AESState;
  signal o_part_subBytes_inst_state   : t_opaque_AESState;
  signal o_part_subBytes_inst_o       : t_opaque_AESState;
  signal o_part_shiftRows_inst_state  : t_opaque_AESState;
  signal o_part_shiftRows_inst_o      : t_opaque_AESState;
  signal o_part_mixColumns_inst_state : t_opaque_AESState;
  signal o_part_mixColumns_inst_o     : t_opaque_AESState;
  signal state_01_state               : t_opaque_AESState;
  signal state_01_key                 : t_opaque_AESRoundKey;
  signal state_01_o                   : t_opaque_AESState;
  signal subBytes_inst_0_state        : t_opaque_AESState;
  signal subBytes_inst_0_o            : t_opaque_AESState;
  signal shiftRows_inst_0_state       : t_opaque_AESState;
  signal shiftRows_inst_0_o           : t_opaque_AESState;
  signal mixColumns_inst_0_state      : t_opaque_AESState;
  signal mixColumns_inst_0_o          : t_opaque_AESState;
  signal state_02_state               : t_opaque_AESState;
  signal state_02_key                 : t_opaque_AESRoundKey;
  signal state_02_o                   : t_opaque_AESState;
  signal subBytes_inst_1_state        : t_opaque_AESState;
  signal subBytes_inst_1_o            : t_opaque_AESState;
  signal shiftRows_inst_1_state       : t_opaque_AESState;
  signal shiftRows_inst_1_o           : t_opaque_AESState;
  signal mixColumns_inst_1_state      : t_opaque_AESState;
  signal mixColumns_inst_1_o          : t_opaque_AESState;
  signal state_03_state               : t_opaque_AESState;
  signal state_03_key                 : t_opaque_AESRoundKey;
  signal state_03_o                   : t_opaque_AESState;
  signal subBytes_inst_2_state        : t_opaque_AESState;
  signal subBytes_inst_2_o            : t_opaque_AESState;
  signal shiftRows_inst_2_state       : t_opaque_AESState;
  signal shiftRows_inst_2_o           : t_opaque_AESState;
  signal mixColumns_inst_2_state      : t_opaque_AESState;
  signal mixColumns_inst_2_o          : t_opaque_AESState;
  signal state_04_state               : t_opaque_AESState;
  signal state_04_key                 : t_opaque_AESRoundKey;
  signal state_04_o                   : t_opaque_AESState;
  signal subBytes_inst_3_state        : t_opaque_AESState;
  signal subBytes_inst_3_o            : t_opaque_AESState;
  signal shiftRows_inst_3_state       : t_opaque_AESState;
  signal shiftRows_inst_3_o           : t_opaque_AESState;
  signal mixColumns_inst_3_state      : t_opaque_AESState;
  signal mixColumns_inst_3_o          : t_opaque_AESState;
  signal state_05_state               : t_opaque_AESState;
  signal state_05_key                 : t_opaque_AESRoundKey;
  signal state_05_o                   : t_opaque_AESState;
  signal subBytes_inst_4_state        : t_opaque_AESState;
  signal subBytes_inst_4_o            : t_opaque_AESState;
  signal shiftRows_inst_4_state       : t_opaque_AESState;
  signal shiftRows_inst_4_o           : t_opaque_AESState;
  signal mixColumns_inst_4_state      : t_opaque_AESState;
  signal mixColumns_inst_4_o          : t_opaque_AESState;
  signal state_06_state               : t_opaque_AESState;
  signal state_06_key                 : t_opaque_AESRoundKey;
  signal state_06_o                   : t_opaque_AESState;
  signal subBytes_inst_5_state        : t_opaque_AESState;
  signal subBytes_inst_5_o            : t_opaque_AESState;
  signal shiftRows_inst_5_state       : t_opaque_AESState;
  signal shiftRows_inst_5_o           : t_opaque_AESState;
  signal mixColumns_inst_5_state      : t_opaque_AESState;
  signal mixColumns_inst_5_o          : t_opaque_AESState;
  signal state_07_state               : t_opaque_AESState;
  signal state_07_key                 : t_opaque_AESRoundKey;
  signal state_07_o                   : t_opaque_AESState;
  signal subBytes_inst_6_state        : t_opaque_AESState;
  signal subBytes_inst_6_o            : t_opaque_AESState;
  signal shiftRows_inst_6_state       : t_opaque_AESState;
  signal shiftRows_inst_6_o           : t_opaque_AESState;
  signal mixColumns_inst_6_state      : t_opaque_AESState;
  signal mixColumns_inst_6_o          : t_opaque_AESState;
  signal state_08_state               : t_opaque_AESState;
  signal state_08_key                 : t_opaque_AESRoundKey;
  signal state_08_o                   : t_opaque_AESState;
  signal subBytes_inst_7_state        : t_opaque_AESState;
  signal subBytes_inst_7_o            : t_opaque_AESState;
  signal shiftRows_inst_7_state       : t_opaque_AESState;
  signal shiftRows_inst_7_o           : t_opaque_AESState;
  signal mixColumns_inst_7_state      : t_opaque_AESState;
  signal mixColumns_inst_7_o          : t_opaque_AESState;
  signal state_09_state               : t_opaque_AESState;
  signal state_09_key                 : t_opaque_AESRoundKey;
  signal state_09_o                   : t_opaque_AESState;
  signal subBytes_inst_8_state        : t_opaque_AESState;
  signal subBytes_inst_8_o            : t_opaque_AESState;
  signal shiftRows_inst_8_state       : t_opaque_AESState;
  signal shiftRows_inst_8_o           : t_opaque_AESState;
  signal state_10_state               : t_opaque_AESState;
  signal state_10_key                 : t_opaque_AESRoundKey;
  signal state_10_o                   : t_opaque_AESState;
begin
  keySchedule : entity work.keyExpansion(keyExpansion_arch) port map (
    key                        => keySchedule_key,
    o                          => keySchedule_o
  );
  state_00 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_00_state,
    key                        => state_00_key,
    o                          => state_00_o
  );
  o_part_subBytes_inst : entity work.subBytes(subBytes_arch) port map (
    state                      => o_part_subBytes_inst_state,
    o                          => o_part_subBytes_inst_o
  );
  o_part_shiftRows_inst : entity work.shiftRows(shiftRows_arch) port map (
    state                      => o_part_shiftRows_inst_state,
    o                          => o_part_shiftRows_inst_o
  );
  o_part_mixColumns_inst : entity work.mixColumns(mixColumns_arch) port map (
    state                      => o_part_mixColumns_inst_state,
    o                          => o_part_mixColumns_inst_o
  );
  state_01 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_01_state,
    key                        => state_01_key,
    o                          => state_01_o
  );
  subBytes_inst_0 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_0_state,
    o                          => subBytes_inst_0_o
  );
  shiftRows_inst_0 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_0_state,
    o                          => shiftRows_inst_0_o
  );
  mixColumns_inst_0 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_0_state,
    o                          => mixColumns_inst_0_o
  );
  state_02 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_02_state,
    key                        => state_02_key,
    o                          => state_02_o
  );
  subBytes_inst_1 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_1_state,
    o                          => subBytes_inst_1_o
  );
  shiftRows_inst_1 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_1_state,
    o                          => shiftRows_inst_1_o
  );
  mixColumns_inst_1 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_1_state,
    o                          => mixColumns_inst_1_o
  );
  state_03 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_03_state,
    key                        => state_03_key,
    o                          => state_03_o
  );
  subBytes_inst_2 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_2_state,
    o                          => subBytes_inst_2_o
  );
  shiftRows_inst_2 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_2_state,
    o                          => shiftRows_inst_2_o
  );
  mixColumns_inst_2 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_2_state,
    o                          => mixColumns_inst_2_o
  );
  state_04 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_04_state,
    key                        => state_04_key,
    o                          => state_04_o
  );
  subBytes_inst_3 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_3_state,
    o                          => subBytes_inst_3_o
  );
  shiftRows_inst_3 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_3_state,
    o                          => shiftRows_inst_3_o
  );
  mixColumns_inst_3 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_3_state,
    o                          => mixColumns_inst_3_o
  );
  state_05 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_05_state,
    key                        => state_05_key,
    o                          => state_05_o
  );
  subBytes_inst_4 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_4_state,
    o                          => subBytes_inst_4_o
  );
  shiftRows_inst_4 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_4_state,
    o                          => shiftRows_inst_4_o
  );
  mixColumns_inst_4 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_4_state,
    o                          => mixColumns_inst_4_o
  );
  state_06 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_06_state,
    key                        => state_06_key,
    o                          => state_06_o
  );
  subBytes_inst_5 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_5_state,
    o                          => subBytes_inst_5_o
  );
  shiftRows_inst_5 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_5_state,
    o                          => shiftRows_inst_5_o
  );
  mixColumns_inst_5 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_5_state,
    o                          => mixColumns_inst_5_o
  );
  state_07 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_07_state,
    key                        => state_07_key,
    o                          => state_07_o
  );
  subBytes_inst_6 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_6_state,
    o                          => subBytes_inst_6_o
  );
  shiftRows_inst_6 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_6_state,
    o                          => shiftRows_inst_6_o
  );
  mixColumns_inst_6 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_6_state,
    o                          => mixColumns_inst_6_o
  );
  state_08 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_08_state,
    key                        => state_08_key,
    o                          => state_08_o
  );
  subBytes_inst_7 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_7_state,
    o                          => subBytes_inst_7_o
  );
  shiftRows_inst_7 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_7_state,
    o                          => shiftRows_inst_7_o
  );
  mixColumns_inst_7 : entity work.mixColumns(mixColumns_arch) port map (
    state                      => mixColumns_inst_7_state,
    o                          => mixColumns_inst_7_o
  );
  state_09 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_09_state,
    key                        => state_09_key,
    o                          => state_09_o
  );
  subBytes_inst_8 : entity work.subBytes(subBytes_arch) port map (
    state                      => subBytes_inst_8_state,
    o                          => subBytes_inst_8_o
  );
  shiftRows_inst_8 : entity work.shiftRows(shiftRows_arch) port map (
    state                      => shiftRows_inst_8_state,
    o                          => shiftRows_inst_8_o
  );
  state_10 : entity work.addRoundKey(addRoundKey_arch) port map (
    state                      => state_10_state,
    key                        => state_10_key,
    o                          => state_10_o
  );
  keySchedule_key              <= key;
  state_00_state               <= data;
  state_00_key                 <= (keySchedule_o(0), keySchedule_o(1), keySchedule_o(2), keySchedule_o(3));
  o_part_subBytes_inst_state   <= state_00_o;
  o_part_shiftRows_inst_state  <= o_part_subBytes_inst_o;
  o_part_mixColumns_inst_state <= o_part_shiftRows_inst_o;
  state_01_state               <= o_part_mixColumns_inst_o;
  state_01_key                 <= (keySchedule_o(4), keySchedule_o(5), keySchedule_o(6), keySchedule_o(7));
  subBytes_inst_0_state        <= state_01_o;
  shiftRows_inst_0_state       <= subBytes_inst_0_o;
  mixColumns_inst_0_state      <= shiftRows_inst_0_o;
  state_02_state               <= mixColumns_inst_0_o;
  state_02_key                 <= (keySchedule_o(8), keySchedule_o(9), keySchedule_o(10), keySchedule_o(11));
  subBytes_inst_1_state        <= state_02_o;
  shiftRows_inst_1_state       <= subBytes_inst_1_o;
  mixColumns_inst_1_state      <= shiftRows_inst_1_o;
  state_03_state               <= mixColumns_inst_1_o;
  state_03_key                 <= (keySchedule_o(12), keySchedule_o(13), keySchedule_o(14), keySchedule_o(15));
  subBytes_inst_2_state        <= state_03_o;
  shiftRows_inst_2_state       <= subBytes_inst_2_o;
  mixColumns_inst_2_state      <= shiftRows_inst_2_o;
  state_04_state               <= mixColumns_inst_2_o;
  state_04_key                 <= (keySchedule_o(16), keySchedule_o(17), keySchedule_o(18), keySchedule_o(19));
  subBytes_inst_3_state        <= state_04_o;
  shiftRows_inst_3_state       <= subBytes_inst_3_o;
  mixColumns_inst_3_state      <= shiftRows_inst_3_o;
  state_05_state               <= mixColumns_inst_3_o;
  state_05_key                 <= (keySchedule_o(20), keySchedule_o(21), keySchedule_o(22), keySchedule_o(23));
  subBytes_inst_4_state        <= state_05_o;
  shiftRows_inst_4_state       <= subBytes_inst_4_o;
  mixColumns_inst_4_state      <= shiftRows_inst_4_o;
  state_06_state               <= mixColumns_inst_4_o;
  state_06_key                 <= (keySchedule_o(24), keySchedule_o(25), keySchedule_o(26), keySchedule_o(27));
  subBytes_inst_5_state        <= state_06_o;
  shiftRows_inst_5_state       <= subBytes_inst_5_o;
  mixColumns_inst_5_state      <= shiftRows_inst_5_o;
  state_07_state               <= mixColumns_inst_5_o;
  state_07_key                 <= (keySchedule_o(28), keySchedule_o(29), keySchedule_o(30), keySchedule_o(31));
  subBytes_inst_6_state        <= state_07_o;
  shiftRows_inst_6_state       <= subBytes_inst_6_o;
  mixColumns_inst_6_state      <= shiftRows_inst_6_o;
  state_08_state               <= mixColumns_inst_6_o;
  state_08_key                 <= (keySchedule_o(32), keySchedule_o(33), keySchedule_o(34), keySchedule_o(35));
  subBytes_inst_7_state        <= state_08_o;
  shiftRows_inst_7_state       <= subBytes_inst_7_o;
  mixColumns_inst_7_state      <= shiftRows_inst_7_o;
  state_09_state               <= mixColumns_inst_7_o;
  state_09_key                 <= (keySchedule_o(36), keySchedule_o(37), keySchedule_o(38), keySchedule_o(39));
  subBytes_inst_8_state        <= state_09_o;
  shiftRows_inst_8_state       <= subBytes_inst_8_o;
  state_10_state               <= shiftRows_inst_8_o;
  state_10_key                 <= (keySchedule_o(40), keySchedule_o(41), keySchedule_o(42), keySchedule_o(43));
  o                            <= state_10_o;
end cipher_0_arch;
