library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity subBytes is
port (
  state : in  t_opaque_AESState;
  o     : out t_opaque_AESState
);
end subBytes;

architecture subBytes_arch of subBytes is
  signal o_part_sbox_inst_00_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_00_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_01_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_01_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_02_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_02_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_03_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_03_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_04_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_04_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_05_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_05_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_06_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_06_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_07_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_07_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_08_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_08_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_09_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_09_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_10_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_10_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_11_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_11_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_12_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_12_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_13_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_13_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_14_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_14_o   : t_opaque_AESByte;
  signal o_part_sbox_inst_15_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_15_o   : t_opaque_AESByte;
begin
  o_part_sbox_inst_00 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_00_lhs,
    o                     => o_part_sbox_inst_00_o
  );
  o_part_sbox_inst_01 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_01_lhs,
    o                     => o_part_sbox_inst_01_o
  );
  o_part_sbox_inst_02 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_02_lhs,
    o                     => o_part_sbox_inst_02_o
  );
  o_part_sbox_inst_03 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_03_lhs,
    o                     => o_part_sbox_inst_03_o
  );
  o_part_sbox_inst_04 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_04_lhs,
    o                     => o_part_sbox_inst_04_o
  );
  o_part_sbox_inst_05 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_05_lhs,
    o                     => o_part_sbox_inst_05_o
  );
  o_part_sbox_inst_06 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_06_lhs,
    o                     => o_part_sbox_inst_06_o
  );
  o_part_sbox_inst_07 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_07_lhs,
    o                     => o_part_sbox_inst_07_o
  );
  o_part_sbox_inst_08 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_08_lhs,
    o                     => o_part_sbox_inst_08_o
  );
  o_part_sbox_inst_09 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_09_lhs,
    o                     => o_part_sbox_inst_09_o
  );
  o_part_sbox_inst_10 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_10_lhs,
    o                     => o_part_sbox_inst_10_o
  );
  o_part_sbox_inst_11 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_11_lhs,
    o                     => o_part_sbox_inst_11_o
  );
  o_part_sbox_inst_12 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_12_lhs,
    o                     => o_part_sbox_inst_12_o
  );
  o_part_sbox_inst_13 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_13_lhs,
    o                     => o_part_sbox_inst_13_o
  );
  o_part_sbox_inst_14 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_14_lhs,
    o                     => o_part_sbox_inst_14_o
  );
  o_part_sbox_inst_15 : entity work.sbox(sbox_arch) port map (
    lhs                   => o_part_sbox_inst_15_lhs,
    o                     => o_part_sbox_inst_15_o
  );
  o_part_sbox_inst_00_lhs <= state(0)(0);
  o_part_sbox_inst_01_lhs <= state(0)(1);
  o_part_sbox_inst_02_lhs <= state(0)(2);
  o_part_sbox_inst_03_lhs <= state(0)(3);
  o_part_sbox_inst_04_lhs <= state(1)(0);
  o_part_sbox_inst_05_lhs <= state(1)(1);
  o_part_sbox_inst_06_lhs <= state(1)(2);
  o_part_sbox_inst_07_lhs <= state(1)(3);
  o_part_sbox_inst_08_lhs <= state(2)(0);
  o_part_sbox_inst_09_lhs <= state(2)(1);
  o_part_sbox_inst_10_lhs <= state(2)(2);
  o_part_sbox_inst_11_lhs <= state(2)(3);
  o_part_sbox_inst_12_lhs <= state(3)(0);
  o_part_sbox_inst_13_lhs <= state(3)(1);
  o_part_sbox_inst_14_lhs <= state(3)(2);
  o_part_sbox_inst_15_lhs <= state(3)(3);
  o                       <= (
    0 => (0 => o_part_sbox_inst_00_o, 1 => o_part_sbox_inst_01_o, 2 => o_part_sbox_inst_02_o, 3 => o_part_sbox_inst_03_o),
    1 => (0 => o_part_sbox_inst_04_o, 1 => o_part_sbox_inst_05_o, 2 => o_part_sbox_inst_06_o, 3 => o_part_sbox_inst_07_o),
    2 => (0 => o_part_sbox_inst_08_o, 1 => o_part_sbox_inst_09_o, 2 => o_part_sbox_inst_10_o, 3 => o_part_sbox_inst_11_o),
    3 => (0 => o_part_sbox_inst_12_o, 1 => o_part_sbox_inst_13_o, 2 => o_part_sbox_inst_14_o, 3 => o_part_sbox_inst_15_o)
  );
end subBytes_arch;
