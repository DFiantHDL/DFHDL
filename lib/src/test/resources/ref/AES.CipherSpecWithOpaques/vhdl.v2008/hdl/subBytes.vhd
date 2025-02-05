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
  signal sbox_inst_00_lhs : t_opaque_AESByte;
  signal sbox_inst_00_o   : t_opaque_AESByte;
  signal sbox_inst_01_lhs : t_opaque_AESByte;
  signal sbox_inst_01_o   : t_opaque_AESByte;
  signal sbox_inst_02_lhs : t_opaque_AESByte;
  signal sbox_inst_02_o   : t_opaque_AESByte;
  signal sbox_inst_03_lhs : t_opaque_AESByte;
  signal sbox_inst_03_o   : t_opaque_AESByte;
  signal sbox_inst_04_lhs : t_opaque_AESByte;
  signal sbox_inst_04_o   : t_opaque_AESByte;
  signal sbox_inst_05_lhs : t_opaque_AESByte;
  signal sbox_inst_05_o   : t_opaque_AESByte;
  signal sbox_inst_06_lhs : t_opaque_AESByte;
  signal sbox_inst_06_o   : t_opaque_AESByte;
  signal sbox_inst_07_lhs : t_opaque_AESByte;
  signal sbox_inst_07_o   : t_opaque_AESByte;
  signal sbox_inst_08_lhs : t_opaque_AESByte;
  signal sbox_inst_08_o   : t_opaque_AESByte;
  signal sbox_inst_09_lhs : t_opaque_AESByte;
  signal sbox_inst_09_o   : t_opaque_AESByte;
  signal sbox_inst_10_lhs : t_opaque_AESByte;
  signal sbox_inst_10_o   : t_opaque_AESByte;
  signal sbox_inst_11_lhs : t_opaque_AESByte;
  signal sbox_inst_11_o   : t_opaque_AESByte;
  signal sbox_inst_12_lhs : t_opaque_AESByte;
  signal sbox_inst_12_o   : t_opaque_AESByte;
  signal sbox_inst_13_lhs : t_opaque_AESByte;
  signal sbox_inst_13_o   : t_opaque_AESByte;
  signal sbox_inst_14_lhs : t_opaque_AESByte;
  signal sbox_inst_14_o   : t_opaque_AESByte;
  signal sbox_inst_15_lhs : t_opaque_AESByte;
  signal sbox_inst_15_o   : t_opaque_AESByte;
begin
  sbox_inst_00 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_00_lhs,
    o              => sbox_inst_00_o
  );
  sbox_inst_01 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_01_lhs,
    o              => sbox_inst_01_o
  );
  sbox_inst_02 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_02_lhs,
    o              => sbox_inst_02_o
  );
  sbox_inst_03 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_03_lhs,
    o              => sbox_inst_03_o
  );
  sbox_inst_04 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_04_lhs,
    o              => sbox_inst_04_o
  );
  sbox_inst_05 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_05_lhs,
    o              => sbox_inst_05_o
  );
  sbox_inst_06 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_06_lhs,
    o              => sbox_inst_06_o
  );
  sbox_inst_07 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_07_lhs,
    o              => sbox_inst_07_o
  );
  sbox_inst_08 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_08_lhs,
    o              => sbox_inst_08_o
  );
  sbox_inst_09 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_09_lhs,
    o              => sbox_inst_09_o
  );
  sbox_inst_10 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_10_lhs,
    o              => sbox_inst_10_o
  );
  sbox_inst_11 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_11_lhs,
    o              => sbox_inst_11_o
  );
  sbox_inst_12 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_12_lhs,
    o              => sbox_inst_12_o
  );
  sbox_inst_13 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_13_lhs,
    o              => sbox_inst_13_o
  );
  sbox_inst_14 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_14_lhs,
    o              => sbox_inst_14_o
  );
  sbox_inst_15 : entity work.sbox(sbox_arch) port map (
    lhs            => sbox_inst_15_lhs,
    o              => sbox_inst_15_o
  );
  sbox_inst_00_lhs <= state(0)(0);
  sbox_inst_01_lhs <= state(0)(1);
  sbox_inst_02_lhs <= state(0)(2);
  sbox_inst_03_lhs <= state(0)(3);
  sbox_inst_04_lhs <= state(1)(0);
  sbox_inst_05_lhs <= state(1)(1);
  sbox_inst_06_lhs <= state(1)(2);
  sbox_inst_07_lhs <= state(1)(3);
  sbox_inst_08_lhs <= state(2)(0);
  sbox_inst_09_lhs <= state(2)(1);
  sbox_inst_10_lhs <= state(2)(2);
  sbox_inst_11_lhs <= state(2)(3);
  sbox_inst_12_lhs <= state(3)(0);
  sbox_inst_13_lhs <= state(3)(1);
  sbox_inst_14_lhs <= state(3)(2);
  sbox_inst_15_lhs <= state(3)(3);
  o                <= (
    (sbox_inst_00_o, sbox_inst_01_o, sbox_inst_02_o, sbox_inst_03_o),
    (sbox_inst_04_o, sbox_inst_05_o, sbox_inst_06_o, sbox_inst_07_o),
    (sbox_inst_08_o, sbox_inst_09_o, sbox_inst_10_o, sbox_inst_11_o),
    (sbox_inst_12_o, sbox_inst_13_o, sbox_inst_14_o, sbox_inst_15_o)
  );
end subBytes_arch;
