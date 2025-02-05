library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.CipherNoOpaques_pkg.all;

entity subWord is
port (
  lhs : in  t_opaque_AESWord;
  o   : out t_opaque_AESWord
);
end subWord;

architecture subWord_arch of subWord is
  signal o_part_sbox_inst_lhs : t_opaque_AESByte;
  signal o_part_sbox_inst_o   : t_opaque_AESByte;
  signal sbox_inst_0_lhs      : t_opaque_AESByte;
  signal sbox_inst_0_o        : t_opaque_AESByte;
  signal sbox_inst_1_lhs      : t_opaque_AESByte;
  signal sbox_inst_1_o        : t_opaque_AESByte;
  signal sbox_inst_2_lhs      : t_opaque_AESByte;
  signal sbox_inst_2_o        : t_opaque_AESByte;
begin
  o_part_sbox_inst : entity work.sbox(sbox_arch) port map (
    lhs                => o_part_sbox_inst_lhs,
    o                  => o_part_sbox_inst_o
  );
  sbox_inst_0 : entity work.sbox(sbox_arch) port map (
    lhs                => sbox_inst_0_lhs,
    o                  => sbox_inst_0_o
  );
  sbox_inst_1 : entity work.sbox(sbox_arch) port map (
    lhs                => sbox_inst_1_lhs,
    o                  => sbox_inst_1_o
  );
  sbox_inst_2 : entity work.sbox(sbox_arch) port map (
    lhs                => sbox_inst_2_lhs,
    o                  => sbox_inst_2_o
  );
  o_part_sbox_inst_lhs <= lhs(0);
  sbox_inst_0_lhs      <= lhs(1);
  sbox_inst_1_lhs      <= lhs(2);
  sbox_inst_2_lhs      <= lhs(3);
  o                    <= (o_part_sbox_inst_o, sbox_inst_0_o, sbox_inst_1_o, sbox_inst_2_o);
end subWord_arch;
