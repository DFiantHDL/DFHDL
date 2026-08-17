library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity subWord is
port (
  lhs : in  AESWord;
  o   : out AESWord
);
end subWord;

architecture subWord_arch of subWord is
  signal o_part_sbox_inst_0_lhs : AESByte;
  signal o_part_sbox_inst_0_o   : AESByte;
  signal o_part_sbox_inst_1_lhs : AESByte;
  signal o_part_sbox_inst_1_o   : AESByte;
  signal o_part_sbox_inst_2_lhs : AESByte;
  signal o_part_sbox_inst_2_o   : AESByte;
  signal o_part_sbox_inst_3_lhs : AESByte;
  signal o_part_sbox_inst_3_o   : AESByte;
begin
  o_part_sbox_inst_0 : entity work.sbox(sbox_arch) port map (
    lhs                  => o_part_sbox_inst_0_lhs,
    o                    => o_part_sbox_inst_0_o
  );
  o_part_sbox_inst_1 : entity work.sbox(sbox_arch) port map (
    lhs                  => o_part_sbox_inst_1_lhs,
    o                    => o_part_sbox_inst_1_o
  );
  o_part_sbox_inst_2 : entity work.sbox(sbox_arch) port map (
    lhs                  => o_part_sbox_inst_2_lhs,
    o                    => o_part_sbox_inst_2_o
  );
  o_part_sbox_inst_3 : entity work.sbox(sbox_arch) port map (
    lhs                  => o_part_sbox_inst_3_lhs,
    o                    => o_part_sbox_inst_3_o
  );
  o_part_sbox_inst_0_lhs <= lhs(0);
  o_part_sbox_inst_1_lhs <= lhs(1);
  o_part_sbox_inst_2_lhs <= lhs(2);
  o_part_sbox_inst_3_lhs <= lhs(3);
  o <= (0 => o_part_sbox_inst_0_o, 1 => o_part_sbox_inst_1_o, 2 => o_part_sbox_inst_2_o, 3 => o_part_sbox_inst_3_o);
end subWord_arch;
