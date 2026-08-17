library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.CipherNoOpaques_pkg.all;

entity CipherNoOpaques is
port (
  key  : in  AESKey;
  data : in  AESData;
  o    : out AESData
);
end CipherNoOpaques;

architecture CipherNoOpaques_arch of CipherNoOpaques is
  signal o_part_cipher_inst_data : AESData;
  signal o_part_cipher_inst_key  : AESKey;
  signal o_part_cipher_inst_o    : AESData;
begin
  o_part_cipher_inst : entity work.cipher(cipher_arch) port map (
    data                  => o_part_cipher_inst_data,
    key                   => o_part_cipher_inst_key,
    o                     => o_part_cipher_inst_o
  );
  o_part_cipher_inst_data <= data;
  o_part_cipher_inst_key  <= key;
  o                       <= o_part_cipher_inst_o;
end CipherNoOpaques_arch;
