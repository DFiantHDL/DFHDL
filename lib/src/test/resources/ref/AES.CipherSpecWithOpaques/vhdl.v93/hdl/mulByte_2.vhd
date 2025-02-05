library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity mulByte_2 is
generic (
  lhs : std_logic_vector(7 downto 0)
);
port (
  rhs : in  t_opaque_AESByte;
  o   : out t_opaque_AESByte
);
end mulByte_2;

architecture mulByte_2_arch of mulByte_2 is
begin
  o <= x"00" xor rhs;
end mulByte_2_arch;
