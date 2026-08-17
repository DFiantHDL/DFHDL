library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity sbox is
port (
  lhs : in  AESByte;
  o   : out AESByte
);
end sbox;

architecture sbox_arch of sbox is
begin
  o <= sboxLookupTable(to_integer(unsigned(lhs)));
end sbox_arch;
