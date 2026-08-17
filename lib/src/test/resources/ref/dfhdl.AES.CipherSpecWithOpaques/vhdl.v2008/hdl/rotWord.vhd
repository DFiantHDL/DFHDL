library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity rotWord is
port (
  lhs : in  AESWord;
  o   : out AESWord
);
end rotWord;

architecture rotWord_arch of rotWord is
begin
  o <= (0 => lhs(1), 1 => lhs(2), 2 => lhs(3), 3 => lhs(0));
end rotWord_arch;
