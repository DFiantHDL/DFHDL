library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity rotWord is
port (
  lhs : in  t_opaque_AESWord;
  o   : out t_opaque_AESWord
);
end rotWord;

architecture rotWord_arch of rotWord is
begin
  o <= (lhs(1), lhs(2), lhs(3), lhs(0));
end rotWord_arch;
