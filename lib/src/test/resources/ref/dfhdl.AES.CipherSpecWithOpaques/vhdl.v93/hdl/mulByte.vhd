library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity mulByte_0 is
generic (
  lhs : std_logic_vector(7 downto 0)
);
port (
  rhs : in  AESByte;
  o   : out AESByte
);
end mulByte_0;

architecture mulByte_0_arch of mulByte_0 is
  signal a_lhs : AESByte;
  signal a_o   : AESByte;
begin
  a : entity work.xtime(xtime_arch) port map (
    lhs => a_lhs,
    o   => a_o
  );
  a_lhs <= rhs;
  o     <= x"00" xor a_o;
end mulByte_0_arch;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity mulByte_1 is
generic (
  lhs : std_logic_vector(7 downto 0)
);
port (
  rhs : in  AESByte;
  o   : out AESByte
);
end mulByte_1;

architecture mulByte_1_arch of mulByte_1 is
  signal a_lhs : AESByte;
  signal a_o   : AESByte;
begin
  a : entity work.xtime(xtime_arch) port map (
    lhs => a_lhs,
    o   => a_o
  );
  a_lhs <= rhs;
  o     <= x"00" xor rhs xor a_o;
end mulByte_1_arch;

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
  rhs : in  AESByte;
  o   : out AESByte
);
end mulByte_2;

architecture mulByte_2_arch of mulByte_2 is
begin
  o <= x"00" xor rhs;
end mulByte_2_arch;
