library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.CipherNoOpaques_pkg.all;

entity mulByte_0 is
generic (
  lhs : std_logic_vector(7 downto 0)
);
port (
  rhs : in  t_opaque_AESByte;
  o   : out t_opaque_AESByte
);
end mulByte_0;

architecture mulByte_0_arch of mulByte_0 is
  signal a_lhs : t_opaque_AESByte;
  signal a_o   : t_opaque_AESByte;
begin
  a : entity work.xtime(xtime_arch) port map (
    lhs => a_lhs,
    o   => a_o
  );
  a_lhs <= rhs;
  o     <= x"00" xor a_o;
end mulByte_0_arch;
