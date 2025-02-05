library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity xtime is
port (
  lhs : in  t_opaque_AESByte;
  o   : out t_opaque_AESByte
);
end xtime;

architecture xtime_arch of xtime is
  signal shifted : std_logic_vector(7 downto 0);
  signal anon    : std_logic_vector(7 downto 0);
begin
  o       <= anon;
  process (all)
  begin
    if lhs(7) then anon <= shifted xor x"1b";
    else anon <= shifted;
    end if;
  end process;
  shifted <= slv_sll(lhs, 1);
end xtime_arch;
