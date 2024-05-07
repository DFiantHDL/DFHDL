library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.IDTop_pkg.all;

entity ID is
port (
  x   : in  signed(15 downto 0);
  y   : out signed(15 downto 0)
);
end ID;

architecture ID_arch of ID is
begin
  async_proc : process (all)
  begin
    y <= x;
  end process;
end ID_arch;