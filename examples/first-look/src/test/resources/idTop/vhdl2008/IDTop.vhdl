library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.IDTop_pkg.all;

entity IDTop is
port (
  x            : in  signed(15 downto 0);
  y            : out signed(15 downto 0)
);
end IDTop;

architecture IDTop_arch of IDTop is  
  signal id1_x : signed(15 downto 0);
  signal id1_y : signed(15 downto 0);
  signal id2_x : signed(15 downto 0);
  signal id2_y : signed(15 downto 0);
begin
  id1 : entity work.ID(ID_arch) port map (
    x          => id1_x,
    y          => id1_y
  );
  id2 : entity work.ID(ID_arch) port map (
    x          => id2_x,
    y          => id2_y
  );
  async_proc : process (all)
  begin
    id1_x      <= x;
    id2_x      <= id1_y;
    y          <= id2_y;
  end process;
end IDTop_arch;