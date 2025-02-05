library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Counter_pkg.all;

entity Counter is
generic (
  width : integer := 8
);
port (
  clk : in  std_logic;
  rst : in  std_logic;
  en  : in  std_logic;
  cnt : out unsigned(width - 1 downto 0)
);
end Counter;

architecture Counter_arch of Counter is
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then cnt <= resize(d"0", width);
      else
        if en then cnt <= cnt + resize(d"1", width);
        end if;
      end if;
    end if;
  end process;
end Counter_arch;
