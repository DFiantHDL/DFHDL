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
  signal cnt_sig : unsigned(width - 1 downto 0);
begin
  cnt <= cnt_sig;
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then cnt_sig <= to_unsigned(0, width);
      else
        if to_bool(en) then cnt_sig <= cnt_sig + to_unsigned(1, width);
        end if;
      end if;
    end if;
  end process;
end Counter_arch;
