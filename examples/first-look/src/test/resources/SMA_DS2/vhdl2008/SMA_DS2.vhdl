library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SMA_DS2_pkg.all;

entity SMA_DS2 is
port (
  clk                 : in  std_logic;
  rst                 : in  std_logic;
  x                   : in  signed(15 downto 0) := 16d"0";
  y                   : out signed(15 downto 0)
);
end SMA_DS2;

architecture SMA_DS2_arch of SMA_DS2 is  
  signal x_prev1      : signed(15 downto 0) := 16d"0";
  signal s0_prev1     : signed(16 downto 0) := 17d"0";
  signal s2           : signed(16 downto 0) := 17d"0";
  signal s0_sig       : signed(16 downto 0);
  signal s0_prev1_sig : signed(16 downto 0);
begin
  async_proc : process (all)  
    variable s0       : signed(16 downto 0);
    variable sum      : signed(17 downto 0);
  begin
    s0                := resize(x, 17) + x_prev1;
    sum               := resize(s0, 18) + s2;
    s0_sig            <= s0;
    s0_prev1_sig      <= s0_prev1;
    y                 <= resize(sum / 4, 16);
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      x_prev1         <= 16d"0";
      s0_prev1        <= 17d"0";
      s2              <= 17d"0";
    elsif rising_edge(clk) then
      x_prev1         <= x;
      s0_prev1        <= s0_sig;
      s2              <= s0_prev1_sig;
    end if;
  end process;
end SMA_DS2_arch;