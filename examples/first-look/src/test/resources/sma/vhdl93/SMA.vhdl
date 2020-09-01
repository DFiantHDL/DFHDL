library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SMA_pkg.all;

entity SMA is
port (
  clk                : in  std_logic;
  rst                : in  std_logic;
  x                  : in  signed(15 downto 0) := to_signed(0, 16);
  y                  : out signed(15 downto 0)
);
end SMA;

architecture SMA_arch of SMA is  
  signal x_prev1     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev2     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev3     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev1_sig : signed(15 downto 0);
  signal x_prev2_sig : signed(15 downto 0);
begin
  async_proc : process (clk, x, x_prev1, x_prev2, x_prev3, x_prev1, x_prev2, rst)  
    variable sum     : signed(15 downto 0) := to_signed(0, 16);
  begin
    sum              := (x + x_prev1) + (x_prev2 + x_prev3);
    x_prev1_sig      <= x_prev1;
    x_prev2_sig      <= x_prev2;
    y                <= sum;
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      x_prev1        <= to_signed(0, 16);
      x_prev2        <= to_signed(0, 16);
      x_prev3        <= to_signed(0, 16);
    elsif rising_edge(clk) then
      x_prev1        <= x;
      x_prev2        <= x_prev1_sig;
      x_prev3        <= x_prev2_sig;
    end if;
  end process;
end SMA_arch;