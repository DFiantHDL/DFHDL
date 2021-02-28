library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SMA_DS_pkg.all;

entity SMA_DS is
port (
  clk                : in  std_logic;
  rst                : in  std_logic;
  x                  : in  signed(15 downto 0) := to_signed(0, 16);
  y                  : out signed(15 downto 0)
);
end SMA_DS;

architecture SMA_DS_arch of SMA_DS is  
  signal x_prev1     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev2     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev3     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev1_sig : signed(15 downto 0);
  signal x_prev2_sig : signed(15 downto 0);
begin
  async_proc : process (clk, x, x_prev1, x_prev2, x_prev3, x_prev1, x_prev2, rst)  
    variable s0      : signed(16 downto 0);
    variable s2      : signed(16 downto 0);
    variable sum     : signed(17 downto 0);
  begin
    s0               := resize(x, 17) + x_prev1;
    s2               := resize(x_prev2, 17) + x_prev3;
    sum              := resize(s0, 18) + s2;
    x_prev1_sig      <= x_prev1;
    x_prev2_sig      <= x_prev2;
    y                <= resize(sum / 4, 16);
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
end SMA_DS_arch;