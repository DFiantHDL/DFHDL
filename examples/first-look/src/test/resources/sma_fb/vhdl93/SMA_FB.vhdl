library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SMA_FB_pkg.all;

entity SMA_FB is
port (
  clk                : in  std_logic;
  rst                : in  std_logic;
  x                  : in  signed(15 downto 0) := to_signed(0, 16);
  y                  : out signed(15 downto 0)
);
end SMA_FB;

architecture SMA_FB_arch of SMA_FB is  
  signal x_prev1     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev2     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev3     : signed(15 downto 0) := to_signed(0, 16);
  signal x_prev4     : signed(15 downto 0) := to_signed(0, 16);
  signal acc_prev1   : signed(17 downto 0) := to_signed(0, 18);
  signal x_prev1_sig : signed(15 downto 0);
  signal x_prev2_sig : signed(15 downto 0);
  signal x_prev3_sig : signed(15 downto 0);
  signal acc_sig     : signed(17 downto 0);
begin
  async_proc : process (clk, acc_prev1, x_prev4, x, x_prev1, x_prev2, x_prev3, rst)  
    variable acc     : signed(17 downto 0) := to_signed(0, 18);
  begin
    acc              := acc_prev1;
    acc              := (acc - x_prev4) + x;
    x_prev1_sig      <= x_prev1;
    x_prev2_sig      <= x_prev2;
    x_prev3_sig      <= x_prev3;
    acc_sig          <= acc;
    y                <= resize(shift_right(acc, to_integer(to_unsigned(2, 2))), 16);
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      x_prev1        <= to_signed(0, 16);
      x_prev2        <= to_signed(0, 16);
      x_prev3        <= to_signed(0, 16);
      x_prev4        <= to_signed(0, 16);
      acc_prev1      <= to_signed(0, 18);
    elsif rising_edge(clk) then
      x_prev1        <= x;
      x_prev2        <= x_prev1_sig;
      x_prev3        <= x_prev2_sig;
      x_prev4        <= x_prev3_sig;
      acc_prev1      <= acc_sig;
    end if;
  end process;
end SMA_FB_arch;