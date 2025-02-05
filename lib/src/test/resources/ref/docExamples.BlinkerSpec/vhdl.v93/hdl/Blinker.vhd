-- This is a led blinker 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Blinker_pkg.all;

entity Blinker is
generic (
  CLK_FREQ_KHz : integer := 50000;
  LED_FREQ_Hz : integer := 1
);
port (
  clk : in  std_logic;
  rst : in  std_logic;
  -- LED output 
  led : out std_logic
);
end Blinker;

architecture Blinker_arch of Blinker is
  -- Half-count of the toggle for 50% duty cycle 
  constant HALF_PERIOD : integer := (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
  signal led_sig       : std_logic;
  signal cnt           : unsigned(clog2(HALF_PERIOD) - 1 downto 0);
begin
  led             <= led_sig;
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        led_sig   <= '1';
        cnt       <= to_unsigned(0, clog2(HALF_PERIOD));
      else
        if cnt = to_unsigned(HALF_PERIOD - 1, clog2(HALF_PERIOD)) then
          cnt     <= to_unsigned(0, clog2(HALF_PERIOD));
          led_sig <= not led_sig;
        else cnt <= cnt + to_unsigned(1, clog2(HALF_PERIOD));
        end if;
      end if;
    end if;
  end process;
end Blinker_arch;
