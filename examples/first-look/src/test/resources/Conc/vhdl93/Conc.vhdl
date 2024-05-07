library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.Conc_pkg.all;

entity Conc is
port (
  i   : in  unsigned(31 downto 0);
  j   : in  unsigned(31 downto 0);
  a   : out unsigned(31 downto 0);
  b   : out unsigned(31 downto 0);
  c   : out unsigned(31 downto 0);
  d   : out unsigned(31 downto 0);
  e   : out unsigned(31 downto 0)
);
end Conc;

architecture Conc_arch of Conc is
begin
  async_proc : process (i, j)
  begin
    a <= i + 5;
    b <= resize(a * 3, 32);
    c <= a + b;
    d <= i - 1;
    e <= j / 4;
  end process;
end Conc_arch;