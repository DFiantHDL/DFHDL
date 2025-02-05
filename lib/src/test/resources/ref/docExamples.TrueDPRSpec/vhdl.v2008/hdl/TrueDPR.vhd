library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.TrueDPR_pkg.all;

entity TrueDPR is
generic (
  DATA_WIDTH : integer := 8;
  ADDR_WIDTH : integer := 8
);
port (
  a_clk  : in  std_logic;
  a_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
  a_addr : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
  a_q    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
  a_we   : in  std_logic;
  b_clk  : in  std_logic;
  b_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
  b_addr : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
  b_q    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
  b_we   : in  std_logic
);
end TrueDPR;

architecture TrueDPR_arch of TrueDPR is
  type t_arrX1_std_logic_vector is array (natural range <>) of std_logic_vector;
  shared variable ram : t_arrX1_std_logic_vector(0 to 2 ** ADDR_WIDTH - 1)(DATA_WIDTH - 1 downto 0);
begin
  process (a_clk)
  begin
    if rising_edge(a_clk) then
      if a_we then ram(to_integer(unsigned(a_addr))) := a_data;
      end if;
      a_q <= ram(to_integer(unsigned(a_addr)));
    end if;
  end process;
  process (b_clk)
  begin
    if rising_edge(b_clk) then
      if b_we then ram(to_integer(unsigned(b_addr))) := b_data;
      end if;
      b_q <= ram(to_integer(unsigned(b_addr)));
    end if;
  end process;
end TrueDPR_arch;
