library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.RegFile_pkg.all;

entity RegFile is
generic (
  DATA_WIDTH : integer := 32;
  REG_NUM : integer := 32
);
port (
  clk      : in  std_logic;
  rs1_addr : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
  rs1_data : out std_logic_vector(DATA_WIDTH - 1 downto 0);
  rs2_addr : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
  rs2_data : out std_logic_vector(DATA_WIDTH - 1 downto 0);
  rd_addr  : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
  rd_data  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
  rd_wren  : in  std_logic
);
end RegFile;

architecture RegFile_arch of RegFile is
  type t_arrXPREG_NUM_slvPDATA_WIDTH is array (0 to REG_NUM - 1) of std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal regs : t_arrXPREG_NUM_slvPDATA_WIDTH;
begin
  process (clk)
  begin
    if rising_edge(clk) then rs1_data <= regs(to_integer(unsigned(rs1_addr)));
    end if;
  end process;
  process (clk)
  begin
    if rising_edge(clk) then rs2_data <= regs(to_integer(unsigned(rs2_addr)));
    end if;
  end process;
  process (clk)
  begin
    if rising_edge(clk) then
      if to_bool(rd_wren) then regs(to_integer(unsigned(rd_addr))) <= rd_data;
      end if;
      regs(0) <= repeat("0", DATA_WIDTH);
    end if;
  end process;
end RegFile_arch;
