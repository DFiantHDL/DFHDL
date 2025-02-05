-- A basic left shifter 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LeftShiftBasic_pkg.all;

entity LeftShiftBasic is
port (
  -- bits input 
  iBits : in  std_logic_vector(7 downto 0);
  -- requested shift 
  shift : in  unsigned(2 downto 0);
  -- bits output 
  oBits : out std_logic_vector(7 downto 0)
);
end LeftShiftBasic;

architecture LeftShiftBasic_arch of LeftShiftBasic is
begin
  oBits <= slv_sll(iBits, to_integer(shift));
end LeftShiftBasic_arch;
