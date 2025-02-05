-- A two-bits left shifter 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LeftShift2_pkg.all;

entity LeftShift2 is
port (
  -- bits input 
  iBits : in  std_logic_vector(7 downto 0);
  -- bits output 
  oBits : out std_logic_vector(7 downto 0)
);
end LeftShift2;

architecture LeftShift2_arch of LeftShift2 is
begin
  oBits <= slv_sll(iBits, 2);
end LeftShift2_arch;
