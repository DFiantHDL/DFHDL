-- A generic left shifter 
--   
-- @param width
--   the width of the input and output bits
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LeftShiftGen_pkg.all;

entity LeftShiftGen is
generic (
  width : integer := 8
);
port (
  -- bits input 
  iBits : in  std_logic_vector(width - 1 downto 0);
  -- requested shift 
  shift : in  unsigned(clog2(width) - 1 downto 0);
  -- bits output 
  oBits : out std_logic_vector(width - 1 downto 0)
);
end LeftShiftGen;

architecture LeftShiftGen_arch of LeftShiftGen is
begin
  oBits <= slv_sll(iBits, to_integer(shift));
end LeftShiftGen_arch;
