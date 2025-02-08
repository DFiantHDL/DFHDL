-- A generic right shifter 
--   
-- @param width
--   the width of the input and output bits
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LRShiftDirect_pkg.all;

entity RightShiftGen is
generic (
  width : integer
);
port (
  -- bits input 
  iBits : in  std_logic_vector(width - 1 downto 0);
  -- requested shift 
  shift : in  unsigned(clog2(width) - 1 downto 0);
  -- bits output 
  oBits : out std_logic_vector(width - 1 downto 0)
);
end RightShiftGen;

architecture RightShiftGen_arch of RightShiftGen is
begin
  oBits <= slv_srl(iBits, to_integer(shift));
end RightShiftGen_arch;
