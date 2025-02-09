-- A left-right bits shifter (flat version)
--
-- @param width
--   the width of the input and output bits
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LRShiftFlat_pkg.all;

entity LRShiftFlat is
generic (
  width : integer := 8
);
port (
  -- bits input 
  iBits : in  std_logic_vector(width - 1 downto 0);
  -- requested shift 
  shift : in  unsigned(clog2(width) - 1 downto 0);
  -- direction of shift 
  dir   : in  t_enum_ShiftDir;
  -- bits output 
  oBits : out std_logic_vector(width - 1 downto 0)
);
end LRShiftFlat;

architecture LRShiftFlat_arch of LRShiftFlat is
begin
  process (dir, iBits, shift)
  begin
    case dir is
      when ShiftDir_Left  => oBits <= slv_sll(iBits, to_integer(shift));
      when ShiftDir_Right => oBits <= slv_srl(iBits, to_integer(shift));
    end case;
  end process;
end LRShiftFlat_arch;
