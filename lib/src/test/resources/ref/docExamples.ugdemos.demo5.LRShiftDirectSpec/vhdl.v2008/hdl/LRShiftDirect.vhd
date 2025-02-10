-- A left-right bits shifter, direct composition
--
-- @param width
--   the width of the input and output bits
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.LRShiftDirect_pkg.all;

entity LRShiftDirect is
generic (
  width : integer := 8
);
port (
  -- bits input 
  iBits : in  std_logic_vector(width - 1 downto 0);
  -- requested shift 
  shift : in  unsigned(clog2(width) - 1 downto 0);
  -- bits output 
  oBits : out std_logic_vector(width - 1 downto 0);
  -- direction of shift 
  dir   : in  t_enum_ShiftDir
);
end LRShiftDirect;

architecture LRShiftDirect_arch of LRShiftDirect is
  signal lshifter_iBits : std_logic_vector(width - 1 downto 0);
  signal lshifter_shift : unsigned(clog2(width) - 1 downto 0);
  signal lshifter_oBits : std_logic_vector(width - 1 downto 0);
  signal rshifter_iBits : std_logic_vector(width - 1 downto 0);
  signal rshifter_shift : unsigned(clog2(width) - 1 downto 0);
  signal rshifter_oBits : std_logic_vector(width - 1 downto 0);
begin
  lshifter : entity work.LeftShiftGen(LeftShiftGen_arch) generic map (
    width        => width
  ) port map (
    iBits        => lshifter_iBits,
    shift        => lshifter_shift,
    oBits        => lshifter_oBits
  );
  rshifter : entity work.RightShiftGen(RightShiftGen_arch) generic map (
    width        => width
  ) port map (
    iBits        => rshifter_iBits,
    shift        => rshifter_shift,
    oBits        => rshifter_oBits
  );
  lshifter_iBits <= iBits;
  lshifter_shift <= shift;
  rshifter_iBits <= iBits;
  rshifter_shift <= shift;
  process (all)
  begin
    case dir is
      when ShiftDir_Left  => oBits <= lshifter_oBits;
      when ShiftDir_Right => oBits <= rshifter_oBits;
    end case;
  end process;
end LRShiftDirect_arch;
