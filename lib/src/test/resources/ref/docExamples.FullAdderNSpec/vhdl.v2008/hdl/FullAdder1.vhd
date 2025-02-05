library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.FullAdderN_pkg.all;

entity FullAdder1 is
port (
  a     : in  std_logic;
  b     : in  std_logic;
  c_in  : in  std_logic;
  sum   : out std_logic;
  c_out : out std_logic
);
end FullAdder1;

architecture FullAdder1_arch of FullAdder1 is
begin
  sum   <= (a xor b) xor c_in;
  c_out <= ((a and b) or (b and c_in)) or (c_in and a);
end FullAdder1_arch;
