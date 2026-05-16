library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.CipherNoOpaques_pkg.all;

entity shiftRows is
port (
  state : in  t_opaque_AESState;
  o     : out t_opaque_AESState
);
end shiftRows;

architecture shiftRows_arch of shiftRows is
begin
  o <= (
    0 => (0 => state(0)(0), 1 => state(1)(1), 2 => state(2)(2), 3 => state(3)(3)),
    1 => (0 => state(1)(0), 1 => state(2)(1), 2 => state(3)(2), 3 => state(0)(3)),
    2 => (0 => state(2)(0), 1 => state(3)(1), 2 => state(0)(2), 3 => state(1)(3)),
    3 => (0 => state(3)(0), 1 => state(0)(1), 2 => state(1)(2), 3 => state(2)(3))
  );
end shiftRows_arch;
