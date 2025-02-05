library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.Cipher_pkg.all;

entity addRoundKey is
port (
  state : in  t_opaque_AESState;
  key   : in  t_opaque_AESRoundKey;
  o     : out t_opaque_AESState
);
end addRoundKey;

architecture addRoundKey_arch of addRoundKey is
begin
  o <= (
    (state(0)(0) xor key(0)(0), state(0)(1) xor key(0)(1), state(0)(2) xor key(0)(2), state(0)(3) xor key(0)(3)),
    (state(1)(0) xor key(1)(0), state(1)(1) xor key(1)(1), state(1)(2) xor key(1)(2), state(1)(3) xor key(1)(3)),
    (state(2)(0) xor key(2)(0), state(2)(1) xor key(2)(1), state(2)(2) xor key(2)(2), state(2)(3) xor key(2)(3)),
    (state(3)(0) xor key(3)(0), state(3)(1) xor key(3)(1), state(3)(2) xor key(3)(2), state(3)(3) xor key(3)(3))
  );
end addRoundKey_arch;
