library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;

package ALU_pkg is
type t_enum_ALUSel is (
  ALUSel_ADD, ALUSel_SUB, ALUSel_SLL, ALUSel_SRL, ALUSel_SRA, ALUSel_AND, ALUSel_OR, ALUSel_XOR, ALUSel_SLT, ALUSel_SLTU, ALUSel_COPY1
);
function bitWidth(A: t_enum_ALUSel) return integer;
function to_slv(A: t_enum_ALUSel) return std_logic_vector;
function to_t_enum_ALUSel(A: std_logic_vector) return t_enum_ALUSel;
function bool_sel(C : boolean; T : t_enum_ALUSel; F : t_enum_ALUSel) return t_enum_ALUSel;


end package ALU_pkg;

package body ALU_pkg is
function bitWidth(A : t_enum_ALUSel) return integer is
begin
  return 4;
end;
function to_slv(A : t_enum_ALUSel) return std_logic_vector is
  variable int_val : integer;
begin
  case A is
    when ALUSel_ADD   => int_val := 0;
    when ALUSel_SUB   => int_val := 1;
    when ALUSel_SLL   => int_val := 2;
    when ALUSel_SRL   => int_val := 3;
    when ALUSel_SRA   => int_val := 4;
    when ALUSel_AND   => int_val := 5;
    when ALUSel_OR    => int_val := 6;
    when ALUSel_XOR   => int_val := 7;
    when ALUSel_SLT   => int_val := 8;
    when ALUSel_SLTU  => int_val := 9;
    when ALUSel_COPY1 => int_val := 10;
  end case;
  return resize(to_slv(int_val), 4);
end;
function to_t_enum_ALUSel(A : std_logic_vector) return t_enum_ALUSel is
begin
  case to_integer(unsigned(A)) is
    when 0            => return ALUSel_ADD;
    when 1            => return ALUSel_SUB;
    when 2            => return ALUSel_SLL;
    when 3            => return ALUSel_SRL;
    when 4            => return ALUSel_SRA;
    when 5            => return ALUSel_AND;
    when 6            => return ALUSel_OR;
    when 7            => return ALUSel_XOR;
    when 8            => return ALUSel_SLT;
    when 9            => return ALUSel_SLTU;
    when 10           => return ALUSel_COPY1;
    when others       => 
      assert false report "Unknown state detected!" severity error;
      return ALUSel_ADD;
  end case;
end;
function bool_sel(C : boolean; T : t_enum_ALUSel; F : t_enum_ALUSel) return t_enum_ALUSel is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;

end package body ALU_pkg;
