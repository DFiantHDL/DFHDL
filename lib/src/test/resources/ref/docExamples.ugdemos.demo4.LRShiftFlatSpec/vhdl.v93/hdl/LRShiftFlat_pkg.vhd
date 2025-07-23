library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;

package LRShiftFlat_pkg is
type t_enum_ShiftDir is (
  ShiftDir_Left, ShiftDir_Right
);
function bitWidth(A: t_enum_ShiftDir) return integer;
function to_slv(A: t_enum_ShiftDir) return std_logic_vector;
function to_t_enum_ShiftDir(A: std_logic_vector) return t_enum_ShiftDir;
function bool_sel(C : boolean; T : t_enum_ShiftDir; F : t_enum_ShiftDir) return t_enum_ShiftDir;
function to_bool(A: t_enum_ShiftDir) return boolean;
function to_sl(A: t_enum_ShiftDir) return std_logic;
function to_t_enum_ShiftDir(A: boolean) return t_enum_ShiftDir;
function to_t_enum_ShiftDir(A: std_logic) return t_enum_ShiftDir;
function toggle(A: t_enum_ShiftDir) return t_enum_ShiftDir;


end package LRShiftFlat_pkg;

package body LRShiftFlat_pkg is
function bitWidth(A : t_enum_ShiftDir) return integer is
begin
  return 1;
end;
function to_slv(A : t_enum_ShiftDir) return std_logic_vector is
  variable int_val : integer;
begin
  case A is
    when ShiftDir_Left  => int_val := 0;
    when ShiftDir_Right => int_val := 1;
  end case;
  return resize(to_slv(int_val), 1);
end;
function to_t_enum_ShiftDir(A : std_logic_vector) return t_enum_ShiftDir is
begin
  case to_integer(unsigned(A)) is
    when 0              => return ShiftDir_Left;
    when 1              => return ShiftDir_Right;
    when others         => 
      assert false report "Unknown state detected!" severity error;
      return ShiftDir_Left;
  end case;
end;
function bool_sel(C : boolean; T : t_enum_ShiftDir; F : t_enum_ShiftDir) return t_enum_ShiftDir is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function to_bool(A : t_enum_ShiftDir) return boolean is
begin
  case A is
    when ShiftDir_Left  => return false;
    when ShiftDir_Right => return true;
  end case;
end;
function to_sl(A : t_enum_ShiftDir) return std_logic is
begin
  case A is
    when ShiftDir_Left  => return '0';
    when ShiftDir_Right => return '1';
  end case;
end;
function to_t_enum_ShiftDir(A : boolean) return t_enum_ShiftDir is
begin
  if A then return ShiftDir_Right;
  else return ShiftDir_Left;
  end if;
end;
function to_t_enum_ShiftDir(A : std_logic) return t_enum_ShiftDir is
begin
  if A = '1' then return ShiftDir_Right;
  else return ShiftDir_Left;
  end if;
end;
function toggle(A : t_enum_ShiftDir) return t_enum_ShiftDir is
begin
  case A is
    when ShiftDir_Left  => return ShiftDir_Right;
    when ShiftDir_Right => return ShiftDir_Left;
  end case;
end;

end package body LRShiftFlat_pkg;
