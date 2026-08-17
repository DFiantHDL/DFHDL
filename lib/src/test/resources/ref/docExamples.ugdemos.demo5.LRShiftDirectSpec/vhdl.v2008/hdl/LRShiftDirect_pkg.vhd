library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;

package LRShiftDirect_pkg is
type ShiftDir is (
  ShiftDir_Left, ShiftDir_Right
);
function bitWidth(A: ShiftDir) return integer;
function to_slv(A: ShiftDir) return std_logic_vector;
function to_ShiftDir(A: std_logic_vector) return ShiftDir;
function bool_sel(C : boolean; T : ShiftDir; F : ShiftDir) return ShiftDir;
function to_bool(A: ShiftDir) return boolean;
function to_sl(A: ShiftDir) return std_logic;
function to_ShiftDir(A: boolean) return ShiftDir;
function to_ShiftDir(A: std_logic) return ShiftDir;
function toggle(A: ShiftDir) return ShiftDir;
end package LRShiftDirect_pkg;

package body LRShiftDirect_pkg is
function bitWidth(A : ShiftDir) return integer is
begin
  return 1;
end;
function to_slv(A : ShiftDir) return std_logic_vector is
  variable int_val : integer;
begin
  case A is
    when ShiftDir_Left  => int_val := 0;
    when ShiftDir_Right => int_val := 1;
  end case;
  return resize(to_slv(int_val), 1);
end;
function to_ShiftDir(A : std_logic_vector) return ShiftDir is
begin
  case to_integer(unsigned(A)) is
    when 0              => return ShiftDir_Left;
    when 1              => return ShiftDir_Right;
    when others         => 
      assert false report "Unknown state detected!" severity error;
      return ShiftDir_Left;
  end case;
end;
function bool_sel(C : boolean; T : ShiftDir; F : ShiftDir) return ShiftDir is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function to_bool(A : ShiftDir) return boolean is
begin
  case A is
    when ShiftDir_Left  => return false;
    when ShiftDir_Right => return true;
  end case;
end;
function to_sl(A : ShiftDir) return std_logic is
begin
  case A is
    when ShiftDir_Left  => return '0';
    when ShiftDir_Right => return '1';
  end case;
end;
function to_ShiftDir(A : boolean) return ShiftDir is
begin
  if A then return ShiftDir_Right;
  else return ShiftDir_Left;
  end if;
end;
function to_ShiftDir(A : std_logic) return ShiftDir is
begin
  if A = '1' then return ShiftDir_Right;
  else return ShiftDir_Left;
  end if;
end;
function toggle(A : ShiftDir) return ShiftDir is
begin
  case A is
    when ShiftDir_Left  => return ShiftDir_Right;
    when ShiftDir_Right => return ShiftDir_Left;
  end case;
end;
end package body LRShiftDirect_pkg;
