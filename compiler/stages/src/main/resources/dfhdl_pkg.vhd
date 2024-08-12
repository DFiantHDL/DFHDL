--  This file (dfhdl_pkg.vhd) is free and unencumbered software released 
--  into the public domain.
--
--  Anyone is free to copy, modify, publish, use, compile, sell, or
--  distribute this software, either in source code form or as a compiled
--  binary, for any purpose, commercial or non-commercial, and by any
--  means.
--  
--  In jurisdictions that recognize copyright laws, the author or authors
--  of this software dedicate any and all copyright interest in the
--  software to the public domain. We make this dedication for the benefit
--  of the public at large and to the detriment of our heirs and
--  successors. We intend this dedication to be an overt act of
--  relinquishment in perpetuity of all present and future rights to this
--  software under copyright law.
--  
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
--  OTHER DEALINGS IN THE SOFTWARE.
--  
--  For more information, please refer to <http://unlicense.org/>

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package dfhdl_pkg is
function cadd(A, B : unsigned) return unsigned;
function cadd(A, B : signed) return signed;
function csub(A, B : unsigned) return unsigned;
function csub(A, B : signed) return signed;
function clog2(n : natural) return natural;
function to_slv(A : unsigned) return std_logic_vector;
function to_slv(A : signed) return std_logic_vector;
function to_slv(A : integer) return std_logic_vector;
function to_slv(A : boolean) return std_logic_vector;
function to_slv(A : std_logic) return std_logic_vector;
function to_sl(A : boolean) return std_logic;
function to_sl(A : std_logic_vector(0 downto 0)) return std_logic;
function to_bool(A : std_logic) return boolean;
function to_bool(A : std_logic_vector(0 downto 0)) return boolean;
function bitWidth(A : std_logic_vector) return integer;
function bitWidth(A : unsigned) return integer;
function bitWidth(A : signed) return integer;
function bitWidth(A : integer) return integer;
function bitWidth(A : boolean) return integer;
function bitWidth(A : std_logic) return integer;
function resize(A : std_logic_vector; new_length : integer) return std_logic_vector;
function repeat(pattern : std_logic_vector; num : integer) return std_logic_vector;
function slv_sll(slv : std_logic_vector; num_shifts : integer) return std_logic_vector;
function slv_srl(slv : std_logic_vector; num_shifts : integer) return std_logic_vector;
function signed_sra(A : signed; num_shifts : integer) return signed;
function bool_sel(C : boolean; T : std_logic_vector; F : std_logic_vector) return std_logic_vector;
function bool_sel(C : boolean; T : unsigned; F : unsigned) return unsigned;
function bool_sel(C : boolean; T : signed; F : signed) return signed;
function bool_sel(C : boolean; T : integer; F : integer) return integer;
function bool_sel(C : boolean; T : boolean; F : boolean) return boolean;
function bool_sel(C : boolean; T : std_logic; F : std_logic) return std_logic;
end package dfhdl_pkg;

package body dfhdl_pkg is
function cadd(A, B : unsigned) return unsigned is
begin
    return unsigned('0' & A) + unsigned('0' & B);
end function;
function cadd(A, B : signed) return signed is
begin
    return signed(A(A'left) & A) + signed(B(B'left) & B);
end function;
function csub(A, B : unsigned) return unsigned is
begin
    return unsigned('0' & A) - unsigned('0' & B);
end function;
function csub(A, B : signed) return signed is
begin
    return signed(A(A'left) & A) - signed(B(B'left) & B);
end function;
function clog2(n : natural) return natural is
  variable result        : natural := 0;
  variable val           : natural := n - 1; 
begin
  while val > 0 loop
    val := val / 2;
    result := result + 1;
  end loop;
  return result;
end function;
function to_slv(A : unsigned) return std_logic_vector is
begin
  return std_logic_vector(A);
end;
function to_slv(A : signed) return std_logic_vector is
begin
  return std_logic_vector(A);
end;
function to_slv(A : integer) return std_logic_vector is
begin
  return std_logic_vector(to_signed(A, 32));
end;
function to_slv(A : boolean) return std_logic_vector is
begin
  if A then 
    return "1";
  else
    return "0";
  end if;
end;
function to_slv(A : std_logic) return std_logic_vector is
begin
  if A = '1' then 
    return "1";
  else
    return "0";
  end if;
end;
function to_sl(A : boolean) return std_logic is
begin
  if (A) then
    return '1';
  else
    return '0';
  end if;
end;
function to_sl(A : std_logic_vector(0 downto 0)) return std_logic is
begin
  if (A = "1") then
    return '1';
  else
    return '0';
  end if;
end;
function to_bool(A : std_logic) return boolean is
begin
  if (A = '1') then
    return true;
  else
    return false;
  end if;
end;
function to_bool(A : std_logic_vector(0 downto 0)) return boolean is
begin
  if (A = "1") then
    return true;
  else
    return false;
  end if;
end;
function bitWidth(A : std_logic_vector) return integer is
begin
  return A'length;
end;
function bitWidth(A : unsigned) return integer is
begin
  return A'length;
end;
function bitWidth(A : signed) return integer is
begin
  return A'length;
end;
function bitWidth(A : integer) return integer is
begin
  return 32;
end;
function bitWidth(A : boolean) return integer is
begin
  return 1;
end;
function bitWidth(A : std_logic) return integer is
begin
  return 1;
end;
function resize(A : std_logic_vector; new_length : integer) return std_logic_vector is
begin
  if new_length > A'length then
    return (new_length - A'length - 1 downto 0 => '0') & A(A'length - 1 downto 0);
  elsif new_length < A'length then
    return A(A'length - 1 downto A'length - new_length);
  else
    return A;
  end if;
end;
function repeat(pattern : std_logic_vector; num : integer) return std_logic_vector is
  variable result        : std_logic_vector((pattern'length * num) - 1 downto 0);
begin
  for i in 0 to num - 1 loop
    result(i * pattern'length + pattern'length - 1 downto i * pattern'length) := pattern;
  end loop;
  return result;
end;
function slv_sll(slv : std_logic_vector; num_shifts : integer) return std_logic_vector is
begin
  return to_slv(unsigned(slv) sll num_shifts);
end;
function slv_srl(slv : std_logic_vector; num_shifts : integer) return std_logic_vector is
begin
  return to_slv(unsigned(slv) srl num_shifts);
end;
function signed_sra(A : signed; num_shifts : integer) return signed is
begin
  return shift_right(A, num_shifts);
end;
function bool_sel(C : boolean; T : std_logic_vector; F : std_logic_vector) return std_logic_vector is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bool_sel(C : boolean; T : unsigned; F : unsigned) return unsigned is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bool_sel(C : boolean; T : signed; F : signed) return signed is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bool_sel(C : boolean; T : integer; F : integer) return integer is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bool_sel(C : boolean; T : boolean; F : boolean) return boolean is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bool_sel(C : boolean; T : std_logic; F : std_logic) return std_logic is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
end package body dfhdl_pkg;
