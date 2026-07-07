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
use std.textio.all;

package dfhdl_pkg is
-- Fixed-point types: `M` integer (magnitude) bits and `F` fraction bits, with the binary
-- point at index 0 (integer bits M-1 downto 0, fraction bits -1 downto -F). They are raw
-- std_logic arrays over an *integer* range so the binary point can lie outside the stored
-- bits; arithmetic is done by converting to numeric_std unsigned/signed.
type ufix is array (integer range <>) of std_logic;
type sfix is array (integer range <>) of std_logic;
-- Fixed-point conversions. `to_slv`/`to_unsigned`/`to_signed` normalize the (possibly
-- negative-indexed) fixed value to a 0-based raw vector. `resize(x, m, f)` reformats to a
-- `(m-1 downto -f)` value preserving the represented value (scaling the raw by the fraction
-- delta); overloads accept an integer (fraction 0) numeric_std source. `to_sfix`/`to_ufix`
-- are the sign casts (mirroring `signed`/`unsigned` on the integer types), also with explicit
-- `m`/`f` for the target range.
function to_slv(A : ufix) return std_logic_vector;
function to_slv(A : sfix) return std_logic_vector;
function to_unsigned(A : ufix) return unsigned;
function to_signed(A : sfix) return signed;
function to_ufix(A : std_logic_vector; m : integer; f : integer) return ufix;
function to_sfix(A : std_logic_vector; m : integer; f : integer) return sfix;
function resize(A : ufix; m : integer; f : integer) return ufix;
function resize(A : sfix; m : integer; f : integer) return sfix;
function resize(A : unsigned; m : integer; f : integer) return ufix;
function resize(A : signed; m : integer; f : integer) return sfix;
function to_sfix(A : ufix) return sfix;
function to_ufix(A : sfix) return ufix;
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
function to_slv(A : boolean; length : integer) return std_logic_vector;
function to_slv(A : std_logic; length : integer) return std_logic_vector;
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
procedure print(msg : string);
procedure println(msg : string);
function to_string(A : unsigned) return string;
function to_string(A : signed) return string;
function to_string(A : integer) return string;
function to_string(A : boolean) return string;
function to_string(A : std_logic) return string;
function to_string(A : std_logic_vector) return string;
function max(A, B : integer) return integer;
function min(A, B : integer) return integer;
function to_unsigned(A : boolean; length : integer) return unsigned;
function to_signed(A : boolean; length : integer) return signed;
function to_unsigned(A : std_logic; length : integer) return unsigned;
function to_signed(A : std_logic; length : integer) return signed;
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
-- fixed-point value -> 0-based raw vector (normalizes the integer/negative index range)
function to_slv(A : ufix) return std_logic_vector is
  variable r : std_logic_vector(A'length - 1 downto 0);
begin
  for i in 0 to A'length - 1 loop
    r(i) := A(A'low + i);
  end loop;
  return r;
end;
function to_slv(A : sfix) return std_logic_vector is
  variable r : std_logic_vector(A'length - 1 downto 0);
begin
  for i in 0 to A'length - 1 loop
    r(i) := A(A'low + i);
  end loop;
  return r;
end;
function to_unsigned(A : ufix) return unsigned is
begin
  return unsigned(to_slv(A));
end;
function to_signed(A : sfix) return signed is
begin
  return signed(to_slv(A));
end;
-- 0-based raw vector -> fixed-point value with an explicit (m-1 downto -f) range (same bits,
-- copied by position so the negative fraction indices are restored)
function to_ufix(A : std_logic_vector; m : integer; f : integer) return ufix is
  variable r : ufix(m - 1 downto -f);
begin
  for i in 0 to A'length - 1 loop
    r(r'low + i) := A(A'low + i);
  end loop;
  return r;
end;
function to_sfix(A : std_logic_vector; m : integer; f : integer) return sfix is
  variable r : sfix(m - 1 downto -f);
begin
  for i in 0 to A'length - 1 loop
    r(r'low + i) := A(A'low + i);
  end loop;
  return r;
end;
-- resize to the (m, f) format, preserving the represented value: scale the raw integer by
-- 2^(f - sourceFraction), then fit into the m+f-bit total width
function resize(A : ufix; m : integer; f : integer) return ufix is
  constant srcF  : integer := -A'low;
  constant total : integer := m + f;
  variable raw   : unsigned(A'length - 1 downto 0);
  variable scaled : unsigned(total - 1 downto 0);
begin
  raw := to_unsigned(A);
  if (f >= srcF) then scaled := resize(raw, total) sll (f - srcF);
  else scaled := resize(shift_right(raw, srcF - f), total);
  end if;
  return to_ufix(std_logic_vector(scaled), m, f);
end;
function resize(A : sfix; m : integer; f : integer) return sfix is
  constant srcF  : integer := -A'low;
  constant total : integer := m + f;
  variable raw   : signed(A'length - 1 downto 0);
  variable scaled : signed(total - 1 downto 0);
begin
  raw := to_signed(A);
  if (f >= srcF) then scaled := resize(raw, total) sll (f - srcF);
  else scaled := resize(shift_right(raw, srcF - f), total);
  end if;
  return to_sfix(std_logic_vector(scaled), m, f);
end;
-- integer (fraction 0) source overloads
function resize(A : unsigned; m : integer; f : integer) return ufix is
  variable scaled : unsigned(m + f - 1 downto 0);
begin
  scaled := resize(A, m + f) sll f;
  return to_ufix(std_logic_vector(scaled), m, f);
end;
function resize(A : signed; m : integer; f : integer) return sfix is
  variable scaled : signed(m + f - 1 downto 0);
begin
  scaled := resize(A, m + f) sll f;
  return to_sfix(std_logic_vector(scaled), m, f);
end;
-- sign casts (value-preserving), mirroring `signed`/`unsigned` on the integer types. The
-- target range is derived from the source: `to_sfix` adds a sign bit (magnitude +1) and
-- `to_ufix` drops it (magnitude -1); the fraction is unchanged.
function to_sfix(A : ufix) return sfix is
  variable r : sfix(A'high + 1 downto A'low) := (others => '0');
begin
  for i in A'range loop
    r(i) := A(i);
  end loop;
  return r;
end;
function to_ufix(A : sfix) return ufix is
  variable r : ufix(A'high - 1 downto A'low);
begin
  for i in r'range loop
    r(i) := A(i);
  end loop;
  return r;
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
function to_slv(A : boolean; length : integer) return std_logic_vector is
begin
  return resize(to_slv(A), length);
end;
function to_slv(A : std_logic; length : integer) return std_logic_vector is
begin
  return resize(to_slv(A), length);
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
  variable AD: std_logic_vector(A'length - 1 downto 0);
begin
  AD := A;
  if new_length > A'length then
    return (new_length - A'length - 1 downto 0 => '0') & AD(A'length - 1 downto 0);
  elsif new_length < A'length then
    return AD(A'length - 1 downto A'length - new_length);
  else
    return AD;
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
procedure print(msg : string) is
begin
  write(output, msg);
end procedure;
procedure println(msg : string) is
  variable l : line;
begin
  write(l, msg);
  writeline(output, l);
end procedure;
function to_string(A : unsigned) return string is
  variable temp : unsigned(A'length-1 downto 0) := A;
  variable digit : natural;
  -- Calculate max possible length: ceiling(bits * log10(2)) + 1 for null termination
  -- log10(2) =~ 0.301, so we multiply by 31/100 as an integer approximation (slightly larger)
  variable max_len : integer := (31 * A'length) / 100 + 2;  -- +1 for rounding, +1 for safety
  variable result : string(1 to max_len);
  variable idx : integer := max_len;
  variable len : integer := 0;
begin
  if A'length <= 31 then
    return integer'image(to_integer(A));
  end if;

  if temp = 0 then
    return "0";
  end if;

  while temp > 0 loop
    digit := to_integer(temp mod 10);
    result(idx) := character'val(character'pos('0') + digit);
    temp := temp / 10;
    idx := idx - 1;
    len := len + 1;
  end loop;

  return result(idx+1 to idx+len);
end;
function to_string(A : signed) return string is
begin
  return integer'image(to_integer(A));
end;
function to_string(A : integer) return string is
begin
  return integer'image(A);
end;
function to_string(A : boolean) return string is
begin
  if A then
    return "true";
  else
    return "false";
  end if;
end;
function to_string(A : std_logic) return string is
begin
  return std_logic'image(A);
end;
function to_string(A : std_logic_vector) return string is
  variable nibble : std_logic_vector(3 downto 0);
  variable hex_digit : character;
  variable num_nibbles : integer := (A'length + 3) / 4;  -- Ceiling division
  variable result : string(1 to num_nibbles + 2);  -- +2 for "0x" prefix
  variable padded_input : std_logic_vector((num_nibbles * 4) - 1 downto 0);
begin
  -- Add "0x" prefix
  result(1 to 2) := "0x";
  
  -- Zero-pad the input if needed
  padded_input := (others => '0');
  padded_input(A'length-1 downto 0) := A;
  
  -- Convert each nibble to hex
  for i in num_nibbles downto 1 loop
    nibble := padded_input((i*4)-1 downto (i-1)*4);
    case nibble is
      when "0000" => hex_digit := '0';
      when "0001" => hex_digit := '1';
      when "0010" => hex_digit := '2';
      when "0011" => hex_digit := '3';
      when "0100" => hex_digit := '4';
      when "0101" => hex_digit := '5';
      when "0110" => hex_digit := '6';
      when "0111" => hex_digit := '7';
      when "1000" => hex_digit := '8';
      when "1001" => hex_digit := '9';
      when "1010" => hex_digit := 'a';
      when "1011" => hex_digit := 'b';
      when "1100" => hex_digit := 'c';
      when "1101" => hex_digit := 'd';
      when "1110" => hex_digit := 'e';
      when "1111" => hex_digit := 'f';
      when others => hex_digit := 'x';  -- For any undefined values
    end case;
    result(num_nibbles - i + 3) := hex_digit;
  end loop;
  
  return result;
end;
function max(A, B : integer) return integer is
begin
    if A > B then
        return A;
    else
        return B;
    end if;
end;
function min(A, B : integer) return integer is
begin
    if A < B then
        return A;
    else
        return B;
    end if;
end;
function to_unsigned(A : boolean; length : integer) return unsigned is
begin
  return unsigned(resize(to_slv(A), length));
end;
function to_signed(A : boolean; length : integer) return signed is
begin
  return signed(resize(to_slv(A), length));
end;
function to_unsigned(A : std_logic; length : integer) return unsigned is
begin
  return unsigned(resize(to_slv(A), length));
end;
function to_signed(A : std_logic; length : integer) return signed is
begin
  return signed(resize(to_slv(A), length));
end;
end package body dfhdl_pkg;
