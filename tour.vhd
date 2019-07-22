
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bla_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector;
         
function to_sl(b : boolean) return std_logic;
         
function to_sl(arg : std_logic_vector) return std_logic;
         
function to_slv(arg : std_logic) return std_logic_vector;
         
function to_slv(arg : unsigned) return std_logic_vector;
         
function to_slv(arg : signed) return std_logic_vector;
         
function to_slv(arg : boolean) return std_logic_vector;
         

end package bla_pkg;

package body bla_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector is
   variable v_s : std_logic_vector(s'high downto s'low);
begin
  for i in s'high downto s'low loop
    v_s(i) := s(s'high - i);
  end loop;
  return v_s;
end bit_reverse;
         
function to_sl(b : boolean) return std_logic is
begin
  if (b) then
    return '1';
  else
    return '0';
  end if;
end to_sl;
         
function to_sl(arg : std_logic_vector) return std_logic is
begin
  return arg(arg'low);
end to_sl;
         
function to_slv(arg : std_logic) return std_logic_vector is
begin
  if (arg = '1') then
    return "1";
  else
    return "0";
  end if;
end to_slv;
         
function to_slv(arg : unsigned) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
function to_slv(arg : signed) return std_logic_vector is
  variable slv : std_logic_vector(arg'length-1 downto 0);
begin
  slv := std_logic_vector(arg);
  return slv;
end to_slv;
         
function to_slv(arg : boolean) return std_logic_vector is
begin
  if (arg) then
    return "1";
  else
    return "0";
  end if;
end to_slv;
         
end package body bla_pkg;
           

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.bla_pkg.all;

entity bla is
port (
  CLK                  : in  std_logic;
  RSTn                 : in  std_logic;
  I                    : in  std_logic;
  I2                   : in  unsigned(1 downto 0);
  O                    : out std_logic
);
end bla;

architecture bla_arch of bla is
  signal r0            : std_logic;
  signal r0_prev1      : std_logic;
  signal r1            : std_logic;
  signal r1_prev1      : std_logic;
  signal r2            : std_logic;
  signal r2_prev1      : std_logic;
  signal r3            : std_logic;
  signal r3_prev1      : std_logic;
begin


sync_proc : process (CLK, RSTn)
begin
  if RSTn = '0' then
    r0_prev1           <= '0';
    r1_prev1           <= '0';
    r2_prev1           <= '0';
    r3_prev1           <= '0';
  elsif rising_edge(CLK) then
    r0_prev1           <= r0;
    r1_prev1           <= r1;
    r2_prev1           <= r2;
    r3_prev1           <= r3;
  end if;
end process sync_proc;

async_proc : process (all)
  variable v_O         : std_logic;
  variable v_r0        : std_logic;
  variable v_r1        : std_logic;
  variable v_r2        : std_logic;
  variable v_r3        : std_logic;
begin
  v_r0                 := r0_prev1;
  v_r1                 := r1_prev1;
  v_r2                 := r2_prev1;
  v_r3                 := r3_prev1;
  if I then
    case to_integer(I2) is
      when 0 =>
        v_O            := v_r0;
      when 1 =>
        v_O            := v_r1;
      when 2 =>
        v_O            := v_r2;
      when 3 =>
        v_O            := v_r3;
      when others =>
    end case;
  end if;
  O                    <= v_O;
  r0                   <= v_r0;
  r1                   <= v_r1;
  r2                   <= v_r2;
  r3                   <= v_r3;
end process async_proc;

end bla_arch;