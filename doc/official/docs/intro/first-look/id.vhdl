
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package id_pkg is

function bit_reverse(s : std_logic_vector) return std_logic_vector;
         
function to_sl(b : boolean) return std_logic;
         
function to_sl(arg : std_logic_vector) return std_logic;
         
function to_slv(arg : std_logic) return std_logic_vector;
         
function to_slv(arg : unsigned) return std_logic_vector;
         
function to_slv(arg : signed) return std_logic_vector;
         

end package id_pkg;

package body id_pkg is

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
         
end package body id_pkg;
           

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.id_pkg.all;

entity id is
port (
  CLK                  : in  std_logic;
  RSTn                 : in  std_logic;
  X                    : in  signed(15 downto 0);
  Y                    : out signed(15 downto 0)
);
end id;

architecture id_arch of id is
begin


async_proc : process (all)
  variable v_Y         : signed(15 downto 0);
begin
  v_Y                  := resize(X, 16);
  Y                    <= v_Y;
end process async_proc;

end id_arch;
