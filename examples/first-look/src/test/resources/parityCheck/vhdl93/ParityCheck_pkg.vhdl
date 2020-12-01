library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ParityCheck_pkg is
  function bit_reverse(s : std_logic_vector) return std_logic_vector;
  function resize(arg : std_logic_vector; size : integer) return std_logic_vector;
  function to_sl(b : boolean) return std_logic;
  function to_sl(arg : std_logic_vector) return std_logic;
  function to_slv(arg : std_logic) return std_logic_vector;
  function to_slv(arg : unsigned) return std_logic_vector;
  function to_slv(arg : signed) return std_logic_vector;
  function to_slv(arg : boolean) return std_logic_vector;
end package ParityCheck_pkg;

package body ParityCheck_pkg is
  function bit_reverse(s : std_logic_vector) return std_logic_vector is
     variable v_s : std_logic_vector(s'high downto s'low);
  begin
    for i in s'high downto s'low loop
      v_s(i) := s(s'high - i);
    end loop;
    return v_s;
  end bit_reverse;
  function resize(arg : std_logic_vector; size : integer) return std_logic_vector is
  begin
    return to_slv(resize(unsigned(arg), size));
  end resize;
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
end package body ParityCheck_pkg;
