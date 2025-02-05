library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.FullAdderN_pkg.all;

entity FullAdderN is
port (
  a     : in  std_logic_vector(3 downto 0);
  b     : in  std_logic_vector(3 downto 0);
  c_in  : in  std_logic;
  sum   : out std_logic_vector(3 downto 0);
  c_out : out std_logic
);
end FullAdderN;

architecture FullAdderN_arch of FullAdderN is
  signal sum_sig       : std_logic_vector(3 downto 0);
  signal adder_0_a     : std_logic;
  signal adder_0_b     : std_logic;
  signal adder_0_c_in  : std_logic;
  signal adder_0_c_out : std_logic;
  signal adder_1_a     : std_logic;
  signal adder_1_b     : std_logic;
  signal adder_1_c_in  : std_logic;
  signal adder_1_c_out : std_logic;
  signal adder_2_a     : std_logic;
  signal adder_2_b     : std_logic;
  signal adder_2_c_in  : std_logic;
  signal adder_2_c_out : std_logic;
  signal adder_3_a     : std_logic;
  signal adder_3_b     : std_logic;
  signal adder_3_c_in  : std_logic;
  signal adder_3_c_out : std_logic;
begin
  adder_0 : entity work.FullAdder1(FullAdder1_arch) port map (
    a          => adder_0_a,
    b          => adder_0_b,
    c_in       => adder_0_c_in,
    c_out      => adder_0_c_out,
    sum        => sum_sig(0)
  );
  adder_1 : entity work.FullAdder1(FullAdder1_arch) port map (
    a          => adder_1_a,
    b          => adder_1_b,
    c_in       => adder_1_c_in,
    c_out      => adder_1_c_out,
    sum        => sum_sig(1)
  );
  adder_2 : entity work.FullAdder1(FullAdder1_arch) port map (
    a          => adder_2_a,
    b          => adder_2_b,
    c_in       => adder_2_c_in,
    c_out      => adder_2_c_out,
    sum        => sum_sig(2)
  );
  adder_3 : entity work.FullAdder1(FullAdder1_arch) port map (
    a          => adder_3_a,
    b          => adder_3_b,
    c_in       => adder_3_c_in,
    c_out      => adder_3_c_out,
    sum        => sum_sig(3)
  );
  sum          <= sum_sig;
  adder_0_a    <= a(0);
  adder_0_b    <= b(0);
  adder_1_c_in <= adder_0_c_out;
  adder_1_a    <= a(1);
  adder_1_b    <= b(1);
  adder_2_c_in <= adder_1_c_out;
  adder_2_a    <= a(2);
  adder_2_b    <= b(2);
  adder_3_c_in <= adder_2_c_out;
  adder_3_a    <= a(3);
  adder_3_b    <= b(3);
  adder_0_c_in <= c_in;
  c_out        <= adder_3_c_out;
end FullAdderN_arch;
