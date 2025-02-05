library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.ALU_pkg.all;

entity ALU is
port (
  op1    : in  std_logic_vector(31 downto 0);
  op2    : in  std_logic_vector(31 downto 0);
  aluSel : in  t_enum_ALUSel;
  aluOut : out std_logic_vector(31 downto 0)
);
end ALU;

architecture ALU_arch of ALU is
  signal shamt   : std_logic_vector(4 downto 0);
  signal outCalc : std_logic_vector(31 downto 0);
begin
  process (all)
  begin
    case aluSel is
      when ALUSel_ADD   => outCalc <= to_slv(unsigned(op1) + unsigned(op2));
      when ALUSel_SUB   => outCalc <= to_slv(unsigned(op1) - unsigned(op2));
      when ALUSel_AND   => outCalc <= op1 and op2;
      when ALUSel_OR    => outCalc <= op1 or op2;
      when ALUSel_XOR   => outCalc <= op1 xor op2;
      when ALUSel_SLT   => outCalc <= resize(to_slv(signed(op1) < signed(op2)), 32);
      when ALUSel_SLTU  => outCalc <= resize(to_slv(unsigned(op1) < unsigned(op2)), 32);
      when ALUSel_SLL   => outCalc <= slv_sll(op1, to_integer(unsigned(shamt)));
      when ALUSel_SRL   => outCalc <= slv_srl(op1, to_integer(unsigned(shamt)));
      when ALUSel_SRA   => outCalc <= to_slv(signed_sra(signed(op1), to_integer(unsigned(shamt))));
      when ALUSel_COPY1 => outCalc <= op1;
      when others       => outCalc <= x"--------";
    end case;
  end process;
  shamt  <= op2(4 downto 0);
  aluOut <= outCalc;
end ALU_arch;
