package docExamples

class ALUSpec extends util.FullCompileSpec:
  def dut = alu.ALU()

  def expectedVerilogCS =
    """|typedef enum [3:0] {
       |  ALUSel_ADD   = 0,
       |  ALUSel_SUB   = 1,
       |  ALUSel_SLL   = 2,
       |  ALUSel_SRL   = 3,
       |  ALUSel_SRA   = 4,
       |  ALUSel_AND   = 5,
       |  ALUSel_OR    = 6,
       |  ALUSel_XOR   = 7,
       |  ALUSel_SLT   = 8,
       |  ALUSel_SLTU  = 9,
       |  ALUSel_COPY1 = 10
       |} t_enum_ALUSel;
       |
       |`default_nettype none
       |`timescale 1ns/1ps
       |`include "ALU_defs.svh"
       |
       |module ALU(
       |  input  wire logic [31:0]  op1,
       |  input  wire logic [31:0]  op2,
       |  input  wire t_enum_ALUSel aluSel,
       |  output      logic [31:0]  aluOut
       |);
       |  logic [4:0]  shamt;
       |  logic [31:0] outCalc;
       |  always_comb
       |  begin
       |    case (aluSel)
       |      ALUSel_ADD:   outCalc = op1 + op2;
       |      ALUSel_SUB:   outCalc = op1 - op2;
       |      ALUSel_AND:   outCalc = op1 & op2;
       |      ALUSel_OR:    outCalc = op1 | op2;
       |      ALUSel_XOR:   outCalc = op1 ^ op2;
       |      ALUSel_SLT:   outCalc = {{(32-1){1'b0}}, {$signed(op1) < $signed(op2)}};
       |      ALUSel_SLTU:  outCalc = {{(32-1){1'b0}}, {op1 < op2}};
       |      ALUSel_SLL:   outCalc = op1 << shamt;
       |      ALUSel_SRL:   outCalc = op1 >> shamt;
       |      ALUSel_SRA:   outCalc = {$signed(op1) >>> shamt};
       |      ALUSel_COPY1: outCalc = op1;
       |      default:      outCalc = 32'hxxxxxxxx;
       |    endcase
       |  end
       |  assign shamt  = op2[4:0];
       |  assign aluOut = outCalc;
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|type t_enum_ALUSel is (
       |  ALUSel_ADD, ALUSel_SUB, ALUSel_SLL, ALUSel_SRL, ALUSel_SRA, ALUSel_AND, ALUSel_OR, ALUSel_XOR, ALUSel_SLT, ALUSel_SLTU, ALUSel_COPY1
       |);
       |
       |library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.ALU_pkg.all;
       |
       |entity ALU is
       |port (
       |  op1    : in  std_logic_vector(31 downto 0);
       |  op2    : in  std_logic_vector(31 downto 0);
       |  aluSel : in  t_enum_ALUSel;
       |  aluOut : out std_logic_vector(31 downto 0)
       |);
       |end ALU;
       |
       |architecture ALU_arch of ALU is
       |  signal shamt   : std_logic_vector(4 downto 0);
       |  signal outCalc : std_logic_vector(31 downto 0);
       |begin
       |  process (all)
       |  begin
       |    case aluSel is
       |      when ALUSel_ADD   => outCalc <= to_slv(unsigned(op1) + unsigned(op2));
       |      when ALUSel_SUB   => outCalc <= to_slv(unsigned(op1) - unsigned(op2));
       |      when ALUSel_AND   => outCalc <= op1 and op2;
       |      when ALUSel_OR    => outCalc <= op1 or op2;
       |      when ALUSel_XOR   => outCalc <= op1 xor op2;
       |      when ALUSel_SLT   => outCalc <= resize(to_slv(signed(op1) < signed(op2)), 32);
       |      when ALUSel_SLTU  => outCalc <= resize(to_slv(unsigned(op1) < unsigned(op2)), 32);
       |      when ALUSel_SLL   => outCalc <= slv_sll(op1, to_integer(unsigned(shamt)));
       |      when ALUSel_SRL   => outCalc <= slv_srl(op1, to_integer(unsigned(shamt)));
       |      when ALUSel_SRA   => outCalc <= to_slv(signed_sra(signed(op1), to_integer(unsigned(shamt))));
       |      when ALUSel_COPY1 => outCalc <= op1;
       |      when others       => outCalc <= x"--------";
       |    end case;
       |  end process;
       |  shamt  <= op2(4 downto 0);
       |  aluOut <= outCalc;
       |end ALU_arch;""".stripMargin
end ALUSpec
