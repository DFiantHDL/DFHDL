package docExamples

class RegFileSpec extends util.FullCompileSpec:
  def dut = regfile.RegFile()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "RegFile_defs.svh"
       |
       |module RegFile#(
       |    parameter int DATA_WIDTH = 32,
       |    parameter int REG_NUM = 32
       |)(
       |  input  wire logic                         clk,
       |  input  wire logic [$clog2(REG_NUM) - 1:0] rs1_addr,
       |  output      logic [DATA_WIDTH - 1:0]      rs1_data,
       |  input  wire logic [$clog2(REG_NUM) - 1:0] rs2_addr,
       |  output      logic [DATA_WIDTH - 1:0]      rs2_data,
       |  input  wire logic [$clog2(REG_NUM) - 1:0] rd_addr,
       |  input  wire logic [DATA_WIDTH - 1:0]      rd_data,
       |  input  wire logic                         rd_wren
       |);
       |  logic [DATA_WIDTH - 1:0] regs [0:REG_NUM - 1];
       |  always_ff @(posedge clk)
       |  begin
       |    rs1_data <= regs[rs1_addr];
       |  end
       |  always_ff @(posedge clk)
       |  begin
       |    rs2_data <= regs[rs2_addr];
       |  end
       |  always_ff @(posedge clk)
       |  begin
       |    if (rd_wren) regs[rd_addr] <= rd_data;
       |    regs[0]  <= {DATA_WIDTH{1'b0}};
       |  end
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.RegFile_pkg.all;
       |
       |entity RegFile is
       |generic (
       |  DATA_WIDTH : integer := 32;
       |  REG_NUM : integer := 32
       |);
       |port (
       |  clk      : in  std_logic;
       |  rs1_addr : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
       |  rs1_data : out std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  rs2_addr : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
       |  rs2_data : out std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  rd_addr  : in  std_logic_vector(clog2(REG_NUM) - 1 downto 0);
       |  rd_data  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  rd_wren  : in  std_logic
       |);
       |end RegFile;
       |
       |architecture RegFile_arch of RegFile is
       |  type t_vecX1_std_logic_vector is array (natural range <>) of std_logic_vector;
       |  signal regs : t_vecX1_std_logic_vector(0 to REG_NUM - 1)(DATA_WIDTH - 1 downto 0);
       |begin
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then rs1_data <= regs(to_integer(unsigned(rs1_addr)));
       |    end if;
       |  end process;
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then rs2_data <= regs(to_integer(unsigned(rs2_addr)));
       |    end if;
       |  end process;
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then
       |      if rd_wren then regs(to_integer(unsigned(rd_addr))) <= rd_data;
       |      end if;
       |      regs(0) <= repeat("0", DATA_WIDTH);
       |    end if;
       |  end process;
       |end RegFile_arch;""".stripMargin
end RegFileSpec
