package docExamples

class TrueDPRSpec extends util.FullCompileSpec:
  def dut = trueDPR.TrueDPR()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "dfhdl_defs.svh"
       |`include "TrueDPR_defs.svh"
       |
       |module TrueDPR#(
       |    parameter int DATA_WIDTH = 8,
       |    parameter int ADDR_WIDTH = 8
       |)(
       |  input  logic                    a_clk,
       |  input  logic [DATA_WIDTH - 1:0] a_data,
       |  input  logic [ADDR_WIDTH - 1:0] a_addr,
       |  output logic [DATA_WIDTH - 1:0] a_q,
       |  input  logic                    a_we,
       |  input  logic                    b_clk,
       |  input  logic [DATA_WIDTH - 1:0] b_data,
       |  input  logic [ADDR_WIDTH - 1:0] b_addr,
       |  output logic [DATA_WIDTH - 1:0] b_q,
       |  input  logic                    b_we
       |);
       |  logic [DATA_WIDTH - 1:0] ram [0:2 ** ADDR_WIDTH - 1];
       |  always_ff @(posedge a_clk)
       |  begin
       |    if (a_we) begin
       |      /* verilator lint_off BLKSEQ */
       |      ram[a_addr] = a_data;
       |      /* verilator lint_on BLKSEQ */
       |    end
       |    a_q           <= ram[a_addr];
       |  end
       |  always_ff @(posedge b_clk)
       |  begin
       |    if (b_we) begin
       |      /* verilator lint_off BLKSEQ */
       |      ram[b_addr] = b_data;
       |      /* verilator lint_on BLKSEQ */
       |    end
       |    b_q           <= ram[b_addr];
       |  end
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.dfhdl_pkg.all;
       |use work.TrueDPR_pkg.all;
       |
       |entity TrueDPR is
       |generic (
       |  DATA_WIDTH : integer := 8;
       |  ADDR_WIDTH : integer := 8
       |);
       |port (
       |  a_clk  : in  std_logic;
       |  a_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  a_addr : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
       |  a_q    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  a_we   : in  std_logic;
       |  b_clk  : in  std_logic;
       |  b_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  b_addr : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
       |  b_q    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
       |  b_we   : in  std_logic
       |);
       |end TrueDPR;
       |
       |architecture TrueDPR_arch of TrueDPR is
       |  type t_vecX1_std_logic_vector is array (natural range <>) of std_logic_vector;
       |  shared variable ram : t_vecX1_std_logic_vector(0 to 2 ** ADDR_WIDTH - 1)(DATA_WIDTH - 1 downto 0);
       |begin
       |  process (a_clk)
       |  begin
       |    if rising_edge(a_clk) then
       |      if a_we then ram(to_integer(unsigned(a_addr))) := a_data;
       |      end if;
       |      a_q <= ram(to_integer(unsigned(a_addr)));
       |    end if;
       |  end process;
       |  process (b_clk)
       |  begin
       |    if rising_edge(b_clk) then
       |      if b_we then ram(to_integer(unsigned(b_addr))) := b_data;
       |      end if;
       |      b_q <= ram(to_integer(unsigned(b_addr)));
       |    end if;
       |  end process;
       |end TrueDPR_arch;""".stripMargin
end TrueDPRSpec
