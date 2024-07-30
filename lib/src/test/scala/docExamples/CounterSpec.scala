package docExamples

class CounterSpec extends util.FullCompileSpec:
  def dut = counter.Counter()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "Counter_defs.svh"
       |
       |module Counter#(parameter int width = 8)(
       |  input  wire logic clk,
       |  input  wire logic rst,
       |  input  wire logic en,
       |  output logic [width - 1:0] cnt
       |);
       |  logic [width - 1:0] cnt_din;
       |  always_comb
       |  begin
       |    cnt_din = cnt;
       |    if (en) cnt_din = cnt + width'(1);
       |  end
       |  always_ff @(posedge clk)
       |  begin
       |    if (rst == 1'b1) cnt <= width'(0);
       |    else cnt <= cnt_din;
       |  end
       |endmodule
       |""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.Counter_pkg.all;
       |
       |entity Counter is
       |generic (
       |  width : integer := 8
       |);
       |port (
       |  clk : in  std_logic;
       |  rst : in  std_logic;
       |  en  : in  std_logic;
       |  cnt : out unsigned(width - 1 downto 0)
       |);
       |end Counter;
       |
       |architecture Counter_arch of Counter is
       |  signal cnt_din : unsigned(width - 1 downto 0);
       |begin
       |  process (all)
       |  begin
       |    cnt_din <= cnt;
       |    if en then cnt_din <= cnt + resize(d"1", width);
       |    end if;
       |  end process;
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then
       |      if rst = '1' then cnt <= resize(d"0", width);
       |      else cnt <= cnt_din;
       |      end if;
       |    end if;
       |  end process;
       |end Counter_arch;
       |""".stripMargin
end CounterSpec
