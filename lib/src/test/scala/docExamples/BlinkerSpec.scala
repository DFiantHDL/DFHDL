package docExamples

class BlinkerSpec extends util.FullCompileSpec:
  def dut = led_blinker.Blinker()

  def expectedVerilogCS =
    """|/* This is a led blinker */
       |`default_nettype none
       |`timescale 1ns/1ps
       |`include "Blinker_defs.svh"
       |
       |module Blinker#(
       |    parameter int CLK_FREQ_KHz = 50000,
       |    parameter int LED_FREQ_Hz = 1
       |)(
       |  input  wire logic clk,
       |  input  wire logic rst,
       |  /* LED output */
       |  output logic led
       |);
       |  /* Half-count of the toggle for 50% duty cycle */
       |  parameter int HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
       |  logic [$clog2(HALF_PERIOD) - 1:0] cnt;
       |  logic led_din;
       |  logic [$clog2(HALF_PERIOD) - 1:0] cnt_din;
       |  always_comb
       |  begin
       |    led_din   = led;
       |    cnt_din   = cnt;
       |    if (cnt == $clog2(HALF_PERIOD)'(HALF_PERIOD - 1)) begin
       |      cnt_din = $clog2(HALF_PERIOD)'(0);
       |      led_din = !led;
       |    end
       |    else cnt_din = cnt + $clog2(HALF_PERIOD)'(1);
       |  end
       |  always_ff @(posedge clk)
       |  begin
       |    if (rst == 1'b1) begin
       |      led     <= 1'b1;
       |      cnt     <= $clog2(HALF_PERIOD)'(0);
       |    end
       |    else begin
       |      led     <= led_din;
       |      cnt     <= cnt_din;
       |    end
       |  end
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|-- This is a led blinker 
       |library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.Blinker_pkg.all;
       |
       |entity Blinker is
       |generic (
       |  CLK_FREQ_KHz : integer := 50000;
       |  LED_FREQ_Hz : integer := 1
       |);
       |port (
       |  clk : in  std_logic;
       |  rst : in  std_logic;
       |  -- LED output 
       |  led : out std_logic
       |);
       |end Blinker;
       |
       |architecture Blinker_arch of Blinker is
       |  -- Half-count of the toggle for 50% duty cycle 
       |  constant HALF_PERIOD : integer := (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
       |  signal cnt           : unsigned(clog2(HALF_PERIOD) - 1 downto 0);
       |  signal led_din       : std_logic;
       |  signal cnt_din       : unsigned(clog2(HALF_PERIOD) - 1 downto 0);
       |begin
       |  process (all)
       |  begin
       |    led_din   <= led;
       |    cnt_din   <= cnt;
       |    if cnt = to_unsigned(HALF_PERIOD - 1, clog2(HALF_PERIOD)) then
       |      cnt_din <= resize(d"0", clog2(HALF_PERIOD));
       |      led_din <= not led;
       |    else cnt_din <= cnt + resize(d"1", clog2(HALF_PERIOD));
       |    end if;
       |  end process;
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then
       |      if rst = '1' then
       |        led   <= '1';
       |        cnt   <= resize(d"0", clog2(HALF_PERIOD));
       |      else
       |        led   <= led_din;
       |        cnt   <= cnt_din;
       |      end if;
       |    end if;
       |  end process;
       |end Blinker_arch;""".stripMargin
end BlinkerSpec
