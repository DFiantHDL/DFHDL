/* This is a led blinker */
`default_nettype none
`timescale 1ns/1ps
`include "Blinker_defs.svh"

module Blinker#(
    parameter int CLK_FREQ_KHz = 50000,
    parameter int LED_FREQ_Hz = 1
)(
  input  wire logic clk,
  input  wire logic rst,
  /* LED output */
  output      logic led
);
  `include "dfhdl_defs.svh"
  /* Half-count of the toggle for 50% duty cycle */
  parameter int HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
  logic [$clog2(HALF_PERIOD) - 1:0] cnt;
  always_ff @(posedge clk)
  begin
    if (rst == 1'b1) begin
      led   <= 1'b1;
      cnt   <= $clog2(HALF_PERIOD)'(0);
    end
    else begin
      if (cnt == $clog2(HALF_PERIOD)'(HALF_PERIOD - 1)) begin
        cnt <= $clog2(HALF_PERIOD)'(0);
        led <= ~led;
      end
      else cnt <= cnt + $clog2(HALF_PERIOD)'(1);
    end
  end
endmodule
