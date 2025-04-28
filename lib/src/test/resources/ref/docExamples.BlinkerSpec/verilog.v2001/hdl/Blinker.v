/* This is a led blinker */
`default_nettype none
`timescale 1ns/1ps
`include "Blinker_defs.vh"

module Blinker#(
    parameter integer CLK_FREQ_KHz = 50000,
    parameter integer LED_FREQ_Hz = 1
)(
  input  wire clk,
  input  wire rst,
  /* LED output */
  output reg  led
);
  `include "dfhdl_defs.vh"
  `include "Blinker_defs.vh"
  /* Half-count of the toggle for 50% duty cycle */
  parameter integer HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
  reg [clog2(HALF_PERIOD) - 1:0] cnt;
  always @(posedge clk)
  begin
    if (rst == 1'b1) begin
      led   <= 1'b1;
      cnt   <= `TO_UNSIGNED(0, 1, clog2(HALF_PERIOD));
    end
    else begin
      if (cnt == (HALF_PERIOD - 1)) begin
        cnt <= `TO_UNSIGNED(0, 1, clog2(HALF_PERIOD));
        led <= ~led;
      end
      else cnt <= cnt + `TO_UNSIGNED(1, 1, clog2(HALF_PERIOD));
    end
  end
endmodule
