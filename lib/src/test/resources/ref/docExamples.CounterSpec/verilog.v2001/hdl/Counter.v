`default_nettype none
`timescale 1ns/1ps
`include "Counter_defs.vh"

module Counter#(parameter integer width = 8)(
  input  wire clk,
  input  wire rst,
  input  wire en,
  output reg [width - 1:0] cnt
);
  `include "dfhdl_defs.vh"
  always @(posedge clk)
  begin
    if (rst == 1'b1) cnt <= `TO_UNSIGNED(0, 1, width);
    else begin
      if (en) cnt <= cnt + `TO_UNSIGNED(1, 1, width);
    end
  end
endmodule
