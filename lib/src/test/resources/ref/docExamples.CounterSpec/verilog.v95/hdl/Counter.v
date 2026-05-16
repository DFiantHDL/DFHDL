`default_nettype none
`timescale 1ns/1ps

module Counter(
  clk,
  rst,
  en,
  cnt
);
  `include "dfhdl_defs.vh"
  parameter integer width = 8;
  input  wire clk;
  input  wire rst;
  input  wire en;
  output reg [width - 1:0] cnt;
  always @(posedge clk)
  begin
    if (rst == 1'b1) cnt <= `EXTEND_U(1'd0, 1, width);
    else begin
      if (en) cnt <= cnt + `EXTEND_U(1'd1, 1, width);
    end
  end
endmodule
