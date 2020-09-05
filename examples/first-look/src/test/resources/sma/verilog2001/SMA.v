`default_nettype	           none
`timescale 1ns/1ps
`include "SMA_defs.v"


module SMA(
  input  wire               clk,
  input  wire               rst,
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  reg         signed [15:0] x_prev1 = 16'sd0;
  reg         signed [15:0] x_prev2 = 16'sd0;
  reg         signed [15:0] x_prev3 = 16'sd0;
  reg         signed [16:0] dsn_part = 17'sd0;
  reg         signed [17:0] sum = 18'sd0;
  reg         signed [15:0] x_prev1_sig;
  reg         signed [15:0] x_prev2_sig;
  always @(*)
  begin
    dsn_part                = ({{1{x[15]}}, x[15:0]}) + x_prev1;
    sum                     = ({{1{dsn_part[16]}}, dsn_part[16:0]}) + (({{1{x_prev2[15]}}, x_prev2[15:0]}) + x_prev3);
    x_prev1_sig             = x_prev1;
    x_prev2_sig             = x_prev2;
    y                       = {sum[17], sum[14:0]};
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) 
    begin
      x_prev1               <= 16'sd0;
      x_prev2               <= 16'sd0;
      x_prev3               <= 16'sd0;
    end
    else 
    begin
      x_prev1               <= x;
      x_prev2               <= x_prev1_sig;
      x_prev3               <= x_prev2_sig;
    end
  end
endmodule