`default_nettype	           none
`timescale 1ns/1ps
`include "SMA_FB_defs.v"


module SMA_FB(
  input  wire               clk,
  input  wire               rst,
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  reg         signed [15:0] x_prev1 = 16'sd0;
  reg         signed [15:0] x_prev2 = 16'sd0;
  reg         signed [15:0] x_prev3 = 16'sd0;
  reg         signed [15:0] x_prev4 = 16'sd0;
  reg         signed [17:0] acc = 18'sd0;
  reg         signed [17:0] acc_prev1 = 18'sd0;
  reg         signed [17:0] y_part = 18'sd0;
  reg         signed [15:0] x_prev1_sig;
  reg         signed [15:0] x_prev2_sig;
  reg         signed [15:0] x_prev3_sig;
  reg         signed [17:0] acc_sig;
  always @(*)
  begin
    acc                     = acc_prev1;
    acc                     = (acc - x_prev4) + x;
    y_part                  = acc >>> 2;
    x_prev1_sig             = x_prev1;
    x_prev2_sig             = x_prev2;
    x_prev3_sig             = x_prev3;
    acc_sig                 = acc;
    y                       = {y_part[17], y_part[14:0]};
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) 
    begin
      x_prev1               <= 16'sd0;
      x_prev2               <= 16'sd0;
      x_prev3               <= 16'sd0;
      x_prev4               <= 16'sd0;
      acc_prev1             <= 18'sd0;
    end
    else 
    begin
      x_prev1               <= x;
      x_prev2               <= x_prev1_sig;
      x_prev3               <= x_prev2_sig;
      x_prev4               <= x_prev3_sig;
      acc_prev1             <= acc_sig;
    end
  end
endmodule