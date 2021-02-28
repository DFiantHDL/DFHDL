`default_nettype	           none
`timescale 1ns/1ps
`include "SMA_DS_defs.v"


module SMA_DS(
  input  wire               clk,
  input  wire               rst,
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  reg         signed [15:0] x_prev1 = 16'sd0;
  reg         signed [15:0] x_prev2 = 16'sd0;
  reg         signed [15:0] x_prev3 = 16'sd0;
  reg         signed [16:0] s0;
  reg         signed [16:0] s2;
  reg         signed [17:0] sum;
  reg         signed [17:0] y_part;
  reg         signed [15:0] x_prev1_sig;
  reg         signed [15:0] x_prev2_sig;
  always @(clk or x or x_prev1 or x_prev2 or x_prev3 or x_prev1 or x_prev2 or rst)
  begin
    s0                      = ({x[15], x[15:0]}) + x_prev1;
    s2                      = ({x_prev2[15], x_prev2[15:0]}) + x_prev3;
    sum                     = ({s0[16], s0[16:0]}) + s2;
    y_part                  = sum / 4;
    x_prev1_sig             = x_prev1;
    x_prev2_sig             = x_prev2;
    y                       = {y_part[17], y_part[14:0]};
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