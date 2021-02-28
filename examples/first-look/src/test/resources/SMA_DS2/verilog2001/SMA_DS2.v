`default_nettype	           none
`timescale 1ns/1ps
`include "SMA_DS2_defs.v"


module SMA_DS2(
  input  wire               clk,
  input  wire               rst,
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  reg         signed [15:0] x_prev1 = 16'sd0;
  reg         signed [16:0] s0;
  reg         signed [16:0] s0_prev1 = 17'sd0;
  reg         signed [16:0] s2 = 17'sd0;
  reg         signed [17:0] sum;
  reg         signed [17:0] y_part;
  reg         signed [16:0] s0_sig;
  reg         signed [16:0] s0_prev1_sig;
  always @(*)
  begin
    s0                      = ({x[15], x[15:0]}) + x_prev1;
    sum                     = ({s0[16], s0[16:0]}) + s2;
    y_part                  = sum / 4;
    s0_sig                  = s0;
    s0_prev1_sig            = s0_prev1;
    y                       = {y_part[17], y_part[14:0]};
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) 
    begin
      x_prev1               <= 16'sd0;
      s0_prev1              <= 17'sd0;
      s2                    <= 17'sd0;
    end
    else 
    begin
      x_prev1               <= x;
      s0_prev1              <= s0_sig;
      s2                    <= s0_prev1_sig;
    end
  end
endmodule