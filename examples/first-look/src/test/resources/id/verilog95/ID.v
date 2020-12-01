`default_nettype	           none
`timescale 1ns/1ps
`include "ID_defs.v"


module ID(
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  always @(x)
  begin
    y                       = x;
  end
endmodule