`default_nettype	           none
`timescale 1ns/1ps
`include "IDTop_defs.v"


module IDTop(
  input  wire signed [15:0] x,
  output reg  signed [15:0] y
);
  wire        signed [15:0] id1_y;
  wire        signed [15:0] id2_y;
  reg         signed [15:0] id1_x;
  reg         signed [15:0] id2_x;
  ID id1(
    .x                      (id1_x),
    .y                      (id1_y)
  );
  ID id2(
    .x                      (id2_x),
    .y                      (id2_y)
  );
  always @(x or id1_y or id2_y)
  begin
    id1_x                   = x;
    id2_x                   = id1_y;
    y                       = id2_y;
  end
endmodule