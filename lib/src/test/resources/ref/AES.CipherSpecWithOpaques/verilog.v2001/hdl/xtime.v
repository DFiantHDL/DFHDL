`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module xtime(
  input  wire  [7:0] lhs,
  output reg [7:0]   o
);
  `include "dfhdl_defs.vh"
  wire [7:0] shifted;
  always @(*)
  begin
    if (lhs[7]) o = shifted ^ 8'h1b;
    else o = shifted;
  end
  assign shifted = lhs << 1;
endmodule
