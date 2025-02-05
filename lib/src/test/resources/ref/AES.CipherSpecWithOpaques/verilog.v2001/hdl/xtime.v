`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module xtime(
  input  wire  [7:0] lhs,
  output wire [7:0]  o
);
  `include "dfhdl_defs.vh"
  wire [7:0] shifted;
  reg [7:0] anon;
  assign o       = anon;
  always @(*)
  begin
    if (lhs[7]) anon = shifted ^ 8'h1b;
    else anon = shifted;
  end
  assign shifted = lhs << 1;
endmodule
