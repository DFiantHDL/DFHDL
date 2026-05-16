`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module xtime(
  input  wire t_opaque_AESByte lhs,
  output t_opaque_AESByte      o
);
  `include "dfhdl_defs.svh"
  logic [7:0] shifted;
  logic [7:0] anon;
  assign o       = anon;
  always_comb
  begin
    if (lhs[7]) anon = shifted ^ 8'h1b;
    else anon = shifted;
  end
  assign shifted = lhs << 1;
endmodule
