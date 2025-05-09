`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module CipherNoOpaques(
  key,
  data,
  o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  input  wire  [7:0] key [0:3] [0:3];
  input  wire  [7:0] data [0:3] [0:3];
  output wire [7:0] o [0:3] [0:3];
  wire [7:0] o_part_cipher_inst_data [0:3] [0:3];
  wire [7:0] o_part_cipher_inst_key [0:3] [0:3];
  wire [7:0] o_part_cipher_inst_o [0:3] [0:3];
  cipher o_part_cipher_inst(
    .data /*<--*/ (o_part_cipher_inst_data),
    .key  /*<--*/ (o_part_cipher_inst_key),
    .o    /*-->*/ (o_part_cipher_inst_o)
  );
  assign o_part_cipher_inst_data = data;
  assign o_part_cipher_inst_key  = key;
  assign o                       = o_part_cipher_inst_o;
endmodule
