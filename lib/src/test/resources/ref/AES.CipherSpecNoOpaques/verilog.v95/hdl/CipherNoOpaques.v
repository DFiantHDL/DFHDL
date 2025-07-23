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
  input  wire [127:0] key;
  input  wire [127:0] data;
  output wire [127:0] o;
  wire [127:0] o_part_cipher_inst_data;
  wire [127:0] o_part_cipher_inst_key;
  wire [127:0] o_part_cipher_inst_o;
  cipher o_part_cipher_inst(
    .data /*<--*/ (o_part_cipher_inst_data),
    .key  /*<--*/ (o_part_cipher_inst_key),
    .o    /*-->*/ (o_part_cipher_inst_o)
  );
  assign o_part_cipher_inst_data = data;
  assign o_part_cipher_inst_key  = key;
  assign o                       = o_part_cipher_inst_o;
endmodule
