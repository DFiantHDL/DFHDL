`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module Cipher(
  input  wire AESKey  key,
  input  wire AESData data,
  output AESData      o
);
  `include "dfhdl_defs.svh"
  AESData o_part_cipher_inst_data;
  AESKey  o_part_cipher_inst_key;
  AESData o_part_cipher_inst_o;
  cipher_0 o_part_cipher_inst(
    .data /*<--*/ (o_part_cipher_inst_data),
    .key  /*<--*/ (o_part_cipher_inst_key),
    .o    /*-->*/ (o_part_cipher_inst_o)
  );
  assign o_part_cipher_inst_data = data;
  assign o_part_cipher_inst_key  = key;
  assign o                       = o_part_cipher_inst_o;
endmodule
