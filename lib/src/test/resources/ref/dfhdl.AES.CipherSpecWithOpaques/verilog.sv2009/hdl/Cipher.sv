`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.svh"

module Cipher(
  input  wire t_opaque_AESKey  key,
  input  wire t_opaque_AESData data,
  output t_opaque_AESData      o
);
  `include "dfhdl_defs.svh"
  t_opaque_AESData o_part_cipher_inst_data;
  t_opaque_AESKey  o_part_cipher_inst_key;
  t_opaque_AESData o_part_cipher_inst_o;
  cipher_0 o_part_cipher_inst(
    .data /*<--*/ (o_part_cipher_inst_data),
    .key  /*<--*/ (o_part_cipher_inst_key),
    .o    /*-->*/ (o_part_cipher_inst_o)
  );
  assign o_part_cipher_inst_data = data;
  assign o_part_cipher_inst_key  = key;
  assign o                       = o_part_cipher_inst_o;
endmodule
