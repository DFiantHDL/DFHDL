`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.vh"

module addRoundKey(
  state,
  key,
  o
);
  `include "dfhdl_defs.vh"
  `include "CipherNoOpaques_defs.vh"
  input  wire [127:0] state;
  input  wire [127:0] key;
  output wire [127:0] o;
  assign o = {
    {state[127:120] ^ key[127:120], state[119:112] ^ key[119:112], state[111:104] ^ key[111:104], state[103:96] ^ key[103:96]},
    {state[95:88] ^ key[95:88], state[87:80] ^ key[87:80], state[79:72] ^ key[79:72], state[71:64] ^ key[71:64]},
    {state[63:56] ^ key[63:56], state[55:48] ^ key[55:48], state[47:40] ^ key[47:40], state[39:32] ^ key[39:32]},
    {state[31:24] ^ key[31:24], state[23:16] ^ key[23:16], state[15:8] ^ key[15:8], state[7:0] ^ key[7:0]}
  };
endmodule
