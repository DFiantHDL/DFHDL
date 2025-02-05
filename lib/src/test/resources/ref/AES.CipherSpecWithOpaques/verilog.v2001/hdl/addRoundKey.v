`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module addRoundKey(
  input  wire  [7:0] state [0:3] [0:3],
  input  wire  [7:0] key [0:3] [0:3],
  output wire [7:0] o [0:3] [0:3]
);
  `include "dfhdl_defs.vh"
  assign o = '{
    '{state[0][0] ^ key[0][0], state[0][1] ^ key[0][1], state[0][2] ^ key[0][2], state[0][3] ^ key[0][3]},
    '{state[1][0] ^ key[1][0], state[1][1] ^ key[1][1], state[1][2] ^ key[1][2], state[1][3] ^ key[1][3]},
    '{state[2][0] ^ key[2][0], state[2][1] ^ key[2][1], state[2][2] ^ key[2][2], state[2][3] ^ key[2][3]},
    '{state[3][0] ^ key[3][0], state[3][1] ^ key[3][1], state[3][2] ^ key[3][2], state[3][3] ^ key[3][3]}
  };
endmodule
