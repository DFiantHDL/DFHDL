`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module shiftRows(
  state,
  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  input  wire  [7:0] state [0:3] [0:3];
  output wire [7:0] o [0:3] [0:3];
  assign o = '{
    '{state[0][0], state[1][1], state[2][2], state[3][3]}, '{state[1][0], state[2][1], state[3][2], state[0][3]},
    '{state[2][0], state[3][1], state[0][2], state[1][3]}, '{state[3][0], state[0][1], state[1][2], state[2][3]}
  };
endmodule
