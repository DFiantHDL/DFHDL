`default_nettype none
`timescale 1ns/1ps
`include "CipherNoOpaques_defs.svh"

module shiftRows(
  input  wire AESState state,
  output AESState      o
);
  `include "dfhdl_defs.svh"
  assign o = '{
    3: '{3: state[2][3], 2: state[1][2], 1: state[0][1], 0: state[3][0]},
    2: '{3: state[1][3], 2: state[0][2], 1: state[3][1], 0: state[2][0]},
    1: '{3: state[0][3], 2: state[3][2], 1: state[2][1], 0: state[1][0]},
    0: '{3: state[3][3], 2: state[2][2], 1: state[1][1], 0: state[0][0]}
  };
endmodule
