`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module sbox(
  input  wire  [7:0] lhs,
  output wire [7:0]  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  reg [7:0] sboxLookupTable_rom [0:255];
  initial begin : sboxLookupTable_rom_init
    integer i;
    for (i = 0; i < 256; i = i + 1) begin
      sboxLookupTable_rom[i] = sboxLookupTable[(256-i)*8-1-:8];
    end
  end
  assign o = sboxLookupTable_rom[lhs];
endmodule
