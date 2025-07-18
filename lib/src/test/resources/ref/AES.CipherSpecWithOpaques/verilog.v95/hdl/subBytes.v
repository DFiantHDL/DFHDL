`default_nettype none
`timescale 1ns/1ps
`include "Cipher_defs.vh"

module subBytes(
  state,
  o
);
  `include "dfhdl_defs.vh"
  `include "Cipher_defs.vh"
  input  wire  [127:0] state;
  output wire [127:0]  o;
  wire [7:0] sbox_inst_00_lhs;
  wire [7:0] sbox_inst_00_o;
  wire [7:0] sbox_inst_01_lhs;
  wire [7:0] sbox_inst_01_o;
  wire [7:0] sbox_inst_02_lhs;
  wire [7:0] sbox_inst_02_o;
  wire [7:0] sbox_inst_03_lhs;
  wire [7:0] sbox_inst_03_o;
  wire [7:0] sbox_inst_04_lhs;
  wire [7:0] sbox_inst_04_o;
  wire [7:0] sbox_inst_05_lhs;
  wire [7:0] sbox_inst_05_o;
  wire [7:0] sbox_inst_06_lhs;
  wire [7:0] sbox_inst_06_o;
  wire [7:0] sbox_inst_07_lhs;
  wire [7:0] sbox_inst_07_o;
  wire [7:0] sbox_inst_08_lhs;
  wire [7:0] sbox_inst_08_o;
  wire [7:0] sbox_inst_09_lhs;
  wire [7:0] sbox_inst_09_o;
  wire [7:0] sbox_inst_10_lhs;
  wire [7:0] sbox_inst_10_o;
  wire [7:0] sbox_inst_11_lhs;
  wire [7:0] sbox_inst_11_o;
  wire [7:0] sbox_inst_12_lhs;
  wire [7:0] sbox_inst_12_o;
  wire [7:0] sbox_inst_13_lhs;
  wire [7:0] sbox_inst_13_o;
  wire [7:0] sbox_inst_14_lhs;
  wire [7:0] sbox_inst_14_o;
  wire [7:0] sbox_inst_15_lhs;
  wire [7:0] sbox_inst_15_o;
  sbox sbox_inst_00(
    .lhs /*<--*/ (sbox_inst_00_lhs),
    .o   /*-->*/ (sbox_inst_00_o)
  );
  sbox sbox_inst_01(
    .lhs /*<--*/ (sbox_inst_01_lhs),
    .o   /*-->*/ (sbox_inst_01_o)
  );
  sbox sbox_inst_02(
    .lhs /*<--*/ (sbox_inst_02_lhs),
    .o   /*-->*/ (sbox_inst_02_o)
  );
  sbox sbox_inst_03(
    .lhs /*<--*/ (sbox_inst_03_lhs),
    .o   /*-->*/ (sbox_inst_03_o)
  );
  sbox sbox_inst_04(
    .lhs /*<--*/ (sbox_inst_04_lhs),
    .o   /*-->*/ (sbox_inst_04_o)
  );
  sbox sbox_inst_05(
    .lhs /*<--*/ (sbox_inst_05_lhs),
    .o   /*-->*/ (sbox_inst_05_o)
  );
  sbox sbox_inst_06(
    .lhs /*<--*/ (sbox_inst_06_lhs),
    .o   /*-->*/ (sbox_inst_06_o)
  );
  sbox sbox_inst_07(
    .lhs /*<--*/ (sbox_inst_07_lhs),
    .o   /*-->*/ (sbox_inst_07_o)
  );
  sbox sbox_inst_08(
    .lhs /*<--*/ (sbox_inst_08_lhs),
    .o   /*-->*/ (sbox_inst_08_o)
  );
  sbox sbox_inst_09(
    .lhs /*<--*/ (sbox_inst_09_lhs),
    .o   /*-->*/ (sbox_inst_09_o)
  );
  sbox sbox_inst_10(
    .lhs /*<--*/ (sbox_inst_10_lhs),
    .o   /*-->*/ (sbox_inst_10_o)
  );
  sbox sbox_inst_11(
    .lhs /*<--*/ (sbox_inst_11_lhs),
    .o   /*-->*/ (sbox_inst_11_o)
  );
  sbox sbox_inst_12(
    .lhs /*<--*/ (sbox_inst_12_lhs),
    .o   /*-->*/ (sbox_inst_12_o)
  );
  sbox sbox_inst_13(
    .lhs /*<--*/ (sbox_inst_13_lhs),
    .o   /*-->*/ (sbox_inst_13_o)
  );
  sbox sbox_inst_14(
    .lhs /*<--*/ (sbox_inst_14_lhs),
    .o   /*-->*/ (sbox_inst_14_o)
  );
  sbox sbox_inst_15(
    .lhs /*<--*/ (sbox_inst_15_lhs),
    .o   /*-->*/ (sbox_inst_15_o)
  );
  assign sbox_inst_00_lhs = state[127:120];
  assign sbox_inst_01_lhs = state[119:112];
  assign sbox_inst_02_lhs = state[111:104];
  assign sbox_inst_03_lhs = state[103:96];
  assign sbox_inst_04_lhs = state[95:88];
  assign sbox_inst_05_lhs = state[87:80];
  assign sbox_inst_06_lhs = state[79:72];
  assign sbox_inst_07_lhs = state[71:64];
  assign sbox_inst_08_lhs = state[63:56];
  assign sbox_inst_09_lhs = state[55:48];
  assign sbox_inst_10_lhs = state[47:40];
  assign sbox_inst_11_lhs = state[39:32];
  assign sbox_inst_12_lhs = state[31:24];
  assign sbox_inst_13_lhs = state[23:16];
  assign sbox_inst_14_lhs = state[15:8];
  assign sbox_inst_15_lhs = state[7:0];
  assign o = {
    {sbox_inst_00_o, sbox_inst_01_o, sbox_inst_02_o, sbox_inst_03_o},
    {sbox_inst_04_o, sbox_inst_05_o, sbox_inst_06_o, sbox_inst_07_o},
    {sbox_inst_08_o, sbox_inst_09_o, sbox_inst_10_o, sbox_inst_11_o},
    {sbox_inst_12_o, sbox_inst_13_o, sbox_inst_14_o, sbox_inst_15_o}
  };
endmodule
