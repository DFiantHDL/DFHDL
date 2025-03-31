`default_nettype none
`timescale 1ns/1ps
`include "FullAdderN_defs.vh"

module FullAdderN(
  a,
  b,
  c_in,
  sum,
  c_out
);
  `include "dfhdl_defs.vh"
  `include "FullAdderN_defs.vh"
  input  wire  [3:0] a;
  input  wire  [3:0] b;
  input  wire        c_in;
  output wire [3:0]  sum;
  output wire        c_out;
  wire   adder_0_a;
  wire   adder_0_b;
  wire   adder_0_c_in;
  wire   adder_0_sum;
  wire   adder_0_c_out;
  wire   adder_1_a;
  wire   adder_1_b;
  wire   adder_1_c_in;
  wire   adder_1_sum;
  wire   adder_1_c_out;
  wire   adder_2_a;
  wire   adder_2_b;
  wire   adder_2_c_in;
  wire   adder_2_sum;
  wire   adder_2_c_out;
  wire   adder_3_a;
  wire   adder_3_b;
  wire   adder_3_c_in;
  wire   adder_3_sum;
  wire   adder_3_c_out;
  FullAdder1 adder_0(
    .a     /*<--*/ (adder_0_a),
    .b     /*<--*/ (adder_0_b),
    .c_in  /*<--*/ (adder_0_c_in),
    .sum   /*-->*/ (adder_0_sum),
    .c_out /*-->*/ (adder_0_c_out)
  );
  FullAdder1 adder_1(
    .a     /*<--*/ (adder_1_a),
    .b     /*<--*/ (adder_1_b),
    .c_in  /*<--*/ (adder_1_c_in),
    .sum   /*-->*/ (adder_1_sum),
    .c_out /*-->*/ (adder_1_c_out)
  );
  FullAdder1 adder_2(
    .a     /*<--*/ (adder_2_a),
    .b     /*<--*/ (adder_2_b),
    .c_in  /*<--*/ (adder_2_c_in),
    .sum   /*-->*/ (adder_2_sum),
    .c_out /*-->*/ (adder_2_c_out)
  );
  FullAdder1 adder_3(
    .a     /*<--*/ (adder_3_a),
    .b     /*<--*/ (adder_3_b),
    .c_in  /*<--*/ (adder_3_c_in),
    .sum   /*-->*/ (adder_3_sum),
    .c_out /*-->*/ (adder_3_c_out)
  );
  assign adder_0_a    = a[0];
  assign adder_0_b    = b[0];
  assign sum[0]       = adder_0_sum;
  assign adder_1_c_in = adder_0_c_out;
  assign adder_1_a    = a[1];
  assign adder_1_b    = b[1];
  assign sum[1]       = adder_1_sum;
  assign adder_2_c_in = adder_1_c_out;
  assign adder_2_a    = a[2];
  assign adder_2_b    = b[2];
  assign sum[2]       = adder_2_sum;
  assign adder_3_c_in = adder_2_c_out;
  assign adder_3_a    = a[3];
  assign adder_3_b    = b[3];
  assign sum[3]       = adder_3_sum;
  assign adder_0_c_in = c_in;
  assign c_out        = adder_3_c_out;
endmodule
