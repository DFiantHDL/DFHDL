`default_nettype none
`timescale 1ns/1ps

module FullAdderN(
  input  wire [3:0] a,
  input  wire [3:0] b,
  input  wire       c_in,
  output wire [3:0] sum,
  output wire       c_out
);
  `include "dfhdl_defs.vh"
  wire [3:0] adder_c_in;
  wire [3:0] adder_c_out;
  wire adder_0_c_in;
  wire adder_0_a;
  wire adder_0_b;
  wire adder_0_sum;
  wire adder_1_c_in;
  wire adder_1_a;
  wire adder_1_b;
  wire adder_1_sum;
  wire adder_2_c_in;
  wire adder_2_a;
  wire adder_2_b;
  wire adder_2_sum;
  wire adder_3_c_in;
  wire adder_3_a;
  wire adder_3_b;
  wire adder_3_sum;
  FullAdder1 adder_0(
    .c_in  /*<--*/ (adder_0_c_in),
    .a     /*<--*/ (adder_0_a),
    .b     /*<--*/ (adder_0_b),
    .sum   /*-->*/ (adder_0_sum),
    .c_out /*-->*/ (adder_c_out[0])
  );
  FullAdder1 adder_1(
    .c_in  /*<--*/ (adder_1_c_in),
    .a     /*<--*/ (adder_1_a),
    .b     /*<--*/ (adder_1_b),
    .sum   /*-->*/ (adder_1_sum),
    .c_out /*-->*/ (adder_c_out[1])
  );
  FullAdder1 adder_2(
    .c_in  /*<--*/ (adder_2_c_in),
    .a     /*<--*/ (adder_2_a),
    .b     /*<--*/ (adder_2_b),
    .sum   /*-->*/ (adder_2_sum),
    .c_out /*-->*/ (adder_c_out[2])
  );
  FullAdder1 adder_3(
    .c_in  /*<--*/ (adder_3_c_in),
    .a     /*<--*/ (adder_3_a),
    .b     /*<--*/ (adder_3_b),
    .sum   /*-->*/ (adder_3_sum),
    .c_out /*-->*/ (adder_c_out[3])
  );
  assign adder_c_in[0] = c_in;
  assign adder_0_a     = a[0];
  assign adder_0_b     = b[0];
  assign sum[0]        = adder_0_sum;
  assign adder_0_c_in  = adder_c_in[0];
  assign adder_c_in[1] = adder_c_out[0];
  assign adder_1_a     = a[1];
  assign adder_1_b     = b[1];
  assign sum[1]        = adder_1_sum;
  assign adder_1_c_in  = adder_c_in[1];
  assign adder_c_in[2] = adder_c_out[1];
  assign adder_2_a     = a[2];
  assign adder_2_b     = b[2];
  assign sum[2]        = adder_2_sum;
  assign adder_2_c_in  = adder_c_in[2];
  assign adder_c_in[3] = adder_c_out[2];
  assign adder_3_a     = a[3];
  assign adder_3_b     = b[3];
  assign sum[3]        = adder_3_sum;
  assign adder_3_c_in  = adder_c_in[3];
  assign c_out         = adder_c_out[3];
endmodule
