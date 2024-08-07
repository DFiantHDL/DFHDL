package docExamples

class FullAdderNSpec extends util.FullCompileSpec:
  def dut = fullAdderN.FullAdderN()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "FullAdderN_defs.svh"
       |
       |module FullAdder1(
       |  input  wire logic a,
       |  input  wire logic b,
       |  input  wire logic c_in,
       |  output logic sum,
       |  output logic c_out
       |);
       |  assign sum   = (a ^ b) ^ c_in;
       |  assign c_out = ((a & b) | (b & c_in)) | (c_in & a);
       |endmodule
       |
       |`default_nettype none
       |`timescale 1ns/1ps
       |`include "FullAdderN_defs.svh"
       |
       |module FullAdderN(
       |  input  wire logic [3:0] a,
       |  input  wire logic [3:0] b,
       |  input  wire logic c_in,
       |  output logic [3:0] sum,
       |  output logic c_out
       |);
       |  logic adder_0_a;
       |  logic adder_0_b;
       |  logic adder_0_c_in;
       |  logic adder_0_sum;
       |  logic adder_0_c_out;
       |  logic adder_1_a;
       |  logic adder_1_b;
       |  logic adder_1_c_in;
       |  logic adder_1_sum;
       |  logic adder_1_c_out;
       |  logic adder_2_a;
       |  logic adder_2_b;
       |  logic adder_2_c_in;
       |  logic adder_2_sum;
       |  logic adder_2_c_out;
       |  logic adder_3_a;
       |  logic adder_3_b;
       |  logic adder_3_c_in;
       |  logic adder_3_sum;
       |  logic adder_3_c_out;
       |  FullAdder1 adder_0(
       |    .a     /*<--*/ (adder_0_a),
       |    .b     /*<--*/ (adder_0_b),
       |    .c_in  /*<--*/ (adder_0_c_in),
       |    .sum   /*-->*/ (adder_0_sum),
       |    .c_out /*-->*/ (adder_0_c_out)
       |  );
       |  FullAdder1 adder_1(
       |    .a     /*<--*/ (adder_1_a),
       |    .b     /*<--*/ (adder_1_b),
       |    .c_in  /*<--*/ (adder_1_c_in),
       |    .sum   /*-->*/ (adder_1_sum),
       |    .c_out /*-->*/ (adder_1_c_out)
       |  );
       |  FullAdder1 adder_2(
       |    .a     /*<--*/ (adder_2_a),
       |    .b     /*<--*/ (adder_2_b),
       |    .c_in  /*<--*/ (adder_2_c_in),
       |    .sum   /*-->*/ (adder_2_sum),
       |    .c_out /*-->*/ (adder_2_c_out)
       |  );
       |  FullAdder1 adder_3(
       |    .a     /*<--*/ (adder_3_a),
       |    .b     /*<--*/ (adder_3_b),
       |    .c_in  /*<--*/ (adder_3_c_in),
       |    .sum   /*-->*/ (adder_3_sum),
       |    .c_out /*-->*/ (adder_3_c_out)
       |  );
       |  assign adder_0_a    = a[0];
       |  assign adder_0_b    = b[0];
       |  assign sum[0]       = adder_0_sum;
       |  assign adder_1_c_in = adder_0_c_out;
       |  assign adder_1_a    = a[1];
       |  assign adder_1_b    = b[1];
       |  assign sum[1]       = adder_1_sum;
       |  assign adder_2_c_in = adder_1_c_out;
       |  assign adder_2_a    = a[2];
       |  assign adder_2_b    = b[2];
       |  assign sum[2]       = adder_2_sum;
       |  assign adder_3_c_in = adder_2_c_out;
       |  assign adder_3_a    = a[3];
       |  assign adder_3_b    = b[3];
       |  assign sum[3]       = adder_3_sum;
       |  assign adder_0_c_in = c_in;
       |  assign c_out        = adder_3_c_out;
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.FullAdderN_pkg.all;
       |
       |entity FullAdder1 is
       |port (
       |  a     : in  std_logic;
       |  b     : in  std_logic;
       |  c_in  : in  std_logic;
       |  sum   : out std_logic;
       |  c_out : out std_logic
       |);
       |end FullAdder1;
       |
       |architecture FullAdder1_arch of FullAdder1 is
       |begin
       |  sum   <= (a xor b) xor c_in;
       |  c_out <= ((a and b) or (b and c_in)) or (c_in and a);
       |end FullAdder1_arch;
       |
       |library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.FullAdderN_pkg.all;
       |
       |entity FullAdderN is
       |port (
       |  a     : in  std_logic_vector(3 downto 0);
       |  b     : in  std_logic_vector(3 downto 0);
       |  c_in  : in  std_logic;
       |  sum   : out std_logic_vector(3 downto 0);
       |  c_out : out std_logic
       |);
       |end FullAdderN;
       |
       |architecture FullAdderN_arch of FullAdderN is
       |  signal adder_0_a     : std_logic;
       |  signal adder_0_b     : std_logic;
       |  signal adder_0_c_in  : std_logic;
       |  signal adder_0_sum   : std_logic;
       |  signal adder_0_c_out : std_logic;
       |  signal adder_1_a     : std_logic;
       |  signal adder_1_b     : std_logic;
       |  signal adder_1_c_in  : std_logic;
       |  signal adder_1_sum   : std_logic;
       |  signal adder_1_c_out : std_logic;
       |  signal adder_2_a     : std_logic;
       |  signal adder_2_b     : std_logic;
       |  signal adder_2_c_in  : std_logic;
       |  signal adder_2_sum   : std_logic;
       |  signal adder_2_c_out : std_logic;
       |  signal adder_3_a     : std_logic;
       |  signal adder_3_b     : std_logic;
       |  signal adder_3_c_in  : std_logic;
       |  signal adder_3_sum   : std_logic;
       |  signal adder_3_c_out : std_logic;
       |begin
       |  adder_0 : entity work.FullAdder1(FullAdder1_arch) port map (
       |    a          => adder_0_a,
       |    b          => adder_0_b,
       |    c_in       => adder_0_c_in,
       |    sum        => adder_0_sum,
       |    c_out      => adder_0_c_out
       |  );
       |  adder_1 : entity work.FullAdder1(FullAdder1_arch) port map (
       |    a          => adder_1_a,
       |    b          => adder_1_b,
       |    c_in       => adder_1_c_in,
       |    sum        => adder_1_sum,
       |    c_out      => adder_1_c_out
       |  );
       |  adder_2 : entity work.FullAdder1(FullAdder1_arch) port map (
       |    a          => adder_2_a,
       |    b          => adder_2_b,
       |    c_in       => adder_2_c_in,
       |    sum        => adder_2_sum,
       |    c_out      => adder_2_c_out
       |  );
       |  adder_3 : entity work.FullAdder1(FullAdder1_arch) port map (
       |    a          => adder_3_a,
       |    b          => adder_3_b,
       |    c_in       => adder_3_c_in,
       |    sum        => adder_3_sum,
       |    c_out      => adder_3_c_out
       |  );
       |  adder_0_a    <= a(0);
       |  adder_0_b    <= b(0);
       |  sum(0)       <= adder_0_sum;
       |  adder_1_c_in <= adder_0_c_out;
       |  adder_1_a    <= a(1);
       |  adder_1_b    <= b(1);
       |  sum(1)       <= adder_1_sum;
       |  adder_2_c_in <= adder_1_c_out;
       |  adder_2_a    <= a(2);
       |  adder_2_b    <= b(2);
       |  sum(2)       <= adder_2_sum;
       |  adder_3_c_in <= adder_2_c_out;
       |  adder_3_a    <= a(3);
       |  adder_3_b    <= b(3);
       |  sum(3)       <= adder_3_sum;
       |  adder_0_c_in <= c_in;
       |  c_out        <= adder_3_c_out;
       |end FullAdderN_arch;""".stripMargin
end FullAdderNSpec
