package docExamples

class FullAdder1Spec extends util.FullCompileSpec:
  def dut = fullAdder1.FullAdder1()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "FullAdder1_defs.svh"
       |
       |module FullAdder1(
       |  input  wire logic a,
       |  input  wire logic b,
       |  input  wire logic c_in,
       |  output      logic sum,
       |  output      logic c_out
       |);
       |  assign sum   = (a ^ b) ^ c_in;
       |  assign c_out = ((a & b) | (b & c_in)) | (c_in & a);
       |endmodule
       |""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.FullAdder1_pkg.all;
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
       |""".stripMargin
end FullAdder1Spec
