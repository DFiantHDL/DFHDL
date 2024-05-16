package issues

import munit.*
import dfhdl.*
import dfhdl.compiler.stages.getCompiledCodeString

class IssuesSpec extends FunSuite:
  test("i116 compiles with no exception"):
    i116.GlobCounter(64).compile
  test("i118 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i118.ShiftIssue().compile.lint
  test("i126 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i126.TypeConvertIssue().compile.lint
  test("i128 compiles with the expected code output"):
    given options.CompilerOptions.Backend = backends.vhdl
    assertNoDiff(
      i128.ArrayIssue().getCompiledCodeString,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.ArrayIssue_pkg.all;
         |
         |entity ArrayIssue is
         |port (
         |  a : in std_logic
         |);
         |end ArrayIssue;
         |
         |architecture ArrayIssue_arch of ArrayIssue is
         |  type t_vecX1_std_logic is array (natural range <>) of std_logic;
         |  type t_vecX2_std_logic is array (natural range <>) of t_vecX1_std_logic;
         |  type t_vecX1_std_logic_vector is array (natural range <>) of std_logic_vector;
         |  type t_vecX2_std_logic_vector is array (natural range <>) of t_vecX1_std_logic_vector;
         |  signal b : t_vecX1_std_logic(0 to 6 - 1);
         |  signal c : t_vecX2_std_logic(0 to 4 - 1)(0 to 5 - 1);
         |  signal d : t_vecX2_std_logic_vector(0 to 2 - 1)(0 to 3 - 1)(3 downto 0);
         |begin
         |  process (all)
         |  begin
         |    b(0) <= a;
         |    c(0)(0) <= a;
         |    d(0)(0)(0) <= a;
         |  end process;
         |end ArrayIssue_arch;
         |""".stripMargin
    )
  test("i129 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i129.StdLogicConvIssue().compile.lint
  test("i131 compiles with no exception"):
    i131.DictControl(fetch_count = 2).compile
  test("i133 compiles with no exception"):
    i133.Width0Issue(1).compile
  test("i135 compiles with the expected code output"):
    assertNoDiff(
      i135.VerilogSRA().getCompiledCodeString,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "VerilogSRA_defs.sv"
         |
         |module VerilogSRA(
         |  input wire logic signed [9:0] a
         |);
         |  logic signed [9:0] b;
         |  always @(*)
         |  begin
         |    b = a >>> 1;
         |  end
         |endmodule
         |""".stripMargin
    )
  test("i142 compiles and passes VHDL linting"):
    given options.CompilerOptions.Backend = backends.vhdl
    i142.IntegerIndexingIssue().compile.lint
end IssuesSpec
