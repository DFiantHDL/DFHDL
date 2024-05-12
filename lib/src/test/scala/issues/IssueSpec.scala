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
end IssuesSpec
