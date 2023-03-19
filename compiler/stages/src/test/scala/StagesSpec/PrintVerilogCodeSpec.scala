package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.verilog.{getVerilogCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = ":=="}]}

class PrintVerilogCodeSpec extends StageSpec:
  class ID extends EDDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y <> x

  class IDTop extends EDDesign:
    self =>
    val x     = SInt(16) <> IN
    val y     = SInt(16) <> OUT
    val id1_x = SInt(16) <> VAR
    val id1_y = SInt(16) <> VAR
    val id2_x = SInt(16) <> VAR
    val id2_y = SInt(16) <> VAR
    val id1 = new ID:
      this.x <> id1_x
      this.y <> id1_y
    val id2 = new ID:
      this.x <> id2_x
      this.y <> id2_y
    id1_x <> x
    id2_x <> id1_y
    y     <> id2_y
  end IDTop

  test("Basic ID design") {
    val id = (new ID).getVerilogCode
    assertNoDiff(
      id,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "ID_defs.sv"
         |
         |module ID(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  assign y = x;
         |endmodule
         |""".stripMargin
    )
  }

  test("Basic hierarchy design") {
    val top = (new IDTop).getVerilogCode
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module ID(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  assign y = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module IDTop(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  logic signed [15:0] id1_x;
         |  logic signed [15:0] id1_y;
         |  logic signed [15:0] id2_x;
         |  logic signed [15:0] id2_y;
         |  ID id1(
         |    .x /*<--*/ (id1_x),
         |    .y /*-->*/ (id1_y)
         |  );
         |  ID id2(
         |    .x /*<--*/ (id2_x),
         |    .y /*-->*/ (id2_y)
         |  );
         |  assign id1_x = x;
         |  assign id2_x = id1_y;
         |  assign y = id2_y;
         |endmodule
         |""".stripMargin
    )
  }
  test("process block") {
    class Top extends EDDesign:
      val clk = Bit      <> IN
      val rst = Bit      <> IN
      val x   = Bits(16) <> IN
      val y   = Bits(16) <> OUT
      val z   = Bits(16) <> VAR
      process(clk, rst) {
        val c = Bits(16) const all(0)
        if (rst)
          y :== c
        else if (clk.rising)
          y :== x
      }
      val myblock = process(all) {
        val my_var = Bits(16) <> VAR
        my_var := x
        y     :== my_var
      }
      process.forever {
        z :== x
        y :== z
      }
    end Top
    val top = (new Top).getVerilogCode(align = true)
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top(
         |  input  wire clk,
         |  input  wire rst,
         |  input  wire [15:0] x,
         |  output reg  [15:0] y
         |);
         |  logic       [15:0] z;
         |  parameter c = 16'h0000;
         |  always @(clk, rst)
         |  begin
         |    if (rst) y <= c;
         |    else if (posedge clk) y <= x;
         |  end
         |  logic       [15:0] my_var;
         |  myblock : always @(*)
         |  begin
         |    my_var = x;
         |    y      <= my_var;
         |  end
         |  always
         |  begin
         |    z      <= x;
         |    y      <= z;
         |  end
         |endmodule
         |""".stripMargin
    )
  }
  test("literals") {
    class Top extends EDDesign:
      val c01 = Bit const 0
      val c02 = Bit const 1
      val c03 = Bit const ?
      val c04 = Boolean const 0
      val c05 = Boolean const 1
      val c06 = Bits(8) const h"22"
      val c07 = Bits(7) const h"7'22"
      val c08 = Bits(3) const b"101"
      val c09 = UInt(3) const 7
      val c10 = UInt(48) const d"48'239794508230343"
      val c11 = SInt(4) const -8
      val c12 = SInt(49) const sd"49'-239794508230343"

    end Top
    val top = (new Top).getVerilogCode
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top;
         |  parameter c01 = 1'b0;
         |  parameter c02 = 1'b1;
         |  parameter c03 = 1'bx;
         |  parameter c04 = 0;
         |  parameter c05 = 1;
         |  parameter c06 = 8'h22;
         |  parameter c07 = 7'h22;
         |  parameter c08 = 3'h5;
         |  parameter c09 = 3'd7;
         |  parameter c10 = 48'd239794508230343;
         |  parameter c11 = -4'sd8;
         |  parameter c12 = -49'sd239794508230343;
         |
         |endmodule
         |""".stripMargin
    )
  }
end PrintVerilogCodeSpec
