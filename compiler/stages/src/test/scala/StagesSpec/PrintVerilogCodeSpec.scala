package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.getCompiledCodeString
// scalafmt: { align.tokens = [{code = ":"}, {code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = ":=="}]}

class PrintVerilogCodeSpec extends StageSpec:
  given options.CompilerOptions.Backend = backends.verilog.sv2005
  given options.PrinterOptions.Align    = false
  class ID extends EDDesign:
    val x  = SInt(16) <> IN
    val y  = SInt(16) <> OUT
    val y2 = SInt(16) <> OUT
    y  <> x
    y2 <> x

  class IDTop extends EDDesign:
    self =>
    val x     = SInt(16) <> IN
    val y     = SInt(16) <> OUT
    val id1_x = SInt(16) <> VAR
    val id1_y = SInt(16) <> VAR
    val id2_x = SInt(16) <> VAR
    val id2_y = SInt(16) <> VAR
    val id1 = new ID:
      this.x  <> id1_x
      this.y  <> id1_y
      this.y2 <> OPEN
    val id2 = new ID:
      this.x  <> id2_x
      this.y  <> id2_y
      this.y2 <> OPEN
    id1_x <> x
    id2_x <> id1_y
    y     <> id2_y
  end IDTop

  test("Basic ID design") {
    val id = (new ID).getCompiledCodeString
    assertNoDiff(
      id,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "ID_defs.sv"
         |
         |module ID(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  assign y = x;
         |  assign y2 = x;
         |endmodule
         |""".stripMargin
    )
  }

  test("Basic hierarchy design") {
    val top = (new IDTop).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module ID(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  assign y = x;
         |  assign y2 = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module IDTop(
         |  input wire logic signed [15:0] x,
         |  output logic signed [15:0] y
         |);
         |  logic signed [15:0] id1_x;
         |  logic signed [15:0] id1_y;
         |  logic signed [15:0] id2_x;
         |  logic signed [15:0] id2_y;
         |  ID id1(
         |    .x /*<--*/ (id1_x),
         |    .y /*-->*/ (id1_y),
         |    .y2 /*-->*/ (/*open*/)
         |  );
         |  ID id2(
         |    .x /*<--*/ (id2_x),
         |    .y /*-->*/ (id2_y),
         |    .y2 /*-->*/ (/*open*/)
         |  );
         |  assign id1_x = x;
         |  assign id2_x = id1_y;
         |  assign y = id2_y;
         |endmodule
         |""".stripMargin
    )
  }

  test("Basic hierarchy design with parameters") {
    class ID(val width: Int <> CONST) extends DFDesign:
      val x = SInt(width) <> IN
      val y = SInt(width) <> OUT
      y := x

    class IDTop(val width: Int <> CONST) extends DFDesign:
      val x   = SInt(width) <> IN
      val y   = SInt(width) <> OUT
      val id1 = ID(width)
      val id2 = ID(width)
      id1.x <> x
      id1.y <> id2.x
      id2.y <> y
    val top = (new IDTop(16)).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module ID#(parameter int width)(
         |  input wire logic signed [width - 1:0] x,
         |  output logic signed [width - 1:0] y
         |);
         |  always @(*)
         |  begin
         |    y = x;
         |  end
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.sv"
         |
         |module IDTop#(parameter int width = 16)(
         |  input wire logic signed [width - 1:0] x,
         |  output logic signed [width - 1:0] y
         |);
         |  logic signed [width - 1:0] id1_x;
         |  logic signed [width - 1:0] id1_y;
         |  logic signed [width - 1:0] id2_x;
         |  logic signed [width - 1:0] id2_y;
         |  ID #(
         |    .width (width)
         |  ) id1(
         |    .x /*<--*/ (id1_x),
         |    .y /*-->*/ (id1_y)
         |  );
         |  ID #(
         |    .width (width)
         |  ) id2(
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

  test("Global, design, and local parameters") {
    val gp: Bit <> CONST = 1
    class ParamTest(dp: Bit <> CONST) extends RTDesign:
      val lp: Bit <> CONST = 1
      val x = Bit <> IN
      val y = Bit <> OUT
      y := x || gp || dp || lp
    val top = ParamTest(1).getCompiledCodeString
    assertNoDiff(
      top,
      """|parameter logic gp = 1'b1;
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "ParamTest_defs.sv"
         |
         |module ParamTest#(parameter logic dp = 1'b1)(
         |  input wire logic x,
         |  output logic y
         |);
         |  parameter logic lp = 1'b1;
         |  always @(*)
         |  begin
         |    y = ((x | gp) | dp) | lp;
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("process block") {
    given options.PrinterOptions.Align = true
    class Top extends EDDesign:
      val clk = Bit      <> IN
      val rst = Bit      <> IN
      val x   = Bits(16) <> IN
      val y   = Bits(16) <> OUT
      val z   = Bits(16) <> VAR
      process(clk, rst) {
        val c: Bits[16] <> CONST = all(0)
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
    val top = (new Top).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top(
         |  input wire logic        clk,
         |  input wire logic        rst,
         |  input wire logic [15:0] x,
         |  output logic [15:0]     y
         |);
         |  logic [15:0] z;
         |  parameter logic [15:0] c = 16'h0000;
         |  logic [15:0] my_var;
         |  always @(posedge clk, posedge rst)
         |  begin
         |    if (rst) y <= c;
         |    else y <= x;
         |  end
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
      val c01: Bit <> CONST             = 0
      val c02: Bit <> CONST             = 1
      val c03: Bit <> CONST             = ?
      val c04: Boolean <> CONST         = false
      val c05: Boolean <> CONST         = true
      val c06: Bits[8] <> CONST         = h"22"
      val c07: Bits[7] <> CONST         = h"7'22"
      val c08: Bits[3] <> CONST         = b"101"
      val c09: UInt[3] <> CONST         = 7
      val c10: UInt[48] <> CONST        = d"48'239794508230343"
      val c11: SInt[4] <> CONST         = -8
      val c12: SInt[49] <> CONST        = sd"49'-239794508230343"
      val c13: UInt[8] <> CONST         = ?
      val c14: SInt[8] <> CONST         = ?
      val c15: (Bits[3], Bit) <> CONST  = (all(0), 1)
      val c16: Bits[8] X 5 X 7 <> CONST = Vector.fill(7)(Vector.tabulate(5)(i => h"8'$i$i"))
    end Top
    val top = (new Top).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.sv"
         |
         |module Top;
         |  typedef struct packed {
         |    logic [2:0] _1;
         |    logic _2;
         |  } t_struct_DFTuple2;
         |  parameter logic c01 = 1'b0;
         |  parameter logic c02 = 1'b1;
         |  parameter logic c03 = 1'bx;
         |  parameter logic c04 = 0;
         |  parameter logic c05 = 1;
         |  parameter logic [7:0] c06 = 8'h22;
         |  parameter logic [6:0] c07 = 7'h22;
         |  parameter logic [2:0] c08 = 3'h5;
         |  parameter logic [2:0] c09 = 3'd7;
         |  parameter logic [47:0] c10 = 48'd239794508230343;
         |  parameter logic signed [3:0] c11 = -4'sd8;
         |  parameter logic signed [48:0] c12 = -49'sd239794508230343;
         |  parameter logic [7:0] c13 = 8'hxx;
         |  parameter logic signed [7:0] c14 = $signed(8'hxx);
         |  parameter t_struct_DFTuple2 c15 = '{3'h0, 1'b1};
         |  parameter logic [7:0] c16 [0:6] [0:4] = {{8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}, {8'h00, 8'h11, 8'h22, 8'h33, 8'h44}};
         |
         |endmodule
         |""".stripMargin
    )
  }

  test("Docstrings"):
    /** HasDocs has docs */
    class HasDocs extends DFDesign:
      /** My in */
      val x = Bit <> IN

      /** My Out
        */
      val y = Bit <> OUT

      /** My very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very very very very very very very very very
        * very very very very very very very very very very long doc
        */
      val z = Bit <> VAR

    val top = (new HasDocs).getCompiledCodeString
    assertNoDiff(
      top,
      """|/* HasDocs has docs */
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "HasDocs_defs.sv"
         |
         |module HasDocs(
         |  /* My in */
         |  input wire logic x,
         |  /* My Out
         |    */
         |  output logic y
         |);
         |  /* My very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very long doc
         |    */
         |  logic z;
         |
         |endmodule
         |""".stripMargin
    )
  test("Bits counter example"):
    class Counter(val width: Int <> CONST) extends RTDesign:
      val cnt = Bits(width) <> OUT init all(0)
      cnt := cnt.reg + 1
    val top = (new Counter(8)).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Counter_defs.sv"
         |
         |module Counter#(parameter int width = 8)(
         |  input wire logic clk,
         |  input wire logic rst,
         |  output logic [width - 1:0] cnt
         |);
         |  logic [width - 1:0] cnt_reg;
         |  always @(*)
         |  begin
         |    cnt = cnt_reg + width'(1);
         |  end
         |  always @(posedge clk)
         |  begin
         |    if (rst == 1'b1) cnt_reg <= {width{1'b0}};
         |    else cnt_reg <= cnt;
         |  end
         |endmodule
         |""".stripMargin
    )
  test("Various operations"):
    class Test(val width: Int <> CONST) extends RTDesign:
      val x = Bits(width)       <> OUT
      val y = Bit               <> OUT
      val z = UInt.until(width) <> OUT
      val w = UInt.to(width)    <> OUT
      x := b"${width}'11"
      x := h"${width}'3"
      x := b"11".resize(width)
      x := h"2'3".resize(width)
      y := x.&
      y := x.|
      y := x.^
      z := 0
      w := 0
    val top = (new Test(10)).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Test_defs.sv"
         |
         |module Test#(parameter int width = 10)(
         |  output logic [width - 1:0] x,
         |  output logic y,
         |  output logic [$clog2(width) - 1:0] z,
         |  output logic [$clog2(width + 1) - 1:0] w
         |);
         |  always @(*)
         |  begin
         |    x = {{(width-2){1'b0}}, 2'h3};
         |    x = {{(width-2){1'b0}}, 2'h3};
         |    x = {{(width-2){1'b0}}, 2'h3};
         |    x = {{(width-2){1'b0}}, 2'h3};
         |    y = &x;
         |    y = |x;
         |    y = ^x;
         |    z = $clog2(width)'(0);
         |    w = $clog2(width + 1)'(0);
         |  end
         |endmodule
         |""".stripMargin
    )
  test("UInt counter example"):
    class Counter(val width: Int <> CONST) extends RTDesign:
      val cnt = UInt(width) <> OUT init d"8'0"
      cnt := cnt.reg + 1
    val top = (new Counter(8)).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Counter_defs.sv"
         |
         |module Counter#(parameter int width = 8)(
         |  input wire logic clk,
         |  input wire logic rst,
         |  output logic [width - 1:0] cnt
         |);
         |  logic [width - 1:0] cnt_reg;
         |  always @(*)
         |  begin
         |    cnt = cnt_reg + width'(1);
         |  end
         |  always @(posedge clk)
         |  begin
         |    if (rst == 1'b1) cnt_reg <= width'(0);
         |    else cnt_reg <= cnt;
         |  end
         |endmodule
         |""".stripMargin
    )
  test("Blinker example"):

    /** This is a led blinker */
    class Blinker(
        val CLK_FREQ_KHz: Int <> CONST,
        val LED_FREQ_Hz:  Int <> CONST
    ) extends RTDesign:
      /** Half-count of the toggle for 50% duty cycle */
      val HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2)

      /** LED output */
      val led = Bit                     <> OUT.REG init 1
      val cnt = UInt.until(HALF_PERIOD) <> VAR.REG init 0
      if (cnt == HALF_PERIOD - 1)
        cnt.din := 0
        led.din := !led
      else cnt.din := cnt + 1
    end Blinker
    val top = (Blinker(50000, 1)).getCompiledCodeString
    assertNoDiff(
      top,
      """|/* This is a led blinker */
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "Blinker_defs.sv"
         |
         |module Blinker#(
         |    parameter int CLK_FREQ_KHz = 50000,
         |    parameter int LED_FREQ_Hz = 1
         |)(
         |  input wire logic clk,
         |  input wire logic rst,
         |  /* LED output */
         |  output logic led
         |);
         |  /* Half-count of the toggle for 50% duty cycle */
         |  parameter int HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
         |  logic [$clog2(HALF_PERIOD) - 1:0] cnt;
         |  logic led_din;
         |  logic [$clog2(HALF_PERIOD) - 1:0] cnt_din;
         |  always @(*)
         |  begin
         |    led_din = led;
         |    cnt_din = cnt;
         |    if (cnt == $clog2(HALF_PERIOD)'(HALF_PERIOD - 1)) begin
         |      cnt_din = $clog2(HALF_PERIOD)'(0);
         |      led_din = !led;
         |    end
         |    else cnt_din = cnt + $clog2(HALF_PERIOD)'(1);
         |  end
         |  always @(posedge clk)
         |  begin
         |    if (rst == 1'b1) begin
         |      led <= 1'b1;
         |      cnt <= $clog2(HALF_PERIOD)'(0);
         |    end
         |    else begin
         |      led <= led_din;
         |      cnt <= cnt_din;
         |    end
         |  end
         |endmodule
         |""".stripMargin
    )
end PrintVerilogCodeSpec
