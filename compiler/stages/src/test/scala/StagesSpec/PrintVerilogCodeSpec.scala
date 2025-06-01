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
    val id1   = new ID:
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
         |`include "ID_defs.svh"
         |
         |module ID(
         |  input  wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  `include "dfhdl_defs.svh"
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
         |`include "IDTop_defs.svh"
         |
         |module ID(
         |  input  wire logic signed [15:0] x,
         |  output logic signed [15:0] y,
         |  output logic signed [15:0] y2
         |);
         |  `include "dfhdl_defs.svh"
         |  assign y = x;
         |  assign y2 = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.svh"
         |
         |module IDTop(
         |  input  wire logic signed [15:0] x,
         |  output logic signed [15:0] y
         |);
         |  `include "dfhdl_defs.svh"
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
    class ID(val width: Int <> CONST = 7) extends DFDesign:
      val x = SInt(width) <> IN
      val y = SInt(width) <> OUT
      y := x

    class IDTop(val width: Int <> CONST = 9) extends DFDesign:
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
         |`include "IDTop_defs.svh"
         |
         |module ID#(parameter int width = 7)(
         |  input  wire logic signed [width - 1:0] x,
         |  output logic signed [width - 1:0] y
         |);
         |  `include "dfhdl_defs.svh"
         |  assign y = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.svh"
         |
         |module IDTop#(parameter int width = 16)(
         |  input  wire logic signed [width - 1:0] x,
         |  output logic signed [width - 1:0] y
         |);
         |  `include "dfhdl_defs.svh"
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

  test("Basic hierarchy design with parameters verilog.v95") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    class ID(val width: Int <> CONST = 7) extends DFDesign:
      val x = SInt(width) <> IN
      val y = SInt(width) <> OUT
      y := x

    class IDTop(val width: Int <> CONST = 9) extends DFDesign:
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
         |`include "IDTop_defs.vh"
         |
         |module ID(
         |  x,
         |  y
         |);
         |  `include "dfhdl_defs.vh"
         |  `include "IDTop_defs.vh"
         |  parameter integer width = 7;
         |  input  wire  [width - 1:0] x;
         |  output wire [width - 1:0] y;
         |  assign y = x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.vh"
         |
         |module IDTop(
         |  x,
         |  y
         |);
         |  `include "dfhdl_defs.vh"
         |  `include "IDTop_defs.vh"
         |  parameter integer width = 16;
         |  input  wire  [width - 1:0] x;
         |  output wire [width - 1:0] y;
         |  wire [width - 1:0] id1_x;
         |  wire [width - 1:0] id1_y;
         |  wire [width - 1:0] id2_x;
         |  wire [width - 1:0] id2_y;
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
         |`include "ParamTest_defs.svh"
         |
         |module ParamTest#(parameter logic dp = 1'b1)(
         |  input  wire logic x,
         |  output logic y
         |);
         |  `include "dfhdl_defs.svh"
         |  parameter logic lp = 1'b1;
         |  assign y = ((x | gp) | dp) | lp;
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
      process {
        z :== x
        y :== z
      }
    end Top
    val top = (new Top).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.svh"
         |
         |module Top(
         |  input  wire logic        clk,
         |  input  wire logic        rst,
         |  input  wire logic [15:0] x,
         |  output      logic [15:0] y
         |);
         |  `include "dfhdl_defs.svh"
         |  parameter logic [15:0] c = 16'h0000;
         |  logic [15:0] z;
         |  logic [15:0] my_var;
         |  always_ff @(posedge clk, posedge rst)
         |  begin
         |    if (rst) y <= c;
         |    else y <= x;
         |  end
         |  myblock : always_comb
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
      val c17: Double <> CONST          = 3.14159
      val c18: Double <> CONST          = -2.71828
      val c19: String <> CONST          = "My\nName\rIs\t\"Earl\""
    end Top
    val top = (new Top).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Top_defs.svh"
         |
         |module Top;
         |  `include "dfhdl_defs.svh"
         |  typedef struct packed {
         |    logic [2:0] _1;
         |    logic _2;
         |  } t_struct_DFTuple2;
         |  parameter logic c01 = 1'b0;
         |  parameter logic c02 = 1'b1;
         |  parameter logic c03 = 1'b?;
         |  parameter logic c04 = 0;
         |  parameter logic c05 = 1;
         |  parameter logic [7:0] c06 = 8'h22;
         |  parameter logic [6:0] c07 = 7'h22;
         |  parameter logic [2:0] c08 = 3'h5;
         |  parameter logic [2:0] c09 = 3'd7;
         |  parameter logic [47:0] c10 = 48'd239794508230343;
         |  parameter logic signed [3:0] c11 = -4'sd8;
         |  parameter logic signed [48:0] c12 = -49'sd239794508230343;
         |  parameter logic [7:0] c13 = 8'h??;
         |  parameter logic signed [7:0] c14 = $signed(8'h??);
         |  parameter t_struct_DFTuple2 c15 = '{3'h0, 1'b1};
         |  parameter logic [7:0] c16 [0:6] [0:4] = '{
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44},
         |    '{8'h00, 8'h11, 8'h22, 8'h33, 8'h44}
         |  };
         |  parameter real c17 = 3.14159;
         |  parameter real c18 = -2.71828;
         |  parameter string c19 = "My\nName\rIs\t\"Earl\"";
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
      z <> x
      y <> z
    end HasDocs

    val top = (new HasDocs).getCompiledCodeString
    assertNoDiff(
      top,
      """|/* HasDocs has docs */
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "HasDocs_defs.svh"
         |
         |module HasDocs(
         |  /* My in */
         |  input  wire logic x,
         |  /* My Out
         |    */
         |  output logic y
         |);
         |  `include "dfhdl_defs.svh"
         |  /* My very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very very very very very very very very very
         |     very very very very very very very very very very long doc
         |    */
         |  logic z;
         |  assign z = x;
         |  assign y = z;
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
         |`include "Counter_defs.svh"
         |
         |module Counter#(parameter int width = 8)(
         |  input  wire logic clk,
         |  input  wire logic rst,
         |  output logic [width - 1:0] cnt
         |);
         |  `include "dfhdl_defs.svh"
         |  logic [width - 1:0] cnt_reg;
         |  always_ff @(posedge clk)
         |  begin
         |    if (rst == 1'b1) cnt_reg <= {width{1'b0}};
         |    else cnt_reg <= cnt;
         |  end
         |  assign cnt = cnt_reg + width'(1);
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
         |`include "Test_defs.svh"
         |
         |module Test#(parameter int width = 10)(
         |  output logic [width - 1:0] x,
         |  output logic y,
         |  output logic [$clog2(width) - 1:0] z,
         |  output logic [$clog2(width + 1) - 1:0] w
         |);
         |  `include "dfhdl_defs.svh"
         |  always_comb
         |  begin
         |    x = `TO_VEC_HEX(3, 2, width);
         |    x = `TO_VEC_HEX(3, 2, width);
         |    x = `TO_VEC_HEX(3, 2, width);
         |    x = `TO_VEC_HEX(3, 2, width);
         |    y = &x;
         |    y = |x;
         |    y = ^x;
         |  end
         |  assign z = $clog2(width)'(0);
         |  assign w = $clog2(width + 1)'(0);
         |endmodule
         |""".stripMargin
    )
  test("UInt counter example"):
    class Counter(val width: Int <> CONST) extends RTDesign:
      val cnt = UInt(width) <> OUT init 0
      cnt := cnt.reg + 1
    val top = (new Counter(8)).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Counter_defs.svh"
         |
         |module Counter#(parameter int width = 8)(
         |  input  wire logic clk,
         |  input  wire logic rst,
         |  output logic [width - 1:0] cnt
         |);
         |  `include "dfhdl_defs.svh"
         |  logic [width - 1:0] cnt_reg;
         |  always_ff @(posedge clk)
         |  begin
         |    if (rst == 1'b1) cnt_reg <= width'(0);
         |    else cnt_reg <= cnt;
         |  end
         |  assign cnt = cnt_reg + width'(1);
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
         |`include "Blinker_defs.svh"
         |
         |module Blinker#(
         |    parameter int CLK_FREQ_KHz = 50000,
         |    parameter int LED_FREQ_Hz = 1
         |)(
         |  input  wire logic clk,
         |  input  wire logic rst,
         |  /* LED output */
         |  output logic led
         |);
         |  `include "dfhdl_defs.svh"
         |  /* Half-count of the toggle for 50% duty cycle */
         |  parameter int HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
         |  logic [$clog2(HALF_PERIOD) - 1:0] cnt;
         |  always_ff @(posedge clk)
         |  begin
         |    if (rst == 1'b1) begin
         |      led <= 1'b1;
         |      cnt <= $clog2(HALF_PERIOD)'(0);
         |    end
         |    else begin
         |      if (cnt == $clog2(HALF_PERIOD)'(HALF_PERIOD - 1)) begin
         |        cnt <= $clog2(HALF_PERIOD)'(0);
         |        led <= ~led;
         |      end
         |      else cnt <= cnt + $clog2(HALF_PERIOD)'(1);
         |    end
         |  end
         |endmodule
         |""".stripMargin
    )

  test("a single register with only init") {
    class IDTop extends RTDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT.REG init 0

    val top = (new IDTop).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.svh"
         |
         |module IDTop(
         |  input  wire logic clk,
         |  input  wire logic rst,
         |  input  wire logic signed [15:0] x,
         |  output logic signed [15:0] y
         |);
         |  `include "dfhdl_defs.svh"
         |  always_ff @(posedge clk)
         |  begin
         |    if (rst == 1'b1) y <= 16'sd0;
         |    else begin end
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("Boolean selection operation") {
    class SelOp extends DFDesign:
      val c  = Boolean <> IN
      val x1 = Bits(8) <> IN
      val x2 = Bits(8) <> IN
      val y1 = Bits(8) <> OUT
      val cp:  Boolean <> CONST = true
      val up1: UInt[8] <> CONST = 11
      val up2: UInt[8] <> CONST = 22
      val up3: UInt[8] <> CONST = cp.sel(up1, up2)
      y1 := c.sel(x1, x2)
      y1 := c.sel(x1, all(0))
      y1 := c.sel(all(0), x2)
    val id = (new SelOp).getCompiledCodeString
    assertNoDiff(
      id,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "SelOp_defs.svh"
         |
         |module SelOp(
         |  input  wire logic c,
         |  input  wire logic [7:0] x1,
         |  input  wire logic [7:0] x2,
         |  output logic [7:0] y1
         |);
         |  `include "dfhdl_defs.svh"
         |  parameter logic cp = 1;
         |  parameter logic [7:0] up1 = 8'd11;
         |  parameter logic [7:0] up2 = 8'd22;
         |  parameter logic [7:0] up3 = cp ? up1 : up2;
         |  always_comb
         |  begin
         |    y1 = c ? x1 : x2;
         |    y1 = c ? x1 : 8'h00;
         |    y1 = c ? 8'h00 : x2;
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("Empty design") {
    class Empty extends DFDesign
    val top = (new Empty).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Empty_defs.svh"
         |
         |module Empty;
         |  `include "dfhdl_defs.svh"
         |
         |endmodule
         |""".stripMargin
    )
  }

  test("HighZ assignment") {
    class HighZ extends RTDesign:
      val x = Bits(8) <> IN
      val y = Bits(8) <> OUT
      if (x.|) y := x
      else y     := NOTHING
    val top = (new HighZ).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "HighZ_defs.svh"
         |
         |module HighZ(
         |  input  wire logic [7:0] x,
         |  output logic [7:0] y
         |);
         |  `include "dfhdl_defs.svh"
         |  always_comb
         |  begin
         |    if (|x) y = x;
         |    else y = 8'bz;
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("Wildcards and don't cares") {
    class Foo extends RTDesign:
      val num = 16
      val x   = Bits(num) <> IN init all(0)
      val y   = Bits(num) <> OUT
      x match
        case h"12??" | h"345?" => y := h"22??"
        case _                 => y := all(1)
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.svh"
         |
         |module Foo(
         |  input  wire logic [15:0] x,
         |  output logic [15:0] y
         |);
         |  `include "dfhdl_defs.svh"
         |  always_comb
         |  begin
         |    case (x) inside
         |      16'h12??, 16'h345?: y = 16'h22??;
         |      default: y = 16'hffff;
         |    endcase
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("Wildcards and don't cares under verilog.v95") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    class Foo extends RTDesign:
      val num = 16
      val x   = Bits(num) <> IN init all(0)
      val y   = Bits(num) <> OUT
      x match
        case h"12??" | h"345?" => y := h"22??"
        case _                 => y := all(1)
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.vh"
         |
         |module Foo(
         |  x,
         |  y
         |);
         |  `include "dfhdl_defs.vh"
         |  `include "Foo_defs.vh"
         |  input  wire  [15:0] x;
         |  output reg [15:0] y;
         |  always @(x)
         |  begin
         |    if ((x[15:8] == 8'h12) | (x[15:4] == 12'h345)) y = 16'h22??;
         |    else y = 16'hffff;
         |  end
         |endmodule
         |""".stripMargin
    )
  }

  test("Global parameters under verilog.v95") {
    given options.CompilerOptions.Backend = backends.verilog.v95
    val width:  Int <> CONST = 8
    val length: Int <> CONST = 10
    class Foo(
        val width5:  Int <> CONST = 8,
        val length5: Int <> CONST = 10
    ) extends RTDesign:
      val x1 = Bits(width) X length <> IN
      val y1 = Bits(width) X length <> OUT
      y1 <> x1
      val x2 = Bits(width) X (length + 1) <> IN
      val y2 = Bits(width) X (length + 1) <> OUT
      y2 <> x2
      val x3 = Bits(width) X 7 <> IN
      val y3 = Bits(width) X 7 <> OUT
      y3 <> x3
      val x4 = Bits(width) X 7 X length <> IN
      val y4 = Bits(width) X 7 X length <> OUT
      y4 <> x4
      val x5 = Bits(width5) X 7 X length5 <> IN
      val y5 = Bits(width5) X 7 X length5 <> OUT
      y5 <> x5
    end Foo
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`define width 8
         |`define length 10
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.vh"
         |
         |module Foo(
         |  x1,
         |  y1,
         |  x2,
         |  y2,
         |  x3,
         |  y3,
         |  x4,
         |  y4,
         |  x5,
         |  y5
         |);
         |  `include "dfhdl_defs.vh"
         |  `include "Foo_defs.vh"
         |  parameter integer width5 = 8;
         |  parameter integer length5 = 10;
         |  input  wire  [`width - 1:0] x1 [0:`length - 1];
         |  output wire [`width - 1:0] y1 [0:`length - 1];
         |  input  wire  [`width - 1:0] x2 [0:`length + 1 - 1];
         |  output wire [`width - 1:0] y2 [0:`length + 1 - 1];
         |  input  wire  [`width - 1:0] x3 [0:6];
         |  output wire [`width - 1:0] y3 [0:6];
         |  input  wire  [`width - 1:0] x4 [0:`length - 1] [0:6];
         |  output wire [`width - 1:0] y4 [0:`length - 1] [0:6];
         |  input  wire  [width5 - 1:0] x5 [0:length5 - 1] [0:6];
         |  output wire [width5 - 1:0] y5 [0:length5 - 1] [0:6];
         |  assign y1 = x1;
         |  assign y2 = x2;
         |  assign y3 = x3;
         |  assign y4 = x4;
         |  assign y5 = x5;
         |endmodule
         |""".stripMargin
    )
  }
  test("wait statements") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      val i = Bit <> IN
      process:
        x :== 1
        waitWhile(i)
        50.ms.wait
        x :== 0
        waitUntil(i.rising)
        50.us.wait
        x :== 1
        waitUntil(i)
        50.ns.wait
        x :== 0
        1.ns.wait
    end Foo
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.svh"
         |
         |module Foo(
         |  output logic x,
         |  input  wire logic i
         |);
         |  `include "dfhdl_defs.svh"
         |  always
         |  begin
         |    x <= 1'b1;
         |    wait(i);
         |    #50ms;
         |    x <= 1'b0;
         |    @(posedge i);
         |    #50us;
         |    x <= 1'b1;
         |    wait(~i);
         |    #50ns;
         |    x <= 1'b0;
         |    #1ns;
         |  end
         |endmodule""".stripMargin
    )
  }
  test("for loop printing") {
    class Foo extends EDDesign:
      val matrix = Bits(10) X 8 X 8 <> OUT
      process:
        for (
          i <- 0 until 8;
          if i % 2 == 0;
          j <- 0 until 8;
          if j % 2 == 0;
          k <- 0 until 10
          if k % 2 == 0
        ) matrix(i)(j)(k) :== 1
        for (
          i <- 0 until 8;
          if i % 2 == 1;
          j <- 0 until 8;
          if j % 2 == 1;
          k <- 0 until 10
          if k % 2 == 1
        ) matrix(i)(j)(k) :== 0
        10.ns.wait
    end Foo
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.svh"
         |
         |module Foo(
         |  output logic [9:0] matrix [0:7] [0:7]
         |);
         |  `include "dfhdl_defs.svh"
         |
         |  always
         |  begin
         |    for (int i = 0; i < 8; i = i + 1) begin
         |      if ((i % 2) == 0) begin
         |        for (int j = 0; j < 8; j = j + 1) begin
         |          if ((j % 2) == 0) begin
         |            for (int k = 0; k < 10; k = k + 1) begin
         |              if ((k % 2) == 0) matrix[i][j][k] <= 1'b1;
         |            end
         |          end
         |        end
         |      end
         |    end
         |    for (int i = 0; i < 8; i = i + 1) begin
         |      if ((i % 2) == 1) begin
         |        for (int j = 0; j < 8; j = j + 1) begin
         |          if ((j % 2) == 1) begin
         |            for (int k = 0; k < 10; k = k + 1) begin
         |              if ((k % 2) == 1) matrix[i][j][k] <= 1'b0;
         |            end
         |          end
         |        end
         |      end
         |    end
         |    #10ns;
         |  end
         |endmodule""".stripMargin
    )
  }
  test("for loop printing verilog.v2001") {
    given options.CompilerOptions.Backend = backends.verilog.v2001
    class Foo extends EDDesign:
      val matrix = Bits(10) X 8 X 8 <> OUT
      process:
        for (
          i <- 0 until 8;
          if i % 2 == 0;
          j <- 0 until 8;
          if j % 2 == 0;
          k <- 0 until 10
          if k % 2 == 0
        ) matrix(i)(j)(k) :== 1
        for (
          i <- 0 until 8;
          if i % 2 == 1;
          j <- 0 until 8;
          if j % 2 == 1;
          k <- 0 until 10
          if k % 2 == 1
        ) matrix(i)(j)(k) :== 0
        10.ns.wait
    end Foo
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.vh"
         |
         |module Foo(
         |  output reg [9:0] matrix [0:7] [0:7]
         |);
         |  `include "dfhdl_defs.vh"
         |  `include "Foo_defs.vh"
         |
         |  always
         |  begin
         |    integer i;
         |    integer j;
         |    integer k;
         |    for (i = 0; i < 8; i = i + 1) begin
         |      if ((i % 2) == 0) begin
         |        for (j = 0; j < 8; j = j + 1) begin
         |          if ((j % 2) == 0) begin
         |            for (k = 0; k < 10; k = k + 1) begin
         |              if ((k % 2) == 0) matrix[i][j][k] <= 1'b1;
         |            end
         |          end
         |        end
         |      end
         |    end
         |    for (i = 0; i < 8; i = i + 1) begin
         |      if ((i % 2) == 1) begin
         |        for (j = 0; j < 8; j = j + 1) begin
         |          if ((j % 2) == 1) begin
         |            for (k = 0; k < 10; k = k + 1) begin
         |              if ((k % 2) == 1) matrix[i][j][k] <= 1'b0;
         |            end
         |          end
         |        end
         |      end
         |    end
         |    #10ns;
         |  end
         |endmodule""".stripMargin
    )
  }
  test("while loop printing") {
    class Foo extends EDDesign:
      val x = Bit <> OUT
      val b = Bit <> IN
      process:
        while (b)
          x :== b
          5.ns.wait
        while (true)
          x :== !b
          5.ns.wait
    end Foo
    val top = (new Foo).getCompiledCodeString
    assertNoDiff(
      top,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.svh"
         |
         |module Foo(
         |  output logic x,
         |  input  wire logic b
         |);
         |  `include "dfhdl_defs.svh"
         |  always
         |  begin
         |    while (b) begin
         |      x <= b;
         |      #5ns;
         |    end
         |    while (1) begin
         |      x <= ~b;
         |      #5ns;
         |    end
         |  end
         |endmodule""".stripMargin
    )
  }
  test("text out printing") {
    class Foo(val param: String <> CONST = "Hello\n..\"World\"!") extends EDDesign:
      val bar    = param + "!"
      val param2 = param + param
      val param3: Int <> CONST = 42
      val param4 = d"22"
      val param5 = h"abc123"
      val param6 = b"101010"
      val param7 = d"-11"
      val param8: Bit <> CONST     = 1
      val param9: Boolean <> CONST = false
      enum MyEnum extends Encoded:
        case A, B, C
      val param10: MyEnum <> CONST = MyEnum.A

      process(all):
        assert(param == "hello2")
        report(param, Severity.Warning)
        assert(param == "hello2", s"I am the one ${param} who knocks")
        assert(param8, s"I\\am\nthe \"one\"(!)\n${param}\nwho\nknocks", Severity.Fatal)
        println(bar)
        println()
        print(s"I am the one ${param2} who knocks")
        print("hello")
        println(
          s"These are the values: $param3, $param4, $param5, $param6, $param7, $param8, $param9, $param10"
        )
        debug(param3, param4, param5, param6, param7, param8, param9, param10)
    end Foo
    object sv2005:
      given options.CompilerOptions.Backend = backends.verilog.sv2005
      val csTop                             = (new Foo).getCompiledCodeString
    object v95:
      given options.CompilerOptions.Backend = backends.verilog.v95
      val csTop                             = (new Foo).getCompiledCodeString
    assertNoDiff(
      sv2005.csTop,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.svh"
         |
         |module Foo#(parameter string param = "Hello\n..\"World\"!");
         |  `include "dfhdl_defs.svh"
         |  parameter int param3 = 42;
         |  typedef enum {
         |    MyEnum_A = 0,
         |    MyEnum_B = 1,
         |    MyEnum_C = 2
         |  } t_enum_MyEnum;
         |  parameter string bar = {param, "!"};
         |  parameter string param2 = {2{param}};
         |  parameter logic [4:0] param4 = 5'd22;
         |  parameter logic [23:0] param5 = 24'habc123;
         |  parameter logic [5:0] param6 = 6'h2a;
         |  parameter logic signed [4:0] param7 = -5'sd11;
         |  parameter logic param8 = 1'b1;
         |  parameter logic param9 = 0;
         |  parameter t_enum_MyEnum param10 = MyEnum_A;
         |  always_comb
         |  begin
         |    assert (param == "hello2");
         |    $warning("%s", param, "");
         |    assert (param == "hello2")
         |    else $error("I am the one %s", param, " who knocks");
         |    assert (param8)
         |    else $fatal(
         |      "I\\am\n",
         |      "the \"one\"(!)%s", param, "\n",
         |      "who\n",
         |      "knocks"
         |    );
         |    $display("%s", bar, "");
         |    $display();
         |    $write("I am the one %s", param2, " who knocks");
         |    $write("hello");
         |    $display("These are the values: %d", param3, ", %d", param4, ", %h", param5, ", %h", param6, ", %d", param7, ", %b", param8, ", %s", param9 ? "true" : "false", ", %s", param10.name(), "");
         |    $info(
         |      "Debug at Foo\n",
         |      "compiler/stages/src/test/scala/StagesSpec/PrintVerilogCodeSpec.scala:1089:9\n",
         |      "param3 = %d\n", param3,
         |      "param4 = %d\n", param4,
         |      "param5 = %h\n", param5,
         |      "param6 = %h\n", param6,
         |      "param7 = %d\n", param7,
         |      "param8 = %b\n", param8,
         |      "param9 = %s\n", param9 ? "true" : "false",
         |      "param10 = %s", param10.name()
         |    );
         |  end
         |endmodule""".stripMargin
    )
    assertNoDiff(
      v95.csTop,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "Foo_defs.vh"
         |
         |module Foo;
         |  `include "dfhdl_defs.vh"
         |  `include "Foo_defs.vh"
         |  parameter param = "Hello\n..\"World\"!";
         |  parameter integer param3 = 42;
         |  `define MyEnum_A 0
         |  `define MyEnum_B 1
         |  `define MyEnum_C 2
         |  function [8*8:1] MyEnum_to_string;
         |    input [1:0] value;
         |    case (value)
         |      `MyEnum_A: MyEnum_to_string = "MyEnum_A";
         |      `MyEnum_B: MyEnum_to_string = "MyEnum_B";
         |      `MyEnum_C: MyEnum_to_string = "MyEnum_C";
         |      default: MyEnum_to_string = "?";
         |    endcase
         |  endfunction
         |  parameter bar = {param, "!"};
         |  parameter param2 = {2{param}};
         |  parameter [4:0] param4 = 5'd22;
         |  parameter [23:0] param5 = 24'habc123;
         |  parameter [5:0] param6 = 6'h2a;
         |  parameter [4:0] param7 = -5'sd11;
         |  parameter param8 = 1'b1;
         |  parameter param9 = 0;
         |  parameter [1:0] param10 = `MyEnum_A;
         |  always
         |  begin
         |    if (!(param == "hello2")) begin
         |      $display("ERROR: ", "Assertion failed!");
         |    end
         |    $display("WARNING: ", "%s", param, "");
         |    if (!(param == "hello2")) begin
         |      $display("ERROR: ", "I am the one %s", param, " who knocks");
         |    end
         |    if (!(param8)) begin
         |      $display("FATAL: ", 
         |        "I\\am\n",
         |        "the \"one\"(!)%s", param, "\n",
         |        "who\n",
         |        "knocks"
         |      );
         |      $finish;
         |    end
         |    $display("%s", bar, "");
         |    $display();
         |    $write("I am the one %s", param2, " who knocks");
         |    $write("hello");
         |    $display("These are the values: %d", param3, ", %d", param4, ", %h", param5, ", %h", param6, ", %d", param7, ", %b", param8, ", %s", param9 ? "true" : "false", ", %s", MyEnum_to_string(param10), "");
         |    $display("INFO: ", 
         |      "Debug at Foo\n",
         |      "compiler/stages/src/test/scala/StagesSpec/PrintVerilogCodeSpec.scala:1089:9\n",
         |      "param3 = %d\n", param3,
         |      "param4 = %d\n", param4,
         |      "param5 = %h\n", param5,
         |      "param6 = %h\n", param6,
         |      "param7 = %d\n", param7,
         |      "param8 = %b\n", param8,
         |      "param9 = %s\n", param9 ? "true" : "false",
         |      "param10 = %s", MyEnum_to_string(param10)
         |    );
         |  end
         |endmodule""".stripMargin
    )
  }
end PrintVerilogCodeSpec
