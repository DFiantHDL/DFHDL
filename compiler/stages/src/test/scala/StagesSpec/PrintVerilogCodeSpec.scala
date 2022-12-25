package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.verilog.{printVerilogCode, getVerilogCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = ":=="}]}

class PrintVerilogCodeSpec extends StageSpec:
  class ID extends EDDesign:
    val x = SInt(16) <> IN
    val y = SInt(16) <> OUT
    y :== x

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
    id1_x :== x
    id2_x :== id1_y
    y     :== id2_y
  end IDTop

  test("Basic ID design") {
    val id = (new ID).getVerilogCode
    assertNoDiff(
      id,
      """|`default_nettype none
         |`timescale 1ns/1ps
         |`include "ID_defs.v"
         |
         |module ID(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  y <= x;
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
         |`include "IDTop_defs.v"
         |
         |module ID(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  y <= x;
         |endmodule
         |
         |`default_nettype none
         |`timescale 1ns/1ps
         |`include "IDTop_defs.v"
         |
         |module IDTop(
         |  input wire signed [15:0] x,
         |  output reg signed [15:0] y
         |);
         |  wire signed [15:0] id1_x;
         |  wire signed [15:0] id1_y;
         |  wire signed [15:0] id2_x;
         |  wire signed [15:0] id2_y;
         |  ID id1(
         |    .x /*<--*/ (id1_x),
         |    .y /*-->*/ (id1_y)
         |  );
         |  ID id2(
         |    .x /*<--*/ (id2_x),
         |    .y /*-->*/ (id2_y)
         |  );
         |  id1_x <= x;
         |  id2_x <= id1_y;
         |  y <= id2_y;
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
         |`include "Top_defs.v"
         |
         |module Top(
         |  input wire        clk,
         |  input wire        rst,
         |  input wire [15:0] x,
         |  output reg [15:0] y
         |);
         |  wire [15:0] z;
         |  process (clk, rst)
         |    constant c      : std_logic_vector(15 downto 0) := x"0000";
         |  begin
         |    if rst then y <= c;
         |    elsif rising_edge(clk) then y <= x;
         |    end if;
         |  end process;
         |  myblock : process (all)
         |    variable my_var : std_logic_vector(15 downto 0);
         |  begin
         |    my_var := x;
         |    y      <= my_var;
         |  end process;
         |  process
         |  begin
         |    z      <= x;
         |    y      <= z;
         |  end process;
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
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Top_pkg.all;
         |
         |entity Top is
         |end Top;
         |
         |architecture Top_arch of Top is
         |  constant c01 : std_logic := '0';
         |  constant c02 : std_logic := '1';
         |  constant c03 : std_logic := '-';
         |  constant c04 : boolean := false;
         |  constant c05 : boolean := true;
         |  constant c06 : std_logic_vector(7 downto 0) := x"22";
         |  constant c07 : std_logic_vector(6 downto 0) := 7x"22";
         |  constant c08 : std_logic_vector(2 downto 0) := "101";
         |  constant c09 : unsigned(2 downto 0) := 3d"7";
         |  constant c10 : unsigned(47 downto 0) := 48d"239794508230343";
         |  constant c11 : signed(3 downto 0) := -4d"8";
         |  constant c12 : signed(48 downto 0) := -49d"239794508230343";
         |begin
         |
         |end Top_arch;
         |""".stripMargin
    )
  }
end PrintVerilogCodeSpec
