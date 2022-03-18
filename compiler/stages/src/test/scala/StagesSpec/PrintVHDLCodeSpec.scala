package StagesSpec

import DFiant.*
import DFiant.compiler.stages.vhdl.{printVHDLCode, getVHDLCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintVHDLCodeSpec extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    y := x

  class IDTop(using DFC) extends DFDesign:
    val x   = DFSInt(16) <> IN
    val y   = DFSInt(16) <> OUT
    val id1 = new ID
    val id2 = new ID
    id1.x <> x
    id1.y <> id2.x
    id2.y <> y

  test("Basic ID design") {
    val id = (new ID).getVHDLCode
    assertNoDiff(
      id,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.ID_pkg.all;
         |
         |entity ID is
         |port (
         |  x : in signed(15 downto 0);
         |  y : out signed(15 downto 0)
         |);
         |end ID;
         |
         |architecture ID_arch of ID is
         |begin
         |  y := x;
         |end ID_arch;
         |""".stripMargin
    )
  }

  test("Basic hierarchy design") {
    val top = (new IDTop).getVHDLCode
    assertNoDiff(
      top,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.IDTop_pkg.all;
         |
         |entity ID is
         |port (
         |  x : in signed(15 downto 0);
         |  y : out signed(15 downto 0)
         |);
         |end ID;
         |
         |architecture ID_arch of ID is
         |begin
         |  y := x;
         |end ID_arch;
         |
         |library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.IDTop_pkg.all;
         |
         |entity IDTop is
         |port (
         |  x : in signed(15 downto 0);
         |  y : out signed(15 downto 0)
         |);
         |end IDTop;
         |
         |architecture IDTop_arch of IDTop is
         |  signal id1_x : signed(15 downto 0);
         |  signal id1_y : signed(15 downto 0);
         |  signal id2_x : signed(15 downto 0);
         |  signal id2_y : signed(15 downto 0);
         |begin
         |  id1 : entity work.ID(ID_arch) port map (
         |    x =>/*<--*/ id1_x,
         |    y =>/*-->*/ id1_y
         |  );
         |  id2 : entity work.ID(ID_arch) port map (
         |    x =>/*<--*/ id2_x,
         |    y =>/*-->*/ id2_y
         |  );
         |  id1_x <= x;
         |  id2_x <= id1_y;
         |  y <= id2_y;
         |end IDTop_arch;
         |""".stripMargin
    )
  }
  test("always block") {
    class Top(using DFC) extends DFDesign:
      val clk = DFBit      <> IN
      val rst = DFBit      <> IN
      val x   = DFBits(16) <> IN
      val y   = DFBits(16) <> OUT
      val z   = DFBits(16) <> VAR
      always(clk, rst) {
        val c = DFBits(16) const all(0)
        if (rst)
          y := c
        else if (clk.rising)
          y := x
      }
      val myblock = always.all {
        val my_var = DFBits(16) <> VAR
        my_var := x
        y      := my_var
      }
      always() {
        z := x
        y := z
      }
    end Top
    val top = (new Top).getVHDLCode
    assertNoDiff(
      top,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Top_pkg.all;
         |
         |entity Top is
         |port (
         |  clk : in std_logic;
         |  rst : in std_logic;
         |  x : in std_logic_vector(15 downto 0);
         |  y : out std_logic_vector(15 downto 0)
         |);
         |end Top;
         |
         |architecture Top_arch of Top is
         |  signal z : std_logic_vector(15 downto 0);
         |begin
         |  process (clk, rst)
         |    constant c : std_logic_vector(15 downto 0) := h"16'0000";
         |  begin
         |    if rst then y := c;
         |    elsif rising_edge(clk) then y := x;
         |    end if;
         |  end process;
         |  myblock : process (all)
         |    variable my_var : std_logic_vector(15 downto 0);
         |  begin
         |    my_var := x;
         |    y := my_var;
         |  end process;
         |  process
         |  begin
         |    z := x;
         |    y := z;
         |  end process;
         |end Top_arch;
         |""".stripMargin
    )
  }
end PrintVHDLCodeSpec
