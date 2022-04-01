package StagesSpec

import DFiant.*
import DFiant.compiler.stages.vhdl.{printVHDLCode, getVHDLCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = "<="}]}

class PrintVHDLCodeSpec extends StageSpec:
  class ID extends RTDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    y <= x

  class IDTop extends RTDesign:
    self =>
    val x     = DFSInt(16) <> IN
    val y     = DFSInt(16) <> OUT
    val id1_x = DFSInt(16) <> VAR
    val id1_y = DFSInt(16) <> VAR
    val id2_x = DFSInt(16) <> VAR
    val id2_y = DFSInt(16) <> VAR
    val id1 = new ID:
      this.x <> id1_x
      this.y <> id1_y
    val id2 = new ID:
      this.x <> id2_x
      this.y <> id2_y
    id1_x <= x
    id2_x <= id1_y
    y     <= id2_y
  end IDTop

  test("Basic ID design") {
    val id = (new ID).printCodeString.getVHDLCode
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
         |  y <= x;
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
         |  y <= x;
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
    class Top extends RTDesign:
      val clk = DFBit      <> IN
      val rst = DFBit      <> IN
      val x   = DFBits(16) <> IN
      val y   = DFBits(16) <> OUT
      val z   = DFBits(16) <> VAR
      always(clk, rst) {
        val c = DFBits(16) const all(0)
        if (rst)
          y <= c
        else if (clk.rising)
          y <= x
      }
      val myblock = always.all {
        val my_var = DFBits(16) <> VAR
        my_var := x
        y      <= my_var
      }
      always() {
        z <= x
        y <= z
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
         |    constant c : std_logic_vector(15 downto 0) := x"0000";
         |  begin
         |    if rst then y <= c;
         |    elsif rising_edge(clk) then y <= x;
         |    end if;
         |  end process;
         |  myblock : process (all)
         |    variable my_var : std_logic_vector(15 downto 0);
         |  begin
         |    my_var := x;
         |    y <= my_var;
         |  end process;
         |  process
         |  begin
         |    z <= x;
         |    y <= z;
         |  end process;
         |end Top_arch;
         |""".stripMargin
    )
  }
  test("literals") {
    class Top extends RTDesign:
      val c01 = DFBit const 0
      val c02 = DFBit const 1
      val c03 = DFBit const ?
      val c04 = DFBool const 0
      val c05 = DFBool const 1
      val c06 = DFBits(8) const h"22"
      val c07 = DFBits(7) const h"7'22"
      val c08 = DFBits(3) const b"101"
      val c09 = DFUInt(3) const 7
      val c10 = DFUInt(48) const d"48'239794508230343"
      val c11 = DFSInt(4) const -8
      val c12 = DFSInt(49) const sd"49'-239794508230343"

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
end PrintVHDLCodeSpec
