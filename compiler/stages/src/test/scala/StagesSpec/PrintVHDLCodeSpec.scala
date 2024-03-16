package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.getCompiledCodeString
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}, {code = ":=="}]}

class PrintVHDLCodeSpec extends StageSpec:
  given options.CompilerOptions.Backend = backends.vhdl.v2008
  given options.PrinterOptions.Align    = false
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
    val id = (new ID).getCompiledCodeString
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
    val top = (new IDTop).getCompiledCodeString
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
         |    x => id1_x,
         |    y => id1_y
         |  );
         |  id2 : entity work.ID(ID_arch) port map (
         |    x => id2_x,
         |    y => id2_y
         |  );
         |  id1_x <= x;
         |  id2_x <= id1_y;
         |  y <= id2_y;
         |end IDTop_arch;
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
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.IDTop_pkg.all;
         |
         |entity ID is
         |generic (
         |  width : integer
         |);
         |port (
         |  x : in signed(width - 1 downto 0);
         |  y : out signed(width - 1 downto 0)
         |);
         |end ID;
         |
         |architecture ID_arch of ID is
         |begin
         |  process (all)
         |  begin
         |    y <= x;
         |  end process;
         |end ID_arch;
         |
         |library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.IDTop_pkg.all;
         |
         |entity IDTop is
         |generic (
         |  width : integer := 16
         |);
         |port (
         |  x : in signed(width - 1 downto 0);
         |  y : out signed(width - 1 downto 0)
         |);
         |end IDTop;
         |
         |architecture IDTop_arch of IDTop is
         |  signal id1_x : signed(width - 1 downto 0);
         |  signal id1_y : signed(width - 1 downto 0);
         |  signal id2_x : signed(width - 1 downto 0);
         |  signal id2_y : signed(width - 1 downto 0);
         |begin
         |  id1 : entity work.ID(ID_arch) generic map (
         |    width => width
         |  ) port map (
         |    x => id1_x,
         |    y => id1_y
         |  );
         |  id2 : entity work.ID(ID_arch) generic map (
         |    width => width
         |  ) port map (
         |    x => id2_x,
         |    y => id2_y
         |  );
         |  id1_x <= x;
         |  id2_x <= id1_y;
         |  y <= id2_y;
         |end IDTop_arch;
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
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Top_pkg.all;
         |
         |entity Top is
         |port (
         |  clk : in  std_logic;
         |  rst : in  std_logic;
         |  x   : in  std_logic_vector(15 downto 0);
         |  y   : out std_logic_vector(15 downto 0)
         |);
         |end Top;
         |
         |architecture Top_arch of Top is
         |  signal z          : std_logic_vector(15 downto 0);
         |begin
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
         |end Top_arch;
         |""".stripMargin
    )
  }
  test("literals") {
    class Top extends EDDesign:
      val c01: Bit <> CONST      = 0
      val c02: Bit <> CONST      = 1
      val c03: Bit <> CONST      = ?
      val c04: Boolean <> CONST  = false
      val c05: Boolean <> CONST  = true
      val c06: Bits[8] <> CONST  = h"22"
      val c07: Bits[7] <> CONST  = h"7'22"
      val c08: Bits[3] <> CONST  = b"101"
      val c09: UInt[3] <> CONST  = 7
      val c10: UInt[48] <> CONST = d"48'239794508230343"
      val c11: SInt[4] <> CONST  = -8
      val c12: SInt[49] <> CONST = sd"49'-239794508230343"
      val c13: UInt[8] <> CONST  = ?
      val c14: SInt[8] <> CONST  = ?
      // val c15: (Bits[3], Bit) <> CONST = (all(0), 1)
    end Top
    val top = (new Top).getCompiledCodeString
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
         |  constant c13 : unsigned(7 downto 0) := unsigned'(x"--");
         |  constant c14 : signed(7 downto 0) := signed'(x"--");
         |begin
         |
         |end Top_arch;
         |""".stripMargin
    )
  }
end PrintVHDLCodeSpec
