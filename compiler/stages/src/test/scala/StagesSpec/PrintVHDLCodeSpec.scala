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
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Top_pkg.all;
         |
         |entity Top is
         |end Top;
         |
         |architecture Top_arch of Top is
         |  type t_struct_DFTuple2 is record
         |    _1 : std_logic_vector(2 downto 0);
         |    _2 : std_logic;
         |  end record;
         |  type t_vecX1_std_logic_vector is array (natural range <>) of std_logic_vector;
         |  type t_vecX2_std_logic_vector is array (natural range <>) of t_vecX1_std_logic_vector;
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
         |  constant c15 : t_struct_DFTuple2 := t_struct_DFTuple2(_1 = "000", _2 = '1');
         |  constant c16 : t_vecX2_std_logic_vector(0 to 6)(0 to 4)(7 downto 0) := (
         |    0 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    1 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    2 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    3 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    4 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    5 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44"),
         |    6 => (0 => x"00", 1 => x"11", 2 => x"22", 3 => x"33", 4 => x"44")
         |  );
         |begin
         |
         |end Top_arch;
         |""".stripMargin
    )
  }
  test("Blinker example") {

    /** This is a led blinker */
    class Blinker(
        val CLK_FREQ_KHz: Int <> CONST,
        val LED_FREQ_Hz: Int <> CONST
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
      """|-- This is a led blinker 
         |library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Blinker_pkg.all;
         |
         |entity Blinker is
         |generic (
         |  CLK_FREQ_KHz : integer := 50000;
         |  LED_FREQ_Hz : integer := 1
         |);
         |port (
         |  clk : in std_logic;
         |  rst : in std_logic;
         |  -- LED output 
         |  led : out std_logic
         |);
         |end Blinker;
         |
         |architecture Blinker_arch of Blinker is
         |  -- Half-count of the toggle for 50% duty cycle 
         |  constant HALF_PERIOD : integer := (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2);
         |  signal cnt : unsigned(clog2(HALF_PERIOD) - 1 downto 0);
         |  signal led_din : std_logic;
         |  signal cnt_din : unsigned(clog2(HALF_PERIOD) - 1 downto 0);
         |begin
         |  process (all)
         |  begin
         |    led_din <= led;
         |    cnt_din <= cnt;
         |    if cnt = to_unsigned(HALF_PERIOD - 1, clog2(HALF_PERIOD)) then
         |      cnt_din <= resize(d"0", clog2(HALF_PERIOD));
         |      led_din <= not led;
         |    else cnt_din <= cnt + resize(d"1", clog2(HALF_PERIOD));
         |    end if;
         |  end process;
         |  process (clk)
         |  begin
         |    if rising_edge(clk) then
         |      if rst = '1' then
         |        led <= '1';
         |        cnt <= resize(d"0", clog2(HALF_PERIOD));
         |      else
         |        led <= led_din;
         |        cnt <= cnt_din;
         |      end if;
         |    end if;
         |  end process;
         |end Blinker_arch;""".stripMargin
    )
  }
  test("Opaque and vector local example") {
    case class Foo() extends Opaque(Bits(12) X 16 X 10)
    class Example() extends RTDesign:
      val x = Bits(1920) <> IN
      val v = Foo        <> VAR.REG init all(all(all(0))).as(Foo)
      val y = Bits(1920) <> OUT
      v.din := x.as(Foo)
      y     := v.bits

    val top = (Example()).getCompiledCodeString
    assertNoDiff(
      top,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Example_pkg.all;
         |
         |entity Example is
         |port (
         |  clk : in std_logic;
         |  rst : in std_logic;
         |  x : in std_logic_vector(1919 downto 0);
         |  y : out std_logic_vector(1919 downto 0)
         |);
         |end Example;
         |
         |architecture Example_arch of Example is
         |  type t_vecX1_std_logic_vector is array (natural range <>) of std_logic_vector;
         |  function bitWidth(A : t_vecX1_std_logic_vector) return integer is
         |  begin
         |    return A'length * bitWidth(A(0));
         |  end;
         |  function to_slv(A : t_vecX1_std_logic_vector) return std_logic_vector is
         |    variable hi : integer;
         |    variable lo : integer;
         |    variable cellBitWidth: integer;
         |    variable ret : std_logic_vector(bitWidth(A) - 1 downto 0);
         |  begin
         |    cellBitWidth := bitWidth(A(0));
         |    lo := bitWidth(A);
         |    for i in 0 to A'length-1 loop
         |      hi := lo - 1; lo := hi - cellBitWidth + 1;
         |      ret(hi downto lo) := A(i);
         |    end loop;
         |    return ret;
         |  end;
         |  function to_t_vecX1_std_logic_vector(A : std_logic_vector; D1 : integer; D0 : integer) return t_vecX1_std_logic_vector is
         |    variable hi : integer;
         |    variable lo : integer;
         |    variable cellBitWidth: integer := D0;
         |    variable ret : t_vecX1_std_logic_vector(0 to D1 - 1)(D0 - 1 downto 0);
         |  begin
         |    lo := A'length;
         |    for i in 0 to D1-1 loop
         |      hi := lo - 1; lo := hi - cellBitWidth + 1;
         |      ret(i) := A(hi downto lo);
         |    end loop;
         |    return ret;
         |  end;
         |  type t_vecX2_std_logic_vector is array (natural range <>) of t_vecX1_std_logic_vector;
         |  function bitWidth(A : t_vecX2_std_logic_vector) return integer is
         |  begin
         |    return A'length * bitWidth(A(0));
         |  end;
         |  function to_slv(A : t_vecX2_std_logic_vector) return std_logic_vector is
         |    variable hi : integer;
         |    variable lo : integer;
         |    variable cellBitWidth: integer;
         |    variable ret : std_logic_vector(bitWidth(A) - 1 downto 0);
         |  begin
         |    cellBitWidth := bitWidth(A(0));
         |    lo := bitWidth(A);
         |    for i in 0 to A'length-1 loop
         |      hi := lo - 1; lo := hi - cellBitWidth + 1;
         |      ret(hi downto lo) := to_slv(A(i));
         |    end loop;
         |    return ret;
         |  end;
         |  function to_t_vecX2_std_logic_vector(A : std_logic_vector; D2 : integer; D1 : integer; D0 : integer) return t_vecX2_std_logic_vector is
         |    variable hi : integer;
         |    variable lo : integer;
         |    variable cellBitWidth: integer := D1 * D0;
         |    variable ret : t_vecX2_std_logic_vector(0 to D2 - 1)(0 to D1 - 1)(D0 - 1 downto 0);
         |  begin
         |    lo := A'length;
         |    for i in 0 to D2-1 loop
         |      hi := lo - 1; lo := hi - cellBitWidth + 1;
         |      ret(i) := to_t_vecX1_std_logic_vector(A(hi downto lo), D1, D0);
         |    end loop;
         |    return ret;
         |  end;
         |  subtype t_opaque_Foo is t_vecX2_std_logic_vector(0 to 9)(0 to 15)(11 downto 0);
         |  function to_t_opaque_Foo(A : std_logic_vector) return t_opaque_Foo is
         |  begin
         |    return to_t_vecX2_std_logic_vector(A, 10, 16, 12);
         |  end;
         |  signal v : t_opaque_Foo;
         |  signal v_din : t_opaque_Foo;
         |begin
         |  process (all)
         |  begin
         |    v_din <= v;
         |    v_din <= to_t_vecX2_std_logic_vector(x, 10, 16, 12);
         |    y <= to_slv(v);
         |  end process;
         |  process (clk)
         |  begin
         |    if rising_edge(clk) then
         |      if rst = '1' then v <= (0 to 9 => (0 to 15 => x"000"));
         |      else v <= v_din;
         |      end if;
         |    end if;
         |  end process;
         |end Example_arch;""".stripMargin
    )
  }
  test("Vector local no conversion example") {
    class Example() extends RTDesign:
      val v = Bits(12) X 16 X 10 <> OUT
      v := all(all(all(0)))

    val top = (Example()).getCompiledCodeString
    assertNoDiff(
      top,
      """|library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Example_pkg.all;
         |
         |entity Example is
         |port (
         |  v : out t_vecX2_std_logic_vector(0 to 9)(0 to 15)(11 downto 0)
         |);
         |end Example;
         |
         |architecture Example_arch of Example is
         |begin
         |  process (all)
         |  begin
         |    v <= (0 to 9 => (0 to 15 => x"000"));
         |  end process;
         |end Example_arch;
         |""".stripMargin
    )
  }
  test("Opaque and vector global example") {
    case class Foo() extends Opaque(Bits(12) X 16 X 10)
    class Example() extends RTDesign:
      val x = Bits(1920) <> IN
      val y = Foo        <> OUT.REG init all(all(all(0))).as(Foo)
      y.din := x.as(Foo)

    val top = (Example()).getCompiledCodeString
    // TODO: consider if we want to leave the t_opaque_Foo under `getCompiledCodeString`
    assertNoDiff(
      top,
      """|subtype t_opaque_Foo is t_vecX2_std_logic_vector(0 to 9)(0 to 15)(11 downto 0);
         |
         |library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.Example_pkg.all;
         |
         |entity Example is
         |port (
         |  clk : in std_logic;
         |  rst : in std_logic;
         |  x : in std_logic_vector(1919 downto 0);
         |  y : out t_opaque_Foo
         |);
         |end Example;
         |
         |architecture Example_arch of Example is
         |  signal y_din : t_opaque_Foo;
         |begin
         |  process (all)
         |  begin
         |    y_din <= y;
         |    y_din <= to_t_vecX2_std_logic_vector(x, 10, 16, 12);
         |  end process;
         |  process (clk)
         |  begin
         |    if rising_edge(clk) then
         |      if rst = '1' then y <= (0 to 9 => (0 to 15 => x"000"));
         |      else y <= y_din;
         |      end if;
         |    end if;
         |  end process;
         |end Example_arch;
         |""".stripMargin
    )
  }
end PrintVHDLCodeSpec
