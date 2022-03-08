package StagesSpec

import DFiant.*
import DFiant.compiler.stages.vhdl.{printVHDLCode, getVHDLCode}
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class PrintVHDLCodeSpec extends StageSpec:
  class ID(using DFC) extends DFDesign:
    val x = DFSInt(16) <> IN
    val y = DFSInt(16) <> OUT
    y := x

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
         |  y := x
         |end ID_arch;
         |""".stripMargin
    )
  }
end PrintVHDLCodeSpec
