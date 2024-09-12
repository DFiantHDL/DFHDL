package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.dropOutportRead
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class DropOutportReadSpec extends StageSpec:
  test("Basic outport read"):
    given options.CompilerOptions.Backend = backends.vhdl.v93
    class ID extends DFDesign:
      val x = SInt(16) <> IN
      val y = SInt(16) <> OUT
      y := y + x
    end ID
    val id = (new ID).dropOutportRead
    assertCodeString(
      id,
      """|class ID extends DFDesign:
         |  val x = SInt(16) <> IN
         |  val y = SInt(16) <> OUT
         |  val y_sig = SInt(16) <> VAR
         |  y <> y_sig
         |  y_sig := y_sig + x
         |end ID
         |""".stripMargin
    )
end DropOutportReadSpec
