package docExamples.trueDPR
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
import dfhdl.*

@top class TrueDPR(
    val DATA_WIDTH: Int <> CONST = 8,
    val ADDR_WIDTH: Int <> CONST = 8
) extends EDDesign:
  val ram = Bits(DATA_WIDTH) X (2 ** ADDR_WIDTH) <> VAR.SHARED

  val a, b = new RTDomain:
    val data = Bits(DATA_WIDTH) <> IN
    val addr = Bits(ADDR_WIDTH) <> IN
    val q    = Bits(DATA_WIDTH) <> OUT.REG
    val we   = Bit              <> IN

    if (we)
      ram(addr) := data
    q.din       := ram(addr)
end TrueDPR

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Elaboration Options:                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////
// Uncomment to set different clock and reset configurations:
// given options.ElaborationOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.ElaborationOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Select backend compiler:
given options.CompilerOptions.Backend = backends.verilog
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDesignCodeAfter = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
