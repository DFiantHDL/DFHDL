package docExamples.regfile
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
import dfhdl.*

given options.CompilerOptions.LogLevel = options.LogLevel.TRACE
@top class RegFile(
    val DATA_WIDTH: Int <> CONST = 32,
    val REG_NUM: Int <> CONST    = 32
) extends RTDesign:
  val regs = Bits(DATA_WIDTH) X REG_NUM <> VAR.REG

  val rs1, rs2 = new RelatedDomain:
    val addr = Bits.until(REG_NUM) <> IN
    val data = Bits(DATA_WIDTH)    <> OUT.REG
    data.din := regs(addr)

  val rd = new RelatedDomain:
    val addr = Bits.until(REG_NUM) <> IN
    val data = Bits(DATA_WIDTH)    <> IN
    val wren = Bit                 <> IN
    if (wren) regs(addr).din := data
    regs(0).din              := all(0) // in RISC-V x0 is always 0
end RegFile

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
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDesignCodeAfter = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
