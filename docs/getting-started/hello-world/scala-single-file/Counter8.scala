//> using scala 3.8.1
//> using dep io.github.dfianthdl::dfhdl::0.17.0
//> using plugin io.github.dfianthdl:::dfhdl-plugin:0.17.0

import dfhdl.* //import all the DFHDL goodness

/** Generates an 8-bit overlapping count */
class Counter8 extends RTDesign:
  val cnt = UInt(8) <> OUT.REG init 0
  cnt.din := cnt + 1

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Elaboration Options:                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////
// Uncomment to set different clock and reset configurations:
// given options.ElaborationOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.ElaborationOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintBackendCode = true
// Uncomment to select vhdl compilation (default is verilog):
// given options.CompilerOptions.Backend = _.vhdl
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDFHDLCode = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDFHDLCode = true
////////////////////////////////////////////////////////////////////////////////////////////////
