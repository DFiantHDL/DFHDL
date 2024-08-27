//> using scala 3.5.0
//> using dep io.github.dfianthdl::dfhdl::0.7.0
//> using plugin io.github.dfianthdl:::dfhdl-plugin:0.7.0
//> using option -deprecation -language:implicitConversions

import dfhdl.* //import all the DFHDL goodness

/** Generates an 8-bit overlapping count */
@top class Counter8 extends RTDesign:
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
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to select vhdl compilation (default is verilog):
// given options.CompilerOptions.Backend = backends.vhdl
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDesignCodeAfter = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
