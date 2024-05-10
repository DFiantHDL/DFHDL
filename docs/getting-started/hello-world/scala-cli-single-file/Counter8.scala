//> using scala 3.4.0
//> using dep io.github.dfianthdl::dfhdl::0.4.4
//> using plugin io.github.dfianthdl:::dfhdl-plugin:0.4.4
//> using option -deprecation -language:implicitConversions

import dfhdl.* //import all the DFHDL goodness

/** Generates an 8-bit overlapping count */
class Counter8 extends RTDesign:
  val cnt = UInt(8) <> OUT.REG init 0
  cnt.din := cnt + 1

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to select vhdl compilation (default is verilog):
// given options.CompilerOptions.Backend = backends.vhdl
// Uncomment to enable printing design code before compilation (after elaboration):
// given options.CompilerOptions.PrintDesignCodeBefore = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
// Uncomment to set different clock and reset configurations:
// given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////

//The entry point to your compilation program starts here
@main def main = Counter8().compile.commit
