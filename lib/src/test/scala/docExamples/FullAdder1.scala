package docExamples.fullAdder1
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
import dfhdl.* //import all the DFHDL goodness

@top class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum   <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Select backend compiler:
given options.CompilerOptions.Backend = backends.vhdl
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDesignCodeAfter = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
