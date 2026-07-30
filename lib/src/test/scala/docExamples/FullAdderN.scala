package docExamples.fullAdderN
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
import dfhdl.* //import all the DFHDL goodness

class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum   <> a ^ b ^ c_in
  c_out <> a && b || b && c_in || c_in && a

class FullAdderN(val n: Int = 4) extends EDDesign:
  val a, b        = Bits(n) <> IN
  val c_in        = Bit     <> IN
  val sum         = Bits(n) <> OUT
  val c_out       = Bit     <> OUT
  val adder_c_in  = Bits(n) <> VAR
  val adder_c_out = Bits(n) <> VAR

  adder_c_in(0) <> c_in
  for (i <- 0 until n)
    val adder = new FullAdder1()
    adder.a     <> a(i)
    adder.b     <> b(i)
    adder.sum   <> sum(i)
    adder.c_in  <> adder_c_in(i)
    adder.c_out <> adder_c_out(i)
    if (i < n - 1)
      adder_c_in(i + 1) <> adder_c_out(i)
  c_out <> adder_c_out(n - 1)
end FullAdderN

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Select backend compiler:
given options.CompilerOptions.Backend = _.verilog
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDFHDLCode = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDFHDLCode = true
////////////////////////////////////////////////////////////////////////////////////////////////
