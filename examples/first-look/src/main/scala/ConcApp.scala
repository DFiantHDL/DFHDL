object ConcApp extends App:
  import dfhdl.compiler.backend.verilog.v2001
  val conc = new Conc
  conc.compile.printGenFiles(colored = false)
