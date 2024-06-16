object SMA_DS2App extends App:
  import dfhdl.compiler.backend.verilog.v2001
  val sma = new SMA_DS2
  sma.compile.printGenFiles(colored = false)
