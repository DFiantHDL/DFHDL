object SMA_DSApp extends App {
  import dfhdl.compiler.backend.verilog.v2001
  val sma = new SMA_DS
  sma.compile.printGenFiles(colored = false)
}
