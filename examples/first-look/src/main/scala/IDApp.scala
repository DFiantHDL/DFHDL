object IDApp extends App {
  import DFiant.compiler.backend.verilog.v2001
  val id = new ID
  id.compile.printGenFiles(colored = false)
}
