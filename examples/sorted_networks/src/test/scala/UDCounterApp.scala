object UDCounterApp extends App {
  val ud = new UDCounter {}
  ud.compileToVHDL.print()
}