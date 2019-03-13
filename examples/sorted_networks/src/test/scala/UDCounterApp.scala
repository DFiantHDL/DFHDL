object UDCounterApp extends App {
  val udCounter = new SampleFilterAccumulator {}
  udCounter.compileToVHDL.print()
}