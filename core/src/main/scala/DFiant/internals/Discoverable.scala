package DFiant.internals

trait Discoverable {
  private var discovered : Boolean = false
  final protected[DFiant] def isNotDiscovered : Boolean = !discovered
  protected def discoveryDepenencies : List[Discoverable]
  protected def postDiscoveryRun : Unit = {}
  final protected def discover : Unit = {
    if (!discovered) {
      discovered = true
      val dependencies = discoveryDepenencies
      dependencies.foreach(d => d.discover)
      postDiscoveryRun
    }
  }
  private def discoverDependencies : Unit = discoveryDepenencies.foreach(d => d.discover)
  final private[DFiant] def rediscoverDependencies : Unit = if (discovered) discoverDependencies
}
