package DFiant.internals

trait Discoverable {
  trait __DevDiscoverable {
    private var discovered : Boolean = false
    final protected[DFiant] def isNotDiscovered : Boolean = !discovered
    protected def discoveryDepenencies : List[Discoverable] = List()
    protected def preDiscoveryRun() : Unit = {}
    protected def postDiscoveryRun() : Unit = {}
    final protected def discover() : Unit = {
      if (!discovered) {
        discovered = true
        val dependencies = discoveryDepenencies
        preDiscoveryRun()
        dependencies.foreach(d => d.__dev.discover())
        postDiscoveryRun()
      }
    }
    private def discoverDependencies() : Unit = discoveryDepenencies.foreach(d => d.__dev.discover())
    final private[DFiant] def rediscoverDependencies() : Unit = if (discovered) discoverDependencies()

  }
  val __dev : __DevDiscoverable = new __DevDiscoverable {}
}
