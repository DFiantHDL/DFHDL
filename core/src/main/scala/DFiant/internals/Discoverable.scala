package DFiant.internals

trait Discoverable {
  protected[DFiant] trait __Dev {
    private var discovered : Boolean = false
    final protected[DFiant] def isNotDiscovered : Boolean = !discovered
    protected def discoveryDependencies : List[Discoverable] = List()
    protected def preDiscoveryRun() : Unit = {}
    protected def postDiscoveryRun() : Unit = {}
    final protected def discover() : Unit = {
      if (!discovered) {
        discovered = true
        val dependencies = discoveryDependencies
        preDiscoveryRun()
        dependencies.foreach(d => d.__dev.discover())
        postDiscoveryRun()
      }
    }
    private def discoverDependencies() : Unit = discoveryDependencies.foreach(d => d.__dev.discover())
    final private[DFiant] def rediscoverDependencies() : Unit = if (discovered) discoverDependencies()

  }
  private[DFiant] lazy val __dev : __Dev = new __Dev {}
}
