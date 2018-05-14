package DFiant.internals

trait Discoverable {
  private var notDiscovered : Boolean = true
  final protected def isNotDiscovered : Boolean = notDiscovered
  protected def discoveryDepenencies : List[Discoverable]
  protected def discovery : Unit
  final protected def discover : Unit = {
    if (notDiscovered) {
      notDiscovered = false
      val dependencies = discoveryDepenencies
      dependencies.foreach(d => d.discover)
      discovery
    }
  }
}
