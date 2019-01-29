package DFiant.internals

trait PostConstruction
object PostConstruction {
  implicit def ev(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
}

trait HasPostConstructionOnlyDefs {
  @scala.annotation.implicitAmbiguous("This is a post-construction definition only!")
  final protected implicit def __PostConstruction1(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
  final protected implicit def __PostConstruction2(implicit lp : shapeless.LowPriority) : PostConstruction = new PostConstruction {}
}
