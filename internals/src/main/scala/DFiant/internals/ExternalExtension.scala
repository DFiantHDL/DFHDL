package DFiant.internals

import scala.annotation.implicitAmbiguous

//Request an implicit to limit extensions methods to be outside this scope
@implicitAmbiguous(
  "The function requiring this implicit cannot run within this scope"
)
trait ExternalExtension
object ExternalExtension {
  implicit val ev: ExternalExtension = new ExternalExtension {}
}

trait DisallowExternalExtensions {
  final protected implicit val __disallowExternalExtensions1
      : ExternalExtension = new ExternalExtension {}
  final protected implicit val __disallowExternalExtensions2
      : ExternalExtension = new ExternalExtension {}
}
