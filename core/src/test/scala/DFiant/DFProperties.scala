package DFiant
import org.scalacheck._
import DFiant.internals.{ClassArgs, Meta}

abstract class DFProperties(name: String) extends Properties(name) with DFDesign.Abstract {
  private[DFiant] final lazy val __ctx : DFDesign.Context = new DFBlock.Context(implicitly[Meta], null, null, ASIS, new DFDesign.DB.Mutable, ClassArgs.empty) {
    def newInterface(updatedCtx : DFInterface.Context) : Any = ???
  }
}
