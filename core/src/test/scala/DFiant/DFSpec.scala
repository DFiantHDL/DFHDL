package DFiant

import DFiant.internals.{ClassArgs, Meta}
import org.scalatest.flatspec.AnyFlatSpec

abstract class DFSpec extends AnyFlatSpec with DFDesign.Abstract {
  private[DFiant] final lazy val __ctx : DFDesign.Context = new DFBlock.Context(
    implicitly[Meta], implicitly[Meta.SymbolOf[DFDesign]], null, ASIS, new DFDesign.DB.Mutable, ClassArgs.empty
  ) {
    def newInterface(updatedCtx : DFInterface.Context) : Any = ???
  }
}