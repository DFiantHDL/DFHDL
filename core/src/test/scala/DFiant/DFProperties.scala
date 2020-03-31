package DFiant
import org.scalacheck._
import DFiant.internals.Meta

abstract class DFProperties(name: String) extends Properties(name) with DFDesign.Infra {
  private[DFiant] lazy val __ctx : DFDesign.Context = new DFBlock.Context(implicitly[Meta], null, new DFDesign.DB.Mutable)
}
