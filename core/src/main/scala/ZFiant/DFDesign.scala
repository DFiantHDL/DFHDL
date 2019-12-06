package ZFiant
import DFiant.internals._

abstract class DFDesign(implicit val ctx : DFDesign.Context) extends DFBlock {

}

object DFDesign {
//  class ContextOf[T <: DFDesign](meta : Meta) extends DFBlock.Context(meta, None)
  protected[ZFiant] type Context = DFBlock.Context
}