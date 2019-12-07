package ZFiant
import DFiant.internals._

abstract class DFDesign(implicit val ctx : DFDesign.Context) extends DFBlock {

}

case class ContextOf[T <: DFDesign](meta : Meta, ownerOption : Option[DFBlock]) extends DFMember.Context {
  lazy val owner : DFBlock = ownerOption.get
}
object DFDesign {
  protected[ZFiant] type Context = DFBlock.Context
}