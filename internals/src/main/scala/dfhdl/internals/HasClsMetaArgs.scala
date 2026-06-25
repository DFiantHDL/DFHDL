package dfhdl.internals

import scala.annotation.Annotation
import scala.collection.immutable.ListMap

trait HasClsMetaArgs:
  // The compiler plugin injects an `override` of this for every DFHDL class in
  // the inheritance chain, each prepending its own meta:
  //   override def __clsMetaArgs = newClsMetaArgs :: super.__clsMetaArgs
  // so the result is the full chain, most-derived first (only concrete user
  // classes appear — abstract library bases are not processed). Containers build
  // their design block directly from this chain at creation, with no mutation:
  // the leaf (head) names the design/interface/resource, and for a blackbox IP
  // the base-most class in the chain names the IP type.
  protected def __clsMetaArgs: List[ClsMetaArgs] = Nil
end HasClsMetaArgs

final case class ClsMetaArgs(
    name: String,
    position: Position,
    docOpt: Option[String],
    annotations: List[Annotation]
) derives CanEqual

object ClsMetaArgs:
  def empty: ClsMetaArgs = ClsMetaArgs("???", Position.unknown, None, Nil)
