package dfhdl.internals

import scala.annotation.Annotation
import scala.collection.immutable.ListMap

trait HasClsMetaArgs:
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit
  protected def __clsMetaArgs: ClsMetaArgs = ClsMetaArgs.empty
end HasClsMetaArgs

final case class ClsMetaArgs(
    name: String,
    position: Position,
    docOpt: Option[String],
    annotations: List[Annotation]
) derives CanEqual

object ClsMetaArgs:
  def empty: ClsMetaArgs = ClsMetaArgs("???", Position.unknown, None, Nil)
