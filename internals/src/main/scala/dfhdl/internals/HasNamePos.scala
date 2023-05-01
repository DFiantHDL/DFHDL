package dfhdl.internals

import scala.annotation.Annotation
import scala.collection.immutable.ListMap

trait HasNamePos:
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation],
      args: ListMap[String, Any]
  ): Unit
