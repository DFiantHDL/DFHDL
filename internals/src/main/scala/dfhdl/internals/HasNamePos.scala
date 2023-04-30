package dfhdl.internals

import scala.collection.immutable.ListMap

trait HasNamePos:
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      args: ListMap[String, Any]
  ): Unit
