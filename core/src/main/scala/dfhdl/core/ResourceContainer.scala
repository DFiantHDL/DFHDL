package dfhdl.core
import dfhdl.internals.*
import scala.annotation.Annotation

class ResourceContainer extends OnCreateEvents, HasDFC, HasClsMetaArgs:
  final lazy val dfc: DFC = __dfc
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit = {}
end ResourceContainer
