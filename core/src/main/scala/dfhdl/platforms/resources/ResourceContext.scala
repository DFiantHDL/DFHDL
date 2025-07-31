package dfhdl.platforms.resources
import dfhdl.internals.*
import dfhdl.core.*
import scala.annotation.Annotation

trait ResourceContext extends OnCreateEvents, HasDFC, HasClsMetaArgs:
  final lazy val dfc: DFC = __dfc
  final lazy val id: String = dfc.nameOpt.get
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit = {}
end ResourceContext
