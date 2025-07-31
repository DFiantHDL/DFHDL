package dfhdl.platforms.resources
import dfhdl.internals.*
import dfhdl.core.*
import scala.annotation.Annotation
import dfhdl.compiler.ir.constraints.Constraint
import scala.collection.mutable.ListBuffer

trait ResourceContext extends OnCreateEvents, HasDFC, HasClsMetaArgs:
  final lazy val dfc: DFC = __dfc
  final lazy val id: String = dfc.nameOpt.get
  private var resourceConstraints = ListBuffer[Constraint]()
  def getResourceConstraints: List[Constraint] =
    resourceConstraints.toList ++ dfc.annotations.collect {
      case constraint: Constraint => constraint
    }
  protected def __dfc: DFC =
    println("Severe error: missing DFHDL context!\nMake sure you enable the DFHDL compiler plugin.")
    sys.exit(1)
  protected def setClsNamePos(
      name: String,
      position: Position,
      docOpt: Option[String],
      annotations: List[Annotation]
  ): Unit =
    annotations.foreach {
      case constraint: dfhdl.hw.constraints.Constraint =>
        resourceConstraints += constraint.asIR
      case _ =>
    }
end ResourceContext
