package dfhdl.tools.toolsCore
import dfhdl.core.{CommittedDesign, Design}

trait Tool:
  final protected def exec[D <: Design](cd: CommittedDesign[D], cmd: String): CommittedDesign[D] =
    import scala.sys.process.*
    Process(cmd).!
    cd

trait Linter extends Tool:
  def lint[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D]
object Linter:
  transparent inline given Linter =
    compiletime.error(
      "Missing an implicit linter tool argument.\nSolve this by importing the proper tool (e.g. `import tools.linters.verilator`)."
    )
