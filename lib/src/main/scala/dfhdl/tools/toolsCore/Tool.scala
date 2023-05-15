package dfhdl.tools.toolsCore
import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign

trait Tool:
  protected def preprocess[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D] = cd
  final protected def exec[D <: Design](cd: CommittedDesign[D], cmd: String): CommittedDesign[D] =
    import scala.sys.process.*
    val newCD = preprocess(cd)
    Process(cmd).!
    newCD

trait Linter extends Tool:
  def lint[D <: Design](cd: CommittedDesign[D]): CommittedDesign[D]
object Linter:
  // default linter will be verilator
  given Linter = dfhdl.tools.linters.verilator
