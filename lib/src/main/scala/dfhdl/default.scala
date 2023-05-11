package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.tools.toolsCore.Linter

extension [D <: Design](cd: CommittedDesign[D])
  def lint(using linter: Linter): CommittedDesign[D] = linter.lint(cd)
