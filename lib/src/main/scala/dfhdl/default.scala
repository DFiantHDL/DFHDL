package dfhdl

import dfhdl.core.{Design, CommittedDesign}
import dfhdl.tools.toolsCore.Linter

extension [D <: Design](cd: CommittedDesign[D])
  def lint2(using linter: Linter): CommittedDesign[D] = linter.lint(cd)
