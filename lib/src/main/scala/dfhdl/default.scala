package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.tools.toolsCore.Linter
import dfhdl.options.CommitOptions

extension [D <: Design](cd: CommittedDesign[D])
  def lint(using linter: Linter, co: CommitOptions): CommittedDesign[D] =
    linter.lint(linter.preprocess(cd))
