package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CommittedDesign
import dfhdl.tools.toolsCore.{Linter, Builder}
import dfhdl.options.CommitOptions

extension [D <: Design](cd: CommittedDesign[D])
  def lint(using linter: Linter, co: CommitOptions): CommittedDesign[D] =
    linter.lint(linter.preprocess(cd))
  def build(using builder: Builder, co: CommitOptions): CommittedDesign[D] =
    builder.build(builder.preprocess(cd))
