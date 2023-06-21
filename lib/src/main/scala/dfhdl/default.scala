package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.{Linter, Builder}
import dfhdl.options.CompilerOptions

extension [D <: Design](cd: CompiledDesign[D])
  def lint(using linter: Linter, co: CompilerOptions): CompiledDesign[D] =
    linter.lint(linter.preprocess(cd))
  def build(using builder: Builder, co: CompilerOptions): CompiledDesign[D] =
    builder.build(builder.preprocess(cd))
