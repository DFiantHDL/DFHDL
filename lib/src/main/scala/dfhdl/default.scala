package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.Builder
import dfhdl.options.{CompilerOptions, LinterOptions, BuilderOptions}
import dfhdl.backends

extension [D <: Design](cd: CompiledDesign[D])
  def lint(using
      co: CompilerOptions,
      lo: LinterOptions
  ): CompiledDesign[D] =
    co.backend match
      case _: backends.verilog => lo.verilogLinter.lint(lo.verilogLinter.preprocess(cd))
      case _: backends.vhdl    => lo.vhdlLinter.lint(lo.vhdlLinter.preprocess(cd))
  def build(using builder: Builder)(using CompilerOptions, BuilderOptions): CompiledDesign[D] =
    builder.build(builder.preprocess(cd))
