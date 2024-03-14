package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.{VerilogLinter, VHDLLinter, Builder}
import dfhdl.options.CompilerOptions
import dfhdl.backends

extension [D <: Design](cd: CompiledDesign[D])
  def lint(using
      verilogLinter: VerilogLinter,
      vhdlLinter: VHDLLinter,
      co: CompilerOptions
  ): CompiledDesign[D] =
    co.backend match
      case _: backends.verilog => verilogLinter.lint(verilogLinter.preprocess(cd))
      case _: backends.vhdl    => vhdlLinter.lint(vhdlLinter.preprocess(cd))
  def build(using builder: Builder, co: CompilerOptions): CompiledDesign[D] =
    builder.build(builder.preprocess(cd))
