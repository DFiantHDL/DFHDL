package dfhdl

import dfhdl.core.Design
import dfhdl.compiler.stages.CompiledDesign
import dfhdl.tools.toolsCore.Builder
import dfhdl.options.{CompilerOptions, LinterOptions, BuilderOptions, SimulatorOptions}
import dfhdl.backends

extension [D <: Design](cd: CompiledDesign[D])
  def lint(using
      co: CompilerOptions,
      lo: LinterOptions
  ): CompiledDesign[D] =
    co.backend match
      case _: backends.verilog => lo.verilogLinter.lint(lo.verilogLinter.lintPreprocess(cd))
      case _: backends.vhdl    => lo.vhdlLinter.lint(lo.vhdlLinter.lintPreprocess(cd))
  def simulate(using
      co: CompilerOptions,
      so: SimulatorOptions
  ): CompiledDesign[D] =
    val stagedDB = cd.stagedDB
    import stagedDB.getSet
    if (stagedDB.inSimulation)
      co.backend match
        case _: backends.verilog =>
          so.verilogSimulator.simulate(so.verilogSimulator.simulatePreprocess(cd))
        case _: backends.vhdl => so.vhdlSimulator.simulate(so.vhdlSimulator.simulatePreprocess(cd))
    else
      throw new Exception(
        s"The top design `${stagedDB.top.getName}` has ports and therefore cannot be simulated."
      )
  end simulate

  def build(using builder: Builder)(using CompilerOptions, BuilderOptions): CompiledDesign[D] =
    builder.build(builder.buildPreprocess(cd))
end extension
